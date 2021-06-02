use std::io::{ErrorKind, Read, Write};
use std::mem;
use std::process::exit;

use cranelift::prelude::*;
use cranelift_codegen::binemit::{NullStackMapSink, NullTrapSink};
use cranelift_codegen::ir::FuncRef;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};

use crate::errors::CompilerError;
use crate::ir::ops::{Op, OpType, LoopDecrement};
use crate::OptimizeConfig;
use crate::parser::Program;

struct Builder<'a> {
    pointer_type: Type,
    bcx: FunctionBuilder<'a>,
    heap_ptr: Value,
    env: Value,
    get_char_func: FuncRef,
    put_char_func: FuncRef,
}

impl<'a> Builder<'a> {
    pub fn append_ops(&mut self, ops: &[Op]) {
        for op in ops {
            match &op.op_type {
                OpType::Start => {
                    // ignore
                }
                OpType::IncPtr(value) => self.inc_ptr(*value),
                OpType::DecPtr(value) => self.dec_ptr(*value),
                OpType::Inc(offset, value) => self.inc(*offset, *value),
                OpType::Dec(offset, value) => self.dec(*offset, *value),
                OpType::Set(offset, value) => self.set(*offset, *value),
                OpType::Add(src_offset, dest_offset, multi) => self.add(*src_offset, *dest_offset, *multi),
                OpType::NzAdd(src_offset, dest_offset, multi) => self.nz_add(*src_offset, *dest_offset, *multi),
                OpType::CAdd(src_offset, dest_offset, value) => self.c_add(*src_offset, *dest_offset, *value),
                OpType::NzCAdd(src_offset, dest_offset, value) => self.nz_c_add(*src_offset, *dest_offset, *value),
                OpType::Sub(src_offset, dest_offset, multi) => self.sub(*src_offset, *dest_offset, *multi),
                OpType::NzSub(src_offset, dest_offset, multi) => self.nz_sub(*src_offset, *dest_offset, *multi),
                OpType::CSub(src_offset, dest_offset, value) => self.c_sub(*src_offset, *dest_offset, *value),
                OpType::NzCSub(src_offset, dest_offset, value) => self.nz_c_sub(*src_offset, *dest_offset, *value),
                OpType::Mul(src_offset, dest_offset, multi) => self.mul(*src_offset, *dest_offset, *multi),
                OpType::NzMul(src_offset, dest_offset, multi) => self.nz_mul(*src_offset, *dest_offset, *multi),
                OpType::Move(src_offset, dest_offset) => self._move(*src_offset, *dest_offset),
                OpType::Copy(src_offset, dest_offset) => self.copy(*src_offset, *dest_offset),
                OpType::DLoop(ops) => self.d_loop(ops),
                OpType::LLoop(ops, _) => self.l_loop(ops),
                OpType::ILoop(ops, step, decrement, _) => self.i_loop(ops, *step, *decrement),
                OpType::CLoop(ops, iterations, decrement, _) => self.c_loop(ops, *iterations, *decrement),
                OpType::TNz(ops, _) => self.tnz(ops),
                OpType::SearchZero(step) => self.search_zero(*step),
                OpType::PutChar(offset) => self.put_char(*offset),
                OpType::PutString(array) => self.put_string(array),
                OpType::GetChar(offset) => self.get_char(*offset),
            }
        }
    }

    fn const_u8(&mut self, value: u8) -> Value {
        self.bcx.ins().iconst(types::I8, value as i64)
    }

    fn load(&mut self, offset: isize) -> Value {
        self.bcx.ins().load(types::I8, MemFlags::new(), self.heap_ptr, offset as i32)
    }

    fn store(&mut self, offset: isize, value: Value) {
        self.bcx.ins().store(MemFlags::new(), value, self.heap_ptr, offset as i32);
    }

    fn inc_ptr(&mut self, value: usize) {
        let value = self.bcx.ins().iconst(self.pointer_type, value as i64);
        self.heap_ptr = self.bcx.ins().iadd(self.heap_ptr, value);
    }

    fn dec_ptr(&mut self, value: usize) {
        let value = self.bcx.ins().iconst(self.pointer_type, value as i64);
        self.heap_ptr = self.bcx.ins().isub(self.heap_ptr, value);
    }

    fn inc(&mut self, offset: isize, value: u8) {
        let value = self.const_u8(value);
        let heap_value = self.load(offset);
        let incremented = self.bcx.ins().iadd(heap_value, value);
        self.store(offset, incremented);
    }

    fn dec(&mut self, offset: isize, value: u8) {
        let value = self.const_u8(value);
        let heap_value = self.load(offset);
        let incremented = self.bcx.ins().isub(heap_value, value);
        self.store(offset, incremented);
    }

    fn set(&mut self, offset: isize, value: u8) {
        let value = self.const_u8(value);
        self.store(offset, value);
    }

    fn nz_add(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        let mut source = self.load(src_offset);
        let target = self.load(dest_offset);

        if multi > 1 {
            let multi = self.const_u8(multi);
            source = self.bcx.ins().imul(source, multi);
        }

        let target = self.bcx.ins().iadd(target, source);

        self.store(dest_offset, target);
    }

    fn add(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        self.nz_add(src_offset, dest_offset, multi);
        self.set(src_offset, 0);
    }

    fn nz_c_add(&mut self, _src_offset: isize, dest_offset: isize, value: u8) {
        let target = self.load(dest_offset);
        let value = self.const_u8(value);
        let target = self.bcx.ins().iadd(target, value);
        self.store(dest_offset, target);
    }

    fn c_add(&mut self, src_offset: isize, dest_offset: isize, value: u8) {
        self.nz_c_add(src_offset, dest_offset, value);
        self.set(src_offset, 0);
    }

    fn nz_sub(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        let mut source = self.load(src_offset);
        let target = self.load(dest_offset);

        if multi > 1 {
            let multi = self.const_u8(multi);
            source = self.bcx.ins().imul(source, multi);
        }

        let target = self.bcx.ins().isub(target, source);

        self.store(dest_offset, target);
    }

    fn sub(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        self.nz_sub(src_offset, dest_offset, multi);
        self.set(src_offset, 0);
    }

    fn nz_c_sub(&mut self, _src_offset: isize, dest_offset: isize, value: u8) {
        let target = self.load(dest_offset);
        let value = self.const_u8(value);
        let target = self.bcx.ins().isub(target, value);
        self.store(dest_offset, target);
    }

    fn c_sub(&mut self, src_offset: isize, dest_offset: isize, value: u8) {
        self.nz_c_sub(src_offset, dest_offset, value);
        self.set(src_offset, 0);
    }

    fn nz_mul(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        let source = self.load(src_offset);

        let multi = self.const_u8(multi);
        let target = self.bcx.ins().imul(source, multi);

        self.store(dest_offset, target);
    }

    fn mul(&mut self, src_offset: isize, dest_offset: isize, multi: u8) {
        self.nz_mul(src_offset, dest_offset, multi);
        self.set(src_offset, 0);
    }

    fn copy(&mut self, src_offset: isize, dest_offset: isize) {
        let value = self.load(src_offset);
        self.store(dest_offset, value);
    }

    fn _move(&mut self, src_offset: isize, dest_offset: isize) {
        self.copy(src_offset, dest_offset);
        self.set(src_offset, 0);
    }

    fn get_char(&mut self, offset: isize) {
        let results = self.bcx.ins().call(self.get_char_func, &[self.env]);
        let results = self.bcx.inst_results(results)[0];
        self.store(offset, results);
    }

    fn put_char(&mut self, offset: isize) {
        let value = self.load(offset);
        self.bcx.ins().call(self.put_char_func, &[self.env, value]);
    }

    fn put_string(&mut self, array: &[u8]) {
        for value in array {
            let value = self.const_u8(*value);
            self.bcx.ins().call(self.put_char_func, &[self.env, value]);
        }
    }

    fn d_loop(&mut self, ops: &[Op]) {
        let head = self.bcx.create_block();
        self.bcx.append_block_param(head, self.pointer_type);

        let body = self.bcx.create_block();
        self.bcx.append_block_param(body, self.pointer_type);

        let next = self.bcx.create_block();
        self.bcx.append_block_param(next, self.pointer_type);

        self.bcx.ins().fallthrough(head, &[self.heap_ptr]);

        // Head with condition
        self.bcx.switch_to_block(head);
        self.heap_ptr = self.bcx.block_params(head)[0];

        let value = self.load(0);
        self.bcx.ins().brz(value, next, &[self.heap_ptr]);
        self.bcx.ins().fallthrough(body, &[self.heap_ptr]);

        // Loop Body
        self.bcx.switch_to_block(body);
        self.append_ops(ops);
        self.bcx.ins().jump(head, &[self.heap_ptr]);

        // Start next block after loop
        self.bcx.switch_to_block(next);
        self.heap_ptr = self.bcx.block_params(next)[0];
        // This is not be needed but it seems to fix a possible bug in cranelift
        self.set(0, 0);
    }

    fn l_loop(&mut self, ops: &[Op]) {
        let head = self.bcx.create_block();
        self.bcx.append_block_param(head, self.pointer_type);

        let body = self.bcx.create_block();
        self.bcx.append_block_param(body, self.pointer_type);

        let next = self.bcx.create_block();
        self.bcx.append_block_param(next, self.pointer_type);

        self.bcx.ins().fallthrough(head, &[self.heap_ptr]);

        // Head with condition
        self.bcx.switch_to_block(head);
        self.heap_ptr = self.bcx.block_params(head)[0];

        let init_heap_ptr = self.heap_ptr;

        let value = self.load(0);
        self.bcx.ins().brz(value, next, &[init_heap_ptr]);
        self.bcx.ins().fallthrough(body, &[init_heap_ptr]);

        // Loop Body
        self.bcx.switch_to_block(body);
        let heap_ptr = self.bcx.block_params(body)[0];
        self.heap_ptr = heap_ptr;
        self.append_ops(ops);
        self.bcx.ins().jump(head, &[heap_ptr]);

        // Start next block after loop
        self.bcx.switch_to_block(next);
        self.heap_ptr = self.bcx.block_params(next)[0];

        // This is not be needed but it seems to fix a possible bug in cranelift
        self.set(0, 0);
    }

    fn i_loop(&mut self, ops: &[Op], step: u8, decrement: LoopDecrement) {
        let head = self.bcx.create_block();
        self.bcx.append_block_param(head, self.pointer_type);
        self.bcx.append_block_param(head, types::I8);

        let body = self.bcx.create_block();
        self.bcx.append_block_param(body, self.pointer_type);
        self.bcx.append_block_param(body, types::I8);

        let next = self.bcx.create_block();
        self.bcx.append_block_param(next, self.pointer_type);


        let value = self.load(0);

        self.bcx.ins().fallthrough(head, &[self.heap_ptr, value]);

        // Head with condition
        self.bcx.switch_to_block(head);
        self.heap_ptr = self.bcx.block_params(head)[0];
        let counter = self.bcx.block_params(head)[1];

        let init_heap_ptr = self.heap_ptr;

        self.bcx.ins().brz(counter, next, &[init_heap_ptr]);
        self.bcx.ins().fallthrough(body, &[init_heap_ptr, counter]);

        // Loop Body
        self.bcx.switch_to_block(body);
        let heap_ptr = self.bcx.block_params(body)[0];
        self.heap_ptr = heap_ptr;
        let mut counter = self.bcx.block_params(body)[1];
        let step = self.const_u8(step);

        if decrement == LoopDecrement::Pre {
            counter = self.bcx.ins().isub(counter, step);
            self.store(0, counter);
        }

        self.append_ops(ops);

        self.heap_ptr = heap_ptr;

        if decrement == LoopDecrement::Post {
            counter = self.bcx.ins().isub(counter, step);
            self.store(0, counter);
        } else if decrement == LoopDecrement::Auto {
            counter = self.bcx.ins().isub(counter, step);
        }

        self.bcx.ins().jump(head, &[heap_ptr, counter]);

        // Start next block after loop
        self.bcx.switch_to_block(next);
        self.heap_ptr = self.bcx.block_params(next)[0];
        self.set(0, 0);
    }

    fn c_loop(&mut self, ops: &[Op], iterations: u8, decrement: LoopDecrement) {
        let head = self.bcx.create_block();
        self.bcx.append_block_param(head, self.pointer_type);
        self.bcx.append_block_param(head, types::I8);

        let body = self.bcx.create_block();
        self.bcx.append_block_param(body, self.pointer_type);
        self.bcx.append_block_param(body, types::I8);

        let next = self.bcx.create_block();
        self.bcx.append_block_param(next, self.pointer_type);

        let iterations = self.const_u8(iterations);

        if decrement == LoopDecrement::Post {
            self.store(0, iterations);
        }

        self.bcx.ins().fallthrough(head, &[self.heap_ptr, iterations]);

        // Head with condition
        self.bcx.switch_to_block(head);
        self.heap_ptr = self.bcx.block_params(head)[0];
        let counter = self.bcx.block_params(head)[1];

        let init_heap_ptr = self.heap_ptr;

        self.bcx.ins().brz(counter, next, &[init_heap_ptr]);
        self.bcx.ins().fallthrough(body, &[init_heap_ptr, counter]);

        // Loop Body
        self.bcx.switch_to_block(body);
        let heap_ptr = self.bcx.block_params(body)[0];
        self.heap_ptr = heap_ptr;
        let mut counter = self.bcx.block_params(body)[1];

        let step = self.const_u8(1);

        if decrement == LoopDecrement::Pre {
            counter = self.bcx.ins().isub(counter, step);
            self.store(0, counter);
        }

        self.append_ops(ops);

        self.heap_ptr = heap_ptr;

        if decrement == LoopDecrement::Post {
            counter = self.bcx.ins().isub(counter, step);
            self.store(0, counter);
        } else if decrement == LoopDecrement::Auto {
            counter = self.bcx.ins().isub(counter, step);
        }

        self.bcx.ins().jump(head, &[heap_ptr, counter]);

        // Start next block after loop
        self.bcx.switch_to_block(next);
        self.heap_ptr = self.bcx.block_params(next)[0];
        self.set(0, 0);
    }

    fn tnz(&mut self, ops: &[Op]) {
        let body = self.bcx.create_block();
        self.bcx.append_block_param(body, self.pointer_type);

        let next = self.bcx.create_block();
        self.bcx.append_block_param(next, self.pointer_type);

        let value = self.load(0);
        self.bcx.ins().brz(value, next, &[self.heap_ptr]);
        self.bcx.ins().fallthrough(body, &[self.heap_ptr]);

        // Condition body
        self.bcx.switch_to_block(body);
        self.heap_ptr = self.bcx.block_params(body)[0];

        let init_heap_ptr = self.heap_ptr;

        self.append_ops(ops);

        self.heap_ptr = init_heap_ptr;
        self.set(0, 0);

        self.bcx.ins().fallthrough(next, &[self.heap_ptr]);

        // Start next block after loop
        self.bcx.switch_to_block(next);
        self.heap_ptr = self.bcx.block_params(next)[0];
    }

    fn search_zero(&mut self, step: isize) {
        let head = self.bcx.create_block();
        self.bcx.append_block_param(head, self.pointer_type);

        let body = self.bcx.create_block();
        self.bcx.append_block_param(body, self.pointer_type);

        let next = self.bcx.create_block();
        self.bcx.append_block_param(next, self.pointer_type);

        self.bcx.ins().fallthrough(head, &[self.heap_ptr]);

        // Head with condition
        self.bcx.switch_to_block(head);
        self.heap_ptr = self.bcx.block_params(head)[0];

        let value = self.load(0);
        self.bcx.ins().brz(value, next, &[self.heap_ptr]);
        self.bcx.ins().fallthrough(body, &[self.heap_ptr]);

        // Body
        self.bcx.switch_to_block(body);
        self.heap_ptr = self.bcx.block_params(body)[0];

        if step > 0 {
            self.inc_ptr(step as usize);
        } else {
            self.dec_ptr(-step as usize);
        }

        self.bcx.ins().jump(head, &[self.heap_ptr]);

        // Start next block after loop
        self.bcx.switch_to_block(next);
        self.heap_ptr = self.bcx.block_params(next)[0];
    }

    fn unwrap(self) -> FunctionBuilder<'a> {
        self.bcx
    }
}

struct Environment<'a, 'b> {
    input: &'a mut dyn Read,
    output: &'b mut dyn Write,
}

impl<'a, 'b> Environment<'a, 'b> {
    pub fn new(input: &'a mut dyn Read, output: &'b mut dyn Write) -> Environment<'a, 'b> {
        Environment {
            input,
            output,
        }
    }
}

fn get_char(env: *mut Environment) -> u8 {
    let input = unsafe { &mut (*env).input };
    let mut buf = [0u8; 1];
    if let Err(error) = input.read_exact(&mut buf) {
        // In case of EOF the system will read 0 as a fallback
        if error.kind() != ErrorKind::UnexpectedEof {
            eprintln!("Input error: {:?}", error);
            eprintln!("Terminating!!!");
            exit(1);
        }
    };
    buf[0]
}

fn put_char(env: *mut Environment, value: u8) {
    let output = unsafe { &mut (*env).output };
    if let Err(error) = if value != 0 && value.is_ascii() {
        write!(output, "{}", value as char)
    } else {
        write!(output, "\\0x{:x}", value)
    } {
        eprintln!("Output error: {:?}", error);
        eprintln!("Terminating!!!");
        exit(1);
    }
}

/// A compiled program that can be executed
pub struct CompiledJitModule {
    module: Option<JITModule>,
    main_func: FuncId,
    clir: String,
}

impl CompiledJitModule {
    /// Compile program
    pub fn new(program: &Program, opt_mode: &OptimizeConfig) -> Result<CompiledJitModule, CompilerError> {
        let mut flag_builder = settings::builder();

        if let Some(jit_level) = &opt_mode.jit_level {
            flag_builder.set("opt_level", jit_level).unwrap();
        } else if opt_mode.optimize() {
            flag_builder.set("opt_level", "speed").unwrap();
        } else {
            flag_builder.set("opt_level", "none").unwrap();
        }

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });

        let isa = isa_builder.finish(settings::Flags::new(flag_builder));

        let mut jit_builder = JITBuilder::with_isa(isa, default_libcall_names());
        jit_builder.symbol("get_char", get_char as *const u8);
        jit_builder.symbol("put_char", put_char as *const u8);

        let mut module = JITModule::new(jit_builder);
        let pointer_type = module.target_config().pointer_type();

        let mut get_char_sig = module.make_signature();
        get_char_sig.params.push(AbiParam::new(pointer_type));
        get_char_sig.returns.push(AbiParam::new(types::I8));

        let get_char_func = module.declare_function("get_char", Linkage::Import, &get_char_sig).unwrap();

        let mut put_char_sig = module.make_signature();
        put_char_sig.params.push(AbiParam::new(pointer_type));
        put_char_sig.params.push(AbiParam::new(types::I8));

        let put_char_func = module.declare_function("put_char", Linkage::Import, &put_char_sig).unwrap();

        let mut ctx = module.make_context();
        let mut func_ctx = FunctionBuilderContext::new();

        let mut trap_sink = NullTrapSink {};
        let mut stack_map_sink = NullStackMapSink {};


        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(pointer_type));
        sig.params.push(AbiParam::new(pointer_type));

        let func = module
            .declare_function("main", Linkage::Local, &sig)
            .unwrap();

        ctx.func.signature = sig;
        ctx.func.name = ExternalName::user(0, func.as_u32());

        let clir = {
            let mut bcx: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

            let block = bcx.create_block();

            bcx.switch_to_block(block);
            bcx.append_block_params_for_function_params(block);

            let heap_ptr = bcx.block_params(block)[0];
            let env = bcx.block_params(block)[1];

            let get_char_func = module.declare_func_in_func(get_char_func, &mut bcx.func);
            let put_char_func = module.declare_func_in_func(put_char_func, &mut bcx.func);

            let mut builder = Builder {
                pointer_type,
                bcx,
                heap_ptr,
                env,
                get_char_func,
                put_char_func,
            };

            builder.append_ops(&program.ops);

            bcx = builder.unwrap();

            bcx.ins().return_(&[]);

            bcx.seal_all_blocks();

            // Check if all blocks are basic because will causes panics on finalize
            for block in bcx.func.layout.blocks() {
                if let Err((_inst, msg)) = bcx.func.is_block_basic(block) {
                    println!("{}", bcx.func);
                    return Err(CompilerError::InternalCompilerError {
                        message: format!("Bad block: {}, {}", msg, block)
                    });
                }
            }

            bcx.finalize();

            format!("{:?}", bcx.func)
        };

        module
            .define_function(func, &mut ctx, &mut trap_sink, &mut stack_map_sink)
            .unwrap();
        module.clear_context(&mut ctx);

        module.finalize_definitions();

        Ok(CompiledJitModule {
            module: Some(module),
            main_func: func,
            clir,
        })
    }

    /// Execute program
    ///
    /// After execution the used heap is returned
    ///
    ///
    pub fn execute<R: Read, W: Write>(&self, mut input: R, mut output: W) -> Vec<u8> {
        let module = self.module.as_ref().expect("Module exists");

        let code = module.get_finalized_function(self.main_func);

        let mut env = Box::new(Environment::new(&mut input, &mut output));

        let exec = unsafe { mem::transmute::<_, fn(*mut u8, *mut Environment)>(code) };

        let mut heap = vec![0_u8; 1024 * 1024];

        exec(heap.as_mut_ptr(), &mut *env);

        heap
    }

    /// Get the cranelift intermediate representation used for the compiled program
    pub fn get_clir(&self) -> String {
        self.clir.clone()
    }
}

impl Drop for CompiledJitModule {
    fn drop(&mut self) {
        unsafe {
            if let Some(module) = self.module.take() {
                module.free_memory();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::{Cursor, Read, Write};

    use crate::{optimize, OptimizeConfig, parse};
    use crate::ir::ops::{Op, LoopDecrement};
    use crate::parser::Program;

    use super::CompiledJitModule;
    use crate::ir::opt_info::BlockInfo;

    fn run<R: Read, W: Write>(program: &Program, input: R, output: W) -> Vec<u8> {
        CompiledJitModule::new(program, &OptimizeConfig::o2()).unwrap().execute(input, output)
    }

    #[test]
    fn test_simple() {
        let mut program = parse("++>>++++-<-").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 2);
        assert_eq!(heap[1], 255);
        assert_eq!(heap[2], 3);
    }

    #[test]
    fn test_output() {
        let mut program = parse(",+.").unwrap();
        optimize(&mut program);

        let input = b"a";
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, b"b");
    }

    #[test]
    fn test_hello_world() {
        let mut program = parse("
                >+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]
                >++++++++[<++++>-] <.>+++++++++++[<++++++++>-]<-.--------.+++
                .------.--------.[-]>++++++++[<++++>- ]<+.[-]++++++++++."
        ).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, b"Hello world!\n");
    }

    #[test]
    fn test_dloop() {
        let mut program = parse(">,<++[->+<]>."
        ).unwrap();
        optimize(&mut program);

        let input = b"1";
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, b"3");
    }

    #[test]
    fn test_hello_world_v2() {
        let mut program = parse(include_str!("../../../test_programs/hello_world.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, b"Hello World!\n");
    }

    #[test]
    fn test_count_loop() {
        let mut program = parse("++++[->++<]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 8);
    }

    #[test]
    fn test_count_loop_inv() {
        let mut program = parse("++++[->++<]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 8);
    }

    #[test]
    fn test_totally_empty() {
        let mut program = parse("").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
    }

    #[test]
    fn test_optimized_empty() {
        let mut program = parse("[>+++++<-]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
    }

    #[test]
    fn test_multiply() {
        let mut program = parse(">>++<<++[>+++++<-]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 10);
    }

    #[test]
    fn test_divide() {
        let mut program = parse(">>++<<++++++++++++[>+<---]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 4);
    }

    #[test]
    fn test_pow_5_3() {
        let mut program = parse("+++++[>+++++[>+++++<-]<-]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 0);
        assert_eq!(heap[2], 125);
    }

    #[test]
    fn test_bad_count_loop() {
        let mut program = parse("++++++++[---->+<++]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 4);
    }

    #[test]
    fn test_conditional_set() {
        let mut program = parse("+>>++<<[->[-]++++++<]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 6);
        assert_eq!(heap[2], 2);
    }

    #[test]
    fn test_overwrite_prev_set() {
        let mut program = parse("++++[-]++").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 2);
    }

    #[test]
    fn test_constant_loop() {
        let mut program = parse("+++++++++++++[>+>++>>+++<<<<-]->>>>>>+").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 255);
        assert_eq!(heap[1], 13);
        assert_eq!(heap[2], 26);
        assert_eq!(heap[3], 0);
        assert_eq!(heap[4], 39);
        assert_eq!(heap[5], 0);
        assert_eq!(heap[6], 1);
    }

    #[test]
    fn test_hello_fizz() {
        let mut program = parse(include_str!("../../../test_programs/fizz.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, b"1W\n");
    }

    #[test]
    fn test_hello_fizzbuzz() {
        let mut program = parse(include_str!("../../../test_programs/fizzbuzz.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, b"987654321");
    }

    #[test]
    fn test_bottles() {
        let mut program = parse(include_str!("../../../test_programs/bottles.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, include_bytes!("../../../test_programs/bottles.bf.out"));
    }

    #[test]
    fn test_factor() {
        let mut program = parse(include_str!("../../../test_programs/factor.bf")).unwrap();
        optimize(&mut program);

        let input = include_bytes!("../../../test_programs/factor.bf.in");
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, include_bytes!("../../../test_programs/factor.bf.out"));
    }

    #[test]
    fn test_life() {
        let mut program = parse(include_str!("../../../test_programs/life.bf")).unwrap();
        optimize(&mut program);

        let input = include_bytes!("../../../test_programs/life.bf.in");
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, include_bytes!("../../../test_programs/life.bf.out"));
    }

    #[test]
    fn test_ptr_ops() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 1),
                Op::inc_ptr(1..2, 2),
                Op::set(0..1, 3),
                Op::dec_ptr(1..2, 1),
                Op::set(0..1, 2),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 1);
        assert_eq!(heap[1], 2);
        assert_eq!(heap[2], 3);
        assert_eq!(heap[3], 0);
    }

    #[test]
    fn test_nz_add() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::nz_add(1..2, 1, 1),
                Op::nz_add(2..3, 3, 2)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 5);
        assert_eq!(heap[1], 5);
        assert_eq!(heap[2], 0);
        assert_eq!(heap[3], 10);
    }

    #[test]
    fn test_add() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::add(1..2, 1, 2),
                Op::add(2..3, 3, 1)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 10);
        assert_eq!(heap[2], 0);
        assert_eq!(heap[3], 0);
    }

    #[test]
    fn test_nz_add_add() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::nz_add(1..2, 1, 1),
                Op::nz_add(2..3, 3, 2),
                Op::add(2..3, 4, 3)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 5);
        assert_eq!(heap[2], 0);
        assert_eq!(heap[3], 10);
        assert_eq!(heap[4], 15);
    }

    #[test]
    fn test_nz_c_add() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 2),
                Op::nz_c_add(1..2, 1, 4),
                Op::nz_c_add(2..3, 3, 8)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 2);
        assert_eq!(heap[1], 4);
        assert_eq!(heap[2], 0);
        assert_eq!(heap[3], 8);
    }

    #[test]
    fn test_c_add() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 2),
                Op::c_add(1..2, 1, 4),
                Op::c_add(2..3, 3, 8)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 4);
        assert_eq!(heap[2], 0);
        assert_eq!(heap[3], 8);
    }

    #[test]
    fn test_sub() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::sub(1..2, 1, 2),
                Op::sub(2..3, 3, 1)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 0u8.wrapping_sub(10));
        assert_eq!(heap[2], 0);
        assert_eq!(heap[3], 0);
    }

    #[test]
    fn test_nz_sub_sub() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::nz_sub(1..2, 1, 1),
                Op::nz_sub(2..3, 3, 2),
                Op::sub(2..3, 4, 3)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 0u8.wrapping_sub(5));
        assert_eq!(heap[2], 0);
        assert_eq!(heap[3], 0u8.wrapping_sub(10));
        assert_eq!(heap[4], 0u8.wrapping_sub(15));
    }

    #[test]
    fn test_nz_c_sub() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 2),
                Op::nz_c_sub(1..2, 1, 4),
                Op::nz_c_sub(2..3, 3, 8)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 2);
        assert_eq!(heap[1], 0u8.wrapping_sub(4));
        assert_eq!(heap[2], 0);
        assert_eq!(heap[3], 0u8.wrapping_sub(8));
    }

    #[test]
    fn test_c_sub() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 2),
                Op::c_sub(1..2, 1, 4),
                Op::c_sub(2..3, 3, 8)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 0u8.wrapping_sub(4));
        assert_eq!(heap[2], 0);
        assert_eq!(heap[3], 0u8.wrapping_sub(8));
    }

    #[test]
    fn test_mul() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::mul(1..2, 1, 2),
                Op::mul(2..3, 3, 1)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(&heap[0..5], &[0, 10, 0, 0, 0]);
    }

    #[test]
    fn test_nz_mul_mul() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::nz_mul(1..2, 1, 2),
                Op::nz_mul(2..3, 3, 3),
                Op::mul(2..3, 4, 4)
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(&heap[0..6], &[0, 10, 0, 15, 20, 0]);
    }

    #[test]
    fn test_move() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 2),
                Op::_move(1..2, 0, 1),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 2);
        assert_eq!(heap[2], 0);
    }

    #[test]
    fn test_copy() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 2),
                Op::copy(1..2, 0, 1),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 2);
        assert_eq!(heap[1], 2);
        assert_eq!(heap[2], 0);
    }

    #[test]
    fn test_get_char() {
        let program = Program {
            ops: vec![
                Op::get_char(0..1),
            ]
        };

        let input = b"\x02";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 2);
        assert_eq!(heap[1], 0);
    }

    #[test]
    fn test_put_char() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 2),
                Op::put_char(1..2),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 2);
        assert_eq!(output[0], 2);
    }

    #[test]
    fn test_d_loop() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::d_loop(1..4, vec![
                    Op::dec(1..2, 1),
                    Op::inc_ptr(1..2, 1),
                    Op::inc(1..2, 1),
                    Op::inc(1..2, 1),
                    Op::dec_ptr(1..2, 1),
                ]),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 10);
    }

    #[test]
    fn test_l_loop() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::l_loop(1..4, vec![
                    Op::inc_ptr(1..2, 1),
                    Op::dec_ptr(1..2, 1),
                    Op::dec(1..2, 1),
                    Op::inc_ptr(1..2, 2),
                    Op::inc(1..2, 1),
                    Op::inc(1..2, 1),
                ], BlockInfo::new_empty()),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 0);
        assert_eq!(heap[2], 10);
    }

    #[test]
    fn test_i_loop() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::i_loop(1..4, vec![
                    Op::inc_ptr(1..2, 2),
                    Op::inc(1..2, 1),
                    Op::inc(1..2, 1),
                ], 1, BlockInfo::new_empty()),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 0);
        assert_eq!(heap[2], 10);
    }

    #[test]
    fn test_i_loop_step() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 10),
                Op::i_loop(1..4, vec![
                    Op::inc_ptr(1..2, 2),
                    Op::inc(1..2, 1),
                    Op::inc(1..2, 1),
                ], 2, BlockInfo::new_empty()),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 0);
        assert_eq!(heap[2], 10);
    }

    #[test]
    fn test_i_loop_pre() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::i_loop_with_decrement(1..4, vec![
                    Op::inc_ptr(1..2, 1),
                    Op::inc(1..2, 1),
                    Op::dec_ptr(1..2, 1),
                    Op::put_char(1..2),
                ], 1, LoopDecrement::Pre, BlockInfo::new_empty()),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 5);
        assert_eq!(&output, b"\x04\x03\x02\x01\\0x0");
    }

    #[test]
    fn test_i_loop_post() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 5),
                Op::i_loop_with_decrement(1..4, vec![
                    Op::put_char(1..2),
                ], 1, LoopDecrement::Post, BlockInfo::new_empty()),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(&output, &[5, 4, 3, 2, 1]);
    }

    #[test]
    fn test_c_loop() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 10),
                Op::c_loop(1..4, vec![
                    Op::inc_with_offset(1..2, 2, 1),
                    Op::inc_with_offset(1..2, 2, 1),
                ], 5, BlockInfo::new_empty()),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 0);
        assert_eq!(heap[2], 10);
    }

    #[test]
    fn test_c_loop_pre() {
        let program = Program {
            ops: vec![
                Op::c_loop_with_decrement(1..4, vec![
                    Op::inc_ptr(1..2, 1),
                    Op::inc(1..2, 1),
                    Op::dec_ptr(1..2, 1),
                    Op::put_char(1..2),
                ], 5, LoopDecrement::Pre, BlockInfo::new_empty()),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 5);
        assert_eq!(&output, b"\x04\x03\x02\x01\\0x0");
    }

    #[test]
    fn test_c_loop_post() {
        let program = Program {
            ops: vec![
                Op::c_loop_with_decrement(1..4, vec![
                    Op::put_char(1..2),
                ], 5, LoopDecrement::Post, BlockInfo::new_empty()),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(&output, &[5, 4, 3, 2, 1]);
    }

    #[test]
    fn test_tnz() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 1),
                Op::t_nz(1..4, vec![
                    Op::set_with_offset(1..2, 2, 10),
                ], BlockInfo::new_empty()),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 0);
        assert_eq!(heap[2], 10);
    }

    #[test]
    fn test_tnz_false() {
        let program = Program {
            ops: vec![
                Op::t_nz(1..4, vec![
                    Op::set_with_offset(1..2, 2, 10),
                ], BlockInfo::new_empty()),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(heap[0], 0);
        assert_eq!(heap[1], 0);
        assert_eq!(heap[2], 0);
    }

    #[test]
    fn test_search_zero() {
        let program = Program {
            ops: vec![
                Op::inc_ptr(0..1, 1),
                Op::set(1..2, 1),
                Op::inc_ptr(0..1, 1),
                Op::set(1..2, 2),
                Op::inc_ptr(0..1, 1),
                Op::set(1..2, 3),
                Op::search_zero(0..1, -1),
                Op::set(1..2, 4),
            ]
        };

        let input = b"";
        let mut output = Vec::new();

        let heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(&heap[0..5], &[4, 1, 2, 3, 0]);
    }

    #[test]
    fn test_test_io() {
        let mut program = parse(",[.,]").unwrap();
        optimize(&mut program);

        let input = b"0123456789aZ";
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, b"0123456789aZ");
    }


    #[test]
    fn test_cell_size() {
        let mut program = parse(include_str!("../../../test_programs/cell_size.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let _heap = run(&program, Cursor::new(input), &mut output);

        assert_eq!(output, b"8 bit cells\n");
    }
}