use crate::parser::{Program, Op, OpType};

use cranelift::prelude::*;
use cranelift_codegen::binemit::{NullStackMapSink, NullTrapSink};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, Linkage, Module, FuncId};
use std::mem;
use std::io::{Read, Write, ErrorKind};
use cranelift_codegen::ir::FuncRef;
use std::cmp::Ordering;
use crate::errors::CompilerError;
use std::process::exit;

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
                OpType::Move(src_offset, dest_offset) => self._move(*src_offset, *dest_offset),
                OpType::Copy(src_offset, dest_offset) => self.copy(*src_offset, *dest_offset),
                OpType::DLoop(ops) => self.d_loop(ops),
                OpType::LLoop(ops, offset) => self.l_loop(ops, *offset),
                OpType::ILoop(ops, offset, step) => self.i_loop(ops, *offset, *step),
                OpType::CLoop(ops, offset, iterations) => self.c_loop(ops, *offset, *iterations),
                OpType::TNz(ops, offset) => self.tnz(ops, *offset),
                OpType::SearchZero(step) => self.search_zero(*step),
                OpType::PutChar => self.put_char(),
                OpType::GetChar => self.get_char(),
            }
        }
    }

    fn const_u8(&mut self, value: u8) -> Value {
        self.bcx.ins().iconst(types::I8, value as i64)
    }

    fn const_isize(&mut self, value: isize) -> Value {
        self.bcx.ins().iconst(self.pointer_type, value as i64)
    }

    fn add_offset(&mut self, value: Value, offset: isize) -> Value {
        match offset.cmp(&0) {
            Ordering::Equal => value,
            Ordering::Greater => {
                let offset = self.const_isize(offset);
                self.bcx.ins().iadd(value, offset)
            }
            Ordering::Less => {
                let offset = self.const_isize(-offset);
                self.bcx.ins().isub(value, offset)
            }
        }
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

    fn copy(&mut self, src_offset: isize, dest_offset: isize) {
        let value = self.load(src_offset);
        self.store(dest_offset, value);
    }

    fn _move(&mut self, src_offset: isize, dest_offset: isize) {
        self.copy(src_offset, dest_offset);
        self.set(src_offset, 0);
    }

    fn get_char(&mut self) {
        let results = self.bcx.ins().call(self.get_char_func, &[self.env]);
        let results = self.bcx.inst_results(results)[0];
        self.store(0, results);
    }

    fn put_char(&mut self) {
        let value = self.load(0);
        self.bcx.ins().call(self.put_char_func, &[self.env, value]);
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

    fn l_loop(&mut self, ops: &[Op], offset: isize) {
        let head = self.bcx.create_block();
        self.bcx.append_block_param(head, self.pointer_type);

        let body = self.bcx.create_block();
        self.bcx.append_block_param(body, self.pointer_type);
        self.bcx.append_block_param(body, self.pointer_type);

        let next = self.bcx.create_block();
        self.bcx.append_block_param(next, self.pointer_type);

        self.bcx.ins().fallthrough(head, &[self.heap_ptr]);

        // Head with condition
        self.bcx.switch_to_block(head);
        self.heap_ptr = self.bcx.block_params(head)[0];

        let init_heap_ptr = self.heap_ptr;
        let loop_heap_ptr = self.add_offset(init_heap_ptr, offset);

        let value = self.load(0);
        self.bcx.ins().brz(value, next, &[init_heap_ptr]);
        self.bcx.ins().fallthrough(body, &[loop_heap_ptr, init_heap_ptr]);

        // Loop Body
        self.bcx.switch_to_block(body);
        self.heap_ptr = self.bcx.block_params(body)[0];
        self.append_ops(ops);
        let heap_ptr = self.bcx.block_params(body)[1];
        self.bcx.ins().jump(head, &[heap_ptr]);

        // Start next block after loop
        self.bcx.switch_to_block(next);
        self.heap_ptr = self.bcx.block_params(next)[0];

        // This is not be needed but it seems to fix a possible bug in cranelift
        self.set(0, 0);
    }

    fn i_loop(&mut self, ops: &[Op], offset: isize, step: u8) {
        let head = self.bcx.create_block();
        self.bcx.append_block_param(head, self.pointer_type);
        self.bcx.append_block_param(head, types::I8);

        let body = self.bcx.create_block();
        self.bcx.append_block_param(body, self.pointer_type);
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
        let loop_heap_ptr = self.add_offset(init_heap_ptr, offset);

        self.bcx.ins().brz(counter, next, &[init_heap_ptr]);
        self.bcx.ins().fallthrough(body, &[loop_heap_ptr, init_heap_ptr, counter]);

        // Loop Body
        self.bcx.switch_to_block(body);
        self.heap_ptr = self.bcx.block_params(body)[0];
        let counter = self.bcx.block_params(body)[2];

        self.append_ops(ops);

        let step = self.const_u8(step);
        let counter = self.bcx.ins().isub(counter, step);
        let heap_ptr = self.bcx.block_params(body)[1];
        self.bcx.ins().jump(head, &[heap_ptr, counter]);

        // Start next block after loop
        self.bcx.switch_to_block(next);
        self.heap_ptr = self.bcx.block_params(next)[0];
        self.set(0, 0);
    }

    fn c_loop(&mut self, ops: &[Op], offset: isize, iterations: u8) {
        let head = self.bcx.create_block();
        self.bcx.append_block_param(head, self.pointer_type);
        self.bcx.append_block_param(head, types::I8);

        let body = self.bcx.create_block();
        self.bcx.append_block_param(body, self.pointer_type);
        self.bcx.append_block_param(body, self.pointer_type);
        self.bcx.append_block_param(body, types::I8);

        let next = self.bcx.create_block();
        self.bcx.append_block_param(next, self.pointer_type);

        let iterations = self.const_u8(iterations);
        self.bcx.ins().fallthrough(head, &[self.heap_ptr, iterations]);

        // Head with condition
        self.bcx.switch_to_block(head);
        self.heap_ptr = self.bcx.block_params(head)[0];
        let counter = self.bcx.block_params(head)[1];

        let init_heap_ptr = self.heap_ptr;
        let loop_heap_ptr = self.add_offset(init_heap_ptr, offset);

        self.bcx.ins().brz(counter, next, &[init_heap_ptr]);
        self.bcx.ins().fallthrough(body, &[loop_heap_ptr, init_heap_ptr, counter]);

        // Loop Body
        self.bcx.switch_to_block(body);
        self.heap_ptr = self.bcx.block_params(body)[0];
        let counter = self.bcx.block_params(body)[2];

        self.append_ops(ops);

        let step = self.const_u8(1);
        let counter = self.bcx.ins().isub(counter, step);
        let heap_ptr = self.bcx.block_params(body)[1];
        self.bcx.ins().jump(head, &[heap_ptr, counter]);

        // Start next block after loop
        self.bcx.switch_to_block(next);
        self.heap_ptr = self.bcx.block_params(next)[0];
        self.set(0, 0);
    }

    fn tnz(&mut self, ops: &[Op], offset: isize) {
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
        self.heap_ptr = self.add_offset(init_heap_ptr, offset);

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

pub struct CompiledModule {
    module: Option<JITModule>,
    main_func: FuncId,
}

impl CompiledModule {
    pub fn new(program: &Program) -> Result<CompiledModule, CompilerError> {
        let mut flag_builder = settings::builder();

        //flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("opt_level", "speed").unwrap();
        flag_builder.set("enable_verifier", "true").unwrap();

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
        {
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
        }

        std::io::stdout().flush().unwrap();

        module
            .define_function(func, &mut ctx, &mut trap_sink, &mut stack_map_sink)
            .unwrap();
        module.clear_context(&mut ctx);

        module.finalize_definitions();

        Ok(CompiledModule {
            module: Some(module),
            main_func: func,
        })
    }

    pub fn run<R: Read, W: Write>(&self, mut input: R, mut output: W) -> Vec<u8> {
        let module = self.module.as_ref().expect("Module exists");

        let code = module.get_finalized_function(self.main_func);

        let mut env = Box::new(Environment::new(&mut input, &mut output));

        let exec = unsafe { mem::transmute::<_, fn(*mut u8, *mut Environment)>(code) };

        let mut heap = vec![0_u8; 1024 * 1024];

        exec(heap.as_mut_ptr(), &mut *env);

        heap
    }
}

impl Drop for CompiledModule {
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
    use crate::{parse, optimize};
    use std::io::{Cursor, Write, Read};
    use crate::parser::{Program, Op};
    use super::CompiledModule;

    fn run<R: Read, W: Write>(program: &Program, input: R, output: W) -> Vec<u8> {
        CompiledModule::new(program).unwrap().run(input, output)
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
                    Op::dec_ptr(1..2, 1),
                    Op::dec(1..2, 1),
                    Op::inc_ptr(1..2, 2),
                    Op::inc(1..2, 1),
                    Op::inc(1..2, 1),
                ], 1),
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
                    Op::inc(1..2, 1),
                    Op::inc(1..2, 1),
                ], 2, 1),
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
                    Op::inc(1..2, 1),
                    Op::inc(1..2, 1),
                ], 2, 2),
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
    fn test_c_loop() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 10),
                Op::c_loop(1..4, vec![
                    Op::inc(1..2, 1),
                    Op::inc(1..2, 1),
                ], 2, 5),
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
    fn test_tnz() {
        let program = Program {
            ops: vec![
                Op::set(0..1, 1),
                Op::t_nz(1..4, vec![
                    Op::set(1..2, 10),
                ], 2),
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
                    Op::set(1..2, 10),
                ], 2),
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