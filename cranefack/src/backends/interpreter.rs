use std::io::{Read, ErrorKind};
use std::io::Write;

use crate::errors::RuntimeError;
use crate::parser::{Op, Program, OpType};
use std::ops::Range;

const MAX_HEAP_SIZE: usize = 16 * 1024 * 1024;

/// Interpreter to execute a program
pub struct Interpreter<R: Read, W: Write> {
    max_heap_size: usize,
    pub(crate) heap: Vec<u8>,
    pointer: usize,
    input: R,
    output: W,
}

impl<R: Read, W: Write> Interpreter<R, W> {
    /// Create a default interpreter
    pub fn new(input: R, output: W) -> Interpreter<R, W> {
        Interpreter {
            max_heap_size: MAX_HEAP_SIZE,
            heap: vec![0; 1024],
            pointer: 0,
            input,
            output,
        }
    }

    /// Execute program
    pub fn execute(&mut self, program: &Program) -> Result<(), RuntimeError> {
        self.execute_ops(&program.ops)
    }

    fn execute_ops(&mut self, ops: &[Op]) -> Result<(), RuntimeError> {
        for op in ops {
            self.execute_op(op)?;
        }

        Ok(())
    }

    fn execute_op(&mut self, op: &Op) -> Result<(), RuntimeError> {
        match &op.op_type {
            OpType::Start => {
                // ignore
            }
            OpType::IncPtr(count) => self.pointer = self.pointer.wrapping_add(*count),
            OpType::DecPtr(count) => self.pointer = self.pointer.wrapping_sub(*count),
            OpType::Inc(offset, count) => {
                let value = self.heap_value_at_offset(&op.span, *offset)?;
                *value = value.wrapping_add(*count);
            }
            OpType::Dec(offset, count) => {
                let value = self.heap_value_at_offset(&op.span, *offset)?;
                *value = value.wrapping_sub(*count);
            }
            OpType::Set(offset, value) => *self.heap_value_at_offset(&op.span, *offset)? = *value,
            OpType::Add(src_offset, dest_offset, multi) => {
                let source = *self.heap_value_at_offset(&op.span, *src_offset)?;
                let target = self.heap_value_at_offset(&op.span, *dest_offset)?;
                *target = target.wrapping_add(source.wrapping_mul(*multi));
                *self.heap_value_at_offset(&op.span, *src_offset)? = 0;
            }
            OpType::NzAdd(src_offset, dest_offset, multi) => {
                let source = *self.heap_value_at_offset(&op.span, *src_offset)?;
                let target = self.heap_value_at_offset(&op.span, *dest_offset)?;
                *target = target.wrapping_add(source.wrapping_mul(*multi));
            }
            OpType::CAdd(src_offset, dest_offset, value) => {
                let target = self.heap_value_at_offset(&op.span, *dest_offset)?;
                *target = target.wrapping_add(*value);
                *self.heap_value_at_offset(&op.span, *src_offset)? = 0;
            }
            OpType::NzCAdd(_src_offset, dest_offset, value) => {
                let target = self.heap_value_at_offset(&op.span, *dest_offset)?;
                *target = target.wrapping_add(*value);
            }
            OpType::Sub(src_offset, dest_offset, multi) => {
                let source = *self.heap_value_at_offset(&op.span, *src_offset)?;
                let target = self.heap_value_at_offset(&op.span, *dest_offset)?;
                *target = target.wrapping_sub(source.wrapping_mul(*multi));
                *self.heap_value_at_offset(&op.span, *src_offset)? = 0;
            }
            OpType::NzSub(src_offset, dest_offset, multi) => {
                let source = *self.heap_value_at_offset(&op.span, *src_offset)?;
                let target = self.heap_value_at_offset(&op.span, *dest_offset)?;
                *target = target.wrapping_sub(source.wrapping_mul(*multi));
            }
            OpType::CSub(src_offset, dest_offset, value) => {
                let target = self.heap_value_at_offset(&op.span, *dest_offset)?;
                *target = target.wrapping_sub(*value);
                *self.heap_value_at_offset(&op.span, *src_offset)? = 0;
            }
            OpType::NzCSub(_src_offset, dest_offset, value) => {
                let target = self.heap_value_at_offset(&op.span, *dest_offset)?;
                *target = target.wrapping_sub(*value);
            }
            OpType::Move(src_offset, dest_offset) => {
                let source = *self.heap_value_at_offset(&op.span, *src_offset)?;
                let target = self.heap_value_at_offset(&op.span, *dest_offset)?;
                *target = source;
                *self.heap_value_at_offset(&op.span, *src_offset)? = 0;
            }
            OpType::Copy(src_offset, dest_offset) => {
                let source = *self.heap_value_at_offset(&op.span, *src_offset)?;
                let target = self.heap_value_at_offset(&op.span, *dest_offset)?;
                *target = source;
            }
            OpType::GetChar => self.get_char(&op.span)?,
            OpType::PutChar => self.put_char(&op.span)?,
            OpType::DLoop(ops) => {
                while *self.heap_value(&op.span)? > 0 {
                    self.execute_ops(ops)?;
                }
            }
            OpType::LLoop(ops, offset) => {
                let heap_pointer = self.pointer;
                let start_heap_pointer = (heap_pointer as isize).wrapping_add(*offset) as usize;

                while *self.heap_value(&op.span)? > 0 {
                    self.pointer = start_heap_pointer;
                    self.execute_ops(ops)?;
                    self.pointer = heap_pointer;
                }
            }
            OpType::ILoop(ops, offset, step) => {
                let heap_pointer = self.pointer;
                let start_heap_pointer = (heap_pointer as isize).wrapping_add(*offset) as usize;

                let mut left = *self.heap_value(&op.span)?;
                while left > 0 {
                    self.pointer = start_heap_pointer;

                    self.execute_ops(ops)?;
                    left = left.wrapping_sub(*step);
                }

                self.pointer = heap_pointer;
                *self.heap_value(&op.span)? = 0;
            }
            OpType::CLoop(ops, offset, iterations) => {
                let heap_pointer = self.pointer;
                let start_heap_pointer = (heap_pointer as isize).wrapping_add(*offset) as usize;

                for _ in 0..*iterations {
                    self.pointer = start_heap_pointer;
                    self.execute_ops(ops)?;
                }

                self.pointer = heap_pointer;
                *self.heap_value(&op.span)? = 0;
            }
            OpType::TNz(ops, offset) => {
                if *self.heap_value(&op.span)? != 0 {
                    let heap_pointer = self.pointer;
                    let start_heap_pointer = (heap_pointer as isize).wrapping_add(*offset) as usize;

                    self.pointer = start_heap_pointer;
                    self.execute_ops(ops)?;

                    self.pointer = heap_pointer;
                    *self.heap_value(&op.span)? = 0;
                }
            }
            OpType::SearchZero(step) => {
                let mut pointer = self.pointer as isize;

                loop {
                    let value = self.heap_value_at(&op.span, pointer)?;

                    if *value == 0 {
                        break;
                    }

                    pointer += step;
                }

                self.pointer = pointer as usize;
            }
        }

        Ok(())
    }

    fn heap_value(&mut self, span: &Range<usize>) -> Result<&mut u8, RuntimeError> {
        if self.pointer >= self.max_heap_size {
            return Err(RuntimeError::MaxHeapSizeReached {
                span: span.clone(),
                max_heap_size: self.max_heap_size,
                required: self.pointer.saturating_add(1),
            });
        }

        while self.pointer > self.heap.len() - 1 {
            self.heap.push(0);
        }
        Ok(&mut self.heap[self.pointer])
    }

    fn heap_value_at(&mut self, span: &Range<usize>, pointer: isize) -> Result<&mut u8, RuntimeError> {
        let pointer = pointer.max(0) as usize;

        if pointer >= self.max_heap_size {
            return Err(RuntimeError::MaxHeapSizeReached {
                span: span.clone(),
                max_heap_size: self.max_heap_size,
                required: self.pointer.saturating_add(1),
            });
        }

        while pointer > self.heap.len() - 1 {
            self.heap.push(0);
        }
        Ok(&mut self.heap[pointer])
    }

    fn heap_value_at_offset(&mut self, span: &Range<usize>, ptr_offset: isize) -> Result<&mut u8, RuntimeError> {
        let pointer = self.pointer as isize + ptr_offset;

        let pointer = pointer.max(0) as usize;

        if pointer >= self.max_heap_size {
            return Err(RuntimeError::MaxHeapSizeReached {
                span: span.clone(),
                max_heap_size: self.max_heap_size,
                required: self.pointer.saturating_add(1),
            });
        }

        while pointer > self.heap.len() - 1 {
            self.heap.push(0);
        }
        Ok(&mut self.heap[pointer])
    }

    fn get_char(&mut self, span: &Range<usize>) -> Result<(), RuntimeError> {
        let mut buf = [0];

        if let Err(error) = self.input.read_exact(&mut buf) {
            // In case of EOF the system will read 0 as a fallback
            if error.kind() != ErrorKind::UnexpectedEof {
                return Err(RuntimeError::IoError {
                    span: span.clone(),
                    error,
                });
            }
        };

        *self.heap_value(span)? = buf[0];

        Ok(())
    }

    fn put_char(&mut self, span: &Range<usize>) -> Result<(), RuntimeError> {
        let ch = *self.heap_value(span)?;

        if ch.is_ascii() {
            write!(self.output, "{}", ch as char)
        } else {
            write!(self.output, "\\0x{:x}", ch)
        }.map_err(|error| RuntimeError::IoError {
            span: span.clone(),
            error,
        })
    }
}


#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::backends::interpreter::Interpreter;
    use crate::parser::parse;
    use crate::optimizations::optimize;

    #[test]
    fn test_out_1() {
        let mut program = parse(">+.").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"\x01");
    }

    #[test]
    fn test_out_b() {
        let mut program = parse(",++.").unwrap();
        optimize(&mut program);

        let input = b"a";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"c");
    }

    #[test]
    fn test_loop() {
        let mut program = parse("+++[>+<-]>.").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"\x03");
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

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"Hello world!\n");
    }

    #[test]
    fn test_hello_world_v2() {
        let mut program = parse(include_str!("../../../test_programs/hello_world.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"Hello World!\n");
    }

    #[test]
    fn test_count_loop() {
        let mut program = parse("++++[->++<]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
        assert_eq!(interpreter.heap[1], 8);
    }

    #[test]
    fn test_count_loop_inv() {
        let mut program = parse("++++[->++<]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
        assert_eq!(interpreter.heap[1], 8);
    }

    #[test]
    fn test_totally_empty() {
        let mut program = parse("").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
    }

    #[test]
    fn test_optimized_empty() {
        let mut program = parse("[>+++++<-]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
    }

    #[test]
    fn test_multiply() {
        let mut program = parse(">>++<<++[>+++++<-]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
        assert_eq!(interpreter.heap[1], 10);
    }

    #[test]
    fn test_divide() {
        let mut program = parse(">>++<<++++++++++++[>+<---]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
        assert_eq!(interpreter.heap[1], 4);
    }

    #[test]
    fn test_pow_5_3() {
        let mut program = parse("+++++[>+++++[>+++++<-]<-]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
        assert_eq!(interpreter.heap[1], 0);
        assert_eq!(interpreter.heap[2], 125);
    }

    #[test]
    fn test_bad_count_loop() {
        let mut program = parse("++++++++[---->+<++]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
        assert_eq!(interpreter.heap[1], 4);
    }

    #[test]
    fn test_conditional_set() {
        let mut program = parse("+>>++<<[->[-]++++++<]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
        assert_eq!(interpreter.heap[1], 6);
        assert_eq!(interpreter.heap[2], 2);
    }

    #[test]
    fn test_overwrite_prev_set() {
        let mut program = parse("++++[-]++").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 2);
    }

    #[test]
    fn test_hello_fizz() {
        let mut program = parse(include_str!("../../../test_programs/fizz.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"1W\n");
    }

    #[test]
    fn test_hello_fizzbuzz() {
        let mut program = parse(include_str!("../../../test_programs/fizzbuzz.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"987654321");
    }

    #[test]
    fn test_bottles() {
        let mut program = parse(include_str!("../../../test_programs/bottles.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, include_bytes!("../../../test_programs/bottles.bf.out"));
    }

    #[test]
    fn test_factor() {
        let mut program = parse(include_str!("../../../test_programs/factor.bf")).unwrap();
        optimize(&mut program);

        let input = include_bytes!("../../../test_programs/factor.bf.in");
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, include_bytes!("../../../test_programs/factor.bf.out"));
    }

    #[test]
    fn test_life() {
        let mut program = parse(include_str!("../../../test_programs/life.bf")).unwrap();
        optimize(&mut program);

        let input = include_bytes!("../../../test_programs/life.bf.in");
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, include_bytes!("../../../test_programs/life.bf.out"));
    }

    #[test]
    fn test_test_io() {
        let mut program = parse(",[.,]").unwrap();
        optimize(&mut program);

        let input = b"0123456789aZ";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"0123456789aZ");
    }

    #[test]
    fn test_cell_size() {
        let mut program = parse(include_str!("../../../test_programs/cell_size.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"8 bit cells\n");
    }
}
