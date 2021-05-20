use std::io::Read;
use std::io::Write;

use crate::errors::RuntimeError;
use crate::parser::{Op, Program, OpType};
use std::ops::Range;

const MAX_HEAP_SIZE: usize = 16 * 1024 * 1024;

pub struct Interpreter<R: Read, W: Write> {
    max_heap_size: usize,
    heap: Vec<u8>,
    pointer: usize,
    input: R,
    output: W,
}

impl<R: Read, W: Write> Interpreter<R, W> {
    pub fn new(input: R, output: W) -> Interpreter<R, W> {
        Interpreter {
            max_heap_size: MAX_HEAP_SIZE,
            heap: vec![0; 1024],
            pointer: 0,
            input,
            output,
        }
    }

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
            OpType::IncPtr => self.pointer = self.pointer.wrapping_add(1),
            OpType::DecPtr => self.pointer = self.pointer.wrapping_sub(1),
            OpType::Inc => {
                let value = self.heap_value(&op.span)?;
                *value = value.wrapping_add(1);
            }
            OpType::Dec => {
                let value = self.heap_value(&op.span)?;
                *value = value.wrapping_sub(1);
            }
            OpType::GetChar => self.get_char(&op.span)?,
            OpType::PutChar => self.put_char(&op.span)?,
            OpType::Loop(ops) => {
                while *self.heap_value(&op.span)? > 0 {
                    self.execute_ops(ops)?;
                }
            }
        }

        Ok(())
    }

    fn heap_value(&mut self, span: &Range<usize>) -> Result<&mut u8, RuntimeError> {
        if self.pointer >= self.max_heap_size {
            return Err(RuntimeError::MaxHeapSizeReached {
                span: span.clone(),
                max_heap_size: self.max_heap_size,
                required: self.pointer + 1,
            });
        }

        while self.pointer > self.heap.len() - 1 {
            self.heap.push(0);
        }
        Ok(&mut self.heap[self.pointer])
    }

    fn get_char(&mut self, span: &Range<usize>) -> Result<(), RuntimeError> {
        let mut buf = [0];
        self.input.read_exact(&mut buf).map_err(|error| RuntimeError::IoError {
            span: span.clone(),
            error,
        })?;

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
