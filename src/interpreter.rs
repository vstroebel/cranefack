use std::io::Read;
use std::io::Write;

use crate::errors::RuntimeError;
use crate::parser::{Op, Program, OpType};
use std::ops::Range;

const MAX_HEAP_SIZE: usize = 16 * 1024 * 1024;

pub struct Interpreter<R: Read, W: Write> {
    max_heap_size: usize,
    pub(crate) heap: Vec<u8>,
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
            OpType::IncPtr(count) => self.pointer = self.pointer.wrapping_add(*count),
            OpType::DecPtr(count) => self.pointer = self.pointer.wrapping_sub(*count),
            OpType::Inc(count) => {
                let value = self.heap_value(&op.span)?;
                *value = value.wrapping_add(*count);
            }
            OpType::Dec(count) => {
                let value = self.heap_value(&op.span)?;
                *value = value.wrapping_sub(*count);
            }
            OpType::Set(value) => *self.heap_value(&op.span)? = *value,
            OpType::Add(ptr_offset, multi) => {
                let source = *self.heap_value(&op.span)?;
                let target = self.heap_value_at_offset(&op.span, *ptr_offset)?;
                *target = target.wrapping_add(source.wrapping_mul(*multi));
                *self.heap_value(&op.span)? = 0;
            }
            OpType::CAdd(ptr_offset, value) => {
                let target = self.heap_value_at_offset(&op.span, *ptr_offset)?;
                *target = target.wrapping_add(*value);
                *self.heap_value(&op.span)? = 0;
            }
            OpType::Sub(ptr_offset, multi) => {
                let source = *self.heap_value(&op.span)?;
                let target = self.heap_value_at_offset(&op.span, *ptr_offset)?;
                *target = target.wrapping_sub(source.wrapping_mul(*multi));
                *self.heap_value(&op.span)? = 0;
            }
            OpType::CSub(ptr_offset, value) => {
                let target = self.heap_value_at_offset(&op.span, *ptr_offset)?;
                *target = target.wrapping_sub(*value);
                *self.heap_value(&op.span)? = 0;
            }
            OpType::GetChar => self.get_char(&op.span)?,
            OpType::PutChar => self.put_char(&op.span)?,
            OpType::DLoop(ops) => {
                while *self.heap_value(&op.span)? > 0 {
                    self.execute_ops(ops)?;
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
