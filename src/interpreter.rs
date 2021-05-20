use crate::parser::{Program, Op};
use std::io::Write;
use std::io::Read;
use std::error::Error;
use std::fmt::{Display, Formatter, Debug};

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
        match op {
            Op::IncPtr => self.pointer = self.pointer.wrapping_add(1),
            Op::DecPtr => self.pointer = self.pointer.wrapping_sub(1),
            Op::Inc => {
                let value = self.heap_value()?;
                *value = value.wrapping_add(1);
            }
            Op::Dec => {
                let value = self.heap_value()?;
                *value = value.wrapping_sub(1);
            }
            Op::GetChar => self.get_char()?,
            Op::PutChar => self.put_char()?,
            Op::Loop(ops) => {
                while *self.heap_value()? > 0 {
                    self.execute_ops(ops)?;
                }
            }
        }

        Ok(())
    }

    fn heap_value(&mut self) -> Result<&mut u8, RuntimeError> {
        if self.pointer >= self.max_heap_size {
            return Err(RuntimeError::MaxHeapSizeReached {
                max_heap_size: self.max_heap_size,
                required: self.pointer + 1,
            });
        }

        while self.pointer > self.heap.len() - 1 {
            self.heap.push(0);
        }
        Ok(&mut self.heap[self.pointer])
    }

    fn get_char(&mut self) -> Result<(), RuntimeError> {
        let mut buf = [0];
        self.input.read_exact(&mut buf)?;
        *self.heap_value()? = buf[0];

        Ok(())
    }

    fn put_char(&mut self) -> Result<(), RuntimeError> {
        let ch = *self.heap_value()?;

        if ch.is_ascii() {
            write!(self.output, "{}", ch as char)?;
        } else {
            write!(self.output, "\\0x{:x}", ch)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    MaxHeapSizeReached {
        max_heap_size: usize,
        required: usize,
    },
    IoError(std::io::Error),
}

impl From<std::io::Error> for RuntimeError {
    fn from(err: std::io::Error) -> RuntimeError {
        RuntimeError::IoError(err)
    }
}

impl Error for RuntimeError {
    fn cause(&self) -> Option<&dyn Error> {
        match self {
            RuntimeError::IoError(err) => Some(err),
            _ => None
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::MaxHeapSizeReached { max_heap_size, required } =>
                write!(f, "Required heap size of {} exceeds limit of {}", max_heap_size, required),
            RuntimeError::IoError(err) => std::fmt::Display::fmt(&err, f),
        }
    }
}
