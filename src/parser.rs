use crate::errors::ParserError;
use std::ops::Range;
use std::io::Write;
use std::error::Error;

pub struct Program {
    pub ops: Vec<Op>,
}

impl Program {
    pub fn get_statistics(&self) -> (usize, usize) {
        self.get_ops_statistics(&self.ops)
    }

    fn get_ops_statistics(&self, ops: &[Op]) -> (usize, usize) {
        let mut op_count = 0;
        let mut loop_count = 0;

        for op in ops {
            op_count += 1;
            if let OpType::Loop(children, ..) = &op.op_type {
                let (ops, loops) = self.get_ops_statistics(children);
                op_count += ops;
                loop_count += 1 + loops;
            }
        }

        (op_count, loop_count)
    }

    pub fn dump<W: Write>(&self, mut output: W) -> Result<(), Box<dyn Error>> {
        self.dump_ops(&mut output, &self.ops, 0)
    }

    fn dump_ops<W: Write>(&self, output: &mut W, ops: &[Op], indent: usize) -> Result<(), Box<dyn Error>> {
        for op in ops {
            let mut pos = format!("0x{:x}..0x{:x}", op.span.start, op.span.end - 1);

            while pos.len() < 16 {
                pos.push(' ');
            }

            write!(output, "{}", pos)?;

            for _ in 0..indent {
                write!(output, "  ")?;
            }

            match &op.op_type {
                OpType::IncPtr(value) => writeln!(output, "INC_PTR {}", value)?,
                OpType::DecPtr(value) => writeln!(output, "DEC_PTR {}", value)?,

                OpType::Inc(value) => writeln!(output, "INC {}", value)?,
                OpType::Dec(value) => writeln!(output, "DEC {}", value)?,
                OpType::Set(value) => writeln!(output, "SET {}", value)?,
                OpType::Add(value, multi) => writeln!(output, "ADD offset: {} multiply: {}", value, multi)?,
                OpType::Sub(value, multi) => writeln!(output, "SUB offset: {} multiply: {}", value, multi)?,

                OpType::PutChar => writeln!(output, "PUT")?,
                OpType::GetChar => writeln!(output, "GET")?,

                OpType::Loop(children, steps) => {
                    match steps {
                        Some((offset, count, steps)) => writeln!(output, "LOOP offset: {} count: {}, steps: {}", offset, count, steps)?,
                        None => writeln!(output, "LOOP")?,
                    }
                    self.dump_ops(output, children, indent + 1)?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Op {
    pub op_type: OpType,
    pub span: Range<usize>,
}

impl Op {
    pub fn inc_ptr(span: Range<usize>, count: usize) -> Op {
        Op {
            op_type: OpType::IncPtr(count),
            span,
        }
    }

    pub fn dec_ptr(span: Range<usize>, count: usize) -> Op {
        Op {
            op_type: OpType::DecPtr(count),
            span,
        }
    }

    pub fn inc(span: Range<usize>, count: u8) -> Op {
        Op {
            op_type: OpType::Inc(count),
            span,
        }
    }

    pub fn dec(span: Range<usize>, count: u8) -> Op {
        Op {
            op_type: OpType::Dec(count),
            span,
        }
    }

    pub fn put_char(span: Range<usize>) -> Op {
        Op {
            op_type: OpType::PutChar,
            span,
        }
    }

    pub fn get_char(span: Range<usize>) -> Op {
        Op {
            op_type: OpType::GetChar,
            span,
        }
    }

    pub fn loop_ops(span: Range<usize>, ops: Vec<Op>) -> Op {
        Op {
            op_type: OpType::Loop(ops, None),
            span,
        }
    }
    pub fn loop_ops_with_steps(span: Range<usize>, ops: Vec<Op>, count: u8, steps: u8, offset: isize) -> Op {
        Op {
            op_type: OpType::Loop(ops, Some((offset, count, steps))),
            span,
        }
    }

    pub fn set(span: Range<usize>, value: u8) -> Op {
        Op {
            op_type: OpType::Set(value),
            span,
        }
    }

    pub fn add(span: Range<usize>, ptr_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::Add(ptr_offset, multi),
            span,
        }
    }

    pub fn sub(span: Range<usize>, ptr_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::Sub(ptr_offset, multi),
            span,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum OpType {
    IncPtr(usize),
    DecPtr(usize),
    Inc(u8),
    Dec(u8),
    Set(u8),
    Add(isize, u8),
    Sub(isize, u8),
    PutChar,
    GetChar,
    Loop(Vec<Op>, Option<(isize, u8, u8)>),
}

impl OpType {
    pub fn is_ptr_inc_or_dec(&self) -> bool {
        matches!(self, OpType::DecPtr(_) | OpType::IncPtr(_))
    }
}

const MAX_LOOP_DEPTH: usize = 1024;

struct Parser {
    stack: Vec<(usize, Vec<Op>)>,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            stack: vec![(1, vec![])],
        }
    }

    pub fn depth(&self) -> usize {
        self.stack.len()
    }

    pub fn push_op(&mut self, op: Op) {
        let tos = self.stack.len() - 1;
        self.stack[tos].1.push(op);
    }

    pub fn open_loop(&mut self, position: usize) -> Result<(), ParserError> {
        if self.stack.len() >= MAX_LOOP_DEPTH {
            Err(ParserError::LoopStackOverflow {
                position,
                max_depth: MAX_LOOP_DEPTH,
            })
        } else {
            self.stack.push((position, vec![]));
            Ok(())
        }
    }

    pub fn close_loop(&mut self, position: usize) -> Result<(), ParserError> {
        let tos = self.stack.len() - 1;

        if tos > 0 {
            let ops = self.stack.remove(tos);
            self.push_op(Op::loop_ops(ops.0..position + 1, ops.1));
            Ok(())
        } else {
            Err(ParserError::BadlyClosedLoop {
                position,
            })
        }
    }

    pub fn get_tos(&self) -> &(usize, Vec<Op>) {
        &self.stack[self.stack.len() - 1]
    }

    pub fn parse(&mut self, source: &str) -> Result<Program, ParserError> {
        for (pos, char) in source.chars().enumerate() {
            match char {
                '>' => self.push_op(Op::inc_ptr(pos..pos + 1, 1)),
                '<' => self.push_op(Op::dec_ptr(pos..pos + 1, 1)),
                '+' => self.push_op(Op::inc(pos..pos + 1, 1)),
                '-' => self.push_op(Op::dec(pos..pos + 1, 1)),
                '.' => self.push_op(Op::put_char(pos..pos + 1)),
                ',' => self.push_op(Op::get_char(pos..pos + 1)),
                '[' => self.open_loop(pos)?,
                ']' => self.close_loop(pos)?,
                _ => {
                    // Ignore comment char
                }
            }
        }

        if self.depth() != 1 {
            return Err(ParserError::UnclosedLoop {
                position: self.get_tos().0,
            });
        }

        Ok(Program {
            ops: self.stack.remove(0).1,
        })
    }
}

pub fn parse(source: &str) -> Result<Program, ParserError> {
    Parser::new().parse(source)
}