use crate::errors::ParserError;
use std::ops::Range;

pub struct Program {
    pub ops: Vec<Op>,
}

#[derive(Debug)]
pub struct Op {
    pub op_type: OpType,
    pub span: Range<usize>,
}

impl Op {
    fn inc_ptr(span: Range<usize>) -> Op {
        Op {
            op_type: OpType::IncPtr,
            span,
        }
    }

    fn dec_ptr(span: Range<usize>) -> Op {
        Op {
            op_type: OpType::DecPtr,
            span,
        }
    }

    fn inc(span: Range<usize>) -> Op {
        Op {
            op_type: OpType::Inc,
            span,
        }
    }

    fn dec(span: Range<usize>) -> Op {
        Op {
            op_type: OpType::Dec,
            span,
        }
    }

    fn put_char(span: Range<usize>) -> Op {
        Op {
            op_type: OpType::PutChar,
            span,
        }
    }

    fn get_char(span: Range<usize>) -> Op {
        Op {
            op_type: OpType::GetChar,
            span,
        }
    }

    fn loop_ops(span: Range<usize>, ops: Vec<Op>) -> Op {
        Op {
            op_type: OpType::Loop(ops),
            span,
        }
    }
}

#[derive(Debug)]
pub enum OpType {
    IncPtr,
    DecPtr,
    Inc,
    Dec,
    PutChar,
    GetChar,
    Loop(Vec<Op>),
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
            self.push_op(Op::loop_ops(position..position + 1, ops.1));
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
                '>' => self.push_op(Op::inc_ptr(pos..pos + 1)),
                '<' => self.push_op(Op::dec_ptr(pos..pos + 1)),
                '+' => self.push_op(Op::inc(pos..pos + 1)),
                '-' => self.push_op(Op::dec(pos..pos + 1)),
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