use std::error::Error;
use std::fmt::{Display, Formatter};

pub struct Program {
    pub ops: Vec<Op>,
}

#[derive(Debug)]
pub enum Op {
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
            self.push_op(Op::Loop(ops.1));
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
                '>' => self.push_op(Op::IncPtr),
                '<' => self.push_op(Op::DecPtr),
                '+' => self.push_op(Op::Inc),
                '-' => self.push_op(Op::Dec),
                '.' => self.push_op(Op::PutChar),
                ',' => self.push_op(Op::GetChar),
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

#[derive(Debug)]
pub enum ParserError {
    LoopStackOverflow { position: usize, max_depth: usize },
    BadlyClosedLoop { position: usize },
    UnclosedLoop { position: usize },
}

impl Error for ParserError {}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::LoopStackOverflow { position, max_depth } => write!(f, "Maximum loop depth of {} reach at pos {}", max_depth, position),
            ParserError::BadlyClosedLoop { position } => write!(f, "Badly closed loop at pos {}", position),
            ParserError::UnclosedLoop { position } => write!(f, "Unclosed loop at pos {}", position),
        }
    }
}