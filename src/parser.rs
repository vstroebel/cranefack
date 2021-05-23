use crate::errors::ParserError;
use std::ops::Range;
use std::io::Write;
use std::error::Error;

pub struct Program {
    pub ops: Vec<Op>,
}

impl Program {
    pub fn get_statistics(&self) -> (usize, usize, usize, usize, usize) {
        self.get_ops_statistics(&self.ops)
    }

    fn get_ops_statistics(&self, ops: &[Op]) -> (usize, usize, usize, usize, usize) {
        let mut op_count = 0;
        let mut dloop_count = 0;
        let mut iloop_count = 0;
        let mut cloop_count = 0;
        let mut if_count = 0;

        for op in ops {
            op_count += 1;
            match &op.op_type {
                OpType::DLoop(children) => {
                    let (ops, dloops, iloops, cloops, ifs) = self.get_ops_statistics(children);
                    op_count += ops;
                    dloop_count += 1;
                    dloop_count += dloops;
                    iloop_count += iloops;
                    cloop_count += cloops;
                    if_count += ifs;
                }
                OpType::ILoop(children, ..) => {
                    let (ops, dloops, iloops, cloops, ifs) = self.get_ops_statistics(children);
                    op_count += ops;
                    iloop_count += 1;
                    dloop_count += dloops;
                    iloop_count += iloops;
                    cloop_count += cloops;
                    if_count += ifs;
                }
                OpType::CLoop(children, ..) => {
                    let (ops, dloops, iloops, cloops, ifs) = self.get_ops_statistics(children);
                    op_count += ops;
                    cloop_count += 1;
                    dloop_count += dloops;
                    iloop_count += iloops;
                    cloop_count += cloops;
                    if_count += ifs;
                }
                OpType::TNz(children, ..) => {
                    let (ops, dloops, iloops, cloops, ifs) = self.get_ops_statistics(children);
                    op_count += ops;
                    if_count += 1;
                    dloop_count += dloops;
                    iloop_count += iloops;
                    cloop_count += cloops;
                    if_count += ifs;
                }
                _ => {}
            }
        }

        (op_count, dloop_count, iloop_count, cloop_count, if_count)
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

                OpType::Inc(offset, value) => writeln!(output, "INC {} offset: {}", value, offset)?,
                OpType::Dec(offset, value) => writeln!(output, "DEC {} offset: {}", value, offset)?,
                OpType::Set(offset, value) => writeln!(output, "SET {} offset: {}", value, offset)?
                ,
                OpType::Add(offset, multi) => writeln!(output, "ADD offset: {} multiply: {}", offset, multi)?,
                OpType::CAdd(offset, value) => writeln!(output, "CADD offset: {} value: {}", offset, value)?,
                OpType::Sub(offset, multi) => writeln!(output, "SUB offset: {} multiply: {}", offset, multi)?,
                OpType::CSub(offset, value) => writeln!(output, "CSUB offset: {} value: {}", offset, value)?,

                OpType::PutChar => writeln!(output, "PUT")?,
                OpType::GetChar => writeln!(output, "GET")?,

                OpType::DLoop(children) => {
                    writeln!(output, "DLOOP")?;
                    self.dump_ops(output, children, indent + 1)?;
                }
                OpType::ILoop(children, offset, step) => {
                    writeln!(output, "ILOOP offset: {} step: {}", offset, step)?;
                    self.dump_ops(output, children, indent + 1)?;
                }
                OpType::CLoop(children, offset, iterations) => {
                    writeln!(output, "CLOOP offset: {} iterations: {}", offset, iterations)?;
                    self.dump_ops(output, children, indent + 1)?;
                }
                OpType::TNz(children, offset) => {
                    writeln!(output, "TNZ offset: {} ", offset)?;
                    self.dump_ops(output, children, indent + 1)?;
                }
                OpType::SearchZero(step) => writeln!(output, "S_ZERO {} ", step)?,
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
            op_type: OpType::Inc(0, count),
            span,
        }
    }

    pub fn inc_with_offset(span: Range<usize>, offset: isize, count: u8) -> Op {
        Op {
            op_type: OpType::Inc(offset, count),
            span,
        }
    }

    pub fn dec(span: Range<usize>, count: u8) -> Op {
        Op {
            op_type: OpType::Dec(0, count),
            span,
        }
    }

    pub fn dec_with_offset(span: Range<usize>, offset: isize, count: u8) -> Op {
        Op {
            op_type: OpType::Dec(offset, count),
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

    pub fn d_loop(span: Range<usize>, ops: Vec<Op>) -> Op {
        Op {
            op_type: OpType::DLoop(ops),
            span,
        }
    }

    pub fn i_loop(span: Range<usize>, ops: Vec<Op>, offset: isize, step: u8) -> Op {
        Op {
            op_type: OpType::ILoop(ops, offset, step),
            span,
        }
    }

    pub fn c_loop(span: Range<usize>, ops: Vec<Op>, offset: isize, iterations: u8) -> Op {
        Op {
            op_type: OpType::CLoop(ops, offset, iterations),
            span,
        }
    }

    pub fn set(span: Range<usize>, value: u8) -> Op {
        Op {
            op_type: OpType::Set(0, value),
            span,
        }
    }

    pub fn set_with_offset(span: Range<usize>, offset: isize, value: u8) -> Op {
        Op {
            op_type: OpType::Set(offset, value),
            span,
        }
    }

    pub fn add(span: Range<usize>, ptr_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::Add(ptr_offset, multi),
            span,
        }
    }

    pub fn c_add(span: Range<usize>, ptr_offset: isize, value: u8) -> Op {
        Op {
            op_type: OpType::CAdd(ptr_offset, value),
            span,
        }
    }

    pub fn sub(span: Range<usize>, ptr_offset: isize, multi: u8) -> Op {
        Op {
            op_type: OpType::Sub(ptr_offset, multi),
            span,
        }
    }

    pub fn c_sub(span: Range<usize>, ptr_offset: isize, value: u8) -> Op {
        Op {
            op_type: OpType::CSub(ptr_offset, value),
            span,
        }
    }

    pub fn t_nz(span: Range<usize>, ops: Vec<Op>, offset: isize) -> Op {
        Op {
            op_type: OpType::TNz(ops, offset),
            span,
        }
    }

    pub fn search_zero(span: Range<usize>, step: isize) -> Op {
        Op {
            op_type: OpType::SearchZero(step),
            span,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum OpType {
    IncPtr(usize),
    DecPtr(usize),
    Inc(isize, u8),
    Dec(isize, u8),
    Set(isize, u8),

    /// Add current value to value at offset and reset current value to 0
    Add(isize, u8),

    /// Add constant value to value at offset and reset current value to 0
    CAdd(isize, u8),

    /// Subtract current value to value at offset and reset current value to 0
    Sub(isize, u8),

    /// Subtract constant value to value at offset and reset current value to 0
    CSub(isize, u8),

    PutChar,
    GetChar,
    /// Dynamic loop as defined in raw brainfuck source
    DLoop(Vec<Op>),

    /// Loop with an iterator variable
    ILoop(Vec<Op>, isize, u8),

    /// Loop with compile time known iteration count
    CLoop(Vec<Op>, isize, u8),

    /// Test if not zero.
    ///
    /// Executes block if current value is not zero. Similar to `if true { ops }`
    TNz(Vec<Op>, isize),

    /// Move heap pointer to first cell containing zero based on step
    SearchZero(isize),
}

impl OpType {
    pub fn is_ptr_inc_or_dec(&self) -> bool {
        matches!(self, OpType::DecPtr(_) | OpType::IncPtr(_))
    }

    /// Return offset of pointer increments and decrements
    pub fn get_ptr_offset(&self) -> Option<isize> {
        match self {
            OpType::IncPtr(count) => Some(*count as isize),
            OpType::DecPtr(count) => Some(-(*count as isize)),
            _ => None,
        }
    }

    /// Test for arithmetic ops without offsets
    pub fn is_simple_arithmetic(&self) -> bool {
        matches!(self,
            OpType::Set(0, _) |
            OpType::Inc(0, _) |
            OpType::Dec(0, _) |
            OpType::IncPtr(_) |
            OpType::DecPtr(_)
        )
    }

    pub fn is_zeroing(&self) -> bool {
        matches!(self,
            OpType::DLoop(..) |
            OpType::ILoop(..) |
            OpType::CLoop(..) |
            OpType::TNz(..) |
            OpType::Add(..) |
            OpType::CAdd(..) |
            OpType::Sub(..) |
            OpType::CSub(..) |
            OpType::SearchZero(..)
        )
    }

    pub fn get_children_mut(&mut self) -> Option<&mut Vec<Op>> {
        match self {
            OpType::DLoop(children) |
            OpType::ILoop(children, ..) |
            OpType::CLoop(children, ..) |
            OpType::TNz(children, ..) => Some(children),
            _ => None,
        }
    }

    pub fn get_children_with_offset_mut(&mut self) -> Option<(isize, &mut Vec<Op>)> {
        match self {
            OpType::DLoop(children) => Some((0, children)),
            OpType::ILoop(children, offset, ..) |
            OpType::CLoop(children, offset, ..) |
            OpType::TNz(children, offset) => Some((*offset, children)),
            _ => None,
        }
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
            self.push_op(Op::d_loop(ops.0..position + 1, ops.1));
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