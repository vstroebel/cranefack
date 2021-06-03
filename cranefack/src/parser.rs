use std::error::Error;
use std::io::Write;

use crate::errors::ParserError;
use crate::ir::ops::{Op, OpType};

/// An executable program
#[derive(Clone)]
pub struct Program {
    pub ops: Vec<Op>,
}

impl Program {
    pub fn get_statistics(&self) -> (usize, usize, usize, usize, usize, usize) {
        self.get_ops_statistics(&self.ops)
    }

    fn get_ops_statistics(&self, ops: &[Op]) -> (usize, usize, usize, usize, usize, usize) {
        let mut op_count = 0;
        let mut dloop_count = 0;
        let mut lloop_count = 0;
        let mut iloop_count = 0;
        let mut cloop_count = 0;
        let mut if_count = 0;

        for op in ops {
            op_count += 1;
            match &op.op_type {
                OpType::DLoop(children) => {
                    let (ops, dloops, lloop, iloops, cloops, ifs) = self.get_ops_statistics(children);
                    op_count += ops;
                    dloop_count += 1;
                    dloop_count += dloops;
                    lloop_count += lloop;
                    iloop_count += iloops;
                    cloop_count += cloops;
                    if_count += ifs;
                }
                OpType::LLoop(children, ..) => {
                    let (ops, dloops, lloop, iloops, cloops, ifs) = self.get_ops_statistics(children);
                    op_count += ops;
                    lloop_count += 1;
                    dloop_count += dloops;
                    lloop_count += lloop;
                    iloop_count += iloops;
                    cloop_count += cloops;
                    if_count += ifs;
                }
                OpType::ILoop(children, ..) => {
                    let (ops, dloops, lloop, iloops, cloops, ifs) = self.get_ops_statistics(children);
                    op_count += ops;
                    iloop_count += 1;
                    dloop_count += dloops;
                    lloop_count += lloop;
                    iloop_count += iloops;
                    cloop_count += cloops;
                    if_count += ifs;
                }
                OpType::CLoop(children, ..) => {
                    let (ops, dloops, lloop, iloops, cloops, ifs) = self.get_ops_statistics(children);
                    op_count += ops;
                    cloop_count += 1;
                    dloop_count += dloops;
                    lloop_count += lloop;
                    iloop_count += iloops;
                    cloop_count += cloops;
                    if_count += ifs;
                }
                OpType::TNz(children, ..) => {
                    let (ops, dloops, lloop, iloops, cloops, ifs) = self.get_ops_statistics(children);
                    op_count += ops;
                    if_count += 1;
                    dloop_count += dloops;
                    lloop_count += lloop;
                    iloop_count += iloops;
                    cloop_count += cloops;
                    if_count += ifs;
                }
                _ => {}
            }
        }

        (op_count, dloop_count, lloop_count, iloop_count, cloop_count, if_count)
    }

    /// Dump program into a assembly like structure
    pub fn dump<W: Write>(&self, mut output: W, debug: bool) -> Result<(), Box<dyn Error>> {
        self.dump_ops(&mut output, &self.ops, 0, debug)
    }

    fn dump_ops<W: Write>(&self, output: &mut W, ops: &[Op], indent: usize, debug: bool) -> Result<(), Box<dyn Error>> {
        let mut ptr_offset = 0;

        for op in ops {
            let mut pos = format!("0x{:x}..0x{:x}", op.span.start, op.span.end - 1);

            while pos.len() < 16 {
                pos.push(' ');
            }

            write!(output, "{}", pos)?;

            if debug {
                let mut offset = format!("{}", ptr_offset);

                while offset.len() < 3 {
                    offset.push(' ');
                }

                write!(output, "{}  ", offset)?;
                if let Some(offset) = op.op_type.get_ptr_offset() {
                    ptr_offset += offset;
                }
            }

            for _ in 0..indent {
                if debug {
                    write!(output, "| ")?;
                } else {
                    write!(output, "  ")?;
                }
            }


            match &op.op_type {
                OpType::Start => writeln!(output, "START")?,
                OpType::IncPtr(value) => writeln!(output, "INC_PTR {}", value)?,
                OpType::DecPtr(value) => writeln!(output, "DEC_PTR {}", value)?,

                OpType::Inc(offset, value) => writeln!(output, "INC {} offset: {}", value, offset)?,
                OpType::Dec(offset, value) => writeln!(output, "DEC {} offset: {}", value, offset)?,
                OpType::Set(offset, value) => writeln!(output, "SET {} offset: {}", value, offset)?
                ,
                OpType::Add(src_offset, dest_offset, multi) =>
                    writeln!(output, "ADD src_offset: {} dest_offset: {} multiply: {}", src_offset, dest_offset, multi)?,
                OpType::NzAdd(src_offset, dest_offset, multi) =>
                    writeln!(output, "NZ_ADD src_offset: {} dest_offset: {} multiply: {}", src_offset, dest_offset, multi)?,
                OpType::CAdd(src_offset, dest_offset, value) =>
                    writeln!(output, "CADD src_offset: {} dest_offset: {} value: {}", src_offset, dest_offset, value)?,
                OpType::NzCAdd(src_offset, dest_offset, value) =>
                    writeln!(output, "NZ_CADD src_offset: {} dest_offset: {} value: {}", src_offset, dest_offset, value)?,
                OpType::Sub(src_offset, dest_offset, multi) =>
                    writeln!(output, "SUB src_offset: {} dest_offset: {} multiply: {}", src_offset, dest_offset, multi)?,
                OpType::NzSub(src_offset, dest_offset, multi) =>
                    writeln!(output, "NZ_SUB src_offset: {} dest_offset: {} multiply: {}", src_offset, dest_offset, multi)?,
                OpType::CSub(src_offset, dest_offset, value) =>
                    writeln!(output, "CSUB src_offset: {} dest_offset: {} value: {}", src_offset, dest_offset, value)?,
                OpType::NzCSub(src_offset, dest_offset, value) =>
                    writeln!(output, "NZ_CSUB src_offset: {} dest_offset: {} value: {}", src_offset, dest_offset, value)?,

                OpType::Mul(src_offset, dest_offset, multi) =>
                    writeln!(output, "MUL src_offset: {} dest_offset: {} multiply: {}", src_offset, dest_offset, multi)?,
                OpType::NzMul(src_offset, dest_offset, multi) =>
                    writeln!(output, "NZ_MUL src_offset: {} dest_offset: {} multiply: {}", src_offset, dest_offset, multi)?,

                OpType::Move(src_offset, dest_offset) =>
                    writeln!(output, "MOVE src_offset: {} dest_offset: {}", src_offset, dest_offset)?,
                OpType::Copy(src_offset, dest_offset) =>
                    writeln!(output, "COPY src_offset: {} dest_offset: {}", src_offset, dest_offset)?,

                OpType::PutChar(offset) => writeln!(output, "PUT offset: {}", offset)?,
                OpType::PutString(array) => {
                    write!(output, "PUT STRING \"")?;

                    for &v in array {
                        if v.is_ascii_graphic() || v == b' ' {
                            write!(output, "{}", v as char)?;
                        } else if v == b'\n' {
                            write!(output, "\\n")?;
                        } else if v == b'\r' {
                            write!(output, "\\r")?;
                        } else if v == b'\t' {
                            write!(output, "\\t")?;
                        } else {
                            write!(output, "\\0x{:x}", v)?;
                        }
                    }

                    writeln!(output, "\"")?
                }
                OpType::GetChar(offset) => writeln!(output, "GET offset: {}", offset)?,

                OpType::DLoop(children) => {
                    writeln!(output, "DLOOP")?;
                    self.dump_ops(output, children, indent + 1, debug)?;
                }
                OpType::LLoop(children, info) => {
                    writeln!(output, "LLOOP info: {}", info.asm(debug))?;
                    self.dump_ops(output, children, indent + 1, debug)?;
                }
                OpType::ILoop(children, step, increment, info) => {
                    writeln!(output, "ILOOP step: {} increment: {:?} info: {}", step, increment, info.asm(debug))?;
                    self.dump_ops(output, children, indent + 1, debug)?;
                }
                OpType::CLoop(children, iterations, increment, info) => {
                    writeln!(output, "CLOOP iterations: {} increment: {:?} info: {}", iterations, increment, info.asm(debug))?;
                    self.dump_ops(output, children, indent + 1, debug)?;
                }
                OpType::TNz(children, info) => {
                    writeln!(output, "TNZ info: {}", info.asm(debug))?;
                    self.dump_ops(output, children, indent + 1, debug)?;
                }
                OpType::SearchZero(step) => writeln!(output, "S_ZERO {} ", step)?,
            }
        }

        Ok(())
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

/// Parse the input source file into an abstract representation
pub fn parse(source: &str) -> Result<Program, ParserError> {
    Parser::new().parse(source)
}