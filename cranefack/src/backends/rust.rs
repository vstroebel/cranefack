use std::error::Error;
use std::fmt::Write;

use crate::ir::ops::{Op, OpType};
use crate::parser::Program;

/// Compile program into a rust file that can be compiled with rustc
pub fn compile_to_rust(program: &Program) -> String {
    let mut code = "".to_owned();

    print_ops(&mut code, &program.ops).expect("No io error");

    include_str!("rust.tpl").replace("{{CODE}}", &code)
}

fn print_ops(out: &mut String, ops: &[Op]) -> Result<(), Box<dyn Error>> {
    for op in ops {
        match &op.op_type {
            OpType::Start => {
                // ignore
            }
            OpType::IncPtr(count) => writeln!(out, "rt.inc_ptr({});", count)?,
            OpType::DecPtr(count) => writeln!(out, "rt.dec_ptr({});", count)?,
            OpType::Inc(offset, count) => writeln!(out, "rt.inc({}, {});", offset, count)?,
            OpType::Dec(offset, count) => writeln!(out, "rt.dec({}, {});", offset, count)?,
            OpType::Set(offset, value) => writeln!(out, "rt.set({}, {});", offset, value)?,
            OpType::Add(src_offset, dest_offset, multi) => writeln!(out, "rt.add({}, {}, {});", src_offset, dest_offset, multi)?,
            OpType::NzAdd(src_offset, dest_offset, multi) => writeln!(out, "rt.nz_add({}, {}, {});", src_offset, dest_offset, multi)?,
            OpType::CAdd(src_offset, dest_offset, count) => writeln!(out, "rt.c_add({}, {}, {});", src_offset, dest_offset, count)?,
            OpType::NzCAdd(src_offset, dest_offset, count) => writeln!(out, "rt.nz_c_add({}, {}, {});", src_offset, dest_offset, count)?,
            OpType::Sub(src_offset, dest_offset, multi) => writeln!(out, "rt.sub({}, {}, {});", src_offset, dest_offset, multi)?,
            OpType::NzSub(src_offset, dest_offset, multi) => writeln!(out, "rt.nz_sub({}, {}, {});", src_offset, dest_offset, multi)?,
            OpType::CSub(src_offset, dest_offset, count) => writeln!(out, "rt.c_sub({}, {}, {});", src_offset, dest_offset, count)?,
            OpType::NzCSub(src_offset, dest_offset, count) => writeln!(out, "rt.nz_c_sub({}, {}, {});", src_offset, dest_offset, count)?,
            OpType::Move(src_offset, dest_offset) => writeln!(out, "rt.move({}, {});", src_offset, dest_offset)?,
            OpType::Copy(src_offset, dest_offset) => writeln!(out, "rt.copy({}, {});", src_offset, dest_offset)?,
            OpType::Mul(src_offset, dest_offset, multi) => writeln!(out, "rt.mul({}, {}, {});", src_offset, dest_offset, multi)?,
            OpType::NzMul(src_offset, dest_offset, multi) => writeln!(out, "rt.nz_mul({}, {}, {});", src_offset, dest_offset, multi)?,
            OpType::GetChar(offset) => writeln!(out, "rt.get_char({});", offset)?,
            OpType::PutChar(offset) => writeln!(out, "rt.put_char({});", offset)?,
            OpType::DLoop(children) => {
                writeln!(out, "{{")?;

                writeln!(out, " while *rt.heap_value() > 0 {{")?;
                print_ops(out, children)?;
                writeln!(out, "}}")?;

                writeln!(out, "}}")?;
            }
            OpType::LLoop(children, _) => {
                writeln!(out, "{{")?;

                writeln!(out, "let heap_pointer = rt.pointer;")?;
                writeln!(out, "while *rt.heap_value() > 0 {{")?;
                writeln!(out, "rt.pointer = heap_pointer;")?;
                print_ops(out, children)?;
                writeln!(out, "}}")?;
                writeln!(out, "rt.pointer = heap_pointer;")?;

                writeln!(out, "}}")?;
            }
            OpType::ILoop(children, step, _) => {
                writeln!(out, "{{")?;

                writeln!(out, "let heap_pointer = rt.pointer;")?;
                writeln!(out, "let mut left = *rt.heap_value();")?;
                writeln!(out, "while left > 0 {{")?;
                writeln!(out, "rt.pointer = heap_pointer;")?;
                print_ops(out, children)?;
                writeln!(out, "left = left.wrapping_sub({});", step)?;
                writeln!(out, "}}")?;
                writeln!(out, "rt.pointer = heap_pointer;")?;
                writeln!(out, "*rt.heap_value() = 0;")?;

                writeln!(out, "}}")?;
            }
            OpType::CLoop(children, iterations, _) => {
                writeln!(out, "{{")?;

                writeln!(out, "let heap_pointer = rt.pointer;")?;

                writeln!(out, "for _ in 0..{} {{", iterations)?;
                writeln!(out, "rt.pointer = heap_pointer;")?;
                print_ops(out, children)?;
                writeln!(out, "}}")?;
                writeln!(out, "rt.pointer = heap_pointer;")?;
                writeln!(out, "*rt.heap_value() = 0;")?;

                writeln!(out, "}}")?;
            }
            OpType::TNz(children, _) => {
                writeln!(out, "{{")?;

                writeln!(out, "if *rt.heap_value() != 0 {{")?;
                writeln!(out, "let heap_pointer = self.pointer;")?;
                writeln!(out, "rt.pointer = heap_pointer;")?;
                print_ops(out, children)?;
                writeln!(out, "rt.pointer = heap_pointer;")?;
                writeln!(out, "*rt.heap_value() = 0;")?;

                writeln!(out, "}}")?;
            }
            OpType::SearchZero(step) => writeln!(out, "rt.search_zero({});", step)?,
        }
    }

    Ok(())
}