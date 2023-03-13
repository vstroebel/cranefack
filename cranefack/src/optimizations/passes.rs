use std::cmp::Ordering;

use crate::ir::ops::{LoopDecrement, Op, OpType};
use crate::ir::opt_info::Cell::Value;
use crate::ir::opt_info::{BlockInfo, Cell, CellAccess};
use crate::optimizations::peephole::run_peephole_pass;
use crate::optimizations::utils;
use crate::optimizations::utils::{
    count_ops_recursive, find_heap_value, find_last_accessing_inc_dec, find_last_put_string,
    run_non_local_pass, CellValue, Change, OpCodes,
};
use std::collections::HashSet;

pub fn remove_dead_loops(ops: &mut Vec<Op>) -> bool {
    run_peephole_pass(ops, remove_dead_loops_check)
}

fn remove_dead_loops_check(ops: [&Op; 2]) -> Change {
    if ops[0].op_type.is_zeroing(0)
        && matches!(
            ops[1].op_type,
            OpType::DLoop(..)
                | OpType::DTNz(..)
                | OpType::LLoop(..)
                | OpType::ILoop(..)
                | OpType::TNz(..)
                | OpType::SearchZero(..)
        )
    {
        Change::RemoveOffset(1)
    } else {
        Change::Ignore
    }
}

// Replace '[-]' that decreases the current point to 0 with set
// [+] can be optimized too assuming a wrapping overflow to 0
pub fn optimize_zero_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        let op = &mut ops[i];
        if let OpType::DLoop(children, _) = &mut op.op_type {
            let mut optimized = false;
            if children.len() == 1 {
                optimized = matches!(children[0].op_type, OpType::Dec(0, 1) | OpType::Inc(0, 1))
            }

            if optimized {
                ops[i] = Op::set(op.span.start..children[0].span.end, 0);
                progress = true;
            } else {
                progress |= optimize_zero_loops(children);
            }
        }

        i += 1;
    }

    progress
}

pub fn optimize_arithmetic_loops(ops: [&Op; 1]) -> Change {
    if let OpType::ILoop(children, 1, _, info) | OpType::TNz(children, info) = &ops[0].op_type {
        let mut replacement_indices = vec![];

        let mut bad_replacement_indices = HashSet::new();

        let mut ptr_offset = 0;

        for child in children {
            match &child.op_type {
                OpType::IncPtr(offset) => ptr_offset += *offset as isize,
                OpType::DecPtr(offset) => ptr_offset -= *offset as isize,
                OpType::Inc(offset, _) | OpType::Dec(offset, _) => {
                    let offset = ptr_offset + *offset;
                    if offset != 0 {
                        if replacement_indices.contains(&offset) {
                            bad_replacement_indices.insert(offset);
                        } else {
                            replacement_indices.push(offset);
                        }
                    }
                }
                OpType::Set(offset, _) => {
                    let offset = ptr_offset + *offset;
                    if offset == 0 {
                        if matches!(&ops[0].op_type, OpType::TNz(..)) {
                            return Change::Ignore;
                        }
                    } else if info.always_used() {
                        if replacement_indices.contains(&offset) {
                            bad_replacement_indices.insert(offset);
                        } else {
                            replacement_indices.push(offset);
                        }
                    }
                }
                OpType::Add(src_offset, dest_offset, _)
                | OpType::NzAdd(src_offset, dest_offset, _)
                | OpType::CAdd(src_offset, dest_offset, _)
                | OpType::Sub(src_offset, dest_offset, _)
                | OpType::NzSub(src_offset, dest_offset, _)
                | OpType::CSub(src_offset, dest_offset, _)
                | OpType::Mul(src_offset, dest_offset, _)
                | OpType::NzMul(src_offset, dest_offset, _)
                | OpType::Move(src_offset, dest_offset)
                | OpType::Copy(src_offset, dest_offset) => {
                    bad_replacement_indices.insert(*src_offset);
                    bad_replacement_indices.insert(*dest_offset);
                }
                OpType::NzCAdd(_, dest_offset, _) | OpType::NzCSub(_, dest_offset, _) => {
                    bad_replacement_indices.insert(*dest_offset);
                }
                _ => {
                    return Change::Ignore;
                }
            }
        }

        replacement_indices.retain(|i| !bad_replacement_indices.contains(i));

        if replacement_indices.is_empty() {
            return Change::Ignore;
        }

        let mut replacements = vec![];
        let mut new_children = vec![];

        for child in children {
            let replaced = match &child.op_type {
                OpType::Inc(offset, multi) => {
                    if replacement_indices.contains(offset) {
                        replacements.push(OpType::NzAdd(0, *offset, *multi));
                        true
                    } else {
                        false
                    }
                }
                OpType::Dec(offset, multi) => {
                    if replacement_indices.contains(offset) {
                        replacements.push(OpType::NzSub(0, *offset, *multi));
                        true
                    } else {
                        false
                    }
                }
                OpType::Set(offset, v) => {
                    if replacement_indices.contains(offset) {
                        replacements.insert(0, OpType::Set(*offset, *v));
                        true
                    } else {
                        false
                    }
                }
                _ => {
                    return Change::Ignore;
                }
            };

            if !replaced {
                new_children.push(child.clone());
            }
        }

        return if replacements.is_empty() {
            Change::Ignore
        } else {
            if new_children.is_empty() {
                let last = replacements.remove(replacements.len() - 1);

                match last {
                    OpType::NzAdd(src, dest, value) => {
                        replacements.push(OpType::Add(src, dest, value));
                    }
                    OpType::NzSub(src, dest, value) => {
                        replacements.push(OpType::Sub(src, dest, value));
                    }
                    OpType::Set(..) => {
                        replacements.push(last);
                    }
                    op_type => unreachable!("Unexpected op type {:?}", op_type),
                }
            } else {
                match &ops[0].op_type {
                    OpType::ILoop(_, _, decrement, info) => {
                        replacements.push(OpType::ILoop(new_children, 1, *decrement, info.clone()))
                    }
                    OpType::TNz(_, info) => {
                        replacements.push(OpType::TNz(new_children, info.clone()))
                    }
                    op_type => unreachable!("Unexpected op type {:?}", op_type),
                }
            }

            Change::Replace(replacements)
        };
    }

    Change::Ignore
}

// Optimize loops that are known to use the same counting variable
pub fn optimize_local_loops(ops: &mut Vec<Op>) -> bool {
    let mut progress = false;

    for op in ops.iter_mut() {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= optimize_local_loops(children);
        }
    }

    let mut i = 0;

    while !ops.is_empty() && !ops.is_empty() && i < ops.len() {
        let replace = if let OpType::DLoop(children, _) = &mut ops[i].op_type {
            if children.is_empty() {
                false
            } else {
                let mut ptr_offset = 0_isize;
                let mut ignore = false;

                for op in children {
                    match &op.op_type {
                        OpType::Start => {
                            // ignore
                        }
                        OpType::IncPtr(value) => {
                            ptr_offset += *value as isize;
                        }
                        OpType::DecPtr(value) => {
                            ptr_offset -= *value as isize;
                        }
                        OpType::DLoop(..) | OpType::DTNz(..) => {
                            ignore = true;
                            break;
                        }
                        OpType::LLoop(children, ..)
                        | OpType::ILoop(children, ..)
                        | OpType::CLoop(children, ..)
                        | OpType::TNz(children, ..) => {
                            if !is_ops_block_local(children, &[-ptr_offset]) {
                                ignore = true;
                                break;
                            }
                        }
                        OpType::SearchZero(..) => {
                            ignore = true;
                            break;
                        }
                        OpType::PutChar(..)
                        | OpType::PutString(..)
                        | OpType::GetChar(..)
                        | OpType::Inc(..)
                        | OpType::Dec(..)
                        | OpType::Set(..)
                        | OpType::Add(..)
                        | OpType::Sub(..)
                        | OpType::CAdd(..)
                        | OpType::CSub(..)
                        | OpType::Move(..)
                        | OpType::NzAdd(..)
                        | OpType::NzSub(..)
                        | OpType::NzCAdd(..)
                        | OpType::NzCSub(..)
                        | OpType::Mul(..)
                        | OpType::NzMul(..)
                        | OpType::Copy(..) => {
                            // ignore
                        }
                    }
                }

                !ignore && ptr_offset == 0
            }
        } else {
            false
        };

        if replace {
            let prev = ops.remove(i);
            let span = prev.span.clone();

            if let OpType::DLoop(mut children, _) = prev.op_type {
                while !children.is_empty()
                    && children[children.len() - 1].op_type.is_ptr_inc_or_dec()
                {
                    children.remove(children.len() - 1);
                }

                let access = get_loop_access(&children, false);

                ops.insert_or_push(i, Op::l_loop(span, children, BlockInfo::new_access(access)));

                progress = true;
            } else {
                unreachable!("Bad loop type {:?}", prev);
            }
        }
        i += 1;
    }

    progress
}

fn get_loop_access(ops: &[Op], wrapping_is_ub: bool) -> Vec<CellAccess> {
    let mut access = vec![];

    let mut start_offset = 0;

    for op in ops {
        match &op.op_type {
            OpType::IncPtr(offset) => start_offset += *offset as isize,
            OpType::DecPtr(offset) => start_offset -= *offset as isize,
            OpType::Set(offset, value) => {
                CellAccess::add(&mut access, start_offset + offset, Cell::Value(*value));
            }
            OpType::Inc(offset, v) | OpType::NzCAdd(_, offset, v) => {
                if wrapping_is_ub {
                    CellAccess::add_with_read(&mut access, start_offset + offset, Cell::NonZero);
                } else {
                    match CellAccess::get(&access, start_offset + offset) {
                        Some(Cell::Bool) => {
                            if *v > 0 && *v < 254 {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + offset,
                                    Cell::NonZero,
                                );
                            } else {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + offset,
                                    Cell::Write,
                                );
                            }
                        }
                        Some(Cell::Range(start, end)) => {
                            if 255 - start >= *v {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + offset,
                                    Cell::Range(start + *v, end + *v),
                                );
                            } else {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + offset,
                                    Cell::Write,
                                );
                            }
                        }
                        _ => CellAccess::add_with_read(
                            &mut access,
                            start_offset + offset,
                            Cell::Write,
                        ),
                    }
                }
            }
            OpType::CAdd(src_offset, dest_offset, v) => {
                CellAccess::add(&mut access, start_offset + src_offset, Cell::Value(0));
                if wrapping_is_ub {
                    CellAccess::add_with_read(
                        &mut access,
                        start_offset + dest_offset,
                        Cell::NonZero,
                    );
                } else {
                    match CellAccess::get(&access, start_offset + dest_offset) {
                        Some(Cell::Bool) => {
                            if *v > 0 && *v < 254 {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + dest_offset,
                                    Cell::NonZero,
                                );
                            } else {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + dest_offset,
                                    Cell::Write,
                                );
                            }
                        }
                        Some(Cell::Range(start, end)) => {
                            if 255 - start >= *v {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + dest_offset,
                                    Cell::Range(start + *v, end + *v),
                                );
                            } else {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + dest_offset,
                                    Cell::Write,
                                );
                            }
                        }
                        _ => CellAccess::add_with_read(
                            &mut access,
                            start_offset + dest_offset,
                            Cell::Write,
                        ),
                    }
                }
            }
            OpType::Dec(offset, v) | OpType::NzCSub(_, offset, v) => {
                match CellAccess::get(&access, start_offset + offset) {
                    Some(Cell::Range(start, end)) => {
                        if start >= *v {
                            CellAccess::add_with_read(
                                &mut access,
                                start_offset + offset,
                                Cell::Range(start - *v, end - *v),
                            );
                        } else {
                            CellAccess::add_with_read(
                                &mut access,
                                start_offset + offset,
                                Cell::Write,
                            );
                        }
                    }
                    _ => CellAccess::add_with_read(&mut access, start_offset + offset, Cell::Write),
                }
            }
            OpType::CSub(src_offset, dest_offset, v) => {
                CellAccess::add(&mut access, start_offset + src_offset, Cell::Value(0));
                match CellAccess::get(&access, start_offset + dest_offset) {
                    Some(Cell::Range(start, end)) => {
                        if start >= *v {
                            CellAccess::add_with_read(
                                &mut access,
                                start_offset + dest_offset,
                                Cell::Range(start - *v, end - *v),
                            );
                        } else {
                            CellAccess::add_with_read(
                                &mut access,
                                start_offset + dest_offset,
                                Cell::Write,
                            );
                        }
                    }
                    _ => CellAccess::add_with_read(
                        &mut access,
                        start_offset + dest_offset,
                        Cell::Write,
                    ),
                }
            }
            OpType::NzAdd(src_offset, dest_offset, _)
            | OpType::NzSub(src_offset, dest_offset, _)
            | OpType::NzMul(src_offset, dest_offset, _) => {
                CellAccess::add(&mut access, start_offset + src_offset, Cell::Read);
                CellAccess::add_with_read(&mut access, start_offset + dest_offset, Cell::Write);
            }
            OpType::Move(src_offset, dest_offset) => {
                if let Some(value) = CellAccess::get(&access, start_offset + src_offset) {
                    CellAccess::add(&mut access, start_offset + dest_offset, value);
                } else {
                    CellAccess::add(&mut access, start_offset + dest_offset, Cell::Write);
                }
                CellAccess::add_with_read(&mut access, start_offset + src_offset, Cell::Value(0));
            }
            OpType::Copy(src_offset, dest_offset) => {
                if let Some(value) = CellAccess::get(&access, start_offset + src_offset) {
                    CellAccess::add(&mut access, start_offset + dest_offset, value);
                } else {
                    CellAccess::add(&mut access, start_offset + dest_offset, Cell::Write);
                }
                CellAccess::add_with_read(&mut access, start_offset + dest_offset, Cell::Read);
            }
            OpType::Add(src_offset, dest_offset, _)
            | OpType::Sub(src_offset, dest_offset, _)
            | OpType::Mul(src_offset, dest_offset, _) => {
                CellAccess::add_with_read(&mut access, start_offset + src_offset, Cell::Value(0));
                CellAccess::add_with_read(&mut access, start_offset + dest_offset, Cell::Write);
            }
            OpType::LLoop(_, info) | OpType::ILoop(_, _, _, info) | OpType::TNz(_, info) => {
                if let Some(cell_access) = info.cell_access() {
                    for cell in cell_access {
                        if info.always_used() {
                            if cell.has_read() {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + cell.offset,
                                    cell.value,
                                );
                            } else {
                                CellAccess::add(
                                    &mut access,
                                    start_offset + cell.offset,
                                    cell.value,
                                );
                            }
                        } else {
                            CellAccess::add_conditional(
                                &mut access,
                                start_offset + cell.offset,
                                cell.value,
                                cell.has_read(),
                            );
                        }
                    }
                }
                CellAccess::add_with_read(&mut access, start_offset, Cell::Value(0));
            }
            OpType::CLoop(_, _, _, info) => {
                if let Some(cell_access) = info.cell_access() {
                    for cell in cell_access {
                        if info.always_used() {
                            if cell.has_read() {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + cell.offset,
                                    cell.value,
                                );
                            } else {
                                CellAccess::add(
                                    &mut access,
                                    start_offset + cell.offset,
                                    cell.value,
                                );
                            }
                        } else {
                            CellAccess::add_conditional(
                                &mut access,
                                start_offset + cell.offset,
                                cell.value,
                                cell.has_read(),
                            );
                        }
                    }
                }
                CellAccess::add(&mut access, start_offset, Cell::Value(0));
            }
            OpType::GetChar(offset) => {
                CellAccess::add(&mut access, start_offset + offset, Cell::Write);
            }
            OpType::PutChar(offset) => {
                CellAccess::add_with_read(&mut access, start_offset + offset, Cell::Read);
            }
            OpType::Start => unreachable!("Must not be called with start in children"),
            OpType::DLoop(..) => unreachable!("Must not be called with dloops in children"),
            OpType::DTNz(..) => unreachable!("Must not be called with dtnz in children"),
            OpType::SearchZero(_, _) => {
                unreachable!("Must not be called with zero search in children")
            }
            OpType::PutString(..) => {
                // ignore
            }
        }
    }

    access
}

fn get_d_loop_access(ops: &[Op], wrapping_is_ub: bool) -> Vec<CellAccess> {
    let mut access = vec![];

    let mut start_offset = 0;
    let mut was_cleared = false;

    for op in ops {
        match &op.op_type {
            OpType::IncPtr(offset) => start_offset += *offset as isize,
            OpType::DecPtr(offset) => start_offset -= *offset as isize,
            OpType::Set(offset, value) => {
                CellAccess::add(&mut access, start_offset + offset, Cell::Value(*value));
            }
            OpType::Inc(offset, v) | OpType::NzCAdd(_, offset, v) => {
                if wrapping_is_ub {
                    CellAccess::add_with_read(&mut access, start_offset + offset, Cell::NonZero);
                } else {
                    match CellAccess::get(&access, start_offset + offset) {
                        Some(Cell::Bool) => {
                            if *v > 0 && *v < 254 {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + offset,
                                    Cell::NonZero,
                                );
                            } else {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + offset,
                                    Cell::Write,
                                );
                            }
                        }
                        Some(Cell::Range(start, end)) => {
                            if 255 - start >= *v {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + offset,
                                    Cell::Range(start + *v, end + *v),
                                );
                            } else {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + offset,
                                    Cell::Write,
                                );
                            }
                        }
                        _ => CellAccess::add_with_read(
                            &mut access,
                            start_offset + offset,
                            Cell::Write,
                        ),
                    }
                }
            }
            OpType::CAdd(src_offset, dest_offset, v) => {
                CellAccess::add(&mut access, start_offset + src_offset, Cell::Value(0));
                if wrapping_is_ub {
                    CellAccess::add_with_read(
                        &mut access,
                        start_offset + dest_offset,
                        Cell::NonZero,
                    );
                } else {
                    match CellAccess::get(&access, start_offset + dest_offset) {
                        Some(Cell::Bool) => {
                            if *v > 0 && *v < 254 {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + dest_offset,
                                    Cell::NonZero,
                                );
                            } else {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + dest_offset,
                                    Cell::Write,
                                );
                            }
                        }
                        Some(Cell::Range(start, end)) => {
                            if 255 - start >= *v {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + dest_offset,
                                    Cell::Range(start + *v, end + *v),
                                );
                            } else {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + dest_offset,
                                    Cell::Write,
                                );
                            }
                        }
                        _ => CellAccess::add_with_read(
                            &mut access,
                            start_offset + dest_offset,
                            Cell::Write,
                        ),
                    }
                }
            }
            OpType::Dec(offset, v) | OpType::NzCSub(_, offset, v) => {
                match CellAccess::get(&access, start_offset + offset) {
                    Some(Cell::Range(start, end)) => {
                        if start >= *v {
                            CellAccess::add_with_read(
                                &mut access,
                                start_offset + offset,
                                Cell::Range(start - *v, end - *v),
                            );
                        } else {
                            CellAccess::add_with_read(
                                &mut access,
                                start_offset + offset,
                                Cell::Write,
                            );
                        }
                    }
                    _ => CellAccess::add_with_read(&mut access, start_offset + offset, Cell::Write),
                }
            }
            OpType::CSub(src_offset, dest_offset, v) => {
                CellAccess::add(&mut access, start_offset + src_offset, Cell::Value(0));
                match CellAccess::get(&access, start_offset + dest_offset) {
                    Some(Cell::Range(start, end)) => {
                        if start >= *v {
                            CellAccess::add_with_read(
                                &mut access,
                                start_offset + dest_offset,
                                Cell::Range(start - *v, end - *v),
                            );
                        } else {
                            CellAccess::add_with_read(
                                &mut access,
                                start_offset + dest_offset,
                                Cell::Write,
                            );
                        }
                    }
                    _ => CellAccess::add_with_read(
                        &mut access,
                        start_offset + dest_offset,
                        Cell::Write,
                    ),
                }
            }
            OpType::NzAdd(src_offset, dest_offset, _)
            | OpType::NzSub(src_offset, dest_offset, _)
            | OpType::NzMul(src_offset, dest_offset, _) => {
                CellAccess::add_with_read(&mut access, start_offset + src_offset, Cell::Read);
                CellAccess::add_with_read(&mut access, start_offset + dest_offset, Cell::Write);
            }
            OpType::Move(src_offset, dest_offset) => {
                if let Some(value) = CellAccess::get(&access, start_offset + src_offset) {
                    CellAccess::add(&mut access, start_offset + dest_offset, value);
                } else {
                    CellAccess::add(&mut access, start_offset + dest_offset, Cell::Write);
                }
                CellAccess::add_with_read(&mut access, start_offset + src_offset, Cell::Value(0));
            }
            OpType::Copy(src_offset, dest_offset) => {
                if let Some(value) = CellAccess::get(&access, start_offset + src_offset) {
                    CellAccess::add(&mut access, start_offset + dest_offset, value);
                } else {
                    CellAccess::add(&mut access, start_offset + dest_offset, Cell::Write);
                }
                CellAccess::add_with_read(&mut access, start_offset + src_offset, Cell::Read);
            }
            OpType::Add(src_offset, dest_offset, _)
            | OpType::Sub(src_offset, dest_offset, _)
            | OpType::Mul(src_offset, dest_offset, _) => {
                CellAccess::add_with_read(&mut access, start_offset + src_offset, Cell::Value(0));
                CellAccess::add_with_read(&mut access, start_offset + dest_offset, Cell::Write);
            }
            OpType::LLoop(_, info) | OpType::ILoop(_, _, _, info) | OpType::TNz(_, info) => {
                if let Some(cell_access) = info.cell_access() {
                    for cell in cell_access {
                        if info.always_used() {
                            if cell.has_read() {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + cell.offset,
                                    cell.value,
                                );
                            } else {
                                CellAccess::add(
                                    &mut access,
                                    start_offset + cell.offset,
                                    cell.value,
                                );
                            }
                        } else {
                            CellAccess::add_conditional(
                                &mut access,
                                start_offset + cell.offset,
                                cell.value,
                                cell.has_read(),
                            );
                        }
                    }
                }
                CellAccess::add_with_read(&mut access, start_offset, Cell::Value(0));
            }
            OpType::CLoop(_, _, _, info) => {
                if let Some(cell_access) = info.cell_access() {
                    for cell in cell_access {
                        if info.always_used() {
                            if cell.has_read() {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + cell.offset,
                                    cell.value,
                                );
                            } else {
                                CellAccess::add(
                                    &mut access,
                                    start_offset + cell.offset,
                                    cell.value,
                                );
                            }
                        } else {
                            CellAccess::add_conditional(
                                &mut access,
                                start_offset + cell.offset,
                                cell.value,
                                cell.has_read(),
                            );
                        }
                    }
                }
                CellAccess::add(&mut access, start_offset, Cell::Value(0));
            }
            OpType::GetChar(offset) => {
                CellAccess::add(&mut access, start_offset + offset, Cell::Write);
            }
            OpType::PutChar(offset) => {
                CellAccess::add_with_read(&mut access, start_offset + offset, Cell::Read);
            }
            OpType::Start => unreachable!("Must not be called with start in children"),
            OpType::DLoop(_, info) | OpType::DTNz(_, _, info) => {
                start_offset = 0;
                was_cleared = true;
                if info.always_used() {
                    access.clear();

                    if let Some(cell_access) = info.cell_access() {
                        for cell in cell_access {
                            if cell.has_read() {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + cell.offset,
                                    cell.value,
                                );
                            } else {
                                CellAccess::add(
                                    &mut access,
                                    start_offset + cell.offset,
                                    cell.value,
                                );
                            }
                        }
                    }
                } else if let Some(cell_access) = info.cell_access() {
                    let old_access = access;
                    access = vec![];

                    for cell in cell_access {
                        if let Some(value) =
                            CellAccess::get(&old_access, start_offset + cell.offset)
                        {
                            if cell.has_read() {
                                CellAccess::add_with_read(
                                    &mut access,
                                    start_offset + cell.offset,
                                    value,
                                );
                            } else {
                                CellAccess::add(&mut access, start_offset + cell.offset, value);
                            }
                            CellAccess::add_conditional(
                                &mut access,
                                start_offset + cell.offset,
                                cell.value,
                                cell.has_read(),
                            );
                        }
                    }
                } else {
                    access.clear();
                }
                CellAccess::add_with_read(&mut access, start_offset, Cell::Value(0));
            }
            OpType::SearchZero(step, always) => {
                start_offset = 0;
                access.clear();
                was_cleared = true;
                CellAccess::add_with_read(&mut access, start_offset, Cell::Value(0));
                if *always {
                    CellAccess::add_with_read(&mut access, -*step, Cell::NonZero);
                }
            }
            OpType::PutString(..) => {
                // ignore
            }
        }
    }

    for cell in &mut access {
        cell.offset -= start_offset;
    }

    if start_offset != 0 && !was_cleared && !CellAccess::was_written(&access, -start_offset) {
        CellAccess::add_with_read(&mut access, -start_offset, Cell::NonZero);
    }

    access
}

fn is_ops_block_local(ops: &[Op], parent_offsets: &[isize]) -> bool {
    let mut ptr_offset = 0_isize;

    for op in ops {
        match &op.op_type {
            OpType::Start => {
                // ignore
            }
            OpType::IncPtr(value) => {
                ptr_offset += *value as isize;
            }
            OpType::DecPtr(value) => {
                ptr_offset -= *value as isize;
            }
            OpType::LLoop(children, ..)
            | OpType::ILoop(children, ..)
            | OpType::CLoop(children, ..)
            | OpType::TNz(children, ..) => {
                let mut offsets = parent_offsets.to_vec();

                offsets.push(-ptr_offset);

                if !is_ops_block_local(children, &offsets) {
                    return false;
                }
            }
            OpType::DLoop(..) | OpType::DTNz(..) | OpType::SearchZero(..) => {
                return false;
            }
            OpType::PutChar(..)
            | OpType::PutString(..)
            | OpType::GetChar(..)
            | OpType::Inc(..)
            | OpType::Dec(..)
            | OpType::Set(..)
            | OpType::Add(..)
            | OpType::Sub(..)
            | OpType::CAdd(..)
            | OpType::CSub(..)
            | OpType::Move(..)
            | OpType::NzAdd(..)
            | OpType::NzSub(..)
            | OpType::NzCAdd(..)
            | OpType::NzCSub(..)
            | OpType::Mul(..)
            | OpType::NzMul(..)
            | OpType::Copy(..) => {
                // ignore
            }
        }
    }

    true
}

// Optimize loops that are known to use the same counting variable
pub fn optimize_count_loops(ops: &mut Vec<Op>) -> bool {
    if ops.is_empty() {
        return false;
    }

    let mut progress = false;

    for op in ops.iter_mut() {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= optimize_count_loops(children);
        }
    }

    let mut i = 0;

    while !ops.is_empty() && !ops.is_empty() && i < ops.len() {
        let replace = if let OpType::LLoop(children, _) = &mut ops[i].op_type {
            let mut ptr_offset = 0;
            let mut ignore = false;
            let mut counter_decrements = vec![];
            let mut counter_reads = vec![];

            for (i, op) in children.iter().enumerate() {
                match &op.op_type {
                    OpType::Start => {
                        // ignore
                    }
                    OpType::IncPtr(value) => {
                        ptr_offset += *value as isize;
                    }
                    OpType::DecPtr(value) => {
                        ptr_offset -= *value as isize;
                    }
                    OpType::Add(src_offset, dest_offset, _)
                    | OpType::CAdd(src_offset, dest_offset, _)
                    | OpType::Sub(src_offset, dest_offset, _)
                    | OpType::CSub(src_offset, dest_offset, _)
                    | OpType::Mul(src_offset, dest_offset, _)
                    | OpType::Move(src_offset, dest_offset) => {
                        if ptr_offset + src_offset == 0 {
                            ignore = true;
                            break;
                        }

                        if ptr_offset + dest_offset == 0 {
                            ignore = true;
                            break;
                        }
                    }
                    OpType::NzAdd(src_offset, dest_offset, _)
                    | OpType::NzSub(src_offset, dest_offset, _)
                    | OpType::NzMul(src_offset, dest_offset, _)
                    | OpType::Copy(src_offset, dest_offset) => {
                        if ptr_offset + dest_offset == 0 {
                            ignore = true;
                            break;
                        }

                        if ptr_offset + src_offset == 0 {
                            counter_reads.push(i)
                        }
                    }
                    OpType::NzCAdd(_, dest_offset, _) | OpType::NzCSub(_, dest_offset, _) => {
                        if ptr_offset + dest_offset == 0 {
                            ignore = true;
                            break;
                        }
                    }

                    OpType::PutChar(offset) => {
                        if ptr_offset + offset == 0 {
                            counter_reads.push(i);
                        }
                    }

                    OpType::Inc(offset, _) | OpType::Set(offset, _) => {
                        if ptr_offset + offset == 0 {
                            ignore = true;
                            break;
                        }
                    }
                    OpType::GetChar(offset) => {
                        if ptr_offset + offset == 0 {
                            ignore = true;
                            break;
                        }
                    }
                    OpType::Dec(offset, v) => {
                        if ptr_offset + offset == 0 {
                            counter_decrements.push((i, *v));
                        }
                    }
                    OpType::DLoop(..) | OpType::DTNz(..) | OpType::LLoop(..) => {
                        ignore = true;
                        break;
                    }
                    OpType::ILoop(children, ..)
                    | OpType::CLoop(children, ..)
                    | OpType::TNz(children, ..) => {
                        if ptr_offset == 0 {
                            ignore = true;
                            break;
                        }

                        if !is_ops_block_unmodified_local(children, &[-ptr_offset]) {
                            ignore = true;
                            break;
                        }
                    }
                    OpType::SearchZero(..) => {
                        ignore = true;
                        break;
                    }
                    OpType::PutString(..) => {
                        // ignore
                    }
                }
            }

            #[allow(clippy::manual_map)]
            if !ignore {
                if !counter_reads.is_empty() {
                    let min_counter = counter_decrements
                        .iter()
                        .map(|(i, _)| *i)
                        .min()
                        .unwrap_or(0);
                    let max_counter = counter_decrements
                        .iter()
                        .map(|(i, _)| *i)
                        .max()
                        .unwrap_or(usize::MAX);

                    let min_read = counter_reads.iter().cloned().min().unwrap_or(0);
                    let max_read = counter_reads.iter().cloned().max().unwrap_or(usize::MAX);

                    if max_counter < min_read {
                        Some((counter_decrements, LoopDecrement::Pre))
                    } else if min_counter > max_read {
                        Some((counter_decrements, LoopDecrement::Post))
                    } else {
                        None
                    }
                } else {
                    Some((counter_decrements, LoopDecrement::Auto))
                }
            } else {
                None
            }
        } else {
            None
        };

        if let Some((counter_decrements, loop_decrement)) = replace {
            let prev = ops.remove(i);
            let span = prev.span;

            if let OpType::LLoop(mut children, info) = prev.op_type {
                let mut step = 0;

                for (index, amount) in counter_decrements.iter().rev() {
                    step += *amount;
                    children.remove(*index);
                }

                while !children.is_empty()
                    && children[children.len() - 1].op_type.is_ptr_inc_or_dec()
                {
                    children.remove(children.len() - 1);
                }

                if step == 1 && children.is_empty() {
                    ops.insert_or_push(i, Op::set(span, 0));
                } else {
                    ops.insert_or_push(
                        i,
                        Op::i_loop_with_decrement(span, children, step, loop_decrement, info),
                    );
                }

                progress = true;
            } else {
                unreachable!();
            }
        }
        i += 1;
    }

    progress
}

fn is_ops_block_unmodified_local(ops: &[Op], parent_offsets: &[isize]) -> bool {
    let mut ptr_offset = 0_isize;

    for op in ops {
        match &op.op_type {
            OpType::Start => {
                // ignore
            }
            OpType::IncPtr(value) => {
                ptr_offset += *value as isize;
            }
            OpType::DecPtr(value) => {
                ptr_offset -= *value as isize;
            }
            OpType::Add(src_offset, dest_offset, _)
            | OpType::Sub(src_offset, dest_offset, _)
            | OpType::CAdd(src_offset, dest_offset, _)
            | OpType::CSub(src_offset, dest_offset, _)
            | OpType::Mul(src_offset, dest_offset, _)
            | OpType::Move(src_offset, dest_offset) => {
                for parent_offset in parent_offsets {
                    if ptr_offset + src_offset == *parent_offset {
                        return false;
                    }
                }

                for parent_offset in parent_offsets {
                    if ptr_offset + dest_offset == *parent_offset {
                        return false;
                    }
                }
            }
            OpType::NzAdd(_, dest_offset, _)
            | OpType::NzSub(_, dest_offset, _)
            | OpType::NzCAdd(_, dest_offset, _)
            | OpType::NzCSub(_, dest_offset, _)
            | OpType::NzMul(_, dest_offset, _)
            | OpType::Copy(_, dest_offset) => {
                for parent_offset in parent_offsets {
                    if ptr_offset + dest_offset == *parent_offset {
                        return false;
                    }
                }
            }
            OpType::Inc(offset, _) | OpType::Dec(offset, _) | OpType::Set(offset, _) => {
                for parent_offset in parent_offsets {
                    if ptr_offset + offset == *parent_offset {
                        return false;
                    }
                }
            }
            OpType::GetChar(offset) => {
                for parent_offset in parent_offsets {
                    if ptr_offset + offset == *parent_offset {
                        return false;
                    }
                }
            }
            OpType::ILoop(children, ..)
            | OpType::CLoop(children, ..)
            | OpType::TNz(children, ..) => {
                let mut offsets = parent_offsets.to_vec();

                offsets.push(-ptr_offset);

                if !is_ops_block_unmodified_local(children, &offsets) {
                    return false;
                }
            }
            OpType::DLoop(..) | OpType::DTNz(..) | OpType::LLoop(..) | OpType::SearchZero(..) => {
                return false;
            }
            OpType::PutChar(..) | OpType::PutString(..) => {
                // ignore
            }
        }
    }

    true
}

pub fn optimize_arithmetics(ops: [&Op; 2]) -> Change {
    match (&ops[0].op_type, &ops[1].op_type) {
        (OpType::Inc(offset, v1), OpType::Inc(offset2, v2)) => {
            if *offset == *offset2 {
                Change::Replace(vec![OpType::Inc(*offset, v1.wrapping_add(*v2))])
            } else {
                Change::Ignore
            }
        }
        (OpType::Dec(offset, v1), OpType::Dec(offset2, v2)) => {
            if *offset == *offset2 {
                Change::Replace(vec![OpType::Dec(*offset, v1.wrapping_add(*v2))])
            } else {
                Change::Ignore
            }
        }
        (OpType::Inc(offset, v1), OpType::Dec(offset2, v2)) => {
            if *offset == *offset2 {
                match v1.cmp(v2) {
                    Ordering::Equal => Change::Remove,
                    Ordering::Greater => Change::Replace(vec![OpType::Inc(*offset, v1 - v2)]),
                    Ordering::Less => Change::Replace(vec![OpType::Dec(*offset, v2 - v1)]),
                }
            } else {
                Change::Ignore
            }
        }
        (OpType::Dec(offset, v1), OpType::Inc(offset2, v2)) => {
            if *offset == *offset2 {
                match v1.cmp(v2) {
                    Ordering::Equal => Change::Remove,
                    Ordering::Greater => Change::Replace(vec![OpType::Dec(*offset, v1 - v2)]),
                    Ordering::Less => Change::Replace(vec![OpType::Inc(*offset, v2 - v1)]),
                }
            } else {
                Change::Ignore
            }
        }
        (OpType::IncPtr(v1), OpType::IncPtr(v2)) => {
            Change::Replace(vec![OpType::IncPtr(v1.wrapping_add(*v2))])
        }
        (OpType::DecPtr(v1), OpType::DecPtr(v2)) => {
            Change::Replace(vec![OpType::DecPtr(v1.wrapping_add(*v2))])
        }
        (OpType::IncPtr(v1), OpType::DecPtr(v2)) => match v1.cmp(v2) {
            Ordering::Equal => Change::Remove,
            Ordering::Greater => Change::Replace(vec![OpType::IncPtr(v1 - v2)]),
            Ordering::Less => Change::Replace(vec![OpType::DecPtr(v2 - v1)]),
        },
        (OpType::DecPtr(v1), OpType::IncPtr(v2)) => match v1.cmp(v2) {
            Ordering::Equal => Change::Remove,
            Ordering::Greater => Change::Replace(vec![OpType::DecPtr(v1 - v2)]),
            Ordering::Less => Change::Replace(vec![OpType::IncPtr(v2 - v1)]),
        },
        (OpType::Set(offset, v1), OpType::Inc(offset2, v2)) => {
            if *offset == *offset2 {
                Change::Replace(vec![OpType::Set(*offset, v1.wrapping_add(*v2))])
            } else {
                Change::Ignore
            }
        }
        (OpType::Set(offset, v1), OpType::Dec(offset2, v2)) => {
            if *offset == *offset2 {
                Change::Replace(vec![OpType::Set(*offset, v1.wrapping_sub(*v2))])
            } else {
                Change::Ignore
            }
        }
        (OpType::Set(offset, v1), OpType::PutChar(offset2)) => {
            if *offset == *offset2 {
                Change::ReplaceOffset(1, ops[1].span.clone(), vec![OpType::PutString(vec![*v1])])
            } else {
                Change::Ignore
            }
        }
        (OpType::PutString(a1), OpType::PutString(a2)) => {
            let mut value = a1.clone();
            value.extend_from_slice(a2);
            Change::Replace(vec![OpType::PutString(value)])
        }
        (op_type, OpType::Inc(offset, v)) => {
            if op_type.is_zeroing(*offset) {
                Change::ReplaceOffset(1, ops[1].span.clone(), vec![OpType::Set(*offset, *v)])
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::Dec(offset, v)) => {
            if op_type.is_zeroing(*offset) {
                Change::ReplaceOffset(
                    1,
                    ops[1].span.clone(),
                    vec![OpType::Set(*offset, 0u8.wrapping_sub(*v))],
                )
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::Set(offset, 0)) => {
            if op_type.is_zeroing(*offset) {
                Change::RemoveOffset(1)
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::Add(src_offset, dest_offset, multi)) => {
            if op_type.is_zeroing(*dest_offset) {
                if *multi == 1 {
                    Change::ReplaceOffset(
                        1,
                        ops[1].span.clone(),
                        vec![OpType::Move(*src_offset, *dest_offset)],
                    )
                } else {
                    Change::ReplaceOffset(
                        1,
                        ops[1].span.clone(),
                        vec![OpType::Mul(*src_offset, *dest_offset, *multi)],
                    )
                }
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::NzAdd(src_offset, dest_offset, multi)) => {
            if op_type.is_zeroing(*dest_offset) {
                if *multi == 1 {
                    Change::ReplaceOffset(
                        1,
                        ops[1].span.clone(),
                        vec![OpType::Copy(*src_offset, *dest_offset)],
                    )
                } else {
                    Change::ReplaceOffset(
                        1,
                        ops[1].span.clone(),
                        vec![OpType::NzMul(*src_offset, *dest_offset, *multi)],
                    )
                }
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::CAdd(src_offset, dest_offset, value)) => {
            if op_type.is_zeroing(*dest_offset) {
                Change::ReplaceOffset(
                    1,
                    ops[1].span.clone(),
                    vec![
                        OpType::Set(*dest_offset, *value),
                        OpType::Set(*src_offset, 0),
                    ],
                )
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::NzCAdd(_src_offset, dest_offset, value)) => {
            if op_type.is_zeroing(*dest_offset) {
                Change::ReplaceOffset(
                    1,
                    ops[1].span.clone(),
                    vec![OpType::Set(*dest_offset, *value)],
                )
            } else {
                Change::Ignore
            }
        }
        _ => Change::Ignore,
    }
}

pub fn remove_dead_stores_before_set(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() - 1 {
        let dest_offset = ops[i + 1].op_type.get_overwriting_dest_offset();
        let unread_zeroing_offset = ops[i + 1].op_type.get_unread_zeroing_src_offset();

        let op1 = &mut ops[i];

        let remove = if let Some(dest_offset) = dest_offset {
            if match &op1.op_type {
                OpType::Inc(offset, _)
                | OpType::Dec(offset, _)
                | OpType::Set(offset, _)
                | OpType::Copy(_, offset)
                | OpType::NzAdd(_, offset, ..)
                | OpType::NzCAdd(_, offset, ..)
                | OpType::NzSub(_, offset, ..)
                | OpType::NzCSub(_, offset, ..)
                | OpType::NzMul(_, offset, ..) => *offset == dest_offset,
                _ => false,
            } {
                true
            } else {
                match &op1.op_type {
                    OpType::Add(src, dest, multi) => {
                        if *src == dest_offset {
                            op1.op_type = OpType::NzAdd(*src, *dest, *multi);
                            progress = true;
                        }
                    }
                    OpType::CAdd(src, dest, value) => {
                        if *src == dest_offset {
                            op1.op_type = OpType::NzCAdd(*src, *dest, *value);
                            progress = true;
                        }
                    }
                    OpType::Sub(src, dest, multi) => {
                        if *src == dest_offset {
                            op1.op_type = OpType::NzSub(*src, *dest, *multi);
                            progress = true;
                        }
                    }
                    OpType::CSub(src, dest, value) => {
                        if *src == dest_offset {
                            op1.op_type = OpType::NzCSub(*src, *dest, *value);
                            progress = true;
                        }
                    }
                    OpType::Mul(src, dest, multi) => {
                        if *src == dest_offset {
                            op1.op_type = OpType::NzMul(*src, *dest, *multi);
                            progress = true;
                        }
                    }
                    OpType::Move(src, dest) => {
                        if *src == dest_offset {
                            op1.op_type = OpType::Copy(*src, *dest);
                            progress = true;
                        }
                    }
                    _ => {
                        // Ignore
                    }
                }
                false
            }
        } else {
            false
        };

        if remove {
            ops.remove(i);
            progress = true;
        } else {
            if let Some(unread_zeroing_offset) = unread_zeroing_offset {
                match &op1.op_type {
                    OpType::Add(src, dest, multi) => {
                        if *src == unread_zeroing_offset {
                            op1.op_type = OpType::NzAdd(*src, *dest, *multi);
                            progress = true;
                        }
                    }
                    OpType::CAdd(src, dest, value) => {
                        if *src == unread_zeroing_offset {
                            op1.op_type = OpType::NzCAdd(*src, *dest, *value);
                            progress = true;
                        }
                    }
                    OpType::Sub(src, dest, multi) => {
                        if *src == unread_zeroing_offset {
                            op1.op_type = OpType::NzSub(*src, *dest, *multi);
                            progress = true;
                        }
                    }
                    OpType::CSub(src, dest, value) => {
                        if *src == unread_zeroing_offset {
                            op1.op_type = OpType::NzCSub(*src, *dest, *value);
                            progress = true;
                        }
                    }
                    OpType::Mul(src, dest, multi) => {
                        if *src == unread_zeroing_offset {
                            op1.op_type = OpType::NzMul(*src, *dest, *multi);
                            progress = true;
                        }
                    }
                    OpType::Move(src, dest) => {
                        if *src == unread_zeroing_offset {
                            op1.op_type = OpType::Copy(*src, *dest);
                            progress = true;
                        }
                    }
                    _ => {
                        // Ignore
                    }
                }
            }

            i += 1;
        }
    }

    for op in ops {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= remove_dead_stores_before_set(children);
        }
    }

    progress
}

pub fn optimize_static_count_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() - 1 {
        let op1 = &ops[i];
        let op2 = &ops[i + 1];

        let count = match (&op1.op_type, &op2.op_type) {
            (OpType::Set(0, v), OpType::ILoop(_, step, ..)) => {
                if *step != 0 && v % step == 0 {
                    Some(v / step)
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(count) = count {
            let prev = ops.remove(i);
            if count == 0 {
                ops.remove(i);
            } else {
                let loop_op = ops.remove(i);
                let span = prev.span.start..loop_op.span.end;

                if let OpType::ILoop(children, _, decrements, access) = loop_op.op_type {
                    ops.insert_or_push(
                        i,
                        Op::c_loop_with_decrement(span, children, count, decrements, access),
                    );
                } else {
                    unreachable!();
                }

                i += 1;
            }
        } else if let Some(children) = ops[i].op_type.get_children_mut() {
            progress |= optimize_static_count_loops(children);
            i += 1;
        } else {
            i += 1;
        }
    }

    if let Some(last) = ops.last_mut() {
        if let Some(children) = last.op_type.get_children_mut() {
            progress |= optimize_static_count_loops(children);
        }
    }

    progress
}

pub fn optimize_non_local_static_count_loops(ops: &mut Vec<Op>, wrapping_is_ub: bool) -> bool {
    run_non_local_pass(
        ops,
        optimize_non_local_static_count_loops_pass,
        true,
        &[],
        wrapping_is_ub,
    )
}

pub fn optimize_non_local_static_count_loops_pass(
    ops: &mut Vec<Op>,
    zeroing: bool,
    inputs: &[(isize, CellValue)],
    wrapping_is_ub: bool,
) -> bool {
    let mut progress = false;

    let mut i = 1;

    while !ops.is_empty() && i < ops.len() {
        let op = &ops[i];

        let count = match &op.op_type {
            OpType::ILoop(_, step, ..) => {
                if let CellValue::Value(v) = utils::find_heap_value(
                    ops,
                    0,
                    i as isize - 1,
                    zeroing,
                    inputs,
                    wrapping_is_ub,
                    true,
                ) {
                    if *step != 0 && v % step == 0 {
                        Some(v / step)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(count) = count {
            let loop_op = ops.remove(i);
            let span = loop_op.span;

            if let OpType::ILoop(children, _, decrement, info) = loop_op.op_type {
                if decrement == LoopDecrement::Auto && count == 1 {
                    ops.insert_or_push(i, Op::t_nz(span, children, info));
                } else if count > 1 {
                    ops.insert_or_push(
                        i,
                        Op::c_loop_with_decrement(span, children, count, decrement, info),
                    );
                }
            } else {
                unreachable!();
            }

            progress = true;
        }
        i += 1;
    }

    progress
}

pub fn optimize_non_local_conditional_loops(ops: &mut Vec<Op>, wrapping_is_ub: bool) -> bool {
    run_non_local_pass(
        ops,
        optimize_non_local_conditional_loops_pass,
        true,
        &[],
        wrapping_is_ub,
    )
}

fn optimize_non_local_conditional_loops_pass(
    ops: &mut Vec<Op>,
    zeroing: bool,
    inputs: &[(isize, CellValue)],
    wrapping_is_ub: bool,
) -> bool {
    let mut progress = false;

    let mut i = 1;

    while !ops.is_empty() && i < ops.len() {
        let op = &ops[i];

        let replace = match &op.op_type {
            OpType::ILoop(_, step, ..) => {
                if *step == 1 {
                    let v = utils::find_heap_value(
                        ops,
                        0,
                        i as isize - 1,
                        zeroing,
                        inputs,
                        wrapping_is_ub,
                        true,
                    );
                    matches!(v, CellValue::Bool)
                } else {
                    false
                }
            }
            OpType::DLoop(children, _) => {
                is_zeroing_d_loop(children, ops, i as isize, zeroing, inputs, wrapping_is_ub)
            }
            _ => false,
        };

        if replace {
            let loop_op = ops.remove(i);
            let span = loop_op.span;

            match loop_op.op_type {
                OpType::ILoop(children, _, _, info) => {
                    ops.insert_or_push(i, Op::t_nz(span, children, info));
                }
                OpType::DLoop(children, info) => {
                    let offset = get_d_loop_ptr_end_offset(&children);
                    ops.insert_or_push(i, Op::d_t_nz(span, children, offset, info));
                }
                _ => unreachable!(),
            }
            progress = true;
        }
        i += 1;
    }

    progress
}

pub fn is_zeroing_d_loop(
    ops: &[Op],
    parent_ops: &[Op],
    parent_index: isize,
    zeroing: bool,
    inputs: &[(isize, CellValue)],
    wrapping_is_ub: bool,
) -> bool {
    let mut ptr_offset = 0;

    for (i, op) in ops.iter().enumerate().rev() {
        if op.op_type.is_zeroing(-ptr_offset) {
            return true;
        }

        match &op.op_type {
            OpType::IncPtr(offset) => ptr_offset -= *offset as isize,
            OpType::DecPtr(offset) => ptr_offset += *offset as isize,
            OpType::Set(offset, v) => {
                if ptr_offset + offset == 0 {
                    return *v == 0;
                }
            }
            OpType::Dec(offset, _)
            | OpType::Inc(offset, _)
            | OpType::GetChar(offset)
            | OpType::Add(_, offset, _)
            | OpType::CAdd(_, offset, _)
            | OpType::NzAdd(_, offset, _)
            | OpType::NzCAdd(_, offset, _)
            | OpType::Sub(_, offset, _)
            | OpType::CSub(_, offset, _)
            | OpType::NzSub(_, offset, _)
            | OpType::NzCSub(_, offset, _)
            | OpType::Mul(_, offset, _)
            | OpType::NzMul(_, offset, _)
            | OpType::Copy(_, offset)
            | OpType::Move(_, offset) => {
                if ptr_offset + offset == 0 {
                    return false;
                }
            }
            OpType::LLoop(.., info)
            | OpType::ILoop(.., info)
            | OpType::CLoop(.., info)
            | OpType::TNz(.., info) => {
                if let Some(cell) = info.get_access_value(-ptr_offset) {
                    if info.always_used() {
                        return cell == Cell::Value(0);
                    } else if cell != Cell::Value(0) {
                        return false;
                    }
                }
            }
            OpType::Start | OpType::PutString(_) | OpType::PutChar(_) => {
                // Ignore
            }
            OpType::SearchZero(..) => {
                return false;
            }
            OpType::DLoop(.., info) => {
                if info.always_used() {
                    return info.get_access_value(-ptr_offset) == Some(Cell::Value(0));
                } else {
                    return false;
                }
            }
            OpType::DTNz(_, Some(offset), info) => {
                if info.get_access_value(-ptr_offset) == Some(Cell::Value(0)) {
                    if find_heap_value(
                        ops,
                        -*offset,
                        i as isize - 1,
                        zeroing,
                        inputs,
                        wrapping_is_ub,
                        true,
                    ) == CellValue::Value(0)
                    {
                        return true;
                    }
                }
                return false;
            }
            OpType::DTNz(..) => {
                return false;
            }
        }
    }

    find_heap_value(
        parent_ops,
        -ptr_offset,
        parent_index - 1,
        zeroing,
        inputs,
        wrapping_is_ub,
        true,
    )
    .is_zero()
}

// Replace loops only containing constant sets with TNz
pub fn optimize_conditional_loops(ops: &mut Vec<Op>) -> bool {
    let mut progress = false;

    for op in ops.iter_mut() {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= optimize_conditional_loops(children);
        }
    }

    let mut i = 0;

    while !ops.is_empty() && i < ops.len() {
        let op = &mut ops[i];

        let replace = match &op.op_type {
            OpType::ILoop(children, ..) | OpType::CLoop(children, ..) => {
                contains_only_constant_sets(children)
            }
            OpType::LLoop(children, info) => {
                #[allow(clippy::needless_bool, clippy::if_same_then_else)]
                if !children.is_empty()
                    && is_zero_end_offset(children, 0)
                    && children[children.len() - 1].op_type.is_zeroing(0)
                {
                    true
                } else if matches!(info.get_access_value(0), Some(Value(0))) {
                    true
                } else {
                    false
                }
            }
            _ => false,
        };

        if replace {
            progress = true;
            let prev = ops.remove(i);
            let span = prev.span;

            match prev.op_type {
                OpType::LLoop(children, info)
                | OpType::ILoop(children, _, _, info)
                | OpType::CLoop(children, _, _, info) => {
                    ops.insert_or_push(i, Op::t_nz(span, children, info));
                }
                _ => unreachable!(),
            }
        }

        i += 1;
    }

    progress
}

fn is_zero_end_offset(ops: &[Op], start_offset: isize) -> bool {
    let mut ptr_offset = start_offset;

    for op in ops {
        match &op.op_type {
            OpType::IncPtr(offset) => ptr_offset += *offset as isize,
            OpType::DecPtr(offset) => ptr_offset -= *offset as isize,
            OpType::Inc(..)
            | OpType::Dec(..)
            | OpType::Set(..)
            | OpType::Add(..)
            | OpType::CAdd(..)
            | OpType::NzAdd(..)
            | OpType::NzCAdd(..)
            | OpType::Sub(..)
            | OpType::CSub(..)
            | OpType::NzSub(..)
            | OpType::NzCSub(..)
            | OpType::Move(..)
            | OpType::Copy(..)
            | OpType::GetChar(..)
            | OpType::PutChar(..)
            | OpType::LLoop(..)
            | OpType::ILoop(..)
            | OpType::CLoop(..)
            | OpType::TNz(..) => {
                // ignore
            }
            _ => return false,
        }
    }

    ptr_offset == 0
}

fn contains_only_constant_sets(ops: &[Op]) -> bool {
    for op in ops {
        match &op.op_type {
            OpType::Set(_, _) | OpType::IncPtr(_) | OpType::DecPtr(_) => {
                // allowed
            }
            _ => return false,
        }
    }

    true
}

pub fn optimize_search_zero(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        let op = &mut ops[i];

        let replace = match &op.op_type {
            OpType::DLoop(children, _) => {
                if children.len() == 1 {
                    match children[0].op_type {
                        OpType::IncPtr(step) => Some(step as isize),
                        OpType::DecPtr(step) => Some(-(step as isize)),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(step) = replace {
            let span = ops[i].span.clone();

            ops[i] = Op::search_zero(span, step);

            progress = true;
        } else if let Some(children) = ops[i].op_type.get_children_mut() {
            progress |= optimize_search_zero(children);
        }

        i += 1;
    }

    progress
}

pub fn optimize_constant_arithmetic_loop(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        let op = &mut ops[i];

        let replace = if let OpType::CLoop(children, _, decrement, ..) = &op.op_type {
            *decrement == LoopDecrement::Auto && contains_only_simple_arithmetics(children)
        } else {
            false
        };

        if replace {
            let prev = ops.remove(i);
            let span = prev.span;

            match prev.op_type {
                OpType::CLoop(children, iterations, _, _) => {
                    let mut ptr_offset = 0;

                    let mut pos = i;

                    #[allow(clippy::comparison_chain)]
                    if ptr_offset > 0 {
                        ops.insert_or_push(
                            pos,
                            Op::inc_ptr(span.start..span.start + 1, ptr_offset as usize),
                        );
                        pos += 1;
                    } else if ptr_offset < 0 {
                        ops.insert_or_push(
                            pos,
                            Op::dec_ptr(span.start..span.start + 1, -ptr_offset as usize),
                        );
                        pos += 1;
                    }

                    for mut child in children {
                        match &mut child.op_type {
                            OpType::Inc(_, v) | OpType::Dec(_, v) => {
                                *v = v.wrapping_mul(iterations)
                            }
                            OpType::IncPtr(v) => ptr_offset += *v as isize,
                            OpType::DecPtr(v) => ptr_offset -= *v as isize,
                            _ => {
                                // ignore
                            }
                        }

                        ops.insert_or_push(pos, child);

                        pos += 1;
                    }

                    #[allow(clippy::comparison_chain)]
                    if ptr_offset > 0 {
                        ops.insert_or_push(
                            pos,
                            Op::dec_ptr(span.end - 1..span.end, ptr_offset as usize),
                        );
                    } else if ptr_offset < 0 {
                        ops.insert_or_push(
                            pos,
                            Op::inc_ptr(span.end - 1..span.end, -ptr_offset as usize),
                        );
                    }

                    pos += 1;

                    ops.insert_or_push(pos, Op::set(span.end - 1..span.end, 0));
                }
                _ => unreachable!(),
            }
        }

        if let Some(children) = ops[i].op_type.get_children_mut() {
            progress |= optimize_constant_arithmetic_loop(children);
        }

        i += 1;
    }

    progress
}

fn contains_only_simple_arithmetics(ops: &[Op]) -> bool {
    for op in ops {
        if !op.op_type.is_simple_arithmetic() {
            return false;
        }
    }
    true
}

pub fn optimize_constant_arithmetics(ops: [&Op; 2]) -> Change {
    match (&ops[0].op_type, &ops[1].op_type) {
        (OpType::Set(offset, value), OpType::Add(src_offset, dest_offset, multi)) => {
            if *offset == *src_offset {
                Change::Replace(vec![OpType::CAdd(
                    *src_offset,
                    *dest_offset,
                    value.wrapping_mul(*multi),
                )])
            } else {
                Change::Ignore
            }
        }
        (OpType::Set(offset, value), OpType::Sub(src_offset, dest_offset, multi)) => {
            if *offset == *src_offset {
                Change::Replace(vec![OpType::CSub(
                    *src_offset,
                    *dest_offset,
                    value.wrapping_mul(*multi),
                )])
            } else {
                Change::Ignore
            }
        }
        _ => Change::Ignore,
    }
}

pub fn optimize_offsets(ops: &mut Vec<Op>, start_offset: isize) -> bool {
    if ops.is_empty() {
        return false;
    }

    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        let mut inner_progress = false;

        let mut ptr_offset = start_offset;

        let start = ops[0].span.start;
        let mut end = start + 1;

        let mut num_ptr_changes = 0;

        while !ops.is_empty() && i < ops.len() {
            let op = &mut ops[i];

            let remove = match &mut op.op_type {
                OpType::Set(offset, _)
                | OpType::Inc(offset, _)
                | OpType::Dec(offset, _)
                | OpType::PutChar(offset)
                | OpType::GetChar(offset) => {
                    let op_offset = ptr_offset + *offset - start_offset;
                    if *offset != op_offset {
                        *offset = op_offset;
                        inner_progress = true;
                    }
                    false
                }
                OpType::Add(src_offset, dest_offset, _)
                | OpType::NzAdd(src_offset, dest_offset, _)
                | OpType::CAdd(src_offset, dest_offset, _)
                | OpType::NzCAdd(src_offset, dest_offset, _)
                | OpType::Sub(src_offset, dest_offset, _)
                | OpType::NzSub(src_offset, dest_offset, _)
                | OpType::CSub(src_offset, dest_offset, _)
                | OpType::NzCSub(src_offset, dest_offset, _)
                | OpType::Mul(src_offset, dest_offset, _)
                | OpType::NzMul(src_offset, dest_offset, _)
                | OpType::Move(src_offset, dest_offset)
                | OpType::Copy(src_offset, dest_offset) => {
                    let op_src_offset = ptr_offset + *src_offset - start_offset;
                    let op_dest_offset = ptr_offset + *dest_offset - start_offset;
                    if *src_offset != op_src_offset || *dest_offset != op_dest_offset {
                        *src_offset = op_src_offset;
                        *dest_offset = op_dest_offset;
                        inner_progress = true;
                    }
                    false
                }
                OpType::IncPtr(v) => {
                    ptr_offset += *v as isize;
                    num_ptr_changes += 1;
                    true
                }
                OpType::DecPtr(v) => {
                    ptr_offset -= *v as isize;
                    num_ptr_changes += 1;
                    true
                }
                OpType::PutString(..) | OpType::Start => false,
                OpType::DLoop(..)
                | OpType::DTNz(..)
                | OpType::LLoop(..)
                | OpType::ILoop(..)
                | OpType::CLoop(..)
                | OpType::TNz(..)
                | OpType::SearchZero(..) => {
                    break;
                }
            };

            end = op.span.end;

            if remove {
                ops.remove(i);
            } else {
                i += 1;
            }
        }

        if (num_ptr_changes != 0 || inner_progress) && ptr_offset != start_offset {
            let span = start..end;

            let offset = ptr_offset - start_offset;

            if offset > 0 {
                ops.insert_or_push(i, Op::inc_ptr(span, offset as usize));
            } else {
                ops.insert_or_push(i, Op::dec_ptr(span, -offset as usize));
            }
        }

        if num_ptr_changes > 1 {
            progress = true;
        }

        i += 1;
    }

    for op in ops {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= optimize_offsets(children, 0);
        }
    }

    progress
}

pub fn optimize_arithmetic_offsets(ops: [&Op; 3]) -> Change {
    match (&ops[0].op_type, &ops[1].op_type, &ops[2].op_type) {
        (OpType::IncPtr(i1), OpType::Add(src_offset, dest_offset, value), OpType::DecPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::Add(
                    src_offset + *i1 as isize,
                    dest_offset + *i1 as isize,
                    *value,
                )])
            } else {
                Change::Ignore
            }
        }
        (OpType::DecPtr(i1), OpType::Add(src_offset, dest_offset, value), OpType::IncPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::Add(
                    src_offset - *i1 as isize,
                    dest_offset - *i1 as isize,
                    *value,
                )])
            } else {
                Change::Ignore
            }
        }
        (OpType::IncPtr(i1), OpType::CAdd(src_offset, dest_offset, value), OpType::DecPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::CAdd(
                    src_offset + *i1 as isize,
                    dest_offset + *i1 as isize,
                    *value,
                )])
            } else {
                Change::Ignore
            }
        }
        (OpType::DecPtr(i1), OpType::CAdd(src_offset, dest_offset, value), OpType::IncPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::CAdd(
                    src_offset - *i1 as isize,
                    dest_offset - *i1 as isize,
                    *value,
                )])
            } else {
                Change::Ignore
            }
        }
        (OpType::IncPtr(i1), OpType::Sub(src_offset, dest_offset, value), OpType::DecPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::Sub(
                    src_offset + *i1 as isize,
                    dest_offset + *i1 as isize,
                    *value,
                )])
            } else {
                Change::Ignore
            }
        }
        (OpType::DecPtr(i1), OpType::Sub(src_offset, dest_offset, value), OpType::IncPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::Sub(
                    src_offset - *i1 as isize,
                    dest_offset - *i1 as isize,
                    *value,
                )])
            } else {
                Change::Ignore
            }
        }
        (OpType::IncPtr(i1), OpType::CSub(src_offset, dest_offset, value), OpType::DecPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::CSub(
                    src_offset + *i1 as isize,
                    dest_offset + *i1 as isize,
                    *value,
                )])
            } else {
                Change::Ignore
            }
        }
        (OpType::DecPtr(i1), OpType::CSub(src_offset, dest_offset, value), OpType::IncPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::CSub(
                    src_offset - *i1 as isize,
                    dest_offset - *i1 as isize,
                    *value,
                )])
            } else {
                Change::Ignore
            }
        }
        _ => Change::Ignore,
    }
}

pub fn remove_trailing_pointer_ops(ops: &mut Vec<Op>, in_framed_block: bool) -> bool {
    let mut progress = false;

    if in_framed_block {
        while !ops.is_empty() && ops[ops.len() - 1].op_type.is_ptr_inc_or_dec() {
            ops.remove(ops.len() - 1);
            progress = true;
        }
    }

    for op in ops {
        match &mut op.op_type {
            OpType::LLoop(children, ..)
            | OpType::ILoop(children, ..)
            | OpType::CLoop(children, ..)
            | OpType::TNz(children, ..) => {
                progress |= remove_trailing_pointer_ops(children, true);
            }
            OpType::DLoop(children, _) | OpType::DTNz(children, _, _) => {
                progress |= remove_trailing_pointer_ops(children, false);
            }
            _ => {
                //ignore
            }
        }
    }

    progress
}

/// Remove useless copies that are a possible the result of a duplication of cell values in the original
/// source that result in sequences like this:
///
/// ```text
/// COPY src_offset: 3 dest_offset 7
/// COPY src_offset: 7 dest_offset 3
/// ```
///
pub fn remove_useless_copy(ops: [&Op; 2]) -> Change {
    match (&ops[0].op_type, &ops[1].op_type) {
        (OpType::Copy(src1, dest1), OpType::Copy(src2, dest2)) => {
            if *src1 == *dest2 && *src2 == *dest1 {
                Change::Replace(vec![OpType::Copy(*src1, *dest1)])
            } else {
                Change::Ignore
            }
        }
        _ => Change::Ignore,
    }
}

pub fn remove_useless_loops(ops: &mut Vec<Op>) -> bool {
    run_peephole_pass(ops, remove_useless_loops_pass)
}

pub fn remove_useless_loops_pass(ops: [&Op; 1]) -> Change {
    match &ops[0].op_type {
        OpType::CLoop(children, _, _, _)
        | OpType::TNz(children, _)
        | OpType::DTNz(children, _, _) => {
            if children.is_empty() {
                Change::Replace(vec![OpType::Set(0, 0)])
            } else {
                Change::Ignore
            }
        }
        _ => Change::Ignore,
    }
}

/// Non local and slower version of arithmetic optimizations
pub fn optimize_non_local_arithmetics(ops: &mut Vec<Op>, wrapping_is_ub: bool) -> bool {
    run_non_local_pass(
        ops,
        optimize_non_local_arithmetics_pass,
        true,
        &[],
        wrapping_is_ub,
    )
}

fn optimize_non_local_arithmetics_pass(
    ops: &mut Vec<Op>,
    zeroed: bool,
    inputs: &[(isize, CellValue)],
    wrapping_is_ub: bool,
) -> bool {
    let mut progress = false;

    let mut i = 0;

    while !ops.is_empty() && i < ops.len() {
        let op = &ops[i];

        let change = match &op.op_type {
            OpType::Inc(offset, value) => {
                if let CellValue::Value(v) = utils::find_heap_value(
                    ops,
                    *offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                ) {
                    Change::Replace(vec![OpType::Set(*offset, v.wrapping_add(*value))])
                } else if let Some((index, value2)) =
                    find_last_accessing_inc_dec(ops, *offset, i as isize - 1)
                {
                    let value = *value as i16 + value2;
                    let remove_index = index - (i as isize);

                    if value >= 0 {
                        Change::RemoveAndReplace(
                            remove_index,
                            vec![OpType::Inc(*offset, value as u8)],
                        )
                    } else {
                        Change::RemoveAndReplace(
                            remove_index,
                            vec![OpType::Dec(*offset, (-value) as u8)],
                        )
                    }
                } else {
                    Change::Ignore
                }
            }
            OpType::Dec(offset, value) => {
                if let CellValue::Value(v) = utils::find_heap_value(
                    ops,
                    *offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                ) {
                    Change::Replace(vec![OpType::Set(*offset, v.wrapping_sub(*value))])
                } else if let Some((index, value2)) =
                    find_last_accessing_inc_dec(ops, *offset, i as isize - 1)
                {
                    let value = value2 - *value as i16;
                    let remove_index = index - (i as isize);

                    if value >= 0 {
                        Change::RemoveAndReplace(
                            remove_index,
                            vec![OpType::Inc(*offset, value as u8)],
                        )
                    } else {
                        Change::RemoveAndReplace(
                            remove_index,
                            vec![OpType::Dec(*offset, (-value) as u8)],
                        )
                    }
                } else {
                    Change::Ignore
                }
            }
            OpType::Set(offset, value) => {
                if let CellValue::Value(v) = utils::find_heap_value(
                    ops,
                    *offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    false,
                ) {
                    if *value == v {
                        Change::Remove
                    } else {
                        Change::Ignore
                    }
                } else {
                    Change::Ignore
                }
            }
            OpType::PutChar(offset) => {
                if let CellValue::Value(v) = utils::find_heap_value(
                    ops,
                    *offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                ) {
                    Change::Replace(vec![OpType::PutString(vec![v])])
                } else {
                    Change::Ignore
                }
            }
            OpType::PutString(value) => {
                if let Some((index, mut value2)) = find_last_put_string(ops, i as isize - 1) {
                    let remove_index = index - (i as isize);

                    value2.extend_from_slice(value);

                    Change::RemoveAndReplace(remove_index, vec![OpType::PutString(value2)])
                } else {
                    Change::Ignore
                }
            }
            OpType::Add(src_offset, dest_offset, multi) => {
                let src = utils::find_heap_value(
                    ops,
                    *src_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                );
                let dest = utils::find_heap_value(
                    ops,
                    *dest_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    false,
                );

                match (src, dest) {
                    (CellValue::Value(0), CellValue::Value(_)) => Change::Remove,
                    (CellValue::Value(src), CellValue::Value(dest)) => {
                        let value = dest.wrapping_add(src.wrapping_mul(*multi));

                        Change::Replace(vec![
                            OpType::Set(*dest_offset, value),
                            OpType::Set(*src_offset, 0),
                        ])
                    }
                    (CellValue::Value(src), _) => {
                        let value = src.wrapping_mul(*multi);

                        Change::Replace(vec![OpType::CAdd(*src_offset, *dest_offset, value)])
                    }
                    (_, CellValue::Value(0)) => {
                        if *multi == 1 {
                            Change::Replace(vec![OpType::Move(*src_offset, *dest_offset)])
                        } else {
                            Change::Replace(vec![OpType::Mul(*src_offset, *dest_offset, *multi)])
                        }
                    }
                    _ => Change::Ignore,
                }
            }
            OpType::NzAdd(src_offset, dest_offset, multi) => {
                let src = utils::find_heap_value(
                    ops,
                    *src_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                );
                let dest = utils::find_heap_value(
                    ops,
                    *dest_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    false,
                );

                match (src, dest) {
                    (CellValue::Value(0), CellValue::Value(_)) => Change::Remove,
                    (CellValue::Value(src), CellValue::Value(dest)) => {
                        let value = dest.wrapping_add(src.wrapping_mul(*multi));

                        Change::Replace(vec![OpType::Set(*dest_offset, value)])
                    }
                    (CellValue::Value(src), _) => {
                        let value = src.wrapping_mul(*multi);

                        Change::Replace(vec![OpType::NzCAdd(*src_offset, *dest_offset, value)])
                    }
                    (_, CellValue::Value(0)) => {
                        if *multi == 1 {
                            Change::Replace(vec![OpType::Copy(*src_offset, *dest_offset)])
                        } else {
                            Change::Replace(vec![OpType::NzMul(*src_offset, *dest_offset, *multi)])
                        }
                    }
                    _ => Change::Ignore,
                }
            }
            OpType::CAdd(src_offset, dest_offset, value) => {
                if let CellValue::Value(value2) = utils::find_heap_value(
                    ops,
                    *dest_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                ) {
                    Change::Replace(vec![
                        OpType::Set(*dest_offset, value2.wrapping_add(*value)),
                        OpType::Set(*src_offset, 0),
                    ])
                } else {
                    Change::Ignore
                }
            }
            OpType::NzCAdd(_src_offset, dest_offset, value) => {
                if let CellValue::Value(value2) = utils::find_heap_value(
                    ops,
                    *dest_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                ) {
                    Change::Replace(vec![OpType::Set(*dest_offset, value2.wrapping_add(*value))])
                } else {
                    Change::Ignore
                }
            }
            OpType::Sub(src_offset, dest_offset, multi) => {
                let src = utils::find_heap_value(
                    ops,
                    *src_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                );
                let dest = utils::find_heap_value(
                    ops,
                    *dest_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    false,
                );

                match (src, dest) {
                    (CellValue::Value(0), CellValue::Value(_)) => Change::Remove,
                    (CellValue::Value(src), CellValue::Value(dest)) => {
                        let value = dest.wrapping_sub(src.wrapping_mul(*multi));

                        Change::Replace(vec![
                            OpType::Set(*dest_offset, value),
                            OpType::Set(*src_offset, 0),
                        ])
                    }
                    (CellValue::Value(src), _) => {
                        let value = src.wrapping_mul(*multi);

                        Change::Replace(vec![OpType::CSub(*src_offset, *dest_offset, value)])
                    }
                    _ => Change::Ignore,
                }
            }
            OpType::NzSub(src_offset, dest_offset, multi) => {
                let src = utils::find_heap_value(
                    ops,
                    *src_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                );
                let dest = utils::find_heap_value(
                    ops,
                    *dest_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    false,
                );

                match (src, dest) {
                    (CellValue::Value(0), CellValue::Value(_)) => Change::Remove,
                    (CellValue::Value(src), CellValue::Value(dest)) => {
                        let value = dest.wrapping_sub(src.wrapping_mul(*multi));

                        Change::Replace(vec![OpType::Set(*dest_offset, value)])
                    }
                    (CellValue::Value(src), _) => {
                        let value = src.wrapping_mul(*multi);

                        Change::Replace(vec![OpType::NzCSub(*src_offset, *dest_offset, value)])
                    }
                    _ => Change::Ignore,
                }
            }
            OpType::CSub(src_offset, dest_offset, value) => {
                if let CellValue::Value(value2) = utils::find_heap_value(
                    ops,
                    *dest_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                ) {
                    Change::Replace(vec![
                        OpType::Set(*dest_offset, value2.wrapping_sub(*value)),
                        OpType::Set(*src_offset, 0),
                    ])
                } else {
                    Change::Ignore
                }
            }
            OpType::NzCSub(_src_offset, dest_offset, value) => {
                if let CellValue::Value(value2) = utils::find_heap_value(
                    ops,
                    *dest_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                ) {
                    Change::Replace(vec![OpType::Set(*dest_offset, value2.wrapping_sub(*value))])
                } else {
                    Change::Ignore
                }
            }
            OpType::Mul(src_offset, dest_offset, multi) => {
                if let CellValue::Value(value) = utils::find_heap_value(
                    ops,
                    *src_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    false,
                ) {
                    if value == 0 {
                        Change::Remove
                    } else {
                        Change::Replace(vec![
                            OpType::Set(*dest_offset, value.wrapping_mul(*multi)),
                            OpType::Set(*src_offset, 0),
                        ])
                    }
                } else {
                    Change::Ignore
                }
            }
            OpType::NzMul(src_offset, dest_offset, multi) => {
                if let CellValue::Value(value) = utils::find_heap_value(
                    ops,
                    *src_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    false,
                ) {
                    if value == 0 {
                        Change::Remove
                    } else {
                        Change::Replace(vec![OpType::Set(*dest_offset, value.wrapping_mul(*multi))])
                    }
                } else {
                    Change::Ignore
                }
            }
            OpType::Move(src_offset, dest_offset) => {
                if let CellValue::Value(value) = utils::find_heap_value(
                    ops,
                    *src_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                ) {
                    Change::Replace(vec![
                        OpType::Set(*dest_offset, value),
                        OpType::Set(*src_offset, 0),
                    ])
                } else {
                    Change::Ignore
                }
            }
            OpType::Copy(src_offset, dest_offset) => {
                if let CellValue::Value(value) = utils::find_heap_value(
                    ops,
                    *src_offset,
                    i as isize - 1,
                    zeroed,
                    inputs,
                    wrapping_is_ub,
                    true,
                ) {
                    Change::Replace(vec![OpType::Set(*dest_offset, value)])
                } else {
                    Change::Ignore
                }
            }
            OpType::SearchZero(step, _) => {
                let mut ptr_offset = None;

                for test in 0..100 {
                    let offset = test * step;
                    let value = utils::find_heap_value(
                        ops,
                        offset,
                        i as isize - 1,
                        zeroed,
                        inputs,
                        wrapping_is_ub,
                        true,
                    );

                    match value {
                        CellValue::Value(v) => {
                            if v == 0 {
                                ptr_offset = Some(offset);
                                break;
                            }
                        }
                        CellValue::Range(start, _) => {
                            if start == 0 {
                                break;
                            }
                        }
                        CellValue::NonZero => {
                            // Ignore
                        }
                        CellValue::Unknown | CellValue::Bool => {
                            break;
                        }
                    }
                }

                if let Some(ptr_offset) = ptr_offset {
                    if ptr_offset == 0 {
                        Change::Remove
                    } else {
                        Change::Replace(vec![OpType::new_ptr_offset(ptr_offset)])
                    }
                } else {
                    Change::Ignore
                }
            }
            _ => Change::Ignore,
        };

        let (changed, removed) = change.apply(ops, i, 1);
        progress |= changed;
        i -= removed;

        i += 1;
    }

    progress
}

pub fn optimize_non_local_dead_stores(ops: &mut Vec<Op>) -> bool {
    let mut progress = false;

    for op in ops.iter_mut() {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= optimize_non_local_dead_stores(children);
        }
    }

    let mut i = 1;

    while !ops.is_empty() && i < ops.len() {
        let op = &ops[i];

        let change = match &op.op_type {
            OpType::Set(offset, _)
            | OpType::Move(_, offset)
            | OpType::Copy(_, offset)
            | OpType::Mul(_, offset, _)
            | OpType::NzMul(_, offset, _)
            | OpType::GetChar(offset) => find_last_unread_set(ops, *offset, i - 1),
            OpType::CAdd(offset, ..) | OpType::CSub(offset, ..) => {
                find_last_unread_set(ops, *offset, i - 1)
            }
            OpType::CLoop(..) => find_last_unread_set(ops, 0, i - 1),
            _ => None,
        };

        if let Some((index, new_op)) = change {
            if let Some(new_op) = new_op {
                ops[index].op_type = new_op;
            } else {
                ops.remove(index);
            }

            progress = true;
            if i > 1 {
                i -= 1
            }
        } else {
            i += 1;
        }
    }

    progress
}

fn get_write_only_indices(op: &Op) -> Vec<isize> {
    match &op.op_type {
        OpType::TNz(.., info) | OpType::DTNz(.., info) => {
            if let Some(cell_access) = info.cell_access() {
                cell_access
                    .iter()
                    .filter(|a| a.value.is_write() && !a.read_after_write)
                    .map(|a| a.offset)
                    .collect()
            } else {
                Vec::new()
            }
        }
        OpType::LLoop(.., info) | OpType::ILoop(.., info) | OpType::CLoop(.., info) => {
            if let Some(cell_access) = info.cell_access() {
                cell_access
                    .iter()
                    .filter(|a| a.value.is_write() && !a.has_read())
                    .map(|a| a.offset)
                    .collect()
            } else {
                Vec::new()
            }
        }
        _ => Vec::new(),
    }
}

pub fn optimize_non_local_dead_block_stores(ops: &mut Vec<Op>) -> bool {
    let mut progress = false;

    for op in ops.iter_mut() {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= optimize_non_local_dead_block_stores(children);
        }
    }

    let mut i = 0;

    while !ops.is_empty() && i < ops.len() - 1 {
        for offset in get_write_only_indices(&ops[i]) {
            if has_unread_set(ops, offset, i + 1) {
                if remove_dead_block_sets(ops[i].op_type.get_children_mut().unwrap(), offset) {
                    progress = true;
                }
            }
        }
        i += 1;
    }

    progress
}

fn remove_dead_block_sets(ops: &mut Vec<Op>, mut ptr_offset: isize) -> bool {
    let mut i = ops.len() as isize - 1;

    while !ops.is_empty() && i >= 0 {
        let change = match &mut ops[i as usize].op_type {
            OpType::IncPtr(offset) => {
                ptr_offset += *offset as isize;
                Change::Ignore
            }
            OpType::DecPtr(offset) => {
                ptr_offset -= *offset as isize;
                Change::Ignore
            }
            OpType::Set(offset, _)
            | OpType::Inc(offset, _)
            | OpType::Dec(offset, _)
            | OpType::NzAdd(_, offset, _)
            | OpType::NzSub(_, offset, _)
            | OpType::NzMul(_, offset, _)
            | OpType::NzCAdd(_, offset, _)
            | OpType::NzCSub(_, offset, _)
            | OpType::Copy(offset, _) => {
                if *offset == ptr_offset {
                    Change::Remove
                } else {
                    Change::Ignore
                }
            }
            OpType::Add(src_offset, dest_offset, v) => {
                if *src_offset == ptr_offset {
                    Change::Replace(vec![OpType::NzAdd(*src_offset, *dest_offset, *v)])
                } else if *dest_offset == ptr_offset {
                    Change::Replace(vec![OpType::Set(*src_offset, 0)])
                } else {
                    Change::Ignore
                }
            }
            OpType::Sub(src_offset, dest_offset, v) => {
                if *src_offset == ptr_offset {
                    Change::Replace(vec![OpType::NzSub(*src_offset, *dest_offset, *v)])
                } else if *dest_offset == ptr_offset {
                    Change::Replace(vec![OpType::Set(*src_offset, 0)])
                } else {
                    Change::Ignore
                }
            }
            OpType::CAdd(src_offset, dest_offset, v) => {
                if *src_offset == ptr_offset {
                    Change::Replace(vec![OpType::NzCAdd(*src_offset, *dest_offset, *v)])
                } else if *dest_offset == ptr_offset {
                    Change::Replace(vec![OpType::Set(*src_offset, 0)])
                } else {
                    Change::Ignore
                }
            }
            OpType::CSub(src_offset, dest_offset, v) => {
                if *src_offset == ptr_offset {
                    Change::Replace(vec![OpType::NzCSub(*src_offset, *dest_offset, *v)])
                } else if *dest_offset == ptr_offset {
                    Change::Replace(vec![OpType::Set(*src_offset, 0)])
                } else {
                    Change::Ignore
                }
            }
            OpType::Mul(src_offset, dest_offset, v) => {
                if *src_offset == ptr_offset {
                    Change::Replace(vec![OpType::NzMul(*src_offset, *dest_offset, *v)])
                } else if *dest_offset == ptr_offset {
                    Change::Replace(vec![OpType::Set(*src_offset, 0)])
                } else {
                    Change::Ignore
                }
            }
            OpType::Move(src_offset, dest_offset) => {
                if *src_offset == ptr_offset {
                    Change::Replace(vec![OpType::Copy(*src_offset, *dest_offset)])
                } else if *dest_offset == ptr_offset {
                    Change::Replace(vec![OpType::Set(*src_offset, 0)])
                } else {
                    Change::Ignore
                }
            }
            OpType::TNz(children, info) | OpType::DTNz(children, _, info) => {
                if let Some(value) = info.get_access(ptr_offset) {
                    if value.value.is_write() && !value.read_after_write {
                        return remove_dead_block_sets(children, ptr_offset);
                    } else {
                        return false;
                    }
                }
                Change::Ignore
            }
            OpType::LLoop(children, info)
            | OpType::ILoop(children, .., info)
            | OpType::CLoop(children, .., info) => {
                if let Some(value) = info.get_access(ptr_offset) {
                    if value.value.is_write() && !value.has_read() {
                        return remove_dead_block_sets(children, ptr_offset);
                    } else {
                        return false;
                    }
                }
                Change::Ignore
            }
            OpType::Start | OpType::GetChar(_) | OpType::PutChar(_) | OpType::PutString(_) => {
                Change::Ignore
            }
            OpType::DLoop(..) | OpType::SearchZero(..) => return false,
        };

        if change.apply(ops, i as usize, 1).0 {
            return true;
        } else {
            i -= 1;
        }
    }

    false
}

fn has_unread_set(ops: &[Op], mut cell_offset: isize, start_index: usize) -> bool {
    let mut i = start_index;

    while i < ops.len() {
        match &ops[i].op_type {
            OpType::IncPtr(offset) => cell_offset += *offset as isize,
            OpType::DecPtr(offset) => cell_offset -= *offset as isize,
            OpType::Set(offset, _) => {
                if *offset == cell_offset {
                    return true;
                }
            }
            OpType::Inc(offset, _) | OpType::Dec(offset, _) => {
                if *offset == cell_offset {
                    return false;
                }
            }
            OpType::Add(src_offset, dest_offset, _) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return false;
                }
            }
            OpType::CAdd(src_offset, dest_offset, _) => {
                if *dest_offset == cell_offset {
                    return true;
                } else if *src_offset == cell_offset {
                    return false;
                }
            }
            OpType::Sub(src_offset, dest_offset, _) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return false;
                }
            }
            OpType::CSub(src_offset, dest_offset, _) => {
                if *dest_offset == cell_offset {
                    return true;
                } else if *src_offset == cell_offset {
                    return false;
                }
            }
            OpType::Mul(src_offset, dest_offset, _) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return false;
                }
            }
            OpType::Move(src_offset, dest_offset) | OpType::Copy(src_offset, dest_offset) => {
                if *dest_offset == cell_offset {
                    return true;
                } else if *src_offset == cell_offset {
                    return false;
                }
            }
            OpType::NzAdd(src_offset, dest_offset, _)
            | OpType::NzCAdd(src_offset, dest_offset, _)
            | OpType::NzSub(src_offset, dest_offset, _)
            | OpType::NzCSub(src_offset, dest_offset, _)
            | OpType::NzMul(src_offset, dest_offset, _) => {
                if *src_offset == cell_offset {
                    break;
                } else if *dest_offset == cell_offset {
                    return false;
                }
            }
            OpType::GetChar(offset) => {
                if cell_offset == *offset {
                    return false;
                }
            }
            OpType::LLoop(_, info)
            | OpType::ILoop(_, _, _, info)
            | OpType::CLoop(_, _, _, info)
            | OpType::TNz(_, info) => {
                if cell_offset == 0 || info.was_cell_accessed(cell_offset) {
                    return false;
                }
            }
            OpType::PutChar(offset) => {
                if cell_offset == *offset {
                    return false;
                }
            }
            OpType::PutString(_) => {
                // Ignore
            }
            _ => break,
        }

        i += 1
    }

    false
}

pub fn unroll_constant_loops(ops: &mut Vec<Op>, limit: usize) -> bool {
    let mut progress = false;

    for op in ops.iter_mut() {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= unroll_constant_loops(children, limit);
        }
    }

    let mut i = 0;

    while !ops.is_empty() && i < ops.len() {
        let op = &ops[i];

        let changes = if let OpType::CLoop(children, iterations, decrement, _) = &op.op_type {
            let num_ops = count_ops_recursive(children);

            let complexity = num_ops * *iterations as usize + ops.len() / 5;

            if complexity < limit {
                let mut changes = vec![];

                let mut ptr_offset = 0;

                for op in children {
                    if let Some(offset) = op.op_type.get_ptr_offset() {
                        ptr_offset += offset;
                    }
                }

                if *decrement == LoopDecrement::Post {
                    changes.push(Op::set(ops[i].span.clone(), *iterations));
                }

                for counter in (0..*iterations).rev() {
                    if *decrement == LoopDecrement::Pre {
                        changes.push(Op::set(ops[i].span.clone(), counter));
                    }

                    for op in children {
                        changes.push(op.clone());
                    }
                    if ptr_offset != 0 {
                        changes.push(Op::ptr_offset(ops[i].span.clone(), -ptr_offset));
                    }

                    if *decrement == LoopDecrement::Post {
                        changes.push(Op::set(ops[i].span.clone(), counter));
                    }
                }

                Some(changes)
            } else {
                None
            }
        } else {
            None
        };

        if let Some(changes) = changes {
            let old = ops.remove(i);

            let len = changes.len();

            for (index, op) in changes.into_iter().enumerate() {
                ops.insert_or_push(i + index, op);
            }

            ops.insert_or_push(i + len, Op::set(old.span, 0));

            // All possible loops within the current loop should already haven been unrolled
            i += len + 1;
            progress = true;
        } else {
            i += 1;
        }
    }

    progress
}

fn find_last_unread_set(
    ops: &[Op],
    mut cell_offset: isize,
    start_index: usize,
) -> Option<(usize, Option<OpType>)> {
    let mut i = start_index as isize;

    while i >= 0 {
        match &ops[i as usize].op_type {
            OpType::IncPtr(offset) => cell_offset += *offset as isize,
            OpType::DecPtr(offset) => cell_offset -= *offset as isize,
            OpType::Set(offset, _) | OpType::Inc(offset, _) | OpType::Dec(offset, _) => {
                if *offset == cell_offset {
                    return Some((i as usize, None));
                }
            }
            OpType::Add(src_offset, dest_offset, multi) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((
                        i as usize,
                        Some(OpType::NzAdd(*src_offset, *dest_offset, *multi)),
                    ));
                }
            }
            OpType::CAdd(src_offset, dest_offset, value) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((
                        i as usize,
                        Some(OpType::NzCAdd(*src_offset, *dest_offset, *value)),
                    ));
                }
            }
            OpType::Sub(src_offset, dest_offset, multi) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((
                        i as usize,
                        Some(OpType::NzSub(*src_offset, *dest_offset, *multi)),
                    ));
                }
            }
            OpType::CSub(src_offset, dest_offset, value) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((
                        i as usize,
                        Some(OpType::NzCSub(*src_offset, *dest_offset, *value)),
                    ));
                }
            }
            OpType::Mul(src_offset, dest_offset, value) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((
                        i as usize,
                        Some(OpType::NzMul(*src_offset, *dest_offset, *value)),
                    ));
                }
            }
            OpType::Move(src_offset, dest_offset) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((i as usize, Some(OpType::Copy(*src_offset, *dest_offset))));
                }
            }
            OpType::NzAdd(src_offset, dest_offset, _)
            | OpType::NzCAdd(src_offset, dest_offset, _)
            | OpType::NzSub(src_offset, dest_offset, _)
            | OpType::NzCSub(src_offset, dest_offset, _)
            | OpType::NzMul(src_offset, dest_offset, _)
            | OpType::Copy(src_offset, dest_offset) => {
                if *src_offset == cell_offset {
                    break;
                } else if *dest_offset == cell_offset {
                    return Some((i as usize, None));
                }
            }
            OpType::GetChar(offset) => {
                if cell_offset == *offset {
                    break;
                }
            }
            OpType::LLoop(_, info)
            | OpType::ILoop(_, _, _, info)
            | OpType::CLoop(_, _, _, info)
            | OpType::TNz(_, info) => {
                if cell_offset == 0 || info.was_cell_accessed(cell_offset) {
                    break;
                }
            }
            OpType::PutChar(offset) => {
                if cell_offset == *offset {
                    break;
                }
            }
            OpType::PutString(_) => {
                // Ignore
            }
            _ => break,
        }

        i -= 1
    }

    None
}

pub fn update_loop_access(ops: &mut Vec<Op>, wrapping_is_ub: bool) -> bool {
    run_non_local_pass(ops, update_loop_access_pass, true, &[], wrapping_is_ub)
}

fn update_loop_access_pass(
    ops: &mut Vec<Op>,
    zeroed: bool,
    inputs: &[(isize, CellValue)],
    wrapping_is_ub: bool,
) -> bool {
    for op in ops.iter_mut() {
        match &mut op.op_type {
            OpType::DLoop(children, info) | OpType::DTNz(children, _, info) => {
                info.set_cell_access(get_d_loop_access(children, wrapping_is_ub));
            }
            OpType::LLoop(children, info)
            | OpType::ILoop(children, _, _, info)
            | OpType::CLoop(children, _, _, info)
            | OpType::TNz(children, info) => {
                info.set_cell_access(get_loop_access(children, wrapping_is_ub));
            }
            _ => {
                // Ignore
            }
        }
    }

    let mut always_used = vec![];

    for (i, op) in ops.iter().enumerate() {
        match &op.op_type {
            OpType::DLoop(.., info)
            | OpType::DTNz(.., info)
            | OpType::LLoop(.., info)
            | OpType::ILoop(.., info)
            | OpType::CLoop(.., info)
            | OpType::TNz(.., info) => {
                if !info.always_used()
                    && find_heap_value(ops, 0, i as isize - 1, zeroed, inputs, wrapping_is_ub, true)
                        .is_not_zero()
                {
                    always_used.push(i);
                }
            }
            OpType::SearchZero(_, always) => {
                if !*always
                    && find_heap_value(ops, 0, i as isize - 1, zeroed, inputs, wrapping_is_ub, true)
                        .is_not_zero()
                {
                    always_used.push(i);
                }
            }
            _ => {
                // Ignore
            }
        }
    }

    for i in always_used {
        match &mut ops[i].op_type {
            OpType::DLoop(.., info)
            | OpType::DTNz(.., info)
            | OpType::LLoop(.., info)
            | OpType::ILoop(.., info)
            | OpType::CLoop(.., info)
            | OpType::TNz(.., info) => {
                info.set_always_used(true);
            }
            OpType::SearchZero(_, always) => {
                *always = true;
            }
            _ => unreachable!(),
        }
    }

    true
}

/// Try to find redundant copy/move ops
///
/// If a cell is copied this is typically done by copying the value
/// into two cells and move the value back into the origin which
/// results in sequences like this:
/// ```text
/// COPY src_offset: 0 dest_offset: 1
/// COPY src_offset: 0 dest_offset: 2
/// MOVE src_offset: 1 dest_offset: 0
/// ```
/// This can be replaced with:
/// ```text
/// COPY src_offset: 0 dest_offset: 2
/// SET 0 offset 1
/// ```
pub fn optimize_non_local_redundant_copies(ops: &mut Vec<Op>) -> bool {
    let mut progress = false;

    for op in ops.iter_mut() {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= optimize_non_local_redundant_copies(children);
        }
    }

    let mut i = 1;

    while !ops.is_empty() && i < ops.len() {
        let op = &ops[i];

        let indices = if let OpType::Move(src_offset, dest_offset) = &op.op_type {
            let mut result = None;

            for (test_index, op2) in ops[0..i].iter().enumerate().rev() {
                match &op2.op_type {
                    OpType::Copy(src_offset2, dest_offset2) => {
                        if src_offset == dest_offset2 {
                            if dest_offset == src_offset2 {
                                result = Some((test_index, *src_offset));
                            }
                            break;
                        } else if dest_offset == dest_offset2 {
                            break;
                        }
                    }
                    _ => {
                        break;
                    }
                }
            }

            result
        } else {
            None
        };

        if let Some((remove_index, reset_index)) = indices {
            ops[i].op_type = OpType::Set(reset_index, 0);
            ops.remove(remove_index);

            progress = true;
            i -= 1;
        } else {
            let remove = if let OpType::Copy(src_offset, dest_offset) = &op.op_type {
                let mut result = false;
                let mut src_offset = *src_offset;
                let mut dest_offset = *dest_offset;

                for op2 in ops[0..i].iter().rev() {
                    match &op2.op_type {
                        OpType::IncPtr(offset) => {
                            src_offset -= *offset as isize;
                            dest_offset -= *offset as isize;
                        }
                        OpType::DecPtr(offset) => {
                            src_offset += *offset as isize;
                            dest_offset += *offset as isize;
                        }
                        OpType::Copy(src_offset2, dest_offset2) => {
                            if src_offset == *dest_offset2 {
                                if dest_offset == *src_offset2 {
                                    result = true;
                                }
                                break;
                            } else if dest_offset == *dest_offset2 {
                                break;
                            }
                        }
                        op => {
                            if op.is_possible_write(src_offset) || op.is_possible_write(dest_offset)
                            {
                                break;
                            }
                        }
                    }
                }

                result
            } else {
                false
            };

            if remove {
                ops.remove(i);

                progress = true;
                i -= 1;
            }
        }

        i += 1;
    }

    progress
}

pub fn partially_unroll_loops(ops: &mut Vec<Op>, limit: usize, wrapping_is_ub: bool) -> bool {
    run_non_local_pass(
        ops,
        |ops, zeroing, inputs, wrapping_is_ub| {
            partially_unroll_loops_pass(ops, zeroing, inputs, wrapping_is_ub, limit)
        },
        true,
        &[],
        wrapping_is_ub,
    )
}

fn partially_unroll_loops_pass(
    ops: &mut Vec<Op>,
    _zeroed: bool,
    _inputs: &[(isize, CellValue)],
    _wrapping_is_ub: bool,
    limit: usize,
) -> bool {
    let mut progress = false;

    let mut i = 0;

    while i < ops.len() {
        let op = &ops[i];

        let prepend = match &op.op_type {
            OpType::DLoop(children, info) => {
                if info.always_used() {
                    if count_ops_recursive(children) < limit {
                        Some(children.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            OpType::LLoop(children, info) => {
                if info.always_used() {
                    if count_ops_recursive(children) < limit {
                        let child_offset = get_l_loop_ptr_end_offset(children);

                        let mut children = children.clone();

                        children.push(Op::ptr_offset(op.span.clone(), -child_offset));

                        Some(children)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            OpType::ILoop(children, step, decrement, info) => {
                if info.always_used() {
                    if count_ops_recursive(children) < limit {
                        let child_offset = get_l_loop_ptr_end_offset(children);

                        let mut children = children.clone();

                        if *decrement == LoopDecrement::Pre {
                            children.insert_or_push(0, Op::dec(op.span.clone(), *step));
                        }

                        children.push(Op::ptr_offset(op.span.clone(), -child_offset));

                        if *decrement != LoopDecrement::Pre {
                            children.push(Op::dec(op.span.clone(), *step));
                        }

                        Some(children)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(prepend) = prepend {
            ops[i]
                .op_type
                .get_block_info_mut()
                .unwrap()
                .set_always_used(false);
            let len = prepend.len();
            for (index, op) in prepend.into_iter().enumerate() {
                ops.insert_or_push(i + index, op);
            }
            i += len;
            progress = true;
        }

        i += 1;
    }

    progress
}

fn get_l_loop_ptr_end_offset(ops: &[Op]) -> isize {
    let mut ptr_offset = 0;

    for op in ops {
        ptr_offset += op.op_type.get_ptr_offset().unwrap_or(0);
    }

    ptr_offset
}

fn get_d_loop_ptr_end_offset(ops: &[Op]) -> Option<isize> {
    let mut ptr_offset = 0;

    for op in ops {
        match &op.op_type {
            OpType::IncPtr(offset) => ptr_offset += *offset as isize,
            OpType::DecPtr(offset) => ptr_offset -= *offset as isize,
            OpType::DTNz(_, offset, _) => {
                if let Some(offset) = offset {
                    ptr_offset += offset;
                } else {
                    return None;
                }
            }
            OpType::DLoop(..) | OpType::SearchZero(..) => {
                return None;
            }
            _ => {
                // Ignore
            }
        }
    }

    Some(ptr_offset)
}

pub fn non_local_remove_dead_loops(ops: &mut Vec<Op>, wrapping_is_ub: bool) -> bool {
    run_non_local_pass(
        ops,
        non_local_remove_dead_loops_pass,
        true,
        &[],
        wrapping_is_ub,
    )
}

fn non_local_remove_dead_loops_pass(
    ops: &mut Vec<Op>,
    zeroed: bool,
    inputs: &[(isize, CellValue)],
    wrapping_is_ub: bool,
) -> bool {
    let mut progress = false;

    let mut i = 0;

    while i < ops.len() {
        let op = &ops[i];

        let remove = match &op.op_type {
            OpType::DLoop(..)
            | OpType::DTNz(..)
            | OpType::LLoop(..)
            | OpType::ILoop(..)
            | OpType::TNz(..) => {
                find_heap_value(ops, 0, i as isize - 1, zeroed, inputs, wrapping_is_ub, true)
                    .is_zero()
            }
            _ => false,
        };

        if remove {
            ops.remove(i);
            progress = true;
        } else {
            i += 1
        }
    }

    progress
}

pub fn remove_true_conditions(ops: &mut Vec<Op>, wrapping_is_ub: bool) -> bool {
    run_non_local_pass(ops, remove_true_conditions_pass, true, &[], wrapping_is_ub)
}

fn remove_true_conditions_pass(
    ops: &mut Vec<Op>,
    _zeroed: bool,
    _inputs: &[(isize, CellValue)],
    _wrapping_is_ub: bool,
) -> bool {
    let mut progress = false;

    let mut i = 0;

    while i < ops.len() {
        let op = &ops[i];

        let unroll = match &op.op_type {
            OpType::TNz(_, info) | OpType::DTNz(_, _, info) => info.always_used(),
            _ => false,
        };

        if unroll {
            let old = ops.remove(i);

            match old.op_type {
                OpType::TNz(children, _) => {
                    let len = children.len();

                    let mut ptr_offset = 0;

                    for child in &children {
                        if let Some(offset) = child.op_type.get_ptr_offset() {
                            ptr_offset += offset;
                        }
                    }

                    for (index, op) in children.into_iter().enumerate() {
                        let index = i + index;
                        if index < ops.len() - 1 {
                            ops.insert_or_push(index, op);
                        } else {
                            ops.push(op);
                        }
                    }

                    let mut index = i + len;

                    if ptr_offset != 0 {
                        if index < ops.len() - 1 {
                            ops.insert_or_push(
                                index,
                                Op::ptr_offset(old.span.clone(), -ptr_offset),
                            );
                        } else {
                            ops.push(Op::ptr_offset(old.span.clone(), -ptr_offset));
                        }
                        index += 1;
                    }

                    if index < ops.len() - 1 {
                        ops.insert_or_push(index, Op::set(old.span, 0));
                    } else {
                        ops.push(Op::set(old.span, 0));
                    }

                    progress = true;
                }
                OpType::DTNz(children, _, _) => {
                    for (index, op) in children.into_iter().enumerate() {
                        ops.insert_or_push(i + index, op);
                    }
                    progress = true;
                }
                _ => unreachable!(),
            }
        }

        i += 1;
    }

    progress
}

pub fn unroll_scanning_d_loops(ops: &mut Vec<Op>, limit: usize, wrapping_is_ub: bool) -> bool {
    run_non_local_pass(
        ops,
        |ops, zeroing, inputs, wrapping_is_ub| {
            unroll_scanning_d_loops_pass(ops, zeroing, inputs, wrapping_is_ub, limit)
        },
        true,
        &[],
        wrapping_is_ub,
    )
}

fn unroll_scanning_d_loops_pass(
    ops: &mut Vec<Op>,
    zeroed: bool,
    inputs: &[(isize, CellValue)],
    wrapping_is_ub: bool,
    limit: usize,
) -> bool {
    let mut progress = false;

    let mut i = 0;

    while i < ops.len() {
        let op = &ops[i];

        let replace: Option<Vec<Op>> = match &op.op_type {
            OpType::DLoop(children, info) => {
                if info.always_used() {
                    if let Some(step) = get_scan_loop_step(children) {
                        let mut count = None;

                        for test in 1..100 {
                            let offset = test as isize * step;
                            let value = utils::find_heap_value(
                                ops,
                                offset,
                                i as isize - 1,
                                zeroed,
                                inputs,
                                wrapping_is_ub,
                                true,
                            );

                            match value {
                                CellValue::Value(v) => {
                                    if v == 0 {
                                        count = Some(test);
                                        break;
                                    }
                                }
                                CellValue::Range(start, _) => {
                                    if start == 0 {
                                        break;
                                    }
                                }
                                CellValue::NonZero => {
                                    // Ignore
                                }
                                CellValue::Unknown | CellValue::Bool => {
                                    break;
                                }
                            }
                        }

                        if let Some(count) = count {
                            if count * (children.len() - 1) < limit {
                                let mut replacement = vec![];

                                for _ in 0..count {
                                    replacement.extend_from_slice(children);
                                }

                                Some(replacement)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(replace) = replace {
            ops.remove(i);
            for (index, op) in replace.into_iter().enumerate() {
                ops.insert_or_push(i + index, op);
            }
            progress = true;
        } else {
            i += 1;
        }
    }

    progress
}

fn get_scan_loop_step(ops: &[Op]) -> Option<isize> {
    let mut ptr_offset = 0;

    for op in ops {
        match &op.op_type {
            OpType::IncPtr(offset) => ptr_offset += *offset as isize,
            OpType::DecPtr(offset) => ptr_offset -= *offset as isize,
            OpType::DLoop(..) | OpType::DTNz(..) | OpType::SearchZero(..) => return None,
            _ => {
                // Ignore
            }
        }
    }

    let step = ptr_offset;

    ptr_offset = 0;

    for op in ops {
        match &op.op_type {
            OpType::IncPtr(offset) => ptr_offset += *offset as isize,
            OpType::DecPtr(offset) => ptr_offset -= *offset as isize,

            OpType::Inc(offset, _)
            | OpType::Dec(offset, _)
            | OpType::Set(offset, _)
            | OpType::GetChar(offset)
            | OpType::NzAdd(_, offset, _)
            | OpType::NzCAdd(_, offset, _)
            | OpType::NzSub(_, offset, _)
            | OpType::NzCSub(_, offset, _)
            | OpType::NzMul(_, offset, _)
            | OpType::Copy(_, offset) => {
                if ptr_offset + offset == step {
                    return None;
                }
            }
            OpType::Add(src_offset, dest_offset, _)
            | OpType::CAdd(src_offset, dest_offset, _)
            | OpType::Sub(src_offset, dest_offset, _)
            | OpType::CSub(src_offset, dest_offset, _)
            | OpType::Mul(src_offset, dest_offset, _)
            | OpType::Move(src_offset, dest_offset) => {
                if ptr_offset + src_offset == step {
                    return None;
                }
                if dest_offset + src_offset == step {
                    return None;
                }
            }
            OpType::LLoop(.., info)
            | OpType::ILoop(.., info)
            | OpType::CLoop(.., info)
            | OpType::TNz(.., info) => {
                if ptr_offset == step || info.was_cell_written(ptr_offset + step) {
                    return None;
                }
            }
            OpType::DLoop(..) | OpType::DTNz(..) | OpType::SearchZero(..) => return None,
            OpType::PutChar(_) | OpType::PutString(_) | OpType::Start => {
                // Ignore
            }
        }
    }

    Some(ptr_offset)
}

#[cfg(test)]
mod tests {
    use crate::ir::ops::Op;
    use crate::optimizations::peephole::run_peephole_pass;

    use super::*;

    #[test]
    fn test_remove_dead_loop() {
        let mut ops = vec![
            Op::start(),
            Op::d_loop(1..2, vec![], BlockInfo::new_empty()),
            Op::inc(2..3, 1),
            Op::d_loop(3..4, vec![Op::inc(4..5, 1)], BlockInfo::new_empty()),
        ];

        remove_dead_loops(&mut ops);

        assert_eq!(
            ops,
            vec![
                Op::start(),
                Op::inc(2..3, 1),
                Op::d_loop(3..4, vec![Op::inc(4..5, 1)], BlockInfo::new_empty()),
            ]
        )
    }

    #[test]
    fn test_optimize_inc() {
        let mut ops = vec![Op::inc(0..1, 1), Op::inc(1..2, 2)];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::inc(0..2, 3),])
    }

    #[test]
    fn test_optimize_dec() {
        let mut ops = vec![Op::dec(0..1, 1), Op::dec(1..2, 2)];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::dec(0..2, 3),])
    }

    #[test]
    fn test_optimize_inc_dec() {
        let mut ops = vec![Op::inc(0..1, 1), Op::dec(1..2, 2), Op::dec(2..3, 3)];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::dec(0..3, 4),])
    }

    #[test]
    fn test_optimize_set_inc() {
        let mut ops = vec![Op::set(0..1, 5), Op::inc(1..2, 2)];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::set(0..2, 7),])
    }

    #[test]
    fn test_optimize_set_dec() {
        let mut ops = vec![Op::set(0..1, 5), Op::dec(1..2, 2)];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::set(0..2, 3),])
    }

    #[test]
    fn test_optimize_inc_dec_0() {
        let mut ops = vec![Op::inc(0..1, 1), Op::dec(1..2, 1), Op::inc_ptr(2..3, 3)];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::inc_ptr(2..3, 3),])
    }

    #[test]
    fn test_optimize_inc_ptr() {
        let mut ops = vec![Op::inc_ptr(0..1, 1), Op::inc_ptr(1..2, 2)];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::inc_ptr(0..2, 3),])
    }

    #[test]
    fn test_optimize_dec_ptr() {
        let mut ops = vec![Op::dec_ptr(0..1, 1), Op::dec_ptr(1..2, 2)];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::dec_ptr(0..2, 3),])
    }

    #[test]
    fn test_optimize_inc_ptr_dec() {
        let mut ops = vec![
            Op::inc_ptr(0..1, 1),
            Op::dec_ptr(1..2, 2),
            Op::dec_ptr(2..3, 3),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::dec_ptr(0..3, 4),])
    }

    #[test]
    fn test_optimize_inc_dec_ptr_0() {
        let mut ops = vec![Op::inc_ptr(0..1, 1), Op::dec_ptr(1..2, 1), Op::inc(2..3, 3)];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::inc(2..3, 3),])
    }

    #[test]
    fn test_optimize_inc_dec_zeroing_inc() {
        let mut ops = vec![
            Op::c_loop(0..2, vec![], 1, BlockInfo::new_empty()),
            Op::inc(2..3, 3),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(
            ops,
            vec![
                Op::c_loop(0..2, vec![], 1, BlockInfo::new_empty()),
                Op::set(2..3, 3),
            ]
        )
    }

    #[test]
    fn test_optimize_zero_loop() {
        let mut ops = vec![Op::d_loop(
            0..1,
            vec![Op::dec(1..2, 1)],
            BlockInfo::new_empty(),
        )];

        optimize_zero_loops(&mut ops);

        assert_eq!(ops, vec![Op::set(0..2, 0),])
    }

    #[test]
    fn test_optimize_zero_loop_inc() {
        let mut ops = vec![Op::d_loop(
            0..1,
            vec![Op::inc(1..2, 1)],
            BlockInfo::new_empty(),
        )];

        optimize_zero_loops(&mut ops);

        assert_eq!(ops, vec![Op::set(0..2, 0),])
    }

    #[test]
    fn test_optimize_add() {
        let mut ops = vec![Op::d_loop(
            0..5,
            vec![
                Op::inc_ptr(1..2, 1),
                Op::inc(2..3, 1),
                Op::dec_ptr(3..4, 1),
                Op::dec(4..5, 1),
            ],
            BlockInfo::new_empty(),
        )];

        optimize_local_loops(&mut ops);
        optimize_offsets(&mut ops, 1);
        optimize_count_loops(&mut ops);
        run_peephole_pass(&mut ops, optimize_arithmetic_loops);

        assert_eq!(ops, vec![Op::add(0..5, 1, 1),])
    }

    #[test]
    fn test_optimize_add_inv() {
        let mut ops = vec![Op::d_loop(
            0..5,
            vec![
                Op::dec(1..2, 1),
                Op::inc_ptr(2..3, 2),
                Op::inc(3..4, 1),
                Op::dec_ptr(4..5, 2),
            ],
            BlockInfo::new_empty(),
        )];

        optimize_local_loops(&mut ops);
        optimize_offsets(&mut ops, 1);
        optimize_count_loops(&mut ops);
        run_peephole_pass(&mut ops, optimize_arithmetic_loops);

        assert_eq!(ops, vec![Op::add(0..5, 2, 1),])
    }

    #[test]
    fn test_optimize_sub() {
        let mut ops = vec![Op::d_loop(
            0..5,
            vec![
                Op::inc_ptr(1..2, 1),
                Op::dec(2..3, 2),
                Op::dec_ptr(3..4, 1),
                Op::dec(4..5, 1),
            ],
            BlockInfo::new_empty(),
        )];

        optimize_local_loops(&mut ops);
        optimize_offsets(&mut ops, 1);
        optimize_count_loops(&mut ops);
        run_peephole_pass(&mut ops, optimize_arithmetic_loops);

        assert_eq!(ops, vec![Op::sub(0..5, 1, 2),])
    }

    #[test]
    fn test_optimize_sub2() {
        let mut ops = vec![Op::d_loop(
            0..5,
            vec![
                Op::dec(1..2, 1),
                Op::inc_ptr(2..3, 1),
                Op::dec(3..4, 2),
                Op::dec_ptr(4..5, 1),
            ],
            BlockInfo::new_empty(),
        )];

        optimize_local_loops(&mut ops);
        optimize_offsets(&mut ops, 1);
        optimize_count_loops(&mut ops);
        run_peephole_pass(&mut ops, optimize_arithmetic_loops);

        assert_eq!(ops, vec![Op::sub(0..5, 1, 2),])
    }

    #[test]
    fn test_optimize_local_loops() {
        let mut ops = vec![
            Op::set(0..1, 6),
            Op::d_loop(
                0..5,
                vec![
                    Op::dec(1..2, 1),
                    Op::inc_ptr(2..3, 1),
                    Op::dec(3..4, 2),
                    Op::dec_ptr(4..5, 1),
                ],
                BlockInfo::new_empty(),
            ),
        ];

        optimize_local_loops(&mut ops);

        assert_eq!(
            ops,
            vec![
                Op::set(0..1, 6),
                Op::l_loop(
                    0..5,
                    vec![Op::dec(1..2, 1), Op::inc_ptr(2..3, 1), Op::dec(3..4, 2),],
                    BlockInfo::new_access(vec![
                        CellAccess::new_write(0, true, false),
                        CellAccess::new_write(1, true, false)
                    ])
                )
            ]
        )
    }

    #[test]
    fn test_optimize_count_loops() {
        let mut ops = vec![
            Op::set(0..1, 6),
            Op::d_loop(
                0..1,
                vec![
                    Op::dec(1..2, 1),
                    Op::inc_ptr(2..3, 1),
                    Op::dec(3..4, 2),
                    Op::dec_ptr(4..5, 1),
                ],
                BlockInfo::new_empty(),
            ),
        ];

        optimize_local_loops(&mut ops);
        optimize_count_loops(&mut ops);

        assert_eq!(
            ops,
            vec![
                Op::set(0..1, 6),
                Op::i_loop(
                    0..1,
                    vec![Op::inc_ptr(2..3, 1), Op::dec(3..4, 2),],
                    1,
                    BlockInfo::new_access(vec![
                        CellAccess::new_write(0, true, false),
                        CellAccess::new_write(1, true, false)
                    ])
                )
            ]
        )
    }

    #[test]
    fn test_optimize_count_loops_inv() {
        let mut ops = vec![
            Op::set(0..1, 6),
            Op::d_loop(
                0..1,
                vec![
                    Op::dec_ptr(1..2, 1),
                    Op::dec(2..3, 3),
                    Op::inc_ptr(3..4, 1),
                    Op::dec(4..5, 2),
                ],
                BlockInfo::new_empty(),
            ),
        ];

        optimize_local_loops(&mut ops);
        optimize_count_loops(&mut ops);

        assert_eq!(
            ops,
            vec![
                Op::set(0..1, 6),
                Op::i_loop(
                    0..1,
                    vec![Op::dec_ptr(1..2, 1), Op::dec(2..3, 3),],
                    2,
                    BlockInfo::new_access(vec![
                        CellAccess::new_write(-1, true, false),
                        CellAccess::new_write(0, true, false)
                    ])
                )
            ]
        )
    }

    #[test]
    fn test_optimize_static_count_loops() {
        let mut ops = vec![
            Op::set(0..1, 6),
            Op::d_loop(
                0..1,
                vec![
                    Op::dec_ptr(1..2, 1),
                    Op::dec(2..3, 3),
                    Op::inc_ptr(3..4, 1),
                    Op::dec(4..5, 2),
                ],
                BlockInfo::new_empty(),
            ),
        ];

        optimize_local_loops(&mut ops);
        optimize_offsets(&mut ops, 1);
        optimize_count_loops(&mut ops);
        optimize_static_count_loops(&mut ops);

        assert_eq!(
            ops,
            vec![Op::c_loop(
                0..1,
                vec![Op::dec_with_offset(2..3, -1, 3),],
                3,
                BlockInfo::new_access(vec![
                    CellAccess::new_write(-1, true, false),
                    CellAccess::new_write(0, true, false)
                ])
            )]
        )
    }

    #[test]
    fn test_optimize_static_count_loops_zero() {
        let mut ops = vec![
            Op::set(0..1, 0),
            Op::d_loop(
                0..1,
                vec![
                    Op::dec_ptr(1..2, 1),
                    Op::dec(2..3, 3),
                    Op::inc_ptr(3..4, 1),
                    Op::dec(4..5, 2),
                ],
                BlockInfo::new_empty(),
            ),
        ];

        optimize_local_loops(&mut ops);
        optimize_count_loops(&mut ops);
        optimize_static_count_loops(&mut ops);

        assert_eq!(ops, vec![])
    }

    #[test]
    fn test_optimize_tnz_set() {
        let mut ops = vec![Op::i_loop(
            0..2,
            vec![Op::set(1..2, 1)],
            1,
            BlockInfo::new_empty(),
        )];

        optimize_conditional_loops(&mut ops);

        assert_eq!(
            ops,
            vec![Op::t_nz(
                0..2,
                vec![Op::set(1..2, 1),],
                BlockInfo::new_empty()
            )]
        )
    }

    #[test]
    fn test_dead_store_set() {
        let mut ops = vec![Op::inc(0..1, 1), Op::set(1..2, 2)];

        remove_dead_stores_before_set(&mut ops);

        assert_eq!(ops, vec![Op::set(1..2, 2),])
    }

    #[test]
    fn test_dead_store_get_char() {
        let mut ops = vec![Op::inc(0..1, 1), Op::get_char(1..2)];

        remove_dead_stores_before_set(&mut ops);

        assert_eq!(ops, vec![Op::get_char(1..2),])
    }

    #[test]
    fn test_optimize_search_zero_inc() {
        let mut ops = vec![Op::d_loop(
            0..2,
            vec![Op::inc_ptr(1..2, 8)],
            BlockInfo::new_empty(),
        )];

        optimize_search_zero(&mut ops);

        assert_eq!(ops, vec![Op::search_zero(0..2, 8),])
    }

    #[test]
    fn test_optimize_nested_search_zero_inc() {
        let mut ops = vec![Op::d_loop(
            0..3,
            vec![Op::d_loop(
                1..3,
                vec![Op::inc_ptr(2..3, 8)],
                BlockInfo::new_empty(),
            )],
            BlockInfo::new_empty(),
        )];

        optimize_search_zero(&mut ops);

        assert_eq!(
            ops,
            vec![Op::d_loop(
                0..3,
                vec![Op::search_zero(1..3, 8),],
                BlockInfo::new_empty()
            )]
        )
    }

    #[test]
    fn test_optimize_search_zero_dec() {
        let mut ops = vec![Op::d_loop(
            0..2,
            vec![Op::dec_ptr(1..2, 8)],
            BlockInfo::new_empty(),
        )];

        optimize_search_zero(&mut ops);

        assert_eq!(ops, vec![Op::search_zero(0..2, -8),])
    }

    #[test]
    fn test_optimize_nested_search_zero_dec() {
        let mut ops = vec![Op::d_loop(
            0..3,
            vec![Op::d_loop(
                1..3,
                vec![Op::dec_ptr(2..3, 8)],
                BlockInfo::new_empty(),
            )],
            BlockInfo::new_empty(),
        )];

        optimize_search_zero(&mut ops);

        assert_eq!(
            ops,
            vec![Op::d_loop(
                0..3,
                vec![Op::search_zero(1..3, -8),],
                BlockInfo::new_empty()
            )]
        )
    }

    #[test]
    fn test_optimize_constant_arithmetic_loop() {
        let mut ops = vec![Op::c_loop(
            0..3,
            vec![Op::inc_ptr(0..1, 2), Op::inc(1..2, 3)],
            5,
            BlockInfo::new_empty(),
        )];

        optimize_constant_arithmetic_loop(&mut ops);

        assert_eq!(
            ops,
            vec![
                Op::inc_ptr(0..1, 2),
                Op::inc(1..2, 15),
                Op::dec_ptr(2..3, 2),
                Op::set(2..3, 0),
            ]
        )
    }

    #[test]
    fn test_optimize_constant_arithmetic_loop_add() {
        let mut ops = vec![Op::set(0..1, 3), Op::add(1..2, 1, 3)];

        run_peephole_pass(&mut ops, optimize_constant_arithmetics);

        assert_eq!(ops, vec![Op::c_add(0..2, 1, 9),])
    }

    #[test]
    fn test_optimize_constant_arithmetic_loop_sub() {
        let mut ops = vec![Op::set(0..1, 3), Op::sub(1..2, 1, 2)];

        run_peephole_pass(&mut ops, optimize_constant_arithmetics);

        assert_eq!(ops, vec![Op::c_sub(0..2, 1, 6),])
    }

    #[test]
    fn test_optimize_offsets() {
        let mut ops = vec![
            Op::set(0..1, 3),
            Op::inc_ptr(1..2, 3),
            Op::inc(2..3, 2),
            Op::dec_ptr(3..4, 1),
            Op::dec(4..5, 5),
            Op::dec_ptr(5..6, 1),
            Op::d_loop(6..8, vec![], BlockInfo::new_empty()),
        ];

        optimize_offsets(&mut ops, 0);

        assert_eq!(
            ops,
            vec![
                Op::set(0..1, 3),
                Op::inc_with_offset(2..3, 3, 2),
                Op::dec_with_offset(4..5, 2, 5),
                Op::inc_ptr(0..6, 1),
                Op::d_loop(6..8, vec![], BlockInfo::new_empty()),
            ]
        )
    }

    #[test]
    fn test_optimize_arithmetic_offsets() {
        let mut ops = vec![
            Op::inc_ptr(0..1, 1),
            Op::c_add(1..2, -1, 72),
            Op::dec_ptr(2..3, 1),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetic_offsets);

        assert_eq!(ops, vec![Op::c_add_with_offset(0..3, 1, 0, 72),])
    }

    #[test]
    fn test_remove_trailing_pointer_ops() {
        let mut ops = vec![
            Op::i_loop(
                0..3,
                vec![Op::c_add(1..2, -1, 72), Op::dec_ptr(2..3, 1)],
                5,
                BlockInfo::new_empty(),
            ),
            Op::inc_ptr(3..4, 1),
        ];

        remove_trailing_pointer_ops(&mut ops, true);

        assert_eq!(
            ops,
            vec![Op::i_loop(
                0..3,
                vec![Op::c_add(1..2, -1, 72),],
                5,
                BlockInfo::new_empty()
            ),]
        )
    }

    #[test]
    fn test_remove_useless_copy() {
        let mut ops = vec![Op::copy(0..1, 3, 7), Op::copy(1..2, 7, 3)];

        run_peephole_pass(&mut ops, remove_useless_copy);

        assert_eq!(ops, vec![Op::copy(0..2, 3, 7),])
    }

    #[test]
    fn test_unroll_constant_loops() {
        let mut ops = vec![Op::c_loop(
            0..4,
            vec![
                Op::inc_with_offset(1..2, 2, 1),
                Op::inc_with_offset(2..3, 1, 2),
            ],
            3,
            BlockInfo::new_empty(),
        )];

        unroll_constant_loops(&mut ops, 40);

        assert_eq!(
            ops,
            vec![
                Op::inc_with_offset(1..2, 2, 1),
                Op::inc_with_offset(2..3, 1, 2),
                Op::inc_with_offset(1..2, 2, 1),
                Op::inc_with_offset(2..3, 1, 2),
                Op::inc_with_offset(1..2, 2, 1),
                Op::inc_with_offset(2..3, 1, 2),
                Op::set(0..4, 0),
            ]
        )
    }

    #[test]
    fn test_nz_mul() {
        let mut ops = vec![Op::set_with_offset(0..1, 1, 0), Op::nz_add(1..2, 1, 2)];

        run_non_local_pass(
            &mut ops,
            optimize_non_local_arithmetics_pass,
            false,
            &[],
            false,
        );

        assert_eq!(
            ops,
            vec![Op::set_with_offset(0..1, 1, 0), Op::nz_mul(1..2, 1, 2),]
        )
    }

    #[test]
    fn test_non_local_bug() {
        let mut ops = vec![
            Op::d_loop(
                0..1,
                vec![
                    Op::inc_ptr(0..1, 1),
                    Op::i_loop(
                        0..1,
                        vec![
                            Op::inc_with_offset(0..1, 5, 1),
                            Op::inc_ptr(0..1, 1),
                            Op::i_loop(
                                0..1,
                                vec![
                                    Op::dec_with_offset(0..1, 4, 1),
                                    Op::inc_with_offset(0..1, -10, 1),
                                    Op::add_with_offset(0..1, 1, 4, 1),
                                ],
                                1,
                                BlockInfo::new_empty(),
                            ),
                            Op::nz_sub_with_offset(0..1, 1, 4, 1),
                            Op::add_with_offset(0..1, 1, -10, 1),
                        ],
                        1,
                        BlockInfo::new_empty(),
                    ),
                    Op::inc_ptr(0..1, 1),
                    Op::i_loop(
                        0..1,
                        vec![
                            Op::inc_with_offset(0..1, 4, 1),
                            Op::nz_sub_with_offset(0..1, 1, 4, 1),
                            Op::add_with_offset(0..1, 1, -10, 1),
                        ],
                        1,
                        BlockInfo::new_empty(),
                    ),
                    Op::add_with_offset(0..1, 1, 4, 1),
                ],
                BlockInfo::new_empty(),
            ),
            Op::dec_ptr(0..1, 11),
        ];

        update_loop_access(&mut ops, false);

        let inputs = ops.clone();

        run_non_local_pass(
            &mut ops,
            optimize_non_local_arithmetics_pass,
            false,
            &[],
            false,
        );

        assert_eq!(ops, inputs)
    }

    #[test]
    fn test_non_local_redundant_copies() {
        let mut ops = vec![
            Op::copy(0..1, 0, 1),
            Op::copy(1..2, 0, 2),
            Op::_move(2..3, 1, 0),
        ];

        optimize_non_local_redundant_copies(&mut ops);

        assert_eq!(
            ops,
            vec![Op::copy(1..2, 0, 2), Op::set_with_offset(2..3, 1, 0),]
        )
    }

    #[test]
    fn test_non_local_dead_stores() {
        let mut ops = vec![
            Op::set_with_offset(0..1, 0, 1),
            Op::set_with_offset(1..2, 2, 10),
            Op::put_char_with_offset(2..3, 2),
            Op::set_with_offset(3..4, 0, 2),
            Op::set_with_offset(4..5, 2, 20),
            Op::put_char_with_offset(5..6, 2),
            Op::set_with_offset(6..7, 0, 3),
            Op::set_with_offset(7..8, 2, 30),
        ];

        optimize_non_local_dead_stores(&mut ops);

        assert_eq!(
            ops,
            vec![
                Op::set_with_offset(1..2, 2, 10),
                Op::put_char_with_offset(2..3, 2),
                Op::set_with_offset(4..5, 2, 20),
                Op::put_char_with_offset(5..6, 2),
                Op::set_with_offset(6..7, 0, 3),
                Op::set_with_offset(7..8, 2, 30),
            ]
        )
    }

    #[test]
    fn test_non_local_zero_search() {
        let mut ops = vec![
            Op::set_with_offset(0..1, 0, 1),
            Op::set_with_offset(1..2, 1, 2),
            Op::set_with_offset(2..3, 3, 3),
            Op::set_with_offset(3..4, 4, 4),
            Op::set_with_offset(4..5, 5, 5),
            Op::set_with_offset(5..6, 6, 6),
            Op::set_with_offset(6..7, 8, 7),
            Op::inc_ptr(7..8, 8),
            Op::search_zero(8..9, -2),
        ];

        optimize_non_local_arithmetics(&mut ops, false);

        assert_eq!(
            ops,
            vec![
                Op::set_with_offset(0..1, 0, 1),
                Op::set_with_offset(1..2, 1, 2),
                Op::set_with_offset(2..3, 3, 3),
                Op::set_with_offset(3..4, 4, 4),
                Op::set_with_offset(4..5, 5, 5),
                Op::set_with_offset(5..6, 6, 6),
                Op::set_with_offset(6..7, 8, 7),
                Op::inc_ptr(7..8, 8),
                Op::dec_ptr(8..9, 6),
            ]
        )
    }

    #[test]
    fn test_ptr_zero_ops() {
        let mut ops = vec![
            Op::dec(0..1, 1),
            Op::inc_ptr(0..1, 1),
            Op::inc_ptr(0..1, 1),
            Op::inc_ptr(0..1, 1),
            Op::inc_ptr(0..1, 1),
            Op::inc_ptr(0..1, 1),
            Op::dec_ptr(0..1, 1),
            Op::dec_ptr(0..1, 1),
            Op::dec_ptr(0..1, 1),
            Op::dec_ptr(0..1, 1),
            Op::dec_ptr(0..1, 1),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![Op::dec(0..1, 1),]);
    }

    #[test]
    fn test_zero_l_loop() {
        let mut ops = vec![Op::l_loop(
            0..2,
            vec![Op::dec(1..2, 1)],
            BlockInfo::new_empty(),
        )];

        optimize_count_loops(&mut ops);

        assert_eq!(ops, vec![Op::set(0..2, 0)]);
    }

    #[test]
    fn test_dec_l_loop() {
        let mut ops = vec![Op::l_loop(
            0..2,
            vec![Op::dec(1..2, 2)],
            BlockInfo::new_empty(),
        )];

        optimize_count_loops(&mut ops);

        assert_eq!(
            ops,
            vec![Op::i_loop(0..2, vec![], 2, BlockInfo::new_empty())]
        );
    }

    #[test]
    fn test_update_d_loop_access() {
        let mut ops = vec![Op::d_loop(
            0..4,
            vec![Op::set(1..2, 2), Op::inc_ptr(2..3, 1)],
            BlockInfo::new_empty(),
        )];

        update_loop_access(&mut ops, false);

        assert_eq!(
            ops,
            vec![Op::d_loop(
                0..4,
                vec![Op::set(1..2, 2), Op::inc_ptr(2..3, 1),],
                BlockInfo::new_access(vec![CellAccess::new_value(-1, 2, false, false)])
            ),]
        );
    }
}
