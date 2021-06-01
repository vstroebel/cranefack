use std::cmp::Ordering;

use crate::ir::ops::{Op, OpType};
use crate::ir::opt_info::{BlockInfo, Cell, CellAccess};
use crate::optimizations::peephole::run_peephole_pass;
use crate::optimizations::utils::{CellValue, Change, count_ops_recursive, run_non_local_pass};
use crate::optimizations::utils;

pub fn remove_dead_loops(ops: &mut Vec<Op>) -> bool {
    run_peephole_pass(ops, remove_dead_loops_check)
}

fn remove_dead_loops_check(ops: [&Op; 2]) -> Change {
    if ops[0].op_type.is_zeroing(0) && matches!(ops[1].op_type,
        OpType::DLoop(..) |
        OpType::LLoop(..) |
        OpType::ILoop(..) |
        OpType::TNz(..) |
        OpType::SearchZero(..)
    ) {
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
        if let OpType::DLoop(children) = &mut op.op_type {
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
    if let OpType::ILoop(children, step, _) = &ops[0].op_type {
        if *step == 1 {
            let mut replacement_indices = vec![];
            let mut replacements = vec![];

            for child in children {
                match &child.op_type {
                    OpType::Inc(offset, multi) => {
                        if replacement_indices.contains(offset) {
                            return Change::Ignore;
                        } else {
                            replacement_indices.push(*offset);
                            replacements.push(OpType::NzAdd(0, *offset as isize, *multi));
                        }
                    }
                    OpType::Dec(offset, multi) => {
                        if replacement_indices.contains(offset) {
                            return Change::Ignore;
                        } else {
                            replacement_indices.push(*offset);
                            replacements.push(OpType::NzSub(0, *offset as isize, *multi));
                        }
                    }
                    _ => {
                        return Change::Ignore;
                    }
                }
            }

            return if replacements.is_empty() {
                Change::Ignore
            } else {
                let last = replacements.remove(replacements.len() - 1);

                match last {
                    OpType::NzAdd(src, dest, value) => {
                        replacements.push(OpType::Add(src, dest, value));
                    }
                    OpType::NzSub(src, dest, value) => {
                        replacements.push(OpType::Sub(src, dest, value));
                    }
                    op_type => unreachable!("Unexpected op type {:?}", op_type)
                }
                Change::Replace(replacements)
            };
        }
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
        let replace = if let OpType::DLoop(children) = &mut ops[i].op_type {
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
                        OpType::DLoop(..) => {
                            ignore = true;
                            break;
                        }
                        OpType::LLoop(children, ..) |
                        OpType::ILoop(children, ..) |
                        OpType::CLoop(children, ..) |
                        OpType::TNz(children, ..) => {
                            if !is_ops_block_local(children, &[-ptr_offset]) {
                                ignore = true;
                                break;
                            }
                        }
                        OpType::SearchZero(..) => {
                            ignore = true;
                            break;
                        }
                        OpType::PutChar(..) |
                        OpType::GetChar(..) |
                        OpType::Inc(..) |
                        OpType::Dec(..) |
                        OpType::Set(..) |
                        OpType::Add(..) |
                        OpType::Sub(..) |
                        OpType::CAdd(..) |
                        OpType::CSub(..) |
                        OpType::Move(..) |
                        OpType::NzAdd(..) |
                        OpType::NzSub(..) |
                        OpType::NzCAdd(..) |
                        OpType::NzCSub(..) |
                        OpType::Mul(..) |
                        OpType::NzMul(..) |
                        OpType::Copy(..)
                        => {
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

            if let OpType::DLoop(mut children) = prev.op_type {
                while !children.is_empty() && children[children.len() - 1].op_type.is_ptr_inc_or_dec() {
                    children.remove(children.len() - 1);
                }

                let access = get_loop_access(&children, 0);

                ops.insert(i, Op::l_loop(span, children, BlockInfo::new_access(access)));

                progress = true;
            } else {
                unreachable!("Bad loop type {:?}", prev);
            }
        }
        i += 1;
    }

    progress
}

fn get_loop_access(ops: &[Op], mut start_offset: isize) -> Vec<CellAccess> {
    let mut access = vec![];

    for op in ops {
        match &op.op_type {
            OpType::IncPtr(offset) => start_offset += *offset as isize,
            OpType::DecPtr(offset) => start_offset -= *offset as isize,
            OpType::Set(offset, value) => {
                CellAccess::add(&mut access, start_offset + offset, Cell::Value(*value));
            }
            OpType::Inc(offset, _) |
            OpType::Dec(offset, _) |
            OpType::NzAdd(_, offset, _) |
            OpType::NzCAdd(_, offset, _) |
            OpType::NzSub(_, offset, _) |
            OpType::NzCSub(_, offset, _) |
            OpType::NzMul(_, offset, _) |
            OpType::Copy(_, offset)
            => {
                CellAccess::add(&mut access, start_offset + offset, Cell::Write);
            }
            OpType::Add(src_offset, dest_offset, _) |
            OpType::CAdd(src_offset, dest_offset, _) |
            OpType::Sub(src_offset, dest_offset, _) |
            OpType::CSub(src_offset, dest_offset, _) |
            OpType::Mul(src_offset, dest_offset, _) |
            OpType::Move(src_offset, dest_offset)
            => {
                CellAccess::add(&mut access, start_offset + src_offset, Cell::Value(0));
                CellAccess::add(&mut access, start_offset + dest_offset, Cell::Write);
            }
            OpType::LLoop(_, info) |
            OpType::ILoop(_, _, info) |
            OpType::CLoop(_, _, info) |
            OpType::TNz(_, info)
            => {
                for cell in info.cell_access() {
                    CellAccess::add(&mut access, start_offset + cell.offset, cell.value);
                }
                CellAccess::add(&mut access, start_offset, Cell::Value(0));
            }
            OpType::GetChar(offset) => {
                CellAccess::add(&mut access, start_offset + offset, Cell::Write);
            }
            OpType::PutChar(..) => {
                //Ignore
            }
            OpType::Start => unreachable!("Must not be called with start in children"),
            OpType::DLoop(_) => unreachable!("Must not be called with dloops in children"),
            OpType::SearchZero(_) => unreachable!("Must not be called with zero search in children"),
        }
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
            OpType::LLoop(children, ..) |
            OpType::ILoop(children, ..) |
            OpType::CLoop(children, ..) |
            OpType::TNz(children, ..) => {
                let mut offsets = parent_offsets.to_vec();

                offsets.push(-ptr_offset);

                if !is_ops_block_local(children, &offsets) {
                    return false;
                }
            }
            OpType::DLoop(_) |
            OpType::SearchZero(..) => {
                return false;
            }
            OpType::PutChar(..) |
            OpType::GetChar(..) |
            OpType::Inc(..) |
            OpType::Dec(..) |
            OpType::Set(..) |
            OpType::Add(..) |
            OpType::Sub(..) |
            OpType::CAdd(..) |
            OpType::CSub(..) |
            OpType::Move(..) |
            OpType::NzAdd(..) |
            OpType::NzSub(..) |
            OpType::NzCAdd(..) |
            OpType::NzCSub(..) |
            OpType::Mul(..) |
            OpType::NzMul(..) |
            OpType::Copy(..)
            => {
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

            let num_ops = children.len();
            if num_ops >= 2 {
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
                        OpType::Add(src_offset, dest_offset, _) |
                        OpType::NzAdd(src_offset, dest_offset, _) |
                        OpType::Sub(src_offset, dest_offset, _) |
                        OpType::NzSub(src_offset, dest_offset, _) |
                        OpType::CAdd(src_offset, dest_offset, _) |
                        OpType::CSub(src_offset, dest_offset, _) |
                        OpType::Mul(src_offset, dest_offset, _) |
                        OpType::Move(src_offset, dest_offset) |
                        OpType::Copy(src_offset, dest_offset)
                        => {
                            if ptr_offset + src_offset == 0 {
                                ignore = true;
                                break;
                            }

                            if ptr_offset + dest_offset == 0 {
                                ignore = true;
                                break;
                            }
                        }
                        OpType::NzCAdd(_, dest_offset, _) |
                        OpType::NzCSub(_, dest_offset, _) |
                        OpType::NzMul(_, dest_offset, _)
                        => {
                            if ptr_offset + dest_offset == 0 {
                                ignore = true;
                                break;
                            }
                        }

                        OpType::Inc(offset, _) |
                        OpType::Set(offset, _) => {
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
                        OpType::DLoop(..) |
                        OpType::LLoop(..)
                        => {
                            ignore = true;
                            break;
                        }
                        OpType::ILoop(children, ..) |
                        OpType::CLoop(children, ..) |
                        OpType::TNz(children, ..) => {
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
                        OpType::PutChar(..) => {
                            // ignore
                        }
                    }
                }
            }

            #[allow(clippy::manual_map)]
            if !ignore {
                Some(counter_decrements)
            } else {
                None
            }
        } else {
            None
        };

        if let Some(counter_decrements) = replace {
            let prev = ops.remove(i);
            let span = prev.span;

            if let OpType::LLoop(mut children, info) = prev.op_type {
                let mut step = 0;

                for (index, amount) in counter_decrements.iter().rev() {
                    step += *amount;
                    children.remove(*index);
                }

                while !children.is_empty() && children[children.len() - 1].op_type.is_ptr_inc_or_dec() {
                    children.remove(children.len() - 1);
                }

                ops.insert(i, Op::i_loop(span, children, step, info));

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
            OpType::Add(src_offset, dest_offset, _) |
            OpType::Sub(src_offset, dest_offset, _) |
            OpType::CAdd(src_offset, dest_offset, _) |
            OpType::CSub(src_offset, dest_offset, _) |
            OpType::Mul(src_offset, dest_offset, _) |
            OpType::Move(src_offset, dest_offset)
            => {
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
            OpType::NzAdd(_, dest_offset, _) |
            OpType::NzSub(_, dest_offset, _) |
            OpType::NzCAdd(_, dest_offset, _) |
            OpType::NzCSub(_, dest_offset, _) |
            OpType::NzMul(_, dest_offset, _) |
            OpType::Copy(_, dest_offset)
            => {
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
            OpType::ILoop(children, ..) |
            OpType::CLoop(children, ..) |
            OpType::TNz(children, ..) => {
                let mut offsets = parent_offsets.to_vec();

                offsets.push(-ptr_offset);

                if !is_ops_block_unmodified_local(children, &offsets) {
                    return false;
                }
            }
            OpType::DLoop(_) |
            OpType::LLoop(..) |
            OpType::SearchZero(..) => {
                return false;
            }
            OpType::PutChar(..) => {
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
        (OpType::IncPtr(v1), OpType::IncPtr(v2)) => Change::Replace(vec![OpType::IncPtr(v1.wrapping_add(*v2))]),
        (OpType::DecPtr(v1), OpType::DecPtr(v2)) => Change::Replace(vec![OpType::DecPtr(v1.wrapping_add(*v2))]),
        (OpType::IncPtr(v1), OpType::DecPtr(v2)) => {
            match v1.cmp(v2) {
                Ordering::Equal => Change::Remove,
                Ordering::Greater => Change::Replace(vec![OpType::IncPtr(v1 - v2)]),
                Ordering::Less => Change::Replace(vec![OpType::DecPtr(v2 - v1)]),
            }
        }
        (OpType::DecPtr(v1), OpType::IncPtr(v2)) => {
            match v1.cmp(v2) {
                Ordering::Equal => Change::Remove,
                Ordering::Greater => Change::Replace(vec![OpType::DecPtr(v1 - v2)]),
                Ordering::Less => Change::Replace(vec![OpType::IncPtr(v2 - v1)]),
            }
        }
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
        (op_type, OpType::Inc(offset, v)) => {
            if op_type.is_zeroing(*offset) {
                Change::ReplaceOffset(1, ops[1].span.clone(), vec![OpType::Set(*offset, *v)])
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::Dec(offset, v)) => {
            if op_type.is_zeroing(*offset) {
                Change::ReplaceOffset(1, ops[1].span.clone(), vec![OpType::Set(*offset, 0u8.wrapping_sub(*v))])
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
                    Change::ReplaceOffset(1, ops[1].span.clone(), vec![OpType::Move(*src_offset, *dest_offset)])
                } else {
                    Change::ReplaceOffset(1, ops[1].span.clone(), vec![OpType::Mul(*src_offset, *dest_offset, *multi)])
                }
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::NzAdd(src_offset, dest_offset, multi)) => {
            if op_type.is_zeroing(*dest_offset) {
                if *multi == 1 {
                    Change::ReplaceOffset(1, ops[1].span.clone(), vec![OpType::Copy(*src_offset, *dest_offset)])
                } else {
                    Change::ReplaceOffset(1, ops[1].span.clone(), vec![OpType::NzMul(*src_offset, *dest_offset, *multi)])
                }
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::CAdd(src_offset, dest_offset, value)) => {
            if op_type.is_zeroing(*dest_offset) {
                Change::ReplaceOffset(1, ops[1].span.clone(), vec![
                    OpType::Set(*dest_offset, *value),
                    OpType::Set(*src_offset, 0),
                ])
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::NzCAdd(_src_offset, dest_offset, value)) => {
            if op_type.is_zeroing(*dest_offset) {
                Change::ReplaceOffset(1, ops[1].span.clone(), vec![
                    OpType::Set(*dest_offset, *value),
                ])
            } else {
                Change::Ignore
            }
        }
        _ => Change::Ignore
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
                OpType::Inc(offset, _) |
                OpType::Dec(offset, _) |
                OpType::Set(offset, _) |
                OpType::Copy(_, offset) |
                OpType::NzAdd(_, offset, ..) |
                OpType::NzCAdd(_, offset, ..) |
                OpType::NzSub(_, offset, ..) |
                OpType::NzCSub(_, offset, ..) |
                OpType::NzMul(_, offset, ..)
                => {
                    *offset == dest_offset
                }
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
            (OpType::Set(0, v), OpType::ILoop(_, step, ..)) =>
                if *step != 0 && v % step == 0 {
                    Some(v / step)
                } else {
                    None
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

                if let OpType::ILoop(children, _, access) = loop_op.op_type {
                    ops.insert(i, Op::c_loop(span, children, count, access));
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

pub fn optimize_non_local_static_count_loops(ops: &mut Vec<Op>) -> bool {
    let mut progress = false;

    for op in ops.iter_mut() {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= optimize_non_local_static_count_loops(children);
        }
    }

    let mut i = 1;

    while !ops.is_empty() && i < ops.len() {
        let op = &ops[i];

        let count = match &op.op_type {
            OpType::ILoop(_, step, ..) =>
                if let CellValue::Value(v) = utils::find_heap_value(&ops, 0, i as isize - 1, false, &[]) {
                    if *step != 0 && v % step == 0 {
                        Some(v / step)
                    } else {
                        None
                    }
                } else {
                    None
                }
            _ => None,
        };

        if let Some(count) = count {
            let loop_op = ops.remove(i);
            let span = loop_op.span;

            if let OpType::ILoop(children, _, access) = loop_op.op_type {
                ops.insert(i, Op::c_loop(span, children, count, access));
            } else {
                unreachable!();
            }

            i += 1;
        } else {
            i += 1;
        }
    }

    progress
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
            OpType::ILoop(children, ..) |
            OpType::CLoop(children, ..) =>
                contains_only_constant_sets(children),
            OpType::LLoop(children, _) => {
                !children.is_empty()
                    && is_zero_end_offset(children, 0)
                    && children[children.len() - 1].op_type.is_zeroing(0)
            }
            _ => false,
        };

        if replace {
            progress = true;
            let prev = ops.remove(i);
            let span = prev.span;

            match prev.op_type {
                OpType::LLoop(children, info) |
                OpType::ILoop(children, _, info) |
                OpType::CLoop(children, _, info) => {
                    ops.insert(i, Op::t_nz(span, children, info));
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
            OpType::Inc(..) |
            OpType::Dec(..) |
            OpType::Set(..) |
            OpType::Add(..) |
            OpType::CAdd(..) |
            OpType::NzAdd(..) |
            OpType::NzCAdd(..) |
            OpType::Sub(..) |
            OpType::CSub(..) |
            OpType::NzSub(..) |
            OpType::NzCSub(..) |
            OpType::Move(..) |
            OpType::Copy(..) |
            OpType::GetChar(..) |
            OpType::PutChar(..) |
            OpType::LLoop(..) |
            OpType::ILoop(..) |
            OpType::CLoop(..) |
            OpType::TNz(..)
            => {
                // ignore
            }
            _ => return false
        }
    }

    ptr_offset == 0
}

fn contains_only_constant_sets(ops: &[Op]) -> bool {
    for op in ops {
        match &op.op_type {
            OpType::Set(_, _) |
            OpType::IncPtr(_) |
            OpType::DecPtr(_) => {
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
            OpType::DLoop(children) => {
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

        let replace = if let OpType::CLoop(children, ..) = &op.op_type {
            contains_only_simple_arithmetics(children)
        } else {
            false
        };

        if replace {
            let prev = ops.remove(i);
            let span = prev.span;

            match prev.op_type {
                OpType::CLoop(children, iterations, _) => {
                    let mut ptr_offset = 0;

                    let mut pos = i;

                    #[allow(clippy::comparison_chain)]
                    if ptr_offset > 0 {
                        ops.insert(pos, Op::inc_ptr(span.start..span.start + 1, ptr_offset as usize));
                        pos += 1;
                    } else if ptr_offset < 0 {
                        ops.insert(pos, Op::dec_ptr(span.start..span.start + 1, -ptr_offset as usize));
                        pos += 1;
                    }

                    for mut child in children {
                        match &mut child.op_type {
                            OpType::Inc(_, v) |
                            OpType::Dec(_, v) => *v = v.wrapping_mul(iterations),
                            OpType::IncPtr(v) => ptr_offset += *v as isize,
                            OpType::DecPtr(v) => ptr_offset -= *v as isize,
                            _ => {
                                // ignore
                            }
                        }

                        ops.insert(pos, child);

                        pos += 1;
                    }

                    #[allow(clippy::comparison_chain)]
                    if ptr_offset > 0 {
                        ops.insert(pos, Op::dec_ptr(span.end - 1..span.end, ptr_offset as usize));
                    } else if ptr_offset < 0 {
                        ops.insert(pos, Op::inc_ptr(span.end - 1..span.end, -ptr_offset as usize));
                    }
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
                Change::Replace(vec![OpType::CAdd(*src_offset, *dest_offset, value * multi)])
            } else {
                Change::Ignore
            }
        }
        (OpType::Set(offset, value), OpType::Sub(src_offset, dest_offset, multi)) => {
            if *offset == *src_offset {
                Change::Replace(vec![OpType::CSub(*src_offset, *dest_offset, value * multi)])
            } else {
                Change::Ignore
            }
        }
        _ => Change::Ignore
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
                OpType::Set(offset, _) |
                OpType::Inc(offset, _) |
                OpType::Dec(offset, _) |
                OpType::PutChar(offset) |
                OpType::GetChar(offset)
                => {
                    let op_offset = ptr_offset + *offset - start_offset;
                    if *offset != op_offset {
                        *offset = op_offset;
                        inner_progress = true;
                    }
                    false
                }
                OpType::Add(src_offset, dest_offset, _) |
                OpType::NzAdd(src_offset, dest_offset, _) |
                OpType::CAdd(src_offset, dest_offset, _) |
                OpType::NzCAdd(src_offset, dest_offset, _) |
                OpType::Sub(src_offset, dest_offset, _) |
                OpType::NzSub(src_offset, dest_offset, _) |
                OpType::CSub(src_offset, dest_offset, _) |
                OpType::NzCSub(src_offset, dest_offset, _) |
                OpType::Move(src_offset, dest_offset) |
                OpType::Copy(src_offset, dest_offset)
                => {
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
                _ => break
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
                ops.insert(i, Op::inc_ptr(span, offset as usize));
            } else {
                ops.insert(i, Op::dec_ptr(span, -offset as usize));
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
                Change::Replace(vec![OpType::Add(src_offset + *i1 as isize, dest_offset + *i1 as isize, *value)])
            } else {
                Change::Ignore
            }
        }
        (OpType::DecPtr(i1), OpType::Add(src_offset, dest_offset, value), OpType::IncPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::Add(src_offset - *i1 as isize, dest_offset - *i1 as isize, *value)])
            } else {
                Change::Ignore
            }
        }
        (OpType::IncPtr(i1), OpType::CAdd(src_offset, dest_offset, value), OpType::DecPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::CAdd(src_offset + *i1 as isize, dest_offset + *i1 as isize, *value)])
            } else {
                Change::Ignore
            }
        }
        (OpType::DecPtr(i1), OpType::CAdd(src_offset, dest_offset, value), OpType::IncPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::CAdd(src_offset - *i1 as isize, dest_offset - *i1 as isize, *value)])
            } else {
                Change::Ignore
            }
        }
        (OpType::IncPtr(i1), OpType::Sub(src_offset, dest_offset, value), OpType::DecPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::Sub(src_offset + *i1 as isize, dest_offset + *i1 as isize, *value)])
            } else {
                Change::Ignore
            }
        }
        (OpType::DecPtr(i1), OpType::Sub(src_offset, dest_offset, value), OpType::IncPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::Sub(src_offset - *i1 as isize, dest_offset - *i1 as isize, *value)])
            } else {
                Change::Ignore
            }
        }
        (OpType::IncPtr(i1), OpType::CSub(src_offset, dest_offset, value), OpType::DecPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::CSub(src_offset + *i1 as isize, dest_offset + *i1 as isize, *value)])
            } else {
                Change::Ignore
            }
        }
        (OpType::DecPtr(i1), OpType::CSub(src_offset, dest_offset, value), OpType::IncPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::CSub(src_offset - *i1 as isize, dest_offset - *i1 as isize, *value)])
            } else {
                Change::Ignore
            }
        }
        _ => Change::Ignore
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
            OpType::LLoop(children, ..) |
            OpType::ILoop(children, ..) |
            OpType::CLoop(children, ..) |
            OpType::TNz(children, ..) => {
                progress |= remove_trailing_pointer_ops(children, true);
            }
            OpType::DLoop(children) => {
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

/// Non local and slower version of arithmetic optimizations
pub fn optimize_non_local_arithmetics(ops: &mut Vec<Op>) -> bool {
    run_non_local_pass(ops, optimize_non_local_arithmetics_pass, true, &[])
}

fn optimize_non_local_arithmetics_pass(mut ops: &mut Vec<Op>, zeroed: bool, inputs: &[(isize, CellValue)]) -> bool {
    let mut progress = false;

    let mut i = 0;

    while !ops.is_empty() && i < ops.len() {
        let op = &ops[i];

        let change = match &op.op_type {
            OpType::Inc(offset, value) => {
                if let CellValue::Value(v) = utils::find_heap_value(ops, *offset, i as isize - 1, zeroed, inputs) {
                    Change::Replace(vec![OpType::Set(*offset, v.wrapping_add(*value))])
                } else {
                    Change::Ignore
                }
            }
            OpType::Dec(offset, value) => {
                if let CellValue::Value(v) = utils::find_heap_value(ops, *offset, i as isize - 1, zeroed, inputs) {
                    Change::Replace(vec![OpType::Set(*offset, v.wrapping_sub(*value))])
                } else {
                    Change::Ignore
                }
            }
            OpType::Set(offset, value) => {
                if let CellValue::Value(v) = utils::find_heap_value(ops, *offset, i as isize - 1, zeroed, inputs) {
                    if *value == v {
                        Change::Remove
                    } else {
                        Change::Ignore
                    }
                } else {
                    Change::Ignore
                }
            }
            OpType::Add(src_offset, dest_offset, multi) => {
                let src = utils::find_heap_value(ops, *src_offset, i as isize - 1, zeroed, inputs);
                let dest = utils::find_heap_value(ops, *dest_offset, i as isize - 1, zeroed, inputs);

                match (src, dest) {
                    (CellValue::Value(0), CellValue::Value(_)) => {
                        Change::Remove
                    }
                    (CellValue::Value(src), CellValue::Value(dest)) => {
                        let value = dest.wrapping_add(src.wrapping_mul(*multi));

                        Change::Replace(vec![
                            OpType::Set(*dest_offset, value),
                            OpType::Set(*src_offset, 0),
                        ])
                    }
                    (CellValue::Value(src), CellValue::Unknown) => {
                        let value = src.wrapping_mul(*multi);

                        Change::Replace(vec![
                            OpType::CAdd(*src_offset, *dest_offset, value),
                        ])
                    }
                    (CellValue::Unknown, CellValue::Value(0)) => {
                        if *multi == 1 {
                            Change::Replace(vec![OpType::Move(*src_offset, *dest_offset)])
                        } else {
                            Change::Replace(vec![OpType::Mul(*src_offset, *dest_offset, *multi)])
                        }
                    }
                    _ => {
                        Change::Ignore
                    }
                }
            }
            OpType::NzAdd(src_offset, dest_offset, multi) => {
                let src = utils::find_heap_value(ops, *src_offset, i as isize - 1, zeroed, inputs);
                let dest = utils::find_heap_value(ops, *dest_offset, i as isize - 1, zeroed, inputs);

                match (src, dest) {
                    (CellValue::Value(0), CellValue::Value(_)) => {
                        Change::Remove
                    }
                    (CellValue::Value(src), CellValue::Value(dest)) => {
                        let value = dest.wrapping_add(src.wrapping_mul(*multi));

                        Change::Replace(vec![
                            OpType::Set(*dest_offset, value),
                        ])
                    }
                    (CellValue::Value(src), CellValue::Unknown) => {
                        let value = src.wrapping_mul(*multi);

                        Change::Replace(vec![
                            OpType::NzCAdd(*src_offset, *dest_offset, value),
                        ])
                    }
                    (CellValue::Unknown, CellValue::Value(0)) => {
                        if *multi == 1 {
                            Change::Replace(vec![OpType::Copy(*src_offset, *dest_offset)])
                        } else {
                            Change::Replace(vec![OpType::NzMul(*src_offset, *dest_offset, *multi)])
                        }
                    }
                    _ => {
                        Change::Ignore
                    }
                }
            }
            OpType::CAdd(src_offset, dest_offset, value) => {
                if let CellValue::Value(value2) = utils::find_heap_value(ops, *dest_offset, i as isize - 1, zeroed, inputs) {
                    Change::Replace(vec![
                        OpType::Set(*dest_offset, value2.wrapping_add(*value)),
                        OpType::Set(*src_offset, 0),
                    ])
                } else {
                    Change::Ignore
                }
            }
            OpType::NzCAdd(_src_offset, dest_offset, value) => {
                if let CellValue::Value(value2) = utils::find_heap_value(ops, *dest_offset, i as isize - 1, zeroed, inputs) {
                    Change::Replace(vec![
                        OpType::Set(*dest_offset, value2.wrapping_add(*value)),
                    ])
                } else {
                    Change::Ignore
                }
            }
            OpType::Sub(src_offset, dest_offset, multi) => {
                let src = utils::find_heap_value(ops, *src_offset, i as isize - 1, zeroed, inputs);
                let dest = utils::find_heap_value(ops, *dest_offset, i as isize - 1, zeroed, inputs);

                match (src, dest) {
                    (CellValue::Value(0), CellValue::Value(_)) => {
                        Change::Remove
                    }
                    (CellValue::Value(src), CellValue::Value(dest)) => {
                        let value = dest.wrapping_sub(src.wrapping_mul(*multi));

                        Change::Replace(vec![
                            OpType::Set(*dest_offset, value),
                            OpType::Set(*src_offset, 0),
                        ])
                    }
                    (CellValue::Value(src), CellValue::Unknown) => {
                        let value = src.wrapping_mul(*multi);

                        Change::Replace(vec![
                            OpType::CSub(*src_offset, *dest_offset, value),
                        ])
                    }
                    _ => {
                        Change::Ignore
                    }
                }
            }
            OpType::NzSub(src_offset, dest_offset, multi) => {
                let src = utils::find_heap_value(ops, *src_offset, i as isize - 1, zeroed, inputs);
                let dest = utils::find_heap_value(ops, *dest_offset, i as isize - 1, zeroed, inputs);

                match (src, dest) {
                    (CellValue::Value(0), CellValue::Value(_)) => {
                        Change::Remove
                    }
                    (CellValue::Value(src), CellValue::Value(dest)) => {
                        let value = dest.wrapping_sub(src.wrapping_mul(*multi));

                        Change::Replace(vec![
                            OpType::Set(*dest_offset, value),
                        ])
                    }
                    (CellValue::Value(src), CellValue::Unknown) => {
                        let value = src.wrapping_mul(*multi);

                        Change::Replace(vec![
                            OpType::NzCSub(*src_offset, *dest_offset, value),
                        ])
                    }
                    _ => {
                        Change::Ignore
                    }
                }
            }
            OpType::CSub(src_offset, dest_offset, value) => {
                if let CellValue::Value(value2) = utils::find_heap_value(ops, *dest_offset, i as isize - 1, zeroed, inputs) {
                    Change::Replace(vec![
                        OpType::Set(*dest_offset, value2.wrapping_sub(*value)),
                        OpType::Set(*src_offset, 0),
                    ])
                } else {
                    Change::Ignore
                }
            }
            OpType::NzCSub(_src_offset, dest_offset, value) => {
                if let CellValue::Value(value2) = utils::find_heap_value(ops, *dest_offset, i as isize - 1, zeroed, inputs) {
                    Change::Replace(vec![
                        OpType::Set(*dest_offset, value2.wrapping_sub(*value)),
                    ])
                } else {
                    Change::Ignore
                }
            }
            OpType::Mul(src_offset, dest_offset, multi) => {
                if let CellValue::Value(value) = utils::find_heap_value(ops, *src_offset, i as isize - 1, zeroed, inputs) {
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
                if let CellValue::Value(value) = utils::find_heap_value(ops, *src_offset, i as isize - 1, zeroed, inputs) {
                    if value == 0 {
                        Change::Remove
                    } else {
                        Change::Replace(vec![
                            OpType::Set(*dest_offset, value.wrapping_mul(*multi)),
                        ])
                    }
                } else {
                    Change::Ignore
                }
            }
            OpType::SearchZero(step) => {
                let mut ptr_offset = None;

                for test in 0..10 {
                    let offset = test * step;
                    let value = utils::find_heap_value(ops, offset, i as isize - 1, zeroed, inputs);

                    if matches!(value, CellValue::Value(0)) {
                        ptr_offset = Some(offset);
                        break;
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
            _ => {
                Change::Ignore
            }
        };

        if change.apply(&mut ops, i, 1) {
            progress = true;
        }

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
            OpType::Set(offset, _) |
            OpType::Move(_, offset) |
            OpType::Copy(_, offset) |
            OpType::Mul(_, offset, _) |
            OpType::NzMul(_, offset, _) |
            OpType::GetChar(offset)
            => {
                find_last_unread_set(&ops, *offset, i - 1)
            }
            OpType::CAdd(offset, ..) |
            OpType::CSub(offset, ..)
            => {
                find_last_unread_set(&ops, *offset, i - 1)
            }
            OpType::CLoop(..)
            => {
                find_last_unread_set(&ops, 0, i - 1)
            }
            _ => {
                None
            }
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

        let changes = if let OpType::CLoop(children, iterations, _) = &op.op_type {
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

                for _ in 0..*iterations {
                    for op in children {
                        changes.push(op.clone());
                    }
                    if ptr_offset != 0 {
                        changes.push(Op::ptr_offset(ops[i].span.clone(), -ptr_offset));
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
                ops.insert(i + index, op);
            }

            ops.insert(i + len, Op::set(old.span, 0));

            // All possible loops within the current loop should already haven been unrolled
            i += len + 1;
            progress = true;
        } else {
            i += 1;
        }
    }

    progress
}

fn find_last_unread_set(ops: &[Op], mut cell_offset: isize, start_index: usize) -> Option<(usize, Option<OpType>)> {
    let mut i = start_index as isize;

    while i >= 0 {
        match &ops[i as usize].op_type {
            OpType::IncPtr(offset) => cell_offset += *offset as isize,
            OpType::DecPtr(offset) => cell_offset -= *offset as isize,
            OpType::Set(offset, _) |
            OpType::Inc(offset, _) |
            OpType::Dec(offset, _) => {
                if *offset == cell_offset {
                    return Some((i as usize, None));
                }
            }
            OpType::Add(src_offset, dest_offset, multi) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((i as usize, Some(OpType::NzAdd(*src_offset, *dest_offset, *multi))));
                }
            }
            OpType::CAdd(src_offset, dest_offset, value) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((i as usize, Some(OpType::NzCAdd(*src_offset, *dest_offset, *value))));
                }
            }
            OpType::Sub(src_offset, dest_offset, multi) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((i as usize, Some(OpType::NzSub(*src_offset, *dest_offset, *multi))));
                }
            }
            OpType::CSub(src_offset, dest_offset, value) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((i as usize, Some(OpType::NzCSub(*src_offset, *dest_offset, *value))));
                }
            }
            OpType::Mul(src_offset, dest_offset, value) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((i as usize, Some(OpType::NzMul(*src_offset, *dest_offset, *value))));
                }
            }
            OpType::Move(src_offset, dest_offset) => {
                if *dest_offset == cell_offset {
                    break;
                } else if *src_offset == cell_offset {
                    return Some((i as usize, Some(OpType::Copy(*src_offset, *dest_offset))));
                }
            }
            OpType::NzAdd(src_offset, dest_offset, _) |
            OpType::NzCAdd(src_offset, dest_offset, _) |
            OpType::NzSub(src_offset, dest_offset, _) |
            OpType::NzCSub(src_offset, dest_offset, _) |
            OpType::NzMul(src_offset, dest_offset, _) |
            OpType::Copy(src_offset, dest_offset)
            => {
                if *src_offset == cell_offset {
                    break;
                } else if *dest_offset == cell_offset {
                    return Some((i as usize, None));
                }
            }
            OpType::GetChar(offset) => {
                if cell_offset + offset == 0 {
                    break;
                }
            }
            OpType::LLoop(_, info) |
            OpType::ILoop(_, _, info) |
            OpType::CLoop(_, _, info) |
            OpType::TNz(_, info)
            => {
                if cell_offset == 0 || info.was_cell_accessed(cell_offset) {
                    break;
                }
            }
            _ => break,
        }

        i -= 1
    }

    None
}

pub fn update_loop_access(ops: &mut Vec<Op>) {
    for op in ops.iter_mut() {
        if let Some(children) = op.op_type.get_children_mut() {
            update_loop_access(children);
        }
    }

    for op in ops {
        match &mut op.op_type {
            OpType::LLoop(children, info) |
            OpType::ILoop(children, _, info) |
            OpType::CLoop(children, _, info) |
            OpType::TNz(children, info)
            => {
                info.set_cell_access(get_loop_access(children, 0));
            }
            _ => {
                // Ignore
            }
        }
    }
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
            Op::d_loop(1..2, vec![]),
            Op::inc(2..3, 1),
            Op::d_loop(3..4, vec![Op::inc(4..5, 1),
            ]),
        ];

        remove_dead_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::start(),
            Op::inc(2..3, 1),
            Op::d_loop(3..4, vec![Op::inc(4..5, 1),
            ]),
        ])
    }

    #[test]
    fn test_optimize_inc() {
        let mut ops = vec![
            Op::inc(0..1, 1),
            Op::inc(1..2, 2),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::inc(0..2, 3),
        ])
    }

    #[test]
    fn test_optimize_dec() {
        let mut ops = vec![
            Op::dec(0..1, 1),
            Op::dec(1..2, 2),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::dec(0..2, 3),
        ])
    }

    #[test]
    fn test_optimize_inc_dec() {
        let mut ops = vec![
            Op::inc(0..1, 1),
            Op::dec(1..2, 2),
            Op::dec(2..3, 3),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::dec(0..3, 4),
        ])
    }

    #[test]
    fn test_optimize_set_inc() {
        let mut ops = vec![
            Op::set(0..1, 5),
            Op::inc(1..2, 2),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::set(0..2, 7),
        ])
    }

    #[test]
    fn test_optimize_set_dec() {
        let mut ops = vec![
            Op::set(0..1, 5),
            Op::dec(1..2, 2),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::set(0..2, 3),
        ])
    }

    #[test]
    fn test_optimize_inc_dec_0() {
        let mut ops = vec![
            Op::inc(0..1, 1),
            Op::dec(1..2, 1),
            Op::inc_ptr(2..3, 3),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::inc_ptr(2..3, 3),
        ])
    }

    #[test]
    fn test_optimize_inc_ptr() {
        let mut ops = vec![
            Op::inc_ptr(0..1, 1),
            Op::inc_ptr(1..2, 2),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::inc_ptr(0..2, 3),
        ])
    }

    #[test]
    fn test_optimize_dec_ptr() {
        let mut ops = vec![
            Op::dec_ptr(0..1, 1),
            Op::dec_ptr(1..2, 2),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::dec_ptr(0..2, 3),
        ])
    }

    #[test]
    fn test_optimize_inc_ptr_dec() {
        let mut ops = vec![
            Op::inc_ptr(0..1, 1),
            Op::dec_ptr(1..2, 2),
            Op::dec_ptr(2..3, 3),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::dec_ptr(0..3, 4),
        ])
    }

    #[test]
    fn test_optimize_inc_dec_ptr_0() {
        let mut ops = vec![
            Op::inc_ptr(0..1, 1),
            Op::dec_ptr(1..2, 1),
            Op::inc(2..3, 3),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::inc(2..3, 3),
        ])
    }

    #[test]
    fn test_optimize_inc_dec_zeroing_inc() {
        let mut ops = vec![
            Op::c_loop(0..2, vec![], 1, BlockInfo::new_empty()),
            Op::inc(2..3, 3),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetics);

        assert_eq!(ops, vec![
            Op::c_loop(0..2, vec![], 1, BlockInfo::new_empty()),
            Op::set(2..3, 3),
        ])
    }

    #[test]
    fn test_optimize_zero_loop() {
        let mut ops = vec![
            Op::d_loop(0..1, vec![Op::dec(1..2, 1)]),
        ];

        optimize_zero_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..2, 0),
        ])
    }

    #[test]
    fn test_optimize_zero_loop_inc() {
        let mut ops = vec![
            Op::d_loop(0..1, vec![Op::inc(1..2, 1)]),
        ];

        optimize_zero_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..2, 0),
        ])
    }

    #[test]
    fn test_optimize_add() {
        let mut ops = vec![
            Op::d_loop(0..5, vec![
                Op::inc_ptr(1..2, 1),
                Op::inc(2..3, 1),
                Op::dec_ptr(3..4, 1),
                Op::dec(4..5, 1),
            ])
        ];

        optimize_local_loops(&mut ops);
        optimize_offsets(&mut ops, 1);
        optimize_count_loops(&mut ops);
        run_peephole_pass(&mut ops, optimize_arithmetic_loops);

        assert_eq!(ops, vec![
            Op::add(0..5, 1, 1),
        ])
    }

    #[test]
    fn test_optimize_add_inv() {
        let mut ops = vec![
            Op::d_loop(0..5, vec![
                Op::dec(1..2, 1),
                Op::inc_ptr(2..3, 2),
                Op::inc(3..4, 1),
                Op::dec_ptr(4..5, 2),
            ])
        ];

        optimize_local_loops(&mut ops);
        optimize_offsets(&mut ops, 1);
        optimize_count_loops(&mut ops);
        run_peephole_pass(&mut ops, optimize_arithmetic_loops);

        assert_eq!(ops, vec![
            Op::add(0..5, 2, 1),
        ])
    }

    #[test]
    fn test_optimize_sub() {
        let mut ops = vec![
            Op::d_loop(0..5, vec![
                Op::inc_ptr(1..2, 1),
                Op::dec(2..3, 2),
                Op::dec_ptr(3..4, 1),
                Op::dec(4..5, 1),
            ])
        ];

        optimize_local_loops(&mut ops);
        optimize_offsets(&mut ops, 1);
        optimize_count_loops(&mut ops);
        run_peephole_pass(&mut ops, optimize_arithmetic_loops);

        assert_eq!(ops, vec![
            Op::sub(0..5, 1, 2),
        ])
    }


    #[test]
    fn test_optimize_sub2() {
        let mut ops = vec![
            Op::d_loop(0..5, vec![
                Op::dec(1..2, 1),
                Op::inc_ptr(2..3, 1),
                Op::dec(3..4, 2),
                Op::dec_ptr(4..5, 1),
            ])
        ];

        optimize_local_loops(&mut ops);
        optimize_offsets(&mut ops, 1);
        optimize_count_loops(&mut ops);
        run_peephole_pass(&mut ops, optimize_arithmetic_loops);

        assert_eq!(ops, vec![
            Op::sub(0..5, 1, 2),
        ])
    }

    #[test]
    fn test_optimize_local_loops() {
        let mut ops = vec![
            Op::set(0..1, 6),
            Op::d_loop(0..5, vec![
                Op::dec(1..2, 1),
                Op::inc_ptr(2..3, 1),
                Op::dec(3..4, 2),
                Op::dec_ptr(4..5, 1),
            ])
        ];

        optimize_local_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..1, 6),
            Op::l_loop(0..5, vec![
                Op::dec(1..2, 1),
                Op::inc_ptr(2..3, 1),
                Op::dec(3..4, 2),
            ], BlockInfo::new_access(vec![CellAccess::new_write(1)]))
        ])
    }

    #[test]
    fn test_optimize_count_loops() {
        let mut ops = vec![
            Op::set(0..1, 6),
            Op::d_loop(0..1, vec![
                Op::dec(1..2, 1),
                Op::inc_ptr(2..3, 1),
                Op::dec(3..4, 2),
                Op::dec_ptr(4..5, 1),
            ])
        ];

        optimize_local_loops(&mut ops);
        optimize_count_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..1, 6),
            Op::i_loop(0..1, vec![
                Op::inc_ptr(2..3, 1),
                Op::dec(3..4, 2),
            ], 1, BlockInfo::new_access(vec![CellAccess::new_write(1)]))
        ])
    }

    #[test]
    fn test_optimize_count_loops_inv() {
        let mut ops = vec![
            Op::set(0..1, 6),
            Op::d_loop(0..1, vec![
                Op::dec_ptr(1..2, 1),
                Op::dec(2..3, 3),
                Op::inc_ptr(3..4, 1),
                Op::dec(4..5, 2),
            ])
        ];

        optimize_local_loops(&mut ops);
        optimize_count_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..1, 6),
            Op::i_loop(0..1, vec![
                Op::dec_ptr(1..2, 1),
                Op::dec(2..3, 3),
            ], 2, BlockInfo::new_access(vec![CellAccess::new_write(-1)]))
        ])
    }

    #[test]
    fn test_optimize_static_count_loops() {
        let mut ops = vec![
            Op::set(0..1, 6),
            Op::d_loop(0..1, vec![
                Op::dec_ptr(1..2, 1),
                Op::dec(2..3, 3),
                Op::inc_ptr(3..4, 1),
                Op::dec(4..5, 2),
            ])
        ];

        optimize_local_loops(&mut ops);
        optimize_offsets(&mut ops, 1);
        optimize_count_loops(&mut ops);
        optimize_static_count_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::c_loop(0..1, vec![
                Op::dec_with_offset(2..3, -1, 3),
            ], 3, BlockInfo::new_access(vec![CellAccess::new_write(-1)]))
        ])
    }

    #[test]
    fn test_optimize_static_count_loops_zero() {
        let mut ops = vec![
            Op::set(0..1, 0),
            Op::d_loop(0..1, vec![
                Op::dec_ptr(1..2, 1),
                Op::dec(2..3, 3),
                Op::inc_ptr(3..4, 1),
                Op::dec(4..5, 2),
            ])
        ];

        optimize_local_loops(&mut ops);
        optimize_count_loops(&mut ops);
        optimize_static_count_loops(&mut ops);

        assert_eq!(ops, vec![])
    }

    #[test]
    fn test_optimize_tnz_set() {
        let mut ops = vec![
            Op::i_loop(0..2, vec![
                Op::set(1..2, 1),
            ], 1, BlockInfo::new_empty())
        ];

        optimize_conditional_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::t_nz(0..2, vec![
                Op::set(1..2, 1),
            ], BlockInfo::new_empty())
        ])
    }

    #[test]
    fn test_dead_store_set() {
        let mut ops = vec![
            Op::inc(0..1, 1),
            Op::set(1..2, 2),
        ];

        remove_dead_stores_before_set(&mut ops);

        assert_eq!(ops, vec![
            Op::set(1..2, 2),
        ])
    }

    #[test]
    fn test_dead_store_get_char() {
        let mut ops = vec![
            Op::inc(0..1, 1),
            Op::get_char(1..2),
        ];

        remove_dead_stores_before_set(&mut ops);

        assert_eq!(ops, vec![
            Op::get_char(1..2),
        ])
    }

    #[test]
    fn test_optimize_search_zero_inc() {
        let mut ops = vec![
            Op::d_loop(0..2, vec![
                Op::inc_ptr(1..2, 8),
            ])
        ];

        optimize_search_zero(&mut ops);

        assert_eq!(ops, vec![
            Op::search_zero(0..2, 8),
        ])
    }

    #[test]
    fn test_optimize_nested_search_zero_inc() {
        let mut ops = vec![
            Op::d_loop(0..3, vec![
                Op::d_loop(1..3, vec![
                    Op::inc_ptr(2..3, 8),
                ])])
        ];

        optimize_search_zero(&mut ops);

        assert_eq!(ops, vec![
            Op::d_loop(0..3, vec![
                Op::search_zero(1..3, 8),
            ])
        ])
    }

    #[test]
    fn test_optimize_search_zero_dec() {
        let mut ops = vec![
            Op::d_loop(0..2, vec![
                Op::dec_ptr(1..2, 8),
            ])
        ];

        optimize_search_zero(&mut ops);

        assert_eq!(ops, vec![
            Op::search_zero(0..2, -8),
        ])
    }

    #[test]
    fn test_optimize_nested_search_zero_dec() {
        let mut ops = vec![
            Op::d_loop(0..3, vec![
                Op::d_loop(1..3, vec![
                    Op::dec_ptr(2..3, 8),
                ])])
        ];

        optimize_search_zero(&mut ops);

        assert_eq!(ops, vec![
            Op::d_loop(0..3, vec![
                Op::search_zero(1..3, -8),
            ])
        ])
    }

    #[test]
    fn test_optimize_constant_arithmetic_loop() {
        let mut ops = vec![
            Op::c_loop(0..3, vec![
                Op::inc_ptr(0..1, 2),
                Op::inc(1..2, 3)
            ], 5, BlockInfo::new_empty())
        ];

        optimize_constant_arithmetic_loop(&mut ops);

        assert_eq!(ops, vec![
            Op::inc_ptr(0..1, 2),
            Op::inc(1..2, 15),
            Op::dec_ptr(2..3, 2),
        ])
    }

    #[test]
    fn test_optimize_constant_arithmetic_loop_add() {
        let mut ops = vec![
            Op::set(0..1, 3),
            Op::add(1..2, 1, 3),
        ];

        run_peephole_pass(&mut ops, optimize_constant_arithmetics);

        assert_eq!(ops, vec![
            Op::c_add(0..2, 1, 9),
        ])
    }


    #[test]
    fn test_optimize_constant_arithmetic_loop_sub() {
        let mut ops = vec![
            Op::set(0..1, 3),
            Op::sub(1..2, 1, 2),
        ];

        run_peephole_pass(&mut ops, optimize_constant_arithmetics);

        assert_eq!(ops, vec![
            Op::c_sub(0..2, 1, 6),
        ])
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
            Op::d_loop(6..8, vec![]),
        ];

        optimize_offsets(&mut ops, 0);

        assert_eq!(ops, vec![
            Op::set(0..1, 3),
            Op::inc_with_offset(2..3, 3, 2),
            Op::dec_with_offset(4..5, 2, 5),
            Op::inc_ptr(0..6, 1),
            Op::d_loop(6..8, vec![]),
        ])
    }


    #[test]
    fn test_optimize_arithmetic_offsets() {
        let mut ops = vec![
            Op::inc_ptr(0..1, 1),
            Op::c_add(1..2, -1, 72),
            Op::dec_ptr(2..3, 1),
        ];

        run_peephole_pass(&mut ops, optimize_arithmetic_offsets);

        assert_eq!(ops, vec![
            Op::c_add_with_offset(0..3, 1, 0, 72),
        ])
    }

    #[test]
    fn test_remove_trailing_pointer_ops() {
        let mut ops = vec![
            Op::i_loop(0..3, vec![
                Op::c_add(1..2, -1, 72),
                Op::dec_ptr(2..3, 1),
            ], 5, BlockInfo::new_empty()),
            Op::inc_ptr(3..4, 1),
        ];

        remove_trailing_pointer_ops(&mut ops, true);

        assert_eq!(ops, vec![
            Op::i_loop(0..3, vec![
                Op::c_add(1..2, -1, 72),
            ], 5, BlockInfo::new_empty()),
        ])
    }

    #[test]
    fn test_remove_useless_copy() {
        let mut ops = vec![
            Op::copy(0..1, 3, 7),
            Op::copy(1..2, 7, 3),
        ];

        run_peephole_pass(&mut ops, remove_useless_copy);

        assert_eq!(ops, vec![
            Op::copy(0..2, 3, 7),
        ])
    }

    #[test]
    fn test_unroll_constant_loops() {
        let mut ops = vec![
            Op::c_loop(0..4, vec![
                Op::inc_with_offset(1..2, 2, 1),
                Op::inc_with_offset(2..3, 1, 2),
            ], 3, BlockInfo::new_empty())
        ];

        unroll_constant_loops(&mut ops, 40);

        assert_eq!(ops, vec![
            Op::inc_with_offset(1..2, 2, 1),
            Op::inc_with_offset(2..3, 1, 2),
            Op::inc_with_offset(1..2, 2, 1),
            Op::inc_with_offset(2..3, 1, 2),
            Op::inc_with_offset(1..2, 2, 1),
            Op::inc_with_offset(2..3, 1, 2),
            Op::set(0..4, 0),
        ])
    }

    #[test]
    fn test_nz_mul() {
        let mut ops = vec![
            Op::set_with_offset(0..1, 1, 0),
            Op::nz_add(1..2, 1, 2),
        ];

        run_non_local_pass(&mut ops, optimize_non_local_arithmetics_pass, false, &[]);

        assert_eq!(ops, vec![
            Op::set_with_offset(0..1, 1, 0),
            Op::nz_mul(1..2, 1, 2),
        ])
    }

    #[test]
    fn test_non_local_bug() {
        let mut ops = vec![
            Op::d_loop(0..1, vec![
                Op::inc_ptr(0..1, 1),
                Op::i_loop(0..1, vec![
                    Op::inc_with_offset(0..1, 5, 1),
                    Op::inc_ptr(0..1, 1),
                    Op::i_loop(0..1, vec![
                        Op::dec_with_offset(0..1, 4, 1),
                        Op::inc_with_offset(0..1, -10, 1),
                        Op::add_with_offset(0..1, 1, 4, 1),
                    ], 1, BlockInfo::new_empty()),
                    Op::nz_sub_with_offset(0..1, 1, 4, 1),
                    Op::add_with_offset(0..1, 1, -10, 1),
                ], 1, BlockInfo::new_empty()),
                Op::inc_ptr(0..1, 1),
                Op::i_loop(0..1, vec![
                    Op::inc_with_offset(0..1, 4, 1),
                    Op::nz_sub_with_offset(0..1, 1, 4, 1),
                    Op::add_with_offset(0..1, 1, -10, 1)
                ], 1, BlockInfo::new_empty()),
                Op::add_with_offset(0..1, 1, 4, 1),
            ]),
            Op::dec_ptr(0..1, 11),
        ];

        update_loop_access(&mut ops);

        let inputs = ops.clone();

        run_non_local_pass(&mut ops, optimize_non_local_arithmetics_pass, false, &[]);

        assert_eq!(ops, inputs)
    }
}