use crate::parser::{Op, OpType};
use std::cmp::Ordering;
use crate::optimizations::peephole::Change;

// Loops at the beginning of a program will not be taken at all
pub fn remove_preceding_loop(ops: &mut Vec<Op>) {
    while !ops.is_empty() {
        if let OpType::DLoop(_) = ops[0].op_type {
            ops.remove(0);
        } else {
            break;
        }
    }
}

// Replace '[-]' that decreases the current point to 0 with set
// [+] can be optimized too assuming a wrapping overflow to 0
pub fn optimize_zero_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        let op = &mut ops[i];
        if let Some(children) = op.op_type.get_children_mut() {
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
    if let OpType::ILoop(children, loop_offset, step) = &ops[0].op_type {
        /*
         * TODO: Allow loops with more than 1 child
         * This will not work correctly with multiple ADD and SUB because they reset
         * the current heap cell to zero
         */
        if *step == 1 && children.len() == 1 {
            let mut replacement_indices = vec![];
            let mut replacements = vec![];

            for child in children {
                match &child.op_type {
                    OpType::Inc(offset, multi) => {
                        if replacement_indices.contains(offset) {
                            return Change::Ignore;
                        } else {
                            replacement_indices.push(*offset);
                            replacements.push(OpType::Add(0, loop_offset + *offset as isize, *multi));
                        }
                    }
                    OpType::Dec(offset, multi) => {
                        if replacement_indices.contains(offset) {
                            return Change::Ignore;
                        } else {
                            replacement_indices.push(*offset);
                            replacements.push(OpType::Sub(0, loop_offset + *offset as isize, *multi));
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
                Change::Replace(replacements)
            };
        }
    }

    Change::Ignore
}

// Optimize loops that are known to use the same counting variable
pub fn optimize_count_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && !ops.is_empty() && i < ops.len() {
        let replace = if let OpType::DLoop(children) = &mut ops[i].op_type {
            let mut ptr_offset = 0_isize;
            let mut ignore = false;
            let mut possible_match = false;
            let mut counter_decrements = vec![];

            let num_ops = children.len();
            if num_ops >= 2 {
                for (i, op) in children.iter().enumerate() {
                    match &op.op_type {
                        OpType::IncPtr(value) => {
                            possible_match = true;
                            ptr_offset += *value as isize;
                        }
                        OpType::DecPtr(value) => {
                            possible_match = true;
                            ptr_offset -= *value as isize;
                        }
                        OpType::Add(src_offset, dest_offset, _) |
                        OpType::Sub(src_offset, dest_offset, _) |
                        OpType::CAdd(src_offset, dest_offset, _) |
                        OpType::CSub(src_offset, dest_offset, _)
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
                        OpType::Inc(offset, _) | OpType::Set(offset, _) => {
                            if ptr_offset + offset == 0 {
                                ignore = true;
                                break;
                            }
                        }
                        OpType::GetChar => {
                            if ptr_offset == 0 {
                                ignore = true;
                                break;
                            }
                        }
                        OpType::Dec(offset, v) => {
                            if ptr_offset + offset == 0 {
                                possible_match = true;
                                counter_decrements.push((i, *v));
                            }
                        }
                        OpType::DLoop(..) => {
                            ignore = true;
                            break;
                        }
                        OpType::ILoop(children, offset, ..) |
                        OpType::CLoop(children, offset, ..) |
                        OpType::TNz(children, offset) => {
                            if !is_ops_block_local(children, &[-ptr_offset - *offset]) {
                                ignore = true;
                                break;
                            }
                        }
                        OpType::SearchZero(..) => {
                            ignore = true;
                            break;
                        }
                        OpType::PutChar => {
                            // ignore
                        }
                    }
                }
            }

            #[allow(clippy::manual_map)]
            if !ignore && possible_match && ptr_offset == 0 && !counter_decrements.is_empty() {
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

            if let OpType::DLoop(mut children) = prev.op_type {
                let mut step = 0;

                for (index, amount) in counter_decrements.iter().rev() {
                    step += *amount;
                    children.remove(*index);
                }

                while !children.is_empty() && children[children.len() - 1].op_type.is_ptr_inc_or_dec() {
                    children.remove(children.len() - 1);
                }

                let offset = if let Some(offset) = children[0].op_type.get_ptr_offset() {
                    children.remove(0);
                    offset
                } else {
                    0
                };

                ops.insert(i, Op::i_loop(span, children, offset, step));

                progress = true;
            } else {
                unreachable!();
            }
        } else if let Some(children) = ops[i].op_type.get_children_mut() {
            progress |= optimize_count_loops(children);
        }
        i += 1;
    }

    progress
}

fn is_ops_block_local(ops: &[Op], parent_offsets: &[isize]) -> bool {
    let mut ptr_offset = 0_isize;

    for op in ops {
        match &op.op_type {
            OpType::IncPtr(value) => {
                ptr_offset += *value as isize;
            }
            OpType::DecPtr(value) => {
                ptr_offset -= *value as isize;
            }
            OpType::Add(src_offset, dest_offset, _) |
            OpType::Sub(src_offset, dest_offset, _) |
            OpType::CAdd(src_offset, dest_offset, _) |
            OpType::CSub(src_offset, dest_offset, _)
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
            OpType::Inc(offset, _) | OpType::Dec(offset, _) | OpType::Set(offset, _) => {
                for parent_offset in parent_offsets {
                    if ptr_offset + offset == *parent_offset {
                        return false;
                    }
                }
            }
            OpType::GetChar => {
                for parent_offset in parent_offsets {
                    if ptr_offset == *parent_offset {
                        return false;
                    }
                }
            }
            OpType::ILoop(children, offset, ..) |
            OpType::CLoop(children, offset, ..) |
            OpType::TNz(children, offset) => {
                let mut offsets = parent_offsets.iter().map(|o| {
                    o - *offset
                }).collect::<Vec<_>>();

                offsets.push(-ptr_offset - *offset);

                if !is_ops_block_local(children, &offsets) {
                    return false;
                }
            }
            OpType::DLoop(_) |
            OpType::SearchZero(..) => {
                return false;
            }
            OpType::PutChar => {
                // ignore
            }
        }
    }

    true
}

// Merge multiple inc/dec or inc_ptr dec_ptr ops and remove zero versions
pub fn optimize_inc_dec(ops: [&Op; 2]) -> Change {
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
        (op_type, OpType::Inc(0, v)) => {
            if op_type.is_zeroing() {
                Change::ReplaceOffset(1, ops[1].span.clone(), vec![OpType::Set(0, *v)])
            } else {
                Change::Ignore
            }
        }
        (op_type, OpType::Dec(0, v)) => {
            if op_type.is_zeroing() {
                Change::ReplaceOffset(1, ops[1].span.clone(), vec![OpType::Set(0, 0u8.wrapping_sub(*v))])
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
        let op1 = &ops[i];
        let op2 = &ops[i + 1];


        let remove = match (&op1.op_type, &op2.op_type) {
            (OpType::Inc(offset, _), OpType::Set(offset2, _)) |
            (OpType::Dec(offset, _), OpType::Set(offset2, _)) |
            (OpType::Set(offset, _), OpType::Set(offset2, _)) => {
                *offset == *offset2
            }
            (OpType::Inc(0, _), OpType::GetChar) |
            (OpType::Dec(0, _), OpType::GetChar) |
            (OpType::Set(0, _), OpType::GetChar) => true,
            _ => false,
        };

        if remove {
            ops.remove(i);
            progress = true;
        } else {
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
            (OpType::Set(0, v), OpType::ILoop(_, _, step)) =>
                if v % step == 0 {
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

                if let OpType::ILoop(children, offset, _) = loop_op.op_type {
                    ops.insert(i, Op::c_loop(span, children, offset, count));
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

// Replace loops only containing constant sets with TNz
pub fn optimize_conditional_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        let op = &mut ops[i];

        let replace = match &op.op_type {
            OpType::ILoop(children, ..) |
            OpType::CLoop(children, ..) =>
                contains_only_constant_sets(children),
            _ => false,
        };

        if replace {
            let prev = ops.remove(i);
            let span = prev.span;

            match prev.op_type {
                OpType::ILoop(children, offset, _) |
                OpType::CLoop(children, offset, _) => {
                    ops.insert(i, Op::t_nz(span, children, offset));
                }
                _ => unreachable!(),
            }
        }

        if let Some(children) = ops[i].op_type.get_children_mut() {
            progress |= optimize_conditional_loops(children);
        }

        i += 1;
    }

    progress
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
                OpType::CLoop(children, offset, iterations) => {
                    let mut ptr_offset = offset;

                    let mut pos = i;

                    if ptr_offset > 0 {
                        ops.insert(pos, Op::inc_ptr(span.start..span.start + 1, ptr_offset as usize));
                        pos += 1;
                    } else if offset < 0 {
                        ops.insert(pos, Op::dec_ptr(span.start..span.start + 1, -ptr_offset as usize));
                        pos += 1;
                    }

                    for mut child in children {
                        match &mut child.op_type {
                            OpType::Inc(_, v) |
                            OpType::Dec(_, v) => *v *= iterations,
                            OpType::IncPtr(v) => ptr_offset += *v as isize,
                            OpType::DecPtr(v) => ptr_offset -= *v as isize,
                            _ => {
                                // ignore
                            }
                        }

                        ops.insert(pos, child);

                        pos += 1;
                    }

                    if ptr_offset > 0 {
                        ops.insert(pos, Op::dec_ptr(span.end - 1..span.end, ptr_offset as usize));
                    } else if offset < 0 {
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

pub fn optimize_heap_initialization(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    let mut heap: Vec<Option<usize>> = vec![];

    let mut ptr = 0;

    while !ops.is_empty() && i < ops.len() {
        let replace = {
            let mut op = &mut ops[i];

            if let Some(offset) = op.op_type.get_ptr_offset() {
                ptr += offset;
                None
            } else if let OpType::Inc(offset, value) = &op.op_type {
                let op_ptr = ptr + offset;

                if op_ptr < 0 {
                    break;
                }

                while op_ptr > heap.len() as isize - 1 {
                    heap.push(None);
                }

                progress = true;

                if let Some(index) = &heap[op_ptr as usize] {
                    Some((index, *value as i16))
                } else {
                    op.op_type = OpType::Set(*offset, *value);
                    heap[op_ptr as usize] = Some(i);
                    None
                }
            } else if let OpType::Dec(offset, value) = &op.op_type {
                let op_ptr = ptr + offset;

                if op_ptr < 0 {
                    break;
                }

                while op_ptr > heap.len() as isize - 1 {
                    heap.push(None);
                }

                progress = true;

                if let Some(index) = &heap[op_ptr as usize] {
                    Some((index, -(*value as i16)))
                } else {
                    op.op_type = OpType::Set(*offset, 0u8.wrapping_sub(*value));
                    heap[op_ptr as usize] = Some(i);
                    None
                }
            } else {
                break;
            }
        };

        if let Some((&index, value)) = replace {
            if let OpType::Set(_, set_v) = &mut ops[index].op_type {
                if value < 0 {
                    *set_v = set_v.wrapping_sub(-value as u8);
                } else {
                    *set_v = set_v.wrapping_add(value as u8);
                }
            }
            ops.remove(i);
            i -= 1;
        }

        i += 1;
    }

    progress
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
                OpType::Dec(offset, _) => {
                    let op_offset = ptr_offset + *offset - start_offset;
                    if *offset != op_offset {
                        *offset = op_offset;
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
        if let Some((offset, children)) = op.op_type.get_children_with_offset_mut() {
            progress |= optimize_offsets(children, offset);
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
        (OpType::IncPtr(i1), OpType::CAdd(src_offset, dest_offset, value), OpType::DecPtr(i2)) => {
            if *i1 == *i2 {
                Change::Replace(vec![OpType::CAdd(src_offset + *i1 as isize, dest_offset + *i1 as isize, *value)])
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

#[cfg(test)]
mod tests {
    use crate::parser::Op;
    use super::*;
    use crate::optimizations::peephole::run_peephole_pass;

    #[test]
    fn test_remove_preceding_loop() {
        let mut ops = vec![
            Op::d_loop(0..1, vec![]),
            Op::d_loop(1..2, vec![]),
            Op::inc(2..3, 1),
            Op::d_loop(3..4, vec![Op::inc(4..5, 1),
            ]),
        ];

        remove_preceding_loop(&mut ops);

        assert_eq!(ops, vec![
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

        run_peephole_pass(&mut ops, optimize_inc_dec);

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

        run_peephole_pass(&mut ops, optimize_inc_dec);

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

        run_peephole_pass(&mut ops, optimize_inc_dec);

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

        run_peephole_pass(&mut ops, optimize_inc_dec);

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

        run_peephole_pass(&mut ops, optimize_inc_dec);

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

        run_peephole_pass(&mut ops, optimize_inc_dec);

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

        run_peephole_pass(&mut ops, optimize_inc_dec);

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

        run_peephole_pass(&mut ops, optimize_inc_dec);

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

        run_peephole_pass(&mut ops, optimize_inc_dec);

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

        run_peephole_pass(&mut ops, optimize_inc_dec);

        assert_eq!(ops, vec![
            Op::inc(2..3, 3),
        ])
    }

    #[test]
    fn test_optimize_inc_dec_zeroing_inc() {
        let mut ops = vec![
            Op::c_loop(0..2, vec![], 1, 1),
            Op::inc(2..3, 3),
        ];

        run_peephole_pass(&mut ops, optimize_inc_dec);

        assert_eq!(ops, vec![
            Op::c_loop(0..2, vec![], 1, 1),
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

        optimize_count_loops(&mut ops);
        run_peephole_pass(&mut ops, optimize_arithmetic_loops);

        assert_eq!(ops, vec![
            Op::sub(0..5, 1, 2),
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

        optimize_count_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..1, 6),
            Op::i_loop(0..1, vec![
                Op::dec(3..4, 2),
            ], 1, 1)
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

        optimize_count_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..1, 6),
            Op::i_loop(0..1, vec![
                Op::dec(2..3, 3),
            ], -1, 2)
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

        optimize_count_loops(&mut ops);
        optimize_static_count_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::c_loop(0..1, vec![
                Op::dec(2..3, 3),
            ], -1, 3)
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

        optimize_count_loops(&mut ops);
        optimize_static_count_loops(&mut ops);

        assert_eq!(ops, vec![])
    }

    #[test]
    fn test_optimize_tnz_set() {
        let mut ops = vec![
            Op::i_loop(0..2, vec![
                Op::set(1..2, 1),
            ], 1, 1)
        ];

        optimize_conditional_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::t_nz(0..2, vec![
                Op::set(1..2, 1),
            ], 1)
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
                Op::inc(1..2, 3)
            ], 2, 5)
        ];

        optimize_constant_arithmetic_loop(&mut ops);

        assert_eq!(ops, vec![
            Op::inc_ptr(0..1, 2),
            Op::inc(1..2, 15),
            Op::dec_ptr(2..3, 2),
        ])
    }

    #[test]
    fn test_optimize_heap_initialization() {
        let mut ops = vec![
            Op::inc_ptr(0..1, 1),
            Op::inc(1..2, 26),
            Op::inc_ptr(2..3, 3),
            Op::inc(3..4, 65),
            Op::dec_ptr(4..5, 3),
            Op::inc(5..6, 1),
            Op::inc_ptr(6..7, 5),
            Op::get_char(8..9),
            Op::inc(9..10, 1),
        ];

        optimize_heap_initialization(&mut ops);

        assert_eq!(ops, vec![
            Op::inc_ptr(0..1, 1),
            Op::set(1..2, 27),
            Op::inc_ptr(2..3, 3),
            Op::set(3..4, 65),
            Op::dec_ptr(4..5, 3),
            Op::inc_ptr(6..7, 5),
            Op::get_char(8..9),
            Op::inc(9..10, 1),
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
            ], 1, 5),
            Op::inc_ptr(3..4, 1),
        ];

        remove_trailing_pointer_ops(&mut ops, true);

        assert_eq!(ops, vec![
            Op::i_loop(0..3, vec![
                Op::c_add(1..2, -1, 72),
            ], 1, 5),
        ])
    }
}