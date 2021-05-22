use crate::parser::{Program, Op, OpType};
use std::cmp::Ordering;

pub fn optimize(program: &mut Program) -> u32 {
    let mut progress = true;

    let mut count = 0;

    while count < 100 && progress {
        progress = false;
        count += 1;

        // No progress tracking needed
        remove_preceding_loop(&mut program.ops);

        progress |= optimize_heap_initialization(&mut program.ops);
        progress |= remove_empty_loops(&mut program.ops);
        progress |= optimize_zero_loops(&mut program.ops);
        progress |= optimize_inc_dec(&mut program.ops, 0);
        progress |= remove_dead_stores_before_set(&mut program.ops);
        progress |= optimize_arithmethic_loops(&mut program.ops);
        progress |= optimize_count_loops(&mut program.ops);
        progress |= optimize_static_count_loops(&mut program.ops);
        progress |= optimize_constant_arithmetic_loop(&mut program.ops);
        progress |= optimize_conditional_loops(&mut program.ops);
        progress |= optimize_search_zero(&mut program.ops);
        progress |= optimize_constant_arithmetics(&mut program.ops);
    }

    count
}

// Loops at the beginning of a program will not be taken at all
fn remove_preceding_loop(ops: &mut Vec<Op>) {
    while !ops.is_empty() {
        if let OpType::DLoop(_) = ops[0].op_type {
            ops.remove(0);
        } else {
            break;
        }
    }
}

// Remove loops that have no children left
// This is common for loop based comments
fn remove_empty_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        if let Some(children) = ops[i].op_type.get_children_mut() {
            if children.is_empty() {
                ops.remove(i);
                progress = true;
            } else {
                progress |= remove_empty_loops(children);
                i += 1;
            }
        } else {
            i += 1;
        }
    }

    progress
}

// Replace '[-]' that decreases the current point to 0 with set
fn optimize_zero_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        let op = &mut ops[i];
        if let Some(children) = op.op_type.get_children_mut() {
            let mut optimized = false;
            if children.len() == 1 {
                if let OpType::Dec(1) = children[0].op_type {
                    optimized = true;
                }
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

fn optimize_arithmethic_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        let op = &mut ops[i];
        if let Some(children) = op.op_type.get_children_mut() {
            let mut optimized = None;
            if children.len() == 4 {
                match (&children[0].op_type, &children[1].op_type, &children[2].op_type, &children[3].op_type) {
                    (OpType::Dec(1), OpType::IncPtr(offset), OpType::Inc(multi), OpType::DecPtr(offset2)) => {
                        if offset == offset2 {
                            optimized = Some(OpType::Add(*offset as isize, *multi));
                        }
                    }
                    (OpType::Dec(1), OpType::DecPtr(offset), OpType::Inc(multi), OpType::IncPtr(offset2)) => {
                        if offset == offset2 {
                            optimized = Some(OpType::Add(-(*offset as isize), *multi));
                        }
                    }
                    (OpType::IncPtr(offset), OpType::Inc(multi), OpType::DecPtr(offset2), OpType::Dec(1)) => {
                        if offset == offset2 {
                            optimized = Some(OpType::Add(*offset as isize, *multi));
                        }
                    }
                    (OpType::DecPtr(offset), OpType::Inc(multi), OpType::IncPtr(offset2), OpType::Dec(1)) => {
                        if offset == offset2 {
                            optimized = Some(OpType::Add(-(*offset as isize), *multi));
                        }
                    }
                    (OpType::Dec(1), OpType::IncPtr(offset), OpType::Dec(multi), OpType::DecPtr(offset2)) => {
                        if offset == offset2 {
                            optimized = Some(OpType::Sub(*offset as isize, *multi));
                        }
                    }
                    (OpType::Dec(1), OpType::DecPtr(offset), OpType::Dec(multi), OpType::IncPtr(offset2)) => {
                        if offset == offset2 {
                            optimized = Some(OpType::Sub(-(*offset as isize), *multi));
                        }
                    }
                    (OpType::IncPtr(offset), OpType::Dec(multi), OpType::DecPtr(offset2), OpType::Dec(1)) => {
                        if offset == offset2 {
                            optimized = Some(OpType::Sub(*offset as isize, *multi));
                        }
                    }
                    (OpType::DecPtr(offset), OpType::Dec(multi), OpType::IncPtr(offset2), OpType::Dec(1)) => {
                        if offset == offset2 {
                            optimized = Some(OpType::Sub(-(*offset as isize), *multi));
                        }
                    }
                    _ => {}
                }
            }

            if let Some(new_op) = optimized {
                ops[i] = Op {
                    span: op.span.start..children[3].span.end,
                    op_type: new_op,
                };
                progress = true;
            } else {
                progress |= optimize_arithmethic_loops(children);
            }
        }

        i += 1;
    }

    progress
}

// Optimize loops that are known to use the same counting variable
fn optimize_count_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && !ops.is_empty() && i < ops.len() {
        let replace = if let OpType::DLoop(children) = &mut ops[i].op_type {
            let mut ptr_offset = 0_isize;
            let mut ignore = false;
            let mut possible_match = false;
            let mut counter_decrements = vec![];

            let num_ops = children.len();
            if num_ops >= 3 {
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
                        OpType::Add(offset, _) | OpType::Sub(offset, _) => {
                            if ptr_offset + offset == 0 {
                                ignore = true;
                                break;
                            }
                        }
                        OpType::Inc(_) | OpType::Set(_) | OpType::GetChar => {
                            if ptr_offset == 0 {
                                ignore = true;
                                break;
                            }
                        }
                        OpType::Dec(v) => {
                            if ptr_offset == 0 {
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
                        _ => {
                            // Ignore
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

                let offset = if let Some(offset) = get_ptr_offset(&children[0].op_type) {
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
            OpType::Add(offset, _) | OpType::Sub(offset, _) => {
                for parent_offset in parent_offsets {
                    if ptr_offset + offset == *parent_offset {
                        return false;
                    }
                }
            }
            OpType::CAdd(..) | OpType::CSub(..) => {
                // TODO:  Check if this changes the counter
                return false;
            }
            OpType::Inc(_) | OpType::Dec(_) | OpType::Set(_) | OpType::GetChar => {
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

fn get_ptr_offset(op_type: &OpType) -> Option<isize> {
    match op_type {
        OpType::IncPtr(count) => Some(*count as isize),
        OpType::DecPtr(count) => Some(-(*count as isize)),
        _ => None,
    }
}

#[derive(Debug)]
enum Mode {
    Ignore,
    Remove,
    Replace(OpType),
}

// Merge multiple inc/dec or inc_ptr dec_ptr ops and remove zero versions
fn optimize_inc_dec(ops: &mut Vec<Op>, depth: usize) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() - 1 {
        let op1 = &ops[i];
        let op2 = &ops[i + 1];

        let mode = match (&op1.op_type, &op2.op_type) {
            (OpType::Inc(v1), OpType::Inc(v2)) => Mode::Replace(OpType::Inc(v1.wrapping_add(*v2))),
            (OpType::Dec(v1), OpType::Dec(v2)) => Mode::Replace(OpType::Dec(v1.wrapping_add(*v2))),
            (OpType::Inc(v1), OpType::Dec(v2)) => {
                match v1.cmp(v2) {
                    Ordering::Equal => Mode::Remove,
                    Ordering::Greater => Mode::Replace(OpType::Inc(v1 - v2)),
                    Ordering::Less => Mode::Replace(OpType::Dec(v2 - v1)),
                }
            }
            (OpType::Dec(v1), OpType::Inc(v2)) => {
                match v1.cmp(v2) {
                    Ordering::Equal => Mode::Remove,
                    Ordering::Greater => Mode::Replace(OpType::Dec(v1 - v2)),
                    Ordering::Less => Mode::Replace(OpType::Inc(v2 - v1)),
                }
            }
            (OpType::IncPtr(v1), OpType::IncPtr(v2)) => Mode::Replace(OpType::IncPtr(v1.wrapping_add(*v2))),
            (OpType::DecPtr(v1), OpType::DecPtr(v2)) => Mode::Replace(OpType::DecPtr(v1.wrapping_add(*v2))),
            (OpType::IncPtr(v1), OpType::DecPtr(v2)) => {
                match v1.cmp(v2) {
                    Ordering::Equal => Mode::Remove,
                    Ordering::Greater => Mode::Replace(OpType::IncPtr(v1 - v2)),
                    Ordering::Less => Mode::Replace(OpType::DecPtr(v2 - v1)),
                }
            }
            (OpType::DecPtr(v1), OpType::IncPtr(v2)) => {
                match v1.cmp(v2) {
                    Ordering::Equal => Mode::Remove,
                    Ordering::Greater => Mode::Replace(OpType::DecPtr(v1 - v2)),
                    Ordering::Less => Mode::Replace(OpType::IncPtr(v2 - v1)),
                }
            }
            (OpType::Set(v1), OpType::Inc(v2)) => Mode::Replace(OpType::Set(v1.wrapping_add(*v2))),
            (OpType::Set(v1), OpType::Dec(v2)) => Mode::Replace(OpType::Set(v1.wrapping_sub(*v2))),
            _ => Mode::Ignore
        };

        match mode {
            Mode::Remove => {
                ops.remove(i);
                ops.remove(i);
                progress = true;
            }
            Mode::Replace(op_type) => {
                let span = op1.span.start..op2.span.end;
                ops[i] = Op {
                    op_type,
                    span,
                };
                ops.remove(i + 1);
                progress = true;
            }
            Mode::Ignore => {
                if let Some(children) = ops[i].op_type.get_children_mut() {
                    progress |= optimize_inc_dec(children, depth + 1);
                }
                i += 1;
            }
        }
    }

    if let Some(last) = ops.last_mut() {
        if let Some(children) = last.op_type.get_children_mut() {
            progress |= optimize_inc_dec(children, depth + 1);
        }
    }

    progress
}

fn remove_dead_stores_before_set(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() - 1 {
        let op1 = &ops[i];
        let op2 = &ops[i + 1];

        #[allow(clippy::match_like_matches_macro)]
            let remove = match (&op1.op_type, &op2.op_type) {
            (OpType::Inc(_), OpType::Set(_)) |
            (OpType::Dec(_), OpType::Set(_)) |
            (OpType::Set(_), OpType::Set(_)) |
            (OpType::Inc(_), OpType::GetChar) |
            (OpType::Dec(_), OpType::GetChar) |
            (OpType::Set(_), OpType::GetChar) => true,
            _ => false,
        };

        if remove {
            ops.remove(i);
        } else {
            i += 1;
        }
    }

    if let Some(last) = ops.last_mut() {
        if let Some(children) = last.op_type.get_children_mut() {
            progress |= remove_dead_stores_before_set(children);
        }
    }

    progress
}

fn optimize_static_count_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() - 1 {
        let op1 = &ops[i];
        let op2 = &ops[i + 1];

        let count = match (&op1.op_type, &op2.op_type) {
            (OpType::Set(v), OpType::ILoop(_, _, step)) =>
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
fn optimize_conditional_loops(ops: &mut Vec<Op>) -> bool {
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
            OpType::Set(_) |
            OpType::IncPtr(_) |
            OpType::DecPtr(_) => {
                // allowed
            }
            _ => return false,
        }
    }

    true
}

fn optimize_search_zero(ops: &mut Vec<Op>) -> bool {
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

fn optimize_constant_arithmetic_loop(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        let op = &mut ops[i];

        let replace = if let OpType::CLoop(children, ..) = &op.op_type {
            contains_only_arithmetics(children)
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
                            OpType::Inc(v) |
                            OpType::Dec(v) => *v = *v * iterations,
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

fn contains_only_arithmetics(ops: &[Op]) -> bool {
    for op in ops {
        match &op.op_type {
            OpType::Set(_) |
            OpType::Inc(_) |
            OpType::Dec(_) |
            OpType::IncPtr(_) |
            OpType::DecPtr(_) => {
                // allowed
            }
            _ => return false,
        }
    }

    true
}

fn optimize_heap_initialization(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    let mut heap: Vec<Option<usize>> = vec![];

    let mut ptr = 0;

    while !ops.is_empty() && i < ops.len() {
        let replace = {
            let mut op = &mut ops[i];

            if let Some(offset) = get_ptr_offset(&op.op_type) {
                ptr += offset;
                None
            } else if let OpType::Inc(value) = &op.op_type {
                if ptr < 0 {
                    break;
                }

                while ptr > heap.len() as isize - 1 {
                    heap.push(None);
                }

                progress = true;

                if let Some(index) = &heap[ptr as usize] {
                    Some((index, *value as i16))
                } else {
                    op.op_type = OpType::Set(*value);
                    heap[ptr as usize] = Some(i);
                    None
                }
            } else if let OpType::Dec(value) = &op.op_type {
                if ptr < 0 {
                    break;
                }

                while ptr > heap.len() as isize - 1 {
                    heap.push(None);
                }

                progress = true;

                if let Some(index) = &heap[ptr as usize] {
                    Some((index, -(*value as i16)))
                } else {
                    op.op_type = OpType::Set(0u8.wrapping_sub(*value));
                    heap[ptr as usize] = Some(i);
                    None
                }
            } else {
                break;
            }
        };

        if let Some((&index, value)) = replace {
            if let OpType::Set(set_v) = &mut ops[index].op_type {
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

fn optimize_constant_arithmetics(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() - 1 {
        let op1 = &ops[i];
        let op2 = &ops[i + 1];

        let mode = match (&op1.op_type, &op2.op_type) {
            (OpType::Set(value), OpType::Add(offset, multi)) => {
                Mode::Replace(OpType::CAdd(*offset, value * multi))
            }
            (OpType::Set(value), OpType::Sub(offset, multi)) => {
                Mode::Replace(OpType::CSub(*offset, value * multi))
            }
            _ => Mode::Ignore
        };

        match mode {
            Mode::Remove => {
                ops.remove(i);
                ops.remove(i);
                progress = true;
            }
            Mode::Replace(op_type) => {
                let span = op1.span.start..op2.span.end;
                ops[i] = Op {
                    op_type,
                    span,
                };
                ops.remove(i + 1);
                progress = true;
            }
            Mode::Ignore => {
                if let Some(children) = ops[i].op_type.get_children_mut() {
                    progress |= optimize_constant_arithmetics(children);
                }
                i += 1;
            }
        }
    }

    if let Some(last) = ops.last_mut() {
        if let Some(children) = last.op_type.get_children_mut() {
            progress |= optimize_constant_arithmetics(children);
        }
    }

    progress
}

#[cfg(test)]
mod tests {
    use crate::parser::Op;
    use crate::optimizer::*;

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
    fn test_remove_empty_loops() {
        let mut ops = vec![
            Op::inc(0..0, 1),
            Op::d_loop(0..0, vec![]),
            Op::d_loop(0..0, vec![Op::inc(0..0, 1),
            ]),
        ];

        remove_empty_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::inc(0..0, 1),
            Op::d_loop(0..0, vec![Op::inc(0..0, 1),
            ]),
        ])
    }

    #[test]
    fn test_optimize_inc() {
        let mut ops = vec![
            Op::inc(0..1, 1),
            Op::inc(1..2, 2),
        ];

        optimize_inc_dec(&mut ops, 1);

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

        optimize_inc_dec(&mut ops, 1);

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

        optimize_inc_dec(&mut ops, 1);

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

        optimize_inc_dec(&mut ops, 1);

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

        optimize_inc_dec(&mut ops, 1);

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

        optimize_inc_dec(&mut ops, 1);

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

        optimize_inc_dec(&mut ops, 1);

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

        optimize_inc_dec(&mut ops, 1);

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

        optimize_inc_dec(&mut ops, 1);

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

        optimize_inc_dec(&mut ops, 1);

        assert_eq!(ops, vec![
            Op::inc(2..3, 3),
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
    fn test_optimize_add() {
        let mut ops = vec![
            Op::d_loop(0..1, vec![
                Op::inc_ptr(1..2, 1),
                Op::inc(2..3, 1),
                Op::dec_ptr(3..4, 1),
                Op::dec(4..5, 1),
            ])
        ];

        optimize_arithmethic_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::add(0..5, 1, 1),
        ])
    }

    #[test]
    fn test_optimize_add_inv() {
        let mut ops = vec![
            Op::d_loop(0..1, vec![
                Op::dec(1..2, 1),
                Op::inc_ptr(2..3, 2),
                Op::inc(3..4, 1),
                Op::dec_ptr(4..5, 2),
            ])
        ];

        optimize_arithmethic_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::add(0..5, 2, 1),
        ])
    }

    #[test]
    fn test_optimize_sub() {
        let mut ops = vec![
            Op::d_loop(0..1, vec![
                Op::inc_ptr(1..2, 1),
                Op::dec(2..3, 2),
                Op::dec_ptr(3..4, 1),
                Op::dec(4..5, 1),
            ])
        ];

        optimize_arithmethic_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::sub(0..5, 1, 2),
        ])
    }


    #[test]
    fn test_optimize_sub2() {
        let mut ops = vec![
            Op::d_loop(0..1, vec![
                Op::dec(1..2, 1),
                Op::inc_ptr(2..3, 1),
                Op::dec(3..4, 2),
                Op::dec_ptr(4..5, 1),
            ])
        ];

        optimize_arithmethic_loops(&mut ops);

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

        optimize_constant_arithmetics(&mut ops);

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

        optimize_constant_arithmetics(&mut ops);

        assert_eq!(ops, vec![
            Op::c_sub(0..2, 1, 6),
        ])
    }
}