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

        // No progress tracking needed
        optimize_first_incs(&mut program.ops);

        progress |= remove_empty_loops(&mut program.ops);
        progress |= optimize_zero_loops(&mut program.ops);
        progress |= optimize_inc_dec(&mut program.ops, 0);
        progress |= optimize_arithmethic_loops(&mut program.ops);
        progress |= optimize_count_loops(&mut program.ops);
    }

    count
}

// Loops at the beginning of a program will not be taken at all
fn remove_preceding_loop(ops: &mut Vec<Op>) {
    while !ops.is_empty() {
        if let OpType::Loop(_, ..) = ops[0].op_type {
            ops.remove(0);
        } else {
            break;
        }
    }
}

// Prepare for better optimization of loops with known size
fn optimize_first_incs(ops: &mut Vec<Op>) {
    while !ops.is_empty() {
        if let OpType::Inc(value) = &ops[0].op_type {
            ops[0].op_type = OpType::Set(*value);
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
        if let OpType::Loop(children, ..) = &mut ops[i].op_type {
            if children.is_empty() {
                ops.remove(i);
                progress = true;
                i -= 1;
            } else {
                progress |= remove_empty_loops(children);
            }
        }
        i += 1;
    }

    progress
}

// Replace '[-]' that decreases the current point to 0 with set
fn optimize_zero_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() {
        let op = &mut ops[i];
        if let OpType::Loop(children, ..) = &mut op.op_type {
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
        if let OpType::Loop(children, ..) = &mut op.op_type {
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
        if let OpType::Loop(children, steps) = &mut ops[i].op_type {
            let mut ptr_offset = 0_isize;
            let mut ignore = false;

            let num_ops = children.len();
            if num_ops >= 3 {
                for (i, op) in children.iter().enumerate() {
                    match &op.op_type {
                        OpType::IncPtr(value) => ptr_offset += *value as isize,
                        OpType::DecPtr(value) => ptr_offset -= *value as isize,
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
                        OpType::Dec(_) => {
                            if ptr_offset == 0 && i > 0 && i < num_ops - 1 {
                                ignore = true;
                                break;
                            }
                        }
                        OpType::Loop(..) => {
                            ignore = true;
                            break;
                        }
                        _ => {
                            // Ignore
                        }
                    }
                }
            }

            if !ignore && ptr_offset == 0 {
                if let Some(v) = get_dec_count(&children[0].op_type) {
                    children.remove(0);

                    if children[children.len() - 1].op_type.is_ptr_inc_or_dec() {
                        children.remove(children.len() - 1);
                    }

                    let offset = if let Some(offset) = get_ptr_offset(&children[0].op_type) {
                        children.remove(0);
                        offset
                    } else {
                        0
                    };

                    *steps = Some((offset, v));

                    progress = true;
                } else if let Some(v) = get_dec_count(&children[children.len() - 1].op_type) {
                    children.remove(children.len() - 1);

                    if children[children.len() - 1].op_type.is_ptr_inc_or_dec() {
                        children.remove(children.len() - 1);
                    }

                    let offset = if let Some(offset) = get_ptr_offset(&children[0].op_type) {
                        children.remove(0);
                        offset
                    } else {
                        0
                    };

                    *steps = Some((offset, v));

                    progress = true;
                }
            }

            progress |= optimize_count_loops(children);
        }
        i += 1;
    }

    progress
}

fn get_dec_count(op_type: &OpType) -> Option<u8> {
    match op_type {
        OpType::Dec(count) => Some(*count),
        _ => None,
    }
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
                if let OpType::Loop(children, ..) = &mut ops[i].op_type {
                    progress |= optimize_inc_dec(children, depth + 1);
                }
                i += 1;
            }
        }
    }

    if let Some(last) = ops.last_mut() {
        if let OpType::Loop(children, ..) = &mut last.op_type {
            progress |= optimize_inc_dec(children, depth + 1);
        }
    }

    progress
}

#[cfg(test)]
mod tests {
    use crate::parser::Op;
    use crate::optimizer::{remove_empty_loops, optimize_inc_dec, remove_preceding_loop, optimize_zero_loops, optimize_arithmethic_loops, optimize_first_incs, optimize_count_loops};

    #[test]
    fn test_remove_preceding_loop() {
        let mut ops = vec![
            Op::loop_ops(0..1, vec![]),
            Op::loop_ops(1..2, vec![]),
            Op::inc(2..3, 1),
            Op::loop_ops(3..4, vec![Op::inc(4..5, 1),
            ]),
        ];

        remove_preceding_loop(&mut ops);

        assert_eq!(ops, vec![
            Op::inc(2..3, 1),
            Op::loop_ops(3..4, vec![Op::inc(4..5, 1),
            ]),
        ])
    }

    #[test]
    fn test_optimize_first_incs() {
        let mut ops = vec![
            Op::inc(0..1, 5),
        ];

        optimize_first_incs(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..1, 5),
        ])
    }

    #[test]
    fn test_remove_empty_loops() {
        let mut ops = vec![
            Op::inc(0..0, 1),
            Op::loop_ops(0..0, vec![]),
            Op::loop_ops(0..0, vec![Op::inc(0..0, 1),
            ]),
        ];

        remove_empty_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::inc(0..0, 1),
            Op::loop_ops(0..0, vec![Op::inc(0..0, 1),
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
            Op::loop_ops(0..1, vec![Op::dec(1..2, 1)]),
        ];

        optimize_zero_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..2, 0),
        ])
    }

    #[test]
    fn test_optimize_add() {
        let mut ops = vec![
            Op::loop_ops(0..1, vec![
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
            Op::loop_ops(0..1, vec![
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
            Op::loop_ops(0..1, vec![
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
            Op::loop_ops(0..1, vec![
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
            Op::loop_ops(0..1, vec![
                Op::dec(1..2, 1),
                Op::inc_ptr(2..3, 1),
                Op::dec(3..4, 2),
                Op::dec_ptr(4..5, 1),
            ])
        ];

        optimize_count_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..1, 6),
            Op::loop_ops_with_steps(0..1, vec![
                Op::dec(3..4, 2),
            ], 1, 1)
        ])
    }

    #[test]
    fn test_optimize_count_loops_inv() {
        let mut ops = vec![
            Op::set(0..1, 6),
            Op::loop_ops(0..1, vec![
                Op::dec_ptr(1..2, 1),
                Op::dec(2..3, 3),
                Op::inc_ptr(3..4, 1),
                Op::dec(4..5, 2),
            ])
        ];

        optimize_count_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::set(0..1, 6),
            Op::loop_ops_with_steps(0..1, vec![
                Op::dec(2..3, 3),
            ], 2, -1)
        ])
    }
}