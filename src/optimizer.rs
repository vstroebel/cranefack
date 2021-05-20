use crate::parser::{Program, Op, OpType};

pub fn optimize(program: &mut Program) -> u32 {
    let mut progress = true;

    let mut count = 0;

    while count < 100 && progress {
        progress = false;
        count += 1;
        progress &= remove_empty_loops(&mut program.ops);
        progress &= optimize_inc_dec(&mut program.ops);
    }

    count
}

// Remove loops that have no children left
// This is common for loop based comments
fn remove_empty_loops(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while i < ops.len() {
        if let OpType::Loop(children) = &mut ops[i].op_type {
            if children.is_empty() {
                ops.remove(i);
                progress = true;
                i -= 1;
            } else {
                progress &= remove_empty_loops(children);
            }
        }
        i += 1;
    }

    progress
}

enum Mode {
    Ignore,
    Remove,
    Replace(OpType),
}

// Merge multiple inc/dec or inc_ptr dec_ptr ops and remove zero versions
fn optimize_inc_dec(ops: &mut Vec<Op>) -> bool {
    let mut i = 0;

    let mut progress = false;

    while i < ops.len() - 1 {
        let op1 = &ops[i];
        let op2 = &ops[i + 1];

        let mode = match (&op1.op_type, &op2.op_type) {
            (OpType::Inc(v1), OpType::Inc(v2)) => Mode::Replace(OpType::Inc(v1.wrapping_add(*v2))),
            (OpType::Dec(v1), OpType::Dec(v2)) => Mode::Replace(OpType::Dec(v1.wrapping_add(*v2))),
            (OpType::Inc(v1), OpType::Dec(v2)) => {
                if v1 == v2 {
                    Mode::Remove
                } else if v1 > v2 {
                    Mode::Replace(OpType::Inc(v1 - v2))
                } else {
                    Mode::Replace(OpType::Dec(v2 - v1))
                }
            }
            (OpType::Dec(v1), OpType::Inc(v2)) => {
                if v1 == v2 {
                    Mode::Remove
                } else if v1 > v2 {
                    Mode::Replace(OpType::Dec(v1 - v2))
                } else {
                    Mode::Replace(OpType::Inc(v2 - v1))
                }
            }
            (OpType::IncPtr(v1), OpType::IncPtr(v2)) => Mode::Replace(OpType::IncPtr(v1.wrapping_add(*v2))),
            (OpType::DecPtr(v1), OpType::DecPtr(v2)) => Mode::Replace(OpType::DecPtr(v1.wrapping_add(*v2))),
            (OpType::IncPtr(v1), OpType::DecPtr(v2)) => {
                if v1 == v2 {
                    Mode::Remove
                } else if v1 > v2 {
                    Mode::Replace(OpType::IncPtr(v1 - v2))
                } else {
                    Mode::Replace(OpType::DecPtr(v2 - v1))
                }
            }
            (OpType::DecPtr(v1), OpType::IncPtr(v2)) => {
                if v1 == v2 {
                    Mode::Remove
                } else if v1 > v2 {
                    Mode::Replace(OpType::DecPtr(v1 - v2))
                } else {
                    Mode::Replace(OpType::IncPtr(v2 - v1))
                }
            }

            _ => Mode::Ignore
        };

        match mode {
            Mode::Remove => {
                ops.remove(i);
                ops.remove(i);
            }
            Mode::Replace(op_type) => {
                let span = op1.span.start..op2.span.end;
                ops[i] = Op {
                    op_type,
                    span,
                };
                ops.remove(i + 1);
            }
            Mode::Ignore => {
                if let OpType::Loop(children) = &mut ops[i].op_type {
                    progress &= optimize_inc_dec(children);
                }
                i += 1;
            }
        }
    }

    progress
}

#[cfg(test)]
mod tests {
    use crate::parser::Op;
    use crate::optimizer::{remove_empty_loops, optimize_inc_dec};

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

        optimize_inc_dec(&mut ops);

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

        optimize_inc_dec(&mut ops);

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

        optimize_inc_dec(&mut ops);

        assert_eq!(ops, vec![
            Op::dec(0..3, 4),
        ])
    }

    #[test]
    fn test_optimize_inc_dec_0() {
        let mut ops = vec![
            Op::inc(0..1, 1),
            Op::dec(1..2, 1),
            Op::inc_ptr(2..3, 3),
        ];

        optimize_inc_dec(&mut ops);

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

        optimize_inc_dec(&mut ops);

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

        optimize_inc_dec(&mut ops);

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

        optimize_inc_dec(&mut ops);

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

        optimize_inc_dec(&mut ops);

        assert_eq!(ops, vec![
            Op::inc(2..3, 3),
        ])
    }
}