use crate::parser::{Program, Op, OpType};

pub fn optimize(program: &mut Program) -> u32 {
    let mut progress = true;

    let mut count = 0;

    while count < 100 && progress {
        progress = false;
        count += 1;
        progress &= remove_empty_loops(&mut program.ops);
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

#[cfg(test)]
mod tests {
    use crate::parser::Op;
    use crate::optimizer::remove_empty_loops;

    #[test]
    fn test_remove_empty_loops() {
        let mut ops = vec![
            Op::inc(0..0),
            Op::loop_ops(0..0, vec![]),
            Op::loop_ops(0..0, vec![Op::inc(0..0),
            ]),
        ];

        remove_empty_loops(&mut ops);

        assert_eq!(ops, vec![
            Op::inc(0..0),
            Op::loop_ops(0..0, vec![Op::inc(0..0),
            ]),
        ])
    }
}