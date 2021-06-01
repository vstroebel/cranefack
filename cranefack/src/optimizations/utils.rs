use std::ops::Range;

use crate::ir::ops::{Op, OpType};

#[derive(Debug)]
pub enum Change {
    Ignore,
    Remove,
    RemoveOffset(usize),
    Replace(Vec<OpType>),
    ReplaceOffset(usize, Range<usize>, Vec<OpType>),
}

impl Change {
    pub fn apply(self, ops: &mut Vec<Op>, i: usize, size: usize) -> bool {
        match self {
            Change::Remove => {
                for _ in 0..size {
                    ops.remove(i);
                }

                true
            }
            Change::RemoveOffset(offset) => {
                ops.remove(i + offset);

                true
            }
            Change::Replace(op_types) => {
                let span = ops[i].span.start..ops[i + size - 1].span.end;

                for _ in 0..size {
                    ops.remove(i);
                }

                for op_type in op_types.into_iter().rev() {
                    ops.insert(i, Op {
                        op_type,
                        span: span.clone(),
                    });
                }

                true
            }
            Change::ReplaceOffset(offset, span, op_types) => {
                for _ in offset..size {
                    ops.remove(i + offset);
                }

                for op_type in op_types.into_iter().rev() {
                    ops.insert(i + offset, Op {
                        op_type,
                        span: span.clone(),
                    });
                }

                true
            }
            Change::Ignore => {
                false
            }
        }
    }
}

pub enum CellValue {
    Unknown,
    Value(u8),
}

pub fn count_ops_recursive(ops: &[Op]) -> usize {
    let mut count = ops.len();

    for op in ops {
        if let Some(children) = op.op_type.get_children() {
            count += count_ops_recursive(children);
        }
    }

    count
}
