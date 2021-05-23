use crate::parser::{OpType, Op};

use std::mem::MaybeUninit;
use std::ops::Range;

#[derive(Debug)]
pub enum Change {
    Ignore,
    Remove,
    Replace(Vec<OpType>),
    ReplaceOffset(usize, Range<usize>, Vec<OpType>),
}

pub fn run_peephole_pass<F, const WINDOW: usize>(ops: &mut Vec<Op>, func: F) -> bool
    where F: Fn([&Op; WINDOW]) -> Change + Copy
{
    let mut i = 0;

    let mut progress = false;

    while !ops.is_empty() && i < ops.len() - (WINDOW - 1) {
        let window = unsafe {
            // Unsafe hackery to initialize window array with parameterized length
            let window: MaybeUninit<[&Op; WINDOW]> = MaybeUninit::uninit().assume_init();

            let mut window = window.assume_init();

            for index in 0..WINDOW {
                window[index] = &ops[i + index];
            }

            window
        };

        let change = func(window);

        match change {
            Change::Remove => {
                for _ in 0..WINDOW {
                    ops.remove(i);
                }
                progress = true;
            }
            Change::Replace(op_types) => {
                let span = window[0].span.start..window[WINDOW - 1].span.end;

                for _ in 0..WINDOW {
                    ops.remove(i);
                }

                for op_type in op_types.into_iter().rev() {
                    ops.insert(i, Op {
                        op_type,
                        span: span.clone(),
                    });
                }

                progress = true;
            }
            Change::ReplaceOffset(offset, span, op_types) => {

                for _ in offset..WINDOW {
                    ops.remove(i + offset);
                }

                for op_type in op_types.into_iter().rev() {
                    ops.insert(i + offset, Op {
                        op_type,
                        span: span.clone(),
                    });
                }

                progress = true;
            }
            Change::Ignore => {
                if let Some(children) = ops[i].op_type.get_children_mut() {
                    progress |= run_peephole_pass(children, func);
                }
                i += 1;
            }
        }
    }

    if let Some(last) = ops.last_mut() {
        if let Some(children) = last.op_type.get_children_mut() {
            progress |= run_peephole_pass(children, func);
        }
    }

    progress
}