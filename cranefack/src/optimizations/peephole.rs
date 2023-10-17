
use crate::ir::ops::Op;
use crate::optimizations::utils::Change;

pub fn run_peephole_pass<F, const WINDOW: usize>(ops: &mut Vec<Op>, func: F) -> bool
where
    F: Fn([&Op; WINDOW]) -> Change + Copy,
{
    let mut i = 0;

    let mut progress = false;

    while ops.len() >= WINDOW && i < ops.len() - (WINDOW - 1) {

        let window = core::array::from_fn(|index| &ops[i + index]);

        let change = func(window);

        let (changed, removed) = change.apply(ops, i, WINDOW);
        i -= removed;
        if changed {
            progress = true;
        } else {
            i += 1;
        }
    }

    for op in ops {
        if let Some(children) = op.op_type.get_children_mut() {
            progress |= run_peephole_pass(children, func);
        }
    }

    progress
}
