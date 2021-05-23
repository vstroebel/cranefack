use crate::parser::Program;

mod passes;

use passes::*;

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
        progress |= optimize_arithmetic_loops(&mut program.ops);
        progress |= optimize_count_loops(&mut program.ops);
        progress |= optimize_static_count_loops(&mut program.ops);
        progress |= optimize_constant_arithmetic_loop(&mut program.ops);
        progress |= optimize_conditional_loops(&mut program.ops);
        progress |= optimize_search_zero(&mut program.ops);
        progress |= optimize_constant_arithmetics(&mut program.ops);
    }

    count
}