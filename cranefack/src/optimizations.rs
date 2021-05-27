use crate::parser::Program;

mod passes;
mod peephole;

use passes::*;
use crate::optimizations::peephole::run_peephole_pass;

pub struct OptimizeConfig {
    complex_loops: bool,
    max_loops: usize,
}

impl OptimizeConfig {
    pub fn o1() -> OptimizeConfig {
        OptimizeConfig {
            complex_loops: false,
            max_loops: 25,
        }
    }

    pub fn o2() -> OptimizeConfig {
        OptimizeConfig {
            complex_loops: true,
            ..Self::o1()
        }
    }
}

pub fn optimize(program: &mut Program) -> usize {
    optimize_with_config(program, &OptimizeConfig::o2())
}

pub fn optimize_with_config(program: &mut Program, config: &OptimizeConfig) -> usize {
    let mut progress = true;

    let mut count = 0;

    while count < config.max_loops && progress {
        progress = false;
        count += 1;

        // No progress tracking needed
        remove_preceding_loop(&mut program.ops);

        progress |= optimize_heap_initialization(&mut program.ops);
        progress |= optimize_zero_loops(&mut program.ops);
        progress |= run_peephole_pass(&mut program.ops, optimize_inc_dec);
        progress |= remove_dead_stores_before_set(&mut program.ops);

        if config.complex_loops {
            progress |= optimize_local_loops(&mut program.ops);
            progress |= optimize_count_loops(&mut program.ops);
            progress |= run_peephole_pass(&mut program.ops, optimize_arithmetic_loops);
            progress |= optimize_static_count_loops(&mut program.ops);
            progress |= optimize_constant_arithmetic_loop(&mut program.ops);
            progress |= optimize_conditional_loops(&mut program.ops);
            progress |= optimize_search_zero(&mut program.ops);
        }

        progress |= run_peephole_pass(&mut program.ops, optimize_constant_arithmetics);
        progress |= optimize_offsets(&mut program.ops, 0);
        progress |= run_peephole_pass(&mut program.ops, optimize_arithmetic_offsets);
        progress |= remove_trailing_pointer_ops(&mut program.ops, true);
        progress |= run_peephole_pass(&mut program.ops, remove_useless_copy);
    }

    count
}