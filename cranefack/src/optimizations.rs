use crate::parser::{Program, Op, OpType};

mod passes;
mod peephole;
mod utils;

use passes::*;
use crate::optimizations::peephole::run_peephole_pass;

/// Configuration to control optimization of the program
pub struct OptimizeConfig {
    /// Maximum of optimization loops. Use zero to disable optimization
    pub max_loops: usize,

    /// Optimize complex loops
    pub complex_loops: bool,

    /// Run non local optimizations passes
    pub non_local: bool,

    /// Optimization level passed to cranelift
    ///
    /// Currently this supports none, speed and speed_and_size.<br>
    /// By default speed is used for all levels expect 0
    pub jit_level: Option<String>,
}

impl OptimizeConfig {
    /// Don't do any optimization
    pub fn o0() -> OptimizeConfig {
        OptimizeConfig {
            complex_loops: false,
            non_local: false,
            max_loops: 0,
            jit_level: None,
        }
    }

    /// Only simple optimizations
    pub fn o1() -> OptimizeConfig {
        OptimizeConfig {
            max_loops: 10,
            ..Self::o0()
        }
    }

    /// Optimize complex loops too
    pub fn o2() -> OptimizeConfig {
        OptimizeConfig {
            max_loops: 30,
            complex_loops: true,
            non_local: true,
            ..Self::o1()
        }
    }

    /// Check if configuration has an optimization enabled
    pub fn optimize(&self) -> bool {
        self.max_loops > 0
    }
}

/// Optimize program with level 2
pub fn optimize(program: &mut Program) -> usize {
    optimize_with_config(program, &OptimizeConfig::o2())
}

/// Optimize program
pub fn optimize_with_config(program: &mut Program, config: &OptimizeConfig) -> usize {
    program.ops.insert(0, Op::start());

    let mut progress = true;

    let mut count = 0;

    while count < config.max_loops && progress {
        progress = false;
        count += 1;

        progress |= remove_dead_loops(&mut program.ops);
        progress |= optimize_zero_loops(&mut program.ops);
        progress |= run_peephole_pass(&mut program.ops, optimize_arithmetics);
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

        if config.non_local {
            progress |= optimize_non_local_arithmetics(&mut program.ops);
            progress |= optimize_non_local_dead_stores(&mut program.ops);
        }
    }

    match program.ops.remove(0).op_type {
        OpType::Start => {
            // Ignore
        }
        // Start marker MUST NOT be removed by any optimization pass
        other => panic!("Start marker was removed: {:?}", other),
    }

    count
}