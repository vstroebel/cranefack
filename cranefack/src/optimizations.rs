use passes::*;

use crate::ir::ops::{Op, OpType};
use crate::optimizations::peephole::run_peephole_pass;
use crate::parser::Program;
use codespan_reporting::term::termcolor::{ColorChoice, ColorSpec, StandardStream, WriteColor, Color};
use std::io::Write;

mod passes;
mod peephole;
mod utils;

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

    /// Limit for loop unrolling pass
    pub unroll_loop_limit: usize,

    /// Print statistics after each pass
    pub debug: bool,
}

impl OptimizeConfig {
    /// Don't do any optimization
    pub fn o0() -> OptimizeConfig {
        OptimizeConfig {
            complex_loops: false,
            non_local: false,
            max_loops: 0,
            jit_level: None,
            unroll_loop_limit: 0,
            debug: false,
        }
    }

    /// Only simple optimizations
    pub fn o1() -> OptimizeConfig {
        OptimizeConfig {
            max_loops: 10,
            ..Self::o0()
        }
    }

    /// Complex loop and non local optimizations
    pub fn o2() -> OptimizeConfig {
        OptimizeConfig {
            max_loops: 30,
            complex_loops: true,
            non_local: true,
            unroll_loop_limit: 25,
            ..Self::o1()
        }
    }

    /// Unroll more loops
    pub fn o3() -> OptimizeConfig {
        OptimizeConfig {
            max_loops: 50,
            unroll_loop_limit: 150,
            jit_level: Some("speed_and_size".to_owned()),
            ..Self::o2()
        }
    }

    /// Try to keep size small
    pub fn size() -> OptimizeConfig {
        OptimizeConfig {
            max_loops: 50,
            unroll_loop_limit: 5,
            ..Self::o2()
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
        print_debug(program, config, "Remove dead loops");

        progress |= optimize_zero_loops(&mut program.ops);
        print_debug(program, config, "Optimize zero loops");

        progress |= run_peephole_pass(&mut program.ops, optimize_arithmetics);
        print_debug(program, config, "Optimize arithmetics");

        progress |= remove_dead_stores_before_set(&mut program.ops);
        print_debug(program, config, "Remove dead stores");

        if config.complex_loops {
            progress |= optimize_local_loops(&mut program.ops);
            print_debug(program, config, "Detect local loops");

            progress |= optimize_count_loops(&mut program.ops);
            print_debug(program, config, "Detect iterating loops");

            progress |= run_peephole_pass(&mut program.ops, optimize_arithmetic_loops);
            print_debug(program, config, "Optimize arithmetic loops");

            progress |= optimize_static_count_loops(&mut program.ops);
            print_debug(program, config, "Detect constant loops");

            progress |= optimize_constant_arithmetic_loop(&mut program.ops);
            print_debug(program, config, "Optimize constant arithmetic loops");

            progress |= optimize_conditional_loops(&mut program.ops);
            print_debug(program, config, "Detect conditional loops");

            progress |= optimize_search_zero(&mut program.ops);
            print_debug(program, config, "Detect scanning loops");
        }

        progress |= run_peephole_pass(&mut program.ops, optimize_constant_arithmetics);
        print_debug(program, config, "Optimize constant arithmetics");

        progress |= optimize_offsets(&mut program.ops, 0);
        print_debug(program, config, "Optimize offsets");

        progress |= run_peephole_pass(&mut program.ops, optimize_arithmetic_offsets);
        print_debug(program, config, "Optimize arithmetic offsets");

        progress |= remove_trailing_pointer_ops(&mut program.ops, true);
        print_debug(program, config, "Remove trailing pointer ops");

        progress |= run_peephole_pass(&mut program.ops, remove_useless_copy);
        print_debug(program, config, "Remove useless copy");

        if config.non_local {
            progress |= optimize_non_local_arithmetics(&mut program.ops);
            print_debug(program, config, "Optimize non local arithmetics");

            progress |= optimize_non_local_static_count_loops(&mut program.ops);
            print_debug(program, config, "Optimize non local constant loops");

            progress |= optimize_non_local_dead_stores(&mut program.ops);
            print_debug(program, config, "Optimize non local dead stores");

            if config.unroll_loop_limit > 0 {
                progress |= unroll_constant_loops(&mut program.ops, config.unroll_loop_limit);
                print_debug(program, config, "Unroll constant loops");
            }

            if progress {
                update_loop_access(&mut program.ops);
                print_debug(program, config, "Updating loop access");
            }
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

fn print_debug(program: &Program, config: &OptimizeConfig, pass: &str) {
    if config.debug {
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        writer.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))).unwrap();

        let (op_count, dloop_count, lloop_count, iloop_count, cloop_count, if_count) = program.get_statistics();

        writeln!(writer, "Pass {} with {} instructions ({},{},{},{}) loops and {} ifs",
                 pass,
                 op_count,
                 dloop_count,
                 lloop_count,
                 iloop_count,
                 cloop_count,
                 if_count,
        ).unwrap();
        writer.reset().unwrap();
    }
}