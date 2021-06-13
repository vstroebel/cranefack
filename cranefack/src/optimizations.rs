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

    /// Limit for partially unrolling loops known to be take at least one time
    pub partially_unroll_loops_limit: usize,

    /// Define wrapping integer overflow as undefined behavior
    ///
    /// Allows some optimization to mark values as non zero if there is a known increment
    pub wrapping_is_ub: bool,

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
            partially_unroll_loops_limit: 0,
            wrapping_is_ub: false,
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
            max_loops: 50,
            complex_loops: true,
            non_local: true,
            unroll_loop_limit: 25,
            partially_unroll_loops_limit: 10,
            ..Self::o1()
        }
    }

    /// Unroll more loops
    pub fn o3() -> OptimizeConfig {
        OptimizeConfig {
            max_loops: 100,
            unroll_loop_limit: 150,
            partially_unroll_loops_limit: 25,
            jit_level: Some("speed_and_size".to_owned()),
            ..Self::o2()
        }
    }

    /// Try to keep size small
    pub fn size() -> OptimizeConfig {
        OptimizeConfig {
            max_loops: 50,
            unroll_loop_limit: 5,
            partially_unroll_loops_limit: 0,
            ..Self::o2()
        }
    }

    /// Turn on all optimization passes on the max
    ///
    /// Only useful for testing...
    pub fn wtf() -> OptimizeConfig {
        OptimizeConfig {
            complex_loops: true,
            non_local: true,
            max_loops: 100000, //usize::MAX,
            jit_level: None,
            unroll_loop_limit: usize::MAX,
            partially_unroll_loops_limit: usize::MAX,
            wrapping_is_ub: false,
            debug: false,
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

        print_debug_iteration(config, count);

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
            update_loop_access(&mut program.ops, config.wrapping_is_ub);

            if optimize_non_local_arithmetics(&mut program.ops, config.wrapping_is_ub) {
                update_loop_access(&mut program.ops, config.wrapping_is_ub);
                progress = true;
            }
            print_debug(program, config, "Optimize non local arithmetics");

            if optimize_non_local_conditional_loops(&mut program.ops, config.wrapping_is_ub) {
                update_loop_access(&mut program.ops, config.wrapping_is_ub);
                progress = true;
            }
            print_debug(program, config, "Optimize non local constant loops");

            if optimize_non_local_static_count_loops(&mut program.ops, config.wrapping_is_ub) {
                update_loop_access(&mut program.ops, config.wrapping_is_ub);
                progress = true;
            }
            print_debug(program, config, "Optimize non local constant loops");

            if optimize_non_local_redundant_copies(&mut program.ops) {
                update_loop_access(&mut program.ops, config.wrapping_is_ub);
                progress = true;
            }
            print_debug(program, config, "Optimize non local redundant copies");

            if optimize_non_local_dead_stores(&mut program.ops) {
                update_loop_access(&mut program.ops, config.wrapping_is_ub);
                progress = true;
            }
            print_debug(program, config, "Optimize non local dead stores");

            if remove_useless_loops(&mut program.ops) {
                update_loop_access(&mut program.ops, config.wrapping_is_ub);
                progress = true;
            }
            print_debug(program, config, "Remove useless loops");

            if remove_true_conditions(&mut program.ops, config.wrapping_is_ub) {
                update_loop_access(&mut program.ops, config.wrapping_is_ub);
                progress = true;
            }
            print_debug(program, config, "Remove true conditions");

            if config.unroll_loop_limit > 0 {
                if unroll_constant_loops(&mut program.ops, config.unroll_loop_limit) {
                    update_loop_access(&mut program.ops, config.wrapping_is_ub);
                    progress = true;
                }
                print_debug(program, config, "Unroll constant loops");

                progress |= unroll_scanning_d_loops(&mut program.ops, config.unroll_loop_limit, config.wrapping_is_ub);
                print_debug(program, config, "Unroll scanning dynamic loops");
            }

            if config.partially_unroll_loops_limit > 0 && (!progress || count > 10) {
                if partially_unroll_loops(&mut program.ops, config.partially_unroll_loops_limit, config.wrapping_is_ub) {
                    non_local_remove_dead_loops(&mut program.ops, config.wrapping_is_ub);
                    update_loop_access(&mut program.ops, config.wrapping_is_ub);
                    progress = true;
                }
                print_debug(program, config, "Partially unroll dynamic loops");
            }

            progress |= non_local_remove_dead_loops(&mut program.ops, config.wrapping_is_ub);
            print_debug(program, config, "Remove non local dead loops");
        } else if config.complex_loops {
            progress |= remove_useless_loops(&mut program.ops);
            print_debug(program, config, "Remove useless loops");
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

fn print_debug_iteration(config: &OptimizeConfig, iteration: usize) {
    if config.debug {
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        writer.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))).unwrap();

        writeln!(writer, "Iteration {}", iteration).unwrap();
        writer.reset().unwrap();
    }
}