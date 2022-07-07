use clap::{crate_description, crate_name, crate_version, App, Arg, ArgMatches, SubCommand};
use std::error::Error;

mod benchmark;
mod compile;
mod errors;
mod run;
mod utils;

use crate::benchmark::benchmark_file;
use crate::compile::compile_file;
use crate::run::run_file;
use crate::utils::get_optimize_config_from_args;
use std::process::exit;

fn main() {
    let matches = create_clap_app().get_matches();

    if let Err(error) = match matches.subcommand() {
        ("run", Some(arg_matches)) => run(arg_matches),
        ("compile", Some(arg_matches)) => compile(arg_matches),
        ("benchmark", Some(arg_matches)) => benchmark(arg_matches),
        _ => {
            eprintln!("{}", matches.usage());
            Ok(())
        }
    } {
        eprintln!("{}", error);
        exit(1)
    }
}

fn create_clap_app() -> App<'static, 'static> {
    App::new(crate_name!())
        .version(crate_version!())
        .about(crate_description!())
        .subcommand(
            SubCommand::with_name("run")
                .about("Run application")
                .arg(get_source_file())
                .arg(
                    Arg::with_name("JIT")
                        .short("j")
                        .long("jit")
                        .help("Use JIT compiler"),
                )
                .arg(get_opt_mode_arg())
                .arg(get_jit_level())
                .arg(get_wrapping_is_ub_arg())
                .arg(get_debug_opt_arg())
                .arg(get_verbose_arg()),
        )
        .subcommand(
            SubCommand::with_name("compile")
                .about("Compile application")
                .arg(get_source_file())
                .arg(
                    Arg::with_name("FORMAT")
                        .short("f")
                        .long("format")
                        .possible_values(&["dump", "clir", "rust"])
                        .value_names(&["format"])
                        .default_value("dump")
                        .help("Format of compiled code"),
                )
                .arg(get_opt_mode_arg())
                .arg(get_jit_level())
                .arg(get_wrapping_is_ub_arg())
                .arg(get_debug_opt_arg())
                .arg(get_verbose_arg()),
        )
        .subcommand(
            SubCommand::with_name("benchmark")
                .about("Benchmark program with different optimization settings")
                .arg(get_source_file())
                .arg(
                    Arg::with_name("ITERATIONS")
                        .short("i")
                        .long("iterations")
                        .default_value("2")
                        .help("Number of benchmarking iterations"),
                )
                .arg(
                    Arg::with_name("RUNS")
                        .short("r")
                        .long("runs")
                        .default_value("4")
                        .help("Number of runs per optimization in each round"),
                )
                .arg(
                    Arg::with_name("OPTIMIZED_ONLY")
                        .short("-o")
                        .long("optimized-only")
                        .help("Don't benchmark O0"),
                )
                .arg(
                    Arg::with_name("JIT_ONLY")
                        .short("-j")
                        .long("-jit")
                        .help("Only benchmark jit"),
                ),
        )
}

fn get_source_file<'a, 'b>() -> Arg<'a, 'b> {
    Arg::with_name("FILE")
        .required(true)
        .help("Brainfuck source file. Use - to read from stdin")
}

fn get_jit_level<'a, 'b>() -> Arg<'a, 'b> {
    Arg::with_name("JIT_LEVEL")
        .long("jit-level")
        .possible_values(&["none", "speed", "speed_and_size"])
        .value_names(&["level"])
        .help("Optimization level for JIT")
}

fn get_verbose_arg<'a, 'b>() -> Arg<'a, 'b> {
    Arg::with_name("VERBOSE").short("v").long("verbose")
}

fn get_opt_mode_arg<'a, 'b>() -> Arg<'a, 'b> {
    Arg::with_name("OPT_MODE")
        .short("O")
        .possible_values(&["0", "1", "2", "3", "s", "wtf"])
        .value_names(&["mode"])
        .default_value("2")
        .help("Optimization mode")
}

fn get_wrapping_is_ub_arg<'a, 'b>() -> Arg<'a, 'b> {
    Arg::with_name("WRAPPING_IS_UB")
        .long("wrapping-is-ub")
        .help("Wrapping overflows are undefined behavior during optimization")
}

fn get_debug_opt_arg<'a, 'b>() -> Arg<'a, 'b> {
    Arg::with_name("DEBUG_OPT")
        .long("debug-optimizations")
        .help("Print statistics for optimization passes")
}

fn is_verbose(matches: &ArgMatches) -> bool {
    matches.is_present("VERBOSE")
}

fn run(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    let path = matches.value_of_os("FILE").unwrap();
    let verbose = is_verbose(matches);
    let opt_mode = get_optimize_config_from_args(matches);
    let jit = matches.is_present("JIT");

    run_file(opt_mode, jit, verbose, path)
}

fn compile(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    let path = matches.value_of_os("FILE").unwrap();
    let verbose = is_verbose(matches);
    let opt_mode = get_optimize_config_from_args(matches);
    let format = matches.value_of("FORMAT").unwrap_or("dump");

    compile_file(opt_mode, verbose, format, path)
}

fn benchmark(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    let path = matches.value_of_os("FILE").unwrap();

    let iterations = matches
        .value_of("ITERATIONS")
        .unwrap_or("2")
        .parse()
        .unwrap_or(2)
        .max(1);

    let runs = matches
        .value_of("RUNS")
        .unwrap_or("4")
        .parse()
        .unwrap_or(4)
        .max(1);

    let optimized_only = matches.is_present("OPTIMIZED_ONLY");
    let jit_only = matches.is_present("JIT_ONLY");

    benchmark_file(path, iterations, runs, optimized_only, jit_only)
}
