use clap::{crate_description, crate_name, crate_version, Arg, ArgMatches, Command, ArgAction};
use std::error::Error;
use std::ffi::OsString;

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
use clap::builder::ValueParser;

fn main() {
    let matches = create_clap_app().get_matches();

    if let Err(error) = match matches.subcommand() {
        Some(("run", arg_matches)) => run(arg_matches),
        Some(("compile", arg_matches)) => compile(arg_matches),
        Some(("benchmark", arg_matches)) => benchmark(arg_matches),
        _ => {
            eprintln!("Missing command");
            Ok(())
        }
    } {
        eprintln!("{}", error);
        exit(1)
    }
}

fn create_clap_app() -> Command {
    Command::new(crate_name!())
        .version(crate_version!())
        .about(crate_description!())
        .subcommand(
            Command::new("run")
                .about("Run application")
                .arg(get_source_file())
                .arg(
                    Arg::new("JIT")
                        .short('j')
                        .long("jit")
                        .help("Use JIT compiler")
                        .action(ArgAction::SetTrue),
                )
                .arg(get_opt_mode_arg())
                .arg(get_jit_level())
                .arg(get_wrapping_is_ub_arg())
                .arg(get_debug_opt_arg())
                .arg(get_verbose_arg()),
        )
        .subcommand(
            Command::new("compile")
                .about("Compile application")
                .arg(get_source_file())
                .arg(
                    Arg::new("FORMAT")
                        .short('f')
                        .long("format")
                        .value_parser(["dump", "clir", "rust"])
                        .value_names(["format"])
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
            Command::new("benchmark")
                .about("Benchmark program with different optimization settings")
                .arg(get_source_file())
                .arg(
                    Arg::new("ITERATIONS")
                        .short('i')
                        .long("iterations")
                        .default_value("2")
                        .help("Number of benchmarking iterations"),
                )
                .arg(
                    Arg::new("RUNS")
                        .short('r')
                        .long("runs")
                        .default_value("4")
                        .help("Number of runs per optimization in each round"),
                )
                .arg(
                    Arg::new("OPTIMIZED_ONLY")
                        .short('o')
                        .long("optimized-only")
                        .help("Don't benchmark O0")
                        .action(ArgAction::SetTrue),
                )
                .arg(
                    Arg::new("JIT_ONLY")
                        .short('j')
                        .long("jit")
                        .help("Only benchmark jit")
                        .action(ArgAction::SetTrue),
                ),
        )
}

fn get_source_file() -> Arg {
    Arg::new("FILE")
        .required(true)
        .value_parser(ValueParser::os_string())
        .help("Brainfuck source file. Use - to read from stdin")
}

fn get_jit_level() -> Arg {
    Arg::new("JIT_LEVEL")
        .long("jit-level")
        .value_parser(["none", "speed", "speed_and_size"])
        .value_names(["level"])
        .help("Optimization level for JIT")
}

fn get_verbose_arg() -> Arg {
    Arg::new("VERBOSE")
        .short('v')
        .long("verbose")
        .action(ArgAction::SetTrue)
}

fn get_opt_mode_arg() -> Arg {
    Arg::new("OPT_MODE")
        .short('O')
        .value_parser(["0", "1", "2", "3", "s", "wtf"])
        .value_names(["mode"])
        .default_value("2")
        .help("Optimization mode")
}

fn get_wrapping_is_ub_arg() -> Arg {
    Arg::new("WRAPPING_IS_UB")
        .long("wrapping-is-ub")
        .help("Wrapping overflows are undefined behavior during optimization")
}

fn get_debug_opt_arg() -> Arg {
    Arg::new("DEBUG_OPT")
        .long("debug-optimizations")
        .help("Print statistics for optimization passes")
}

fn is_verbose(matches: &ArgMatches) -> bool {
    matches.contains_id("VERBOSE")
}

fn run(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    let path: &OsString = matches.get_one("FILE").unwrap();
    let verbose = is_verbose(matches);
    let opt_mode = get_optimize_config_from_args(matches);
    let jit = matches.contains_id("JIT");

    run_file(opt_mode, jit, verbose, path)
}

fn compile(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    let path: &OsString = matches.get_one("FILE").unwrap();
    let verbose = is_verbose(matches);
    let opt_mode = get_optimize_config_from_args(matches);
    let format = matches.get_one::<String>("FORMAT").map(|s| s.as_str()).unwrap_or("dump");

    compile_file(opt_mode, verbose, format, path)
}

fn benchmark(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    let path: &OsString = matches.get_one("FILE").unwrap();

    let iterations = matches
        .get_one::<String>("ITERATIONS")
        .map(|s| s.as_str())
        .unwrap_or("2")
        .parse()
        .unwrap_or(2)
        .max(1);

    let runs = matches
        .get_one::<String>("RUNS")
        .map(|s| s.as_str())
        .unwrap_or("4")
        .parse()
        .unwrap_or(4)
        .max(1);

    let optimized_only = matches.contains_id("OPTIMIZED_ONLY");
    let jit_only = matches.contains_id("JIT_ONLY");

    benchmark_file(path, iterations, runs, optimized_only, jit_only)
}
