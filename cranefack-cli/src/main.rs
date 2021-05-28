use std::error::Error;
use clap::{App, Arg, ArgMatches, crate_description, crate_name, crate_version, SubCommand};

mod utils;
mod run;
mod compile;
mod benchmark;

use crate::utils::get_optimize_config_from_args;
use crate::run::run_file;
use crate::compile::compile_file;
use crate::benchmark::benchmark_file;

fn main() -> Result<(), Box<dyn Error>> {
    let matches = create_clap_app().get_matches();

    match matches.subcommand() {
        ("run", Some(arg_matches)) => run(arg_matches),
        ("compile", Some(arg_matches)) => compile(arg_matches),
        ("benchmark", Some(arg_matches)) => benchmark(arg_matches),
        _ => {
            eprintln!("{}", matches.usage());
            Ok(())
        }
    }
}

fn create_clap_app() -> App<'static, 'static> {
    App::new(crate_name!())
        .version(crate_version!())
        .about(crate_description!())
        .subcommand(SubCommand::with_name("run")
            .about("Run application")
            .arg(Arg::with_name("FILE")
                .required(true)
                .help("Brainfuck source file"))
            .arg(Arg::with_name("JIT")
                .short("j")
                .long("jit")
                .help("Use JIT compiler"))
            .arg(get_opt_mode_arg())
            .arg(get_jit_level())
            .arg(get_verbose_arg())
        )
        .subcommand(SubCommand::with_name("compile")
            .about("Compile application")
            .arg(Arg::with_name("FILE")
                .required(true)
                .help("Brainfuck source file"))
            .arg(Arg::with_name("FORMAT")
                .short("f")
                .long("format")
                .possible_values(&["dump", "clir", "rust"])
                .value_names(&["format"])
                .default_value("dump")
                .help("Format of compiled code")
            )
            .arg(get_opt_mode_arg())
            .arg(get_jit_level())
            .arg(get_verbose_arg())
        )
        .subcommand(SubCommand::with_name("benchmark")
            .about("Benchmark program with different optimization settings")
            .arg(Arg::with_name("FILE")
                .required(true)
                .help("Brainfuck source file")
            )
            .arg(Arg::with_name("ITERATIONS")
                .short("i")
                .long("iterations")
                .default_value("2")
                .help("Number of benchmarking iterations")
            )
            .arg(Arg::with_name("RUNS")
                .short("r")
                .long("runs")
                .default_value("4")
                .help("Number of runs per optimization in each round")
            )
        )
}

fn get_jit_level<'a, 'b>() -> Arg<'a, 'b> {
    Arg::with_name("JIT_LEVEL")
        .long("jit-level")
        .possible_values(&["none", "speed", "speed_and_size"])
        .value_names(&["level"])
        .help("Optimization level for JIT")
}

fn get_verbose_arg<'a, 'b>() -> Arg<'a, 'b> {
    Arg::with_name("VERBOSE")
        .short("v")
        .long("verbose")
}

fn get_opt_mode_arg<'a, 'b>() -> Arg<'a, 'b> {
    Arg::with_name("OPT_MODE")
        .short("O")
        .possible_values(&["0", "1", "2", "3", "s"])
        .value_names(&["mode"])
        .default_value("2")
        .help("Optimization mode")
}

fn is_verbose(matches: &ArgMatches) -> bool {
    matches.is_present("VERBOSE")
}

fn run(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    let path = matches.value_of_os("FILE").unwrap();
    let verbose = is_verbose(&matches);
    let opt_mode = get_optimize_config_from_args(&matches);
    let jit = matches.is_present("JIT");

    run_file(
        opt_mode,
        jit,
        verbose,
        path,
    )
}

fn compile(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    let path = matches.value_of_os("FILE").unwrap();
    let verbose = is_verbose(&matches);
    let opt_mode = get_optimize_config_from_args(&matches);
    let format = matches.value_of("FORMAT").unwrap_or("dump");

    compile_file(
        opt_mode,
        verbose,
        format,
        path,
    )
}

fn benchmark(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    let path = matches.value_of_os("FILE").unwrap();

    let iterations = matches.value_of("ITERATIONS")
        .unwrap_or("2").parse().unwrap_or(2).max(1);

    let runs = matches.value_of("RUNS")
        .unwrap_or("4").parse().unwrap_or(4).max(1);

    benchmark_file(path, iterations, runs)
}
