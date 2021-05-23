use clap::{App, SubCommand, Arg, crate_name, crate_version, crate_description};
use std::ffi::OsStr;
use std::error::Error;
use std::fs::File;
use std::io::{Read, Write, stdin, stdout};
use cranefack::{parse, Interpreter, CraneFuckError, optimize};
use std::time::SystemTime;
use codespan_reporting::term::termcolor::{StandardStream, Color, ColorChoice, WriteColor, ColorSpec};

fn main() -> Result<(), Box<dyn Error>> {
    let matches = create_clap_app().get_matches();

    match matches.subcommand() {
        ("run", Some(arg_matches)) => {
            run_file(
                arg_matches.value_of("OPT_MODE").unwrap_or("1"),
                arg_matches.is_present("VERBOSE"),
                arg_matches.value_of_os("FILE").unwrap(),
            )
        }
        ("compile", Some(arg_matches)) => {
            compile_file(
                arg_matches.value_of("OPT_MODE").unwrap_or("1"),
                arg_matches.is_present("VERBOSE"),
                arg_matches.value_of("FORMAT").unwrap_or("dump"),
                arg_matches.value_of_os("FILE").unwrap(),
            )
        }
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
            .arg(Arg::with_name("OPT_MODE")
                .short("O")
                .possible_values(&["0", "1", "2", "3"])
                .value_names(&["mode"])
                .default_value("1")
                .help("Optimization mode")
            )
            .arg(Arg::with_name("VERBOSE")
                .short("v")
                .long("verbose"))
        )
        .subcommand(SubCommand::with_name("compile")
            .about("Compile application")
            .arg(Arg::with_name("FILE")
                .required(true)
                .help("Brainfuck source file"))
            .arg(Arg::with_name("OPT_MODE")
                .short("O")
                .possible_values(&["0", "1", "2", "3"])
                .value_names(&["mode"])
                .default_value("1")
                .help("Optimization mode"))
            .arg(Arg::with_name("FORMAT")
                .short("f")
                .long("format")
                .possible_values(&["dump", "rust"])
                .value_names(&["format"])
                .default_value("dump")
                .help("Format of compiled code")
            )
            .arg(Arg::with_name("VERBOSE")
                .short("v")
                .long("verbose"))
        )
}

fn read_input(path: &OsStr) -> Result<String, Box<dyn Error>> {
    if path == "-" {
        let mut source = "".to_owned();
        stdin().read_to_string(&mut source)?;
        Ok(source)
    } else {
        let mut file = File::open(path)?;
        let mut source = "".to_owned();
        file.read_to_string(&mut source)?;
        Ok(source)
    }
}

fn run_file(opt_mode: &str, verbose: bool, path: &OsStr) -> Result<(), Box<dyn Error>> {
    let source = read_input(path)?;

    let mut ts = SystemTime::now();

    let mut program = match parse(&source) {
        Ok(program) => program,
        Err(err) => {
            return err.pretty_print(&source, Some(&path.to_string_lossy()));
        }
    };

    if verbose {
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        writer.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;

        let (op_count, dloop_count, iloop_count, cloop_count, if_count) = program.get_statistics();

        writeln!(writer, "Parsed program with {} instructions ({},{},{}) loops and {} ifs in {}ms",
                 op_count,
                 dloop_count,
                 iloop_count,
                 cloop_count,
                 if_count,
                 ts.elapsed()?.as_micros() as f32 / 1000.0
        )?;
        writer.reset()?;
        ts = SystemTime::now();
    }

    if opt_mode == "1" {
        let opt_loop_count = optimize(&mut program);

        if verbose {
            let mut writer = StandardStream::stderr(ColorChoice::Auto);
            writer.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;

            let (op_count, dloop_count, iloop_count, cloop_count, if_count) = program.get_statistics();

            writeln!(writer, "Optimized program with {} instructions ({},{},{}) loops and {} ifs in {}ms and {} iterations",
                     op_count,
                     dloop_count,
                     iloop_count,
                     cloop_count,
                     if_count,
                     ts.elapsed()?.as_micros() as f32 / 1000.0,
                     opt_loop_count
            )?;
            writer.reset()?;
            ts = SystemTime::now();
        }
    }

    let mut interpreter = Interpreter::new(stdin(), stdout());

    if let Err(err) = interpreter.execute(&program) {
        return err.pretty_print(&source, Some(&path.to_string_lossy()));
    }

    if verbose {
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        writer.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
        writeln!(writer, "Executed program in {}ms",
                 ts.elapsed()?.as_micros() as f32 / 1000.0
        )?;
        writer.reset()?;
    }

    Ok(())
}

fn compile_file(opt_mode: &str, verbose: bool, format: &str, path: &OsStr) -> Result<(), Box<dyn Error>> {
    let source = read_input(path)?;

    let mut ts = SystemTime::now();

    let mut program = match parse(&source) {
        Ok(program) => program,
        Err(err) => {
            return err.pretty_print(&source, Some(&path.to_string_lossy()));
        }
    };

    if verbose {
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        writer.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;

        let (op_count, dloop_count, iloop_count, cloop_count, if_count) = program.get_statistics();

        writeln!(writer, "Parsed program with {} instructions ({},{},{}) loops and {} ifs in {}ms",
                 op_count,
                 dloop_count,
                 iloop_count,
                 cloop_count,
                 if_count,
                 ts.elapsed()?.as_micros() as f32 / 1000.0
        )?;

        writer.reset()?;
        ts = SystemTime::now();
    }

    if opt_mode != "0" {
        let opt_loop_count = optimize(&mut program);

        if verbose {
            let mut writer = StandardStream::stderr(ColorChoice::Auto);
            writer.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;

            let (op_count, dloop_count, iloop_count, cloop_count, if_count) = program.get_statistics();

            writeln!(writer, "Optimized program with {} instructions ({},{},{}) loops and {} ifs in {}ms and {} iterations",
                     op_count,
                     dloop_count,
                     iloop_count,
                     cloop_count,
                     if_count,
                     ts.elapsed()?.as_micros() as f32 / 1000.0,
                     opt_loop_count
            )?;
            writer.reset()?;
        }
    }

    match format {
        "rust" => println!("{}", cranefack::backends::rust::build_file(&program, opt_mode)),
        _ => program.dump(stdout())?
    }


    Ok(())
}