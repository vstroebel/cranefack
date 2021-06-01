use std::error::Error;
use std::ffi::OsStr;
use std::io::{stdout, Write};
use std::time::SystemTime;

use codespan_reporting::term::termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use cranefack::{analyze, CraneFackError, optimize_with_config, OptimizeConfig, parse, Warning, Program, compile_to_rust};

use crate::utils;
use cranefack::CompiledJitModule;

pub fn compile_file(opt_mode: OptimizeConfig, verbose: bool, format: &str, path: &OsStr) -> Result<(), Box<dyn Error>> {
    let source = utils::read_input(path)?;

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

        let (op_count, dloop_count, lloop_count, iloop_count, cloop_count, if_count) = program.get_statistics();

        writeln!(writer, "Parsed program with {} instructions ({},{},{},{}) loops and {} ifs in {}ms",
                 op_count,
                 dloop_count,
                 lloop_count,
                 iloop_count,
                 cloop_count,
                 if_count,
                 ts.elapsed()?.as_micros() as f32 / 1000.0
        )?;

        writer.reset()?;
        ts = SystemTime::now();
    }

    if opt_mode.optimize() {
        let opt_loop_count = optimize_with_config(&mut program, &opt_mode);

        if verbose {
            let mut writer = StandardStream::stderr(ColorChoice::Auto);
            writer.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;

            let (op_count, dloop_count, lloop_count, iloop_count, cloop_count, if_count) = program.get_statistics();

            writeln!(writer, "Optimized program with {} instructions ({},{},{},{}) loops and {} ifs in {}ms and {} iterations",
                     op_count,
                     dloop_count,
                     lloop_count,
                     iloop_count,
                     cloop_count,
                     if_count,
                     ts.elapsed()?.as_micros() as f32 / 1000.0,
                     opt_loop_count
            )?;
            writer.reset()?;
        }
    }

    let warnings = analyze(&program);
    if !warnings.is_empty() {
        Warning::pretty_print(&warnings, &source, Some(&path.to_string_lossy()))?;
    }

    match format {
        "rust" => println!("{}", compile_to_rust(&program)),
        "clir" => println!("{}", build_clir(&program, &opt_mode)?),
        _ => program.dump(stdout(), opt_mode.debug)?
    }

    Ok(())
}

fn build_clir(program: &Program, opt_mode: &OptimizeConfig) -> Result<String, Box<dyn Error>> {
    let module = CompiledJitModule::new(program, opt_mode)?;
    Ok(module.get_clir())
}
