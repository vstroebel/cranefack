use std::error::Error;
use std::ffi::OsStr;
use std::io::{stdin, stdout, Write};
use std::time::SystemTime;

use codespan_reporting::term::termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use cranefack::{analyze, CraneFuckError, Interpreter, optimize_with_config, OptimizeConfig, parse, Warning};
use cranefack::backends::cranelift::CompiledModule;

use crate::utils::read_input;

pub fn run_file(opt_mode: OptimizeConfig, jit: bool, verbose: bool, path: &OsStr) -> Result<(), Box<dyn Error>> {
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
            ts = SystemTime::now();
        }
    }

    let warnings = analyze(&program);
    if !warnings.is_empty() {
        Warning::pretty_print(&warnings, &source, Some(&path.to_string_lossy()))?;
    }

    if jit {
        let module = match CompiledModule::new(&program) {
            Ok(module) => module,
            Err(err) => {
                return err.pretty_print(&source, Some(&path.to_string_lossy()));
            }
        };

        if verbose {
            let mut writer = StandardStream::stderr(ColorChoice::Auto);
            writer.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
            writeln!(writer, "Compiled program in {}ms",
                     ts.elapsed()?.as_micros() as f32 / 1000.0
            )?;
            writer.reset()?;
            ts = SystemTime::now();
        }

        module.run(stdin(), stdout());
    } else {
        let mut interpreter = Interpreter::new(stdin(), stdout());

        if let Err(err) = interpreter.execute(&program) {
            return err.pretty_print(&source, Some(&path.to_string_lossy()));
        }
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