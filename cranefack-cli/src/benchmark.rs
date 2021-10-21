use crate::utils::read_input;
use cranefack::CompiledJitModule;
use cranefack::{
    optimize_with_config, parse, CraneFackError, Interpreter, OptimizeConfig, Program,
};
use std::error::Error;
use std::ffi::OsStr;
use std::io::Cursor;
use std::time::SystemTime;

pub fn benchmark_file(
    path: &OsStr,
    iterations: usize,
    runs: usize,
    optimized_only: bool,
    jit_only: bool,
) -> Result<(), Box<dyn Error>> {
    let source = read_input(path)?;

    let program = match parse(&source) {
        Ok(program) => program,
        Err(err) => {
            return err.pretty_print(&source, Some(&path.to_string_lossy()));
        }
    };

    let mut o0 = OptimizeConfig::o0();
    o0.jit_level = Some("none".to_owned());
    let mut o0_fast = OptimizeConfig::o0();
    o0_fast.jit_level = Some("speed".to_owned());

    let mut o1 = OptimizeConfig::o1();
    o1.jit_level = Some("none".to_owned());
    let mut o1_fast = OptimizeConfig::o1();
    o1_fast.jit_level = Some("speed".to_owned());

    let mut o2 = OptimizeConfig::o2();
    o2.jit_level = Some("none".to_owned());
    let mut o2_fast = OptimizeConfig::o2();
    o2_fast.jit_level = Some("speed".to_owned());

    let mut o3 = OptimizeConfig::o2();
    o3.jit_level = Some("none".to_owned());
    let mut o3_fast = OptimizeConfig::o2();
    o3_fast.jit_level = Some("speed".to_owned());

    let program_o0 = program.clone();

    let mut program_o1 = program.clone();
    optimize_with_config(&mut program_o1, &o1);

    let mut program_o2 = program.clone();
    optimize_with_config(&mut program_o2, &o2);

    let mut program_o3 = program;
    optimize_with_config(&mut program_o3, &o3);

    let mut results = [0u128; 12];

    let total = iterations * runs;

    println!(
        "Starting benchmark with {} iterations and {} runs per iteration",
        iterations, runs
    );

    for iteration in 0..iterations {
        println!("Iteration {} ", iteration + 1);

        if !jit_only {
            if !optimized_only {
                println!("Run interpreter with O0");
                results[0] += run_interpreter(&program_o0, runs)?;
            }

            println!("Run interpreter with O1");
            results[1] += run_interpreter(&program_o1, runs)?;

            println!("Run interpreter with O2");
            results[2] += run_interpreter(&program_o2, runs)?;

            println!("Run interpreter with O3");
            results[3] += run_interpreter(&program_o3, runs)?;
        }

        if !optimized_only {
            println!("Run jit with O0");
            results[4] += run_jit(&program_o0, runs, &o0)?;

            println!("Run jit with O0 speed");
            results[5] += run_jit(&program_o0, runs, &o0_fast)?;
        }

        println!("Run jit with O1");
        results[6] += run_jit(&program_o1, runs, &o1)?;

        println!("Run jit with O1 speed");
        results[7] += run_jit(&program_o1, runs, &o1_fast)?;

        println!("Run jit with O2");
        results[8] += run_jit(&program_o2, runs, &o2)?;

        println!("Run jit with O2 speed");
        results[9] += run_jit(&program_o2, runs, &o2_fast)?;

        println!("Run jit with O3");
        results[10] += run_jit(&program_o3, runs, &o3)?;

        println!("Run jit with O3 speed");
        results[11] += run_jit(&program_o3, runs, &o3_fast)?;
    }

    println!("Results:");

    if !jit_only {
        if !optimized_only {
            println!("Int O0       {} ms/run", get_millis(results[0], total));
        }
        println!("Int O1       {} ms/run", get_millis(results[1], total));
        println!("Int O2       {} ms/run", get_millis(results[2], total));
        println!("Int O3       {} ms/run", get_millis(results[3], total));
    }

    if !optimized_only {
        println!("Jit O0       {} ms/run", get_millis(results[4], total));
        println!("Jit O0 speed {} ms/run", get_millis(results[5], total));
    }
    println!("Jit O1       {} ms/run", get_millis(results[6], total));
    println!("Jit O1 speed {} ms/run", get_millis(results[7], total));
    println!("Jit O2       {} ms/run", get_millis(results[8], total));
    println!("Jit O2 speed {} ms/run", get_millis(results[9], total));
    println!("Jit O3       {} ms/run", get_millis(results[10], total));
    println!("Jit O3 speed {} ms/run", get_millis(results[11], total));

    Ok(())
}

fn get_millis(nanos: u128, total: usize) -> String {
    let per_iter_micros = nanos / total as u128 / 1000;

    let millis = per_iter_micros as f64 / 1000.0;

    let mut result = format!("{:0.3}", millis);

    while result.len() < 10 {
        result.insert(0, ' ');
    }

    result
}

fn run_interpreter(program: &Program, runs: usize) -> Result<u128, Box<dyn Error>> {
    let mut input = Cursor::new(b"");
    let mut output = Vec::new();

    let ts = SystemTime::now();

    for _ in 0..runs {
        input.set_position(0);
        output.clear();

        Interpreter::new(&mut input, &mut output).execute(program)?;
    }

    Ok(ts.elapsed()?.as_nanos())
}

fn run_jit(
    program: &Program,
    runs: usize,
    opt_mode: &OptimizeConfig,
) -> Result<u128, Box<dyn Error>> {
    let mut input = Cursor::new(b"");
    let mut output = Vec::new();

    let module = CompiledJitModule::new(program, opt_mode)?;

    let ts = SystemTime::now();

    for _ in 0..runs {
        input.set_position(0);
        output.clear();

        module.execute(&mut input, &mut output);
    }

    Ok(ts.elapsed()?.as_nanos())
}
