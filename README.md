# Cranefack

[![docs.rs badge](https://docs.rs/cranefack/badge.svg)](https://docs.rs/cranefack/)
[![crates.io badge](https://img.shields.io/crates/v/cranefack.svg)](https://crates.io/crates/cranefack/)
[![Rust](https://github.com/vstroebel/cranefack/actions/workflows/rust.yml/badge.svg)](https://github.com/vstroebel/cranefack/actions/workflows/rust.yml)

A cranelift powered optimizing brainfuck compiler suite.

## Commandline interface

Cranefack provides a command line utility to run, compile and benchmark programs.

```shell
cargo install cranefack-cli
```

### Run

Run a program with interpreter or jit.<br>
Passing the `-v` option prints some statistics and execution time.

```text
USAGE:
    cranefack run [FLAGS] [OPTIONS] <FILE>

FLAGS:
        --debug-optimizations    Print statistics for optimization passes
    -j, --jit                    Use JIT compiler
    -v, --verbose    
    -h, --help                   Prints help information
    -V, --version                Prints version information

OPTIONS:
        --jit-level <level>    Optimization level for JIT [possible values: none, speed, speed_and_size]
    -O <mode>                  Optimization mode [default: 2]  [possible values: 0, 1, 2, 3, s]

ARGS:
    <FILE>    Brainfuck source file. Use - to read from stdin
```

### Compile

Compile the program.<br>
As of now this will create an assembly like representation by default that is only useful for debugging.<br>
In case you need something to compile into a native binary you can use the `rust` output format to get ugly rust code
that can be compiled with rustc:

```shell
cranefack compile -f=rust some_app.bf > some_app.rs
rustc -O some_app.rs
./some_app
```

```text
USAGE:
    cranefack compile [FLAGS] [OPTIONS] <FILE>

FLAGS:
        --debug-optimizations    Print statistics for optimization passes
    -v, --verbose    
    -h, --help                   Prints help information
    -V, --version                Prints version information

OPTIONS:
    -f, --format <format>      Format of compiled code [default: dump]  [possible values: dump, clir, rust]
        --jit-level <level>    Optimization level for JIT [possible values: none, speed, speed_and_size]
    -O <mode>                  Optimization mode [default: 2]  [possible values: 0, 1, 2, 3, s]

ARGS:
    <FILE>    Brainfuck source file. Use - to read from stdin
```

### Benchmark

Runs a program with different optimization settings and returns a table this the time for each program run.

```text
USAGE:
    cranefack benchmark [OPTIONS] <FILE>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -i, --iterations <ITERATIONS>    Number of benchmarking iterations [default: 2]
    -r, --runs <RUNS>                Number of runs per optimization in each round [default: 4]

ARGS:
    <FILE>    Brainfuck source file. Use - to read from stdin
```

## Use cranefack as a library

To use cranefack as a library add the following to your Cargo.toml dependencies:

```toml
cranefack = "0.1"
```

To run a program with jit compilation:

```rust
use std::error::Error;
use cranefack::{parse, optimize_with_config, OptimizeConfig, CompiledJitModule};

fn main() -> Result<(), Box<dyn Error>> {

    // Parse program
    let mut program = parse("++[<].")?;

    // Create optimization config for level 2
    let opt_level = OptimizeConfig::o2();

    // Optimize with optimization level 2
    optimize_with_config(&mut program, &opt_level);

    // Compile program into module
    let module = CompiledJitModule::new(&program, &opt_level)?;

    // Execute compiled module reading from stdin and writing to stdout
    module.execute(std::io::stdin(), std::io::stdout());

    Ok(())
}
```

## License

This project is licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or https://www.apache.org/licenses/LICENSE-2.0)
* MIT license ([LICENSE-MIT](LICENSE-MIT) or https://opensource.org/licenses/MIT)

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in cranefack by you, as
defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
