//! ## Basic usage
//!
//! ### Run program with interpreter
//!
//! ```no_run
//! # use std::error::Error;
//! # fn main() -> Result<(), Box<dyn Error>> {
//!     use cranefack::{parse, optimize_with_config, OptimizeConfig, Interpreter};
//!
//!     // Parse program
//!     let mut program = parse("++[<].")?;
//!
//!     // Optimize with optimization level 2
//!     optimize_with_config(&mut program, &OptimizeConfig::o2());
//!
//!     // Create new interpreter reading from stdin and writing to stdout
//!     let mut interpreter = Interpreter::new(std::io::stdin(), std::io::stdout());
//!
//!     // Execute program
//!     interpreter.execute(&program);
//!
//! # Ok(())
//! # }
//! ```

//! ### Run program with jit
//!
//! ```no_run
//! # use std::error::Error;
//! # fn main() -> Result<(), Box<dyn Error>> {
//!     use cranefack::{parse, optimize_with_config, OptimizeConfig, CompiledJitModule};
//!
//!     // Parse program
//!     let mut program = parse("++[<].")?;
//!
//!     // Create optimization config for level 2
//!     let opt_level = OptimizeConfig::o2();
//!
//!     // Optimize with optimization level 2
//!     optimize_with_config(&mut program, &opt_level);
//!
//!     // Compile program into module
//!     let module = CompiledJitModule::new(&program, &opt_level)?;
//!
//!     // Execute compiled module reading from stdin and writing to stdout
//!     module.execute(std::io::stdin(), std::io::stdout());
//!
//! # Ok(())
//! # }
//! ```

mod parser;
mod errors;
mod optimizations;
mod backends;
mod analyzer;

pub use backends::interpreter::Interpreter;
pub use backends::cranelift::CompiledJitModule;
pub use backends::rust::compile_to_rust;
pub use errors::{CraneFackError, ParserError, RuntimeError, CompilerError};
pub use analyzer::{analyze, Warning, WarningType};
pub use optimizations::{optimize, optimize_with_config, OptimizeConfig};
pub use parser::{parse, Program};
