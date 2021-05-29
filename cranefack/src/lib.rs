mod parser;
mod errors;
mod optimizations;
mod backends;
mod analyzer;

pub use backends::interpreter::Interpreter;
pub use backends::cranelift::CompiledJitModule;
pub use backends::rust::compile_to_rust;
pub use errors::{CraneFackError, ParserError, RuntimeError};
pub use analyzer::{analyze, Warning, WarningType};
pub use optimizations::{optimize, optimize_with_config, OptimizeConfig};
pub use parser::{parse, Program};
