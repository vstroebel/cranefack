pub use backends::interpreter::Interpreter;
pub use errors::{CraneFackError, ParserError, RuntimeError};

pub use crate::analyzer::{analyze, Warning, WarningType};
pub use crate::optimizations::{optimize, optimize_with_config, OptimizeConfig};
pub use crate::parser::{parse, Program};

mod parser;
mod errors;
mod optimizations;
pub mod backends;
mod analyzer;

