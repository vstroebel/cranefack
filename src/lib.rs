mod parser;
mod interpreter;
mod errors;
mod optimizations;
pub mod backends;
mod analyzer;

pub use errors::{CraneFuckError, ParserError, RuntimeError};

pub use crate::interpreter::Interpreter;
pub use crate::parser::parse;
pub use crate::optimizations::{optimize, optimize_with_config, OptimizeConfig};
pub use crate::analyzer::{analyze, Warning, WarningType};
