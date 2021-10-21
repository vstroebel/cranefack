use std::error::Error;
use std::ffi::OsString;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
#[allow(clippy::enum_variant_names)]
pub enum CliError {
    SourceFileNotFound(OsString),
    SourceFilePermissionDenied(OsString),
    SourceReadError(OsString, std::io::Error),
}

impl Error for CliError {}

impl Display for CliError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CliError::SourceFileNotFound(path) => {
                write!(f, "Source file not found: {}", path.to_string_lossy())
            }
            CliError::SourceFilePermissionDenied(path) => {
                write!(f, "Permission denied: {}", path.to_string_lossy())
            }
            CliError::SourceReadError(path, error) => write!(
                f,
                "Error reading source file {}:{}",
                path.to_string_lossy(),
                error
            ),
        }
    }
}
