use std::error::Error;
use std::fmt::{Display, Formatter};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::ops::Range;
use std::option::Option::Some;

/// Trait all internal errors must implement
pub trait CraneFackError: Error {
    /// Return error message with optional source position and optional source label
    fn get_message(&self) -> (Option<Range<usize>>, String, Option<String>);

    /// Print error to stderr with colors and other fancy stuff
    fn pretty_print(&self, source: &str, filename: Option<&str>) -> Result<(), Box<dyn Error>> {
        let mut files = SimpleFiles::new();

        let file_id = files.add(filename.unwrap_or(""), source);

        let (range, message, label_message) = self.get_message();

        let diagnostic =
            match range {
                Some(range) => Diagnostic::error().with_message(message).with_labels(vec![
                    match label_message {
                        Some(message) => Label::primary(file_id, range).with_message(message),
                        None => Label::primary(file_id, range),
                    },
                ]),
                None => Diagnostic::error().with_message(message),
            };

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;

        Ok(())
    }
}

/// Error type for parser related errors
#[derive(Debug)]
pub enum ParserError {
    /// Maximum depth of nested loops has been reached
    LoopStackOverflow { position: usize, max_depth: usize },

    /// A closing ] without matching [ was found
    BadlyClosedLoop { position: usize },

    /// There are still som unclosed loops at the end of the source
    UnclosedLoop { position: usize },
}

impl Error for ParserError {}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::LoopStackOverflow {
                position,
                max_depth,
            } => write!(
                f,
                "Maximum loop depth of {} reach at pos {}",
                max_depth, position
            ),
            ParserError::BadlyClosedLoop { position } => {
                write!(f, "Badly closed loop at pos {}", position)
            }
            ParserError::UnclosedLoop { position } => {
                write!(f, "Unclosed loop at pos {}", position)
            }
        }
    }
}

impl CraneFackError for ParserError {
    fn get_message(&self) -> (Option<Range<usize>>, String, Option<String>) {
        match self {
            ParserError::LoopStackOverflow { position, .. } => {
                (Some(*position..position + 1), self.to_string(), None)
            }
            ParserError::BadlyClosedLoop { position, .. } => (
                Some(*position..position + 1),
                self.to_string(),
                Some("Expected matching [".to_owned()),
            ),
            ParserError::UnclosedLoop { position, .. } => (
                Some(*position..position + 1),
                self.to_string(),
                Some("Expected ] found end of file".to_owned()),
            ),
        }
    }
}

/// Runtime errors for interpreter invocations
#[derive(Debug)]
pub enum RuntimeError {
    /// The interpreter's configured [`Limiter`][crate::limiters::Limiter] triggered
    LimiterTriggered {
        span: Range<usize>,
    },

    /// The program tries to use a heap cell beyond the maximu allowed size
    MaxHeapSizeReached {
        span: Range<usize>,
        max_heap_size: usize,
        required: usize,
    },

    /// Reading or writing with ops , or . failed
    IoError {
        span: Range<usize>,
        error: std::io::Error,
    },
}

impl Error for RuntimeError {
    fn cause(&self) -> Option<&dyn Error> {
        match self {
            RuntimeError::IoError { error, .. } => Some(error),
            _ => None,
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::LimiterTriggered { .. } => write!(f, "Max number of cycles reached"),
            RuntimeError::MaxHeapSizeReached {
                max_heap_size,
                required,
                ..
            } => write!(
                f,
                "Required heap size of 0x{:x} exceeds limit of 0x{:x}",
                required, max_heap_size
            ),
            RuntimeError::IoError { error, .. } => std::fmt::Display::fmt(&error, f),
        }
    }
}

impl CraneFackError for RuntimeError {
    fn get_message(&self) -> (Option<Range<usize>>, String, Option<String>) {
        match self {
            RuntimeError::LimiterTriggered { span } => (Some(span.clone()), self.to_string(), None),
            RuntimeError::MaxHeapSizeReached { span, .. } => {
                (Some(span.clone()), self.to_string(), None)
            }
            RuntimeError::IoError { span, .. } => (Some(span.clone()), self.to_string(), None),
        }
    }
}

/// Jit compilation error
#[derive(Debug)]
pub enum CompilerError {
    /// Some unknown and unexpected shit happened during compilation
    InternalCompilerError { message: String },
}

impl Error for CompilerError {}

impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::InternalCompilerError { message } => {
                write!(f, "Internal compiler Error: {}", message)
            }
        }
    }
}

impl CraneFackError for CompilerError {
    fn get_message(&self) -> (Option<Range<usize>>, String, Option<String>) {
        match self {
            CompilerError::InternalCompilerError { message: _ } => (None, self.to_string(), None),
        }
    }
}
