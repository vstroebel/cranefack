use std::error::Error;
use std::fmt::{Display, Formatter};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::option::Option::Some;
use std::ops::Range;

pub trait CraneFuckError: Error {
    fn get_message(&self) -> (Range<usize>, String, Option<String>);

    fn pretty_print(&self, source: &str, filename: Option<&str>) -> Result<(), Box<dyn Error>> {
        let mut files = SimpleFiles::new();

        let file_id = files.add(
            filename.unwrap_or(""),
            source);

        let (range, message, label_message) = self.get_message();

        let diagnostic = Diagnostic::error()
            .with_message(message)
            .with_labels(vec![
                match label_message {
                    Some(message) => Label::primary(file_id, range).with_message(message),
                    None => Label::primary(file_id, range)
                }]);

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;

        Ok(())
    }
}

#[derive(Debug)]
pub enum ParserError {
    LoopStackOverflow { position: usize, max_depth: usize },
    BadlyClosedLoop { position: usize },
    UnclosedLoop { position: usize },
}

impl Error for ParserError {}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::LoopStackOverflow { position, max_depth } => write!(f, "Maximum loop depth of {} reach at pos {}", max_depth, position),
            ParserError::BadlyClosedLoop { position } => write!(f, "Badly closed loop at pos {}", position),
            ParserError::UnclosedLoop { position } => write!(f, "Unclosed loop at pos {}", position),
        }
    }
}

impl CraneFuckError for ParserError {
    fn get_message(&self) -> (Range<usize>, String, Option<String>) {
        match self {
            ParserError::LoopStackOverflow { position, .. } => (
                *position..position + 1,
                self.to_string(),
                None
            ),
            ParserError::BadlyClosedLoop { position, .. } => (
                *position..position + 1,
                self.to_string(),
                Some("Expected matching [".to_owned())
            ),
            ParserError::UnclosedLoop { position, .. } => (
                *position..position + 1,
                self.to_string(),
                Some("Expected ] found end of file".to_owned())
            ),
        }
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    MaxHeapSizeReached {
        span: Range<usize>,
        max_heap_size: usize,
        required: usize,
    },
    IoError {
        span: Range<usize>,
        error: std::io::Error,
    },
}

impl Error for RuntimeError {
    fn cause(&self) -> Option<&dyn Error> {
        match self {
            RuntimeError::IoError { error, .. } => Some(error),
            _ => None
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::MaxHeapSizeReached { max_heap_size, required, .. } =>
                write!(f, "Required heap size of {} exceeds limit of {}", max_heap_size, required),
            RuntimeError::IoError { error, .. } => std::fmt::Display::fmt(&error, f),
        }
    }
}

impl CraneFuckError for RuntimeError {
    fn get_message(&self) -> (Range<usize>, String, Option<String>) {
        match self {
            RuntimeError::MaxHeapSizeReached { span, .. } => (
                span.clone(),
                self.to_string(),
                None
            ),
            RuntimeError::IoError { span, .. } => (
                span.clone(),
                self.to_string(),
                None
            ),
        }
    }
}
