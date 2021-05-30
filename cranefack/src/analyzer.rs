use crate::parser::{Program, Op, OpType};
use std::ops::Range;
use codespan_reporting::files::SimpleFiles;
use std::error::Error;
use codespan_reporting::diagnostic::{Label, Diagnostic};
use codespan_reporting::term::termcolor::{StandardStream, ColorChoice};

/// WarningType found in a call to [analyze]
#[derive(Debug, Clone, PartialEq)]
pub enum WarningType {
    /// A possible infinite loop was found
    InfiniteLoop,
}

/// Warning found in a call to [analyze]
#[derive(Debug, Clone, PartialEq)]
pub struct Warning {
    /// Code position the warning was found
    pub span: Range<usize>,

    /// Type of the warning
    pub warning_type: WarningType,
}

impl Warning {
    pub fn infinite_loop(span: Range<usize>) -> Warning {
        Warning {
            span,
            warning_type: WarningType::InfiniteLoop,
        }
    }

    pub fn pretty_print(warnings: &[Warning], source: &str, filename: Option<&str>) -> Result<(), Box<dyn Error>> {
        let mut files = SimpleFiles::new();

        let file_id = files.add(
            filename.unwrap_or(""),
            source);

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        for warning in warnings {
            let span = warning.span.clone();

            let diagnostic = match &warning.warning_type {
                WarningType::InfiniteLoop => {
                    Diagnostic::warning()
                        .with_message("Possible infinite loop")
                        .with_labels(vec![
                            Label::primary(file_id, span)
                        ])
                }
            };

            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;
        }

        Ok(())
    }
}

/// Analyze program for possible errors
pub fn analyze(program: &Program) -> Vec<Warning> {
    let mut warnings = vec![];

    analyze_ops(&mut warnings, &program.ops);

    warnings
}

fn analyze_ops(warnings: &mut Vec<Warning>, ops: &[Op]) {
    for op in ops.windows(2) {
        check_infinite_loop(warnings, &op[0], &op[1]);
    }

    for op in ops {
        if let Some(children) = op.op_type.get_children() {
            analyze_ops(warnings, children);
        }
    }
}

fn check_infinite_loop(warnings: &mut Vec<Warning>, op1: &Op, op2: &Op) {
    if matches!(op2.op_type, OpType::ILoop(_,_,0,_)) && !op1.op_type.is_zeroing(0) {
        warnings.push(Warning::infinite_loop(op2.span.clone()));
    }
}

#[cfg(test)]
mod test {
    use crate::{parse, optimize};
    use crate::analyzer::{analyze, Warning};

    #[test]
    fn test_infinite_loop_check() {
        let mut program = parse("+[<++>]").unwrap();
        optimize(&mut program);

        let warnings = analyze(&program);

        assert_eq!(warnings, vec![Warning::infinite_loop(1..7)])
    }
}
