pub use errors::{CraneFuckError, ParserError, RuntimeError};

pub use crate::interpreter::Interpreter;
pub use crate::parser::parse;

mod parser;
mod interpreter;
mod errors;

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::interpreter::Interpreter;
    use crate::parser::parse;

    #[test]
    fn test_out_1() {
        let program = parse(">+.").unwrap();

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"\x01");
    }

    #[test]
    fn test_out_b() {
        let program = parse(",++.").unwrap();

        let input = b"a";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"c");
    }

    #[test]
    fn test_loop() {
        let program = parse("+++[>+<-]>.").unwrap();

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"\x03");
    }

    #[test]
    fn test_hello_world() {
        let program = parse(include_str!("test_programs/hello_world.bf")).unwrap();

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"Hello World!\n");
    }
}
