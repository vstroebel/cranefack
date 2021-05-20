mod parser;
mod interpreter;

pub use crate::parser::{parse, ParserError};
pub use crate::interpreter::{Interpreter, RuntimeError};

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use crate::interpreter::Interpreter;
    use std::io::Cursor;

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
