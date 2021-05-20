mod parser;
mod interpreter;
mod errors;
mod optimizer;

pub use errors::{CraneFuckError, ParserError, RuntimeError};

pub use crate::interpreter::Interpreter;
pub use crate::parser::parse;
pub use crate::optimizer::optimize;


#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::interpreter::Interpreter;
    use crate::parser::parse;
    use crate::optimize;

    #[test]
    fn test_out_1() {
        let mut program = parse(">+.").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"\x01");
    }

    #[test]
    fn test_out_b() {
        let mut program = parse(",++.").unwrap();
        optimize(&mut program);

        let input = b"a";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"c");
    }

    #[test]
    fn test_loop() {
        let mut program = parse("+++[>+<-]>.").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"\x03");
    }

    #[test]
    fn test_hello_world() {
        let mut program = parse(include_str!("test_programs/hello_world.bf")).unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        Interpreter::new(Cursor::new(input), &mut output).execute(&program).unwrap();

        assert_eq!(output, b"Hello World!\n");
    }

    #[test]
    fn test_count_loop() {
        let mut program = parse("++++[->++<]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
        assert_eq!(interpreter.heap[1], 8);
    }

    #[test]
    fn test_count_loop_inv() {
        let mut program = parse("++++[->++<]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
        assert_eq!(interpreter.heap[1], 8);
    }

    #[test]
    fn test_totally_empty() {
        let mut program = parse("").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
    }

    #[test]
    fn test_optimized_empty() {
        let mut program = parse("[>+++++<-]").unwrap();
        optimize(&mut program);

        let input = b"";
        let mut output = Vec::new();

        let mut interpreter = Interpreter::new(Cursor::new(input), &mut output);

        interpreter.execute(&program).unwrap();

        assert_eq!(interpreter.heap[0], 0);
    }
}
