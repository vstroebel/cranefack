use clap::{App, SubCommand, Arg, crate_name, crate_version, crate_description};
use std::ffi::OsStr;
use std::error::Error;
use std::fs::File;
use std::io::{Read, stdin, stdout};
use cranefuck::{parse, Interpreter};

fn main() -> Result<(), Box<dyn Error>> {
    let matches = create_clap_app().get_matches();

    match matches.subcommand() {
        ("run", Some(arg_matches)) => {
            run_file(arg_matches.value_of_os("FILE").unwrap())
        }
        _ => {
            eprintln!("{}", matches.usage());
            Ok(())
        }
    }
}

fn create_clap_app() -> App<'static, 'static> {
    App::new(crate_name!())
        .version(crate_version!())
        .about(crate_description!())
        .subcommand(SubCommand::with_name("run")
            .about("Run application")
            .arg(Arg::with_name("FILE")
                .required(true)
                .help("Brainfuck source file"))
        )
}

fn run_file(path: &OsStr) -> Result<(), Box<dyn Error>> {
    let mut file = File::open(path)?;

    let mut source = "".to_owned();

    file.read_to_string(&mut source)?;

    let program = parse(&source)?;

    let mut interpreter = Interpreter::new(stdin(), stdout());

    interpreter.execute(&program)?;

    Ok(())
}
