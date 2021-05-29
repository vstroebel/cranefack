use std::error::Error;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{Read, stdin, ErrorKind};
use clap::ArgMatches;
use cranefack::OptimizeConfig;
use crate::errors::CliError;

pub fn read_input(path: &OsStr) -> Result<String, Box<dyn Error>> {
    if path == "-" {
        let mut source = "".to_owned();
        stdin().read_to_string(&mut source).map_err(|err| CliError::SourceReadError(path.to_owned(), err))?;
        Ok(source)
    } else {
        let mut file = File::open(path).map_err(|err| -> Box<dyn Error> {
            match err.kind() {
                ErrorKind::NotFound => CliError::SourceFileNotFound(path.to_owned()).into(),
                ErrorKind::PermissionDenied => CliError::SourceFilePermissionDenied(path.to_owned()).into(),
                _ => err.into(),
            }
        })?;

        let mut source = "".to_owned();
        file.read_to_string(&mut source).map_err(|err| CliError::SourceReadError(path.to_owned(), err))?;
        Ok(source)
    }
}

pub fn get_optimize_config_from_args(matches: &ArgMatches) -> OptimizeConfig {
    let mut cfg = match matches.value_of("OPT_MODE").unwrap_or("2") {
        "1" => OptimizeConfig::o1(),
        "2" => OptimizeConfig::o2(),
        "3" => OptimizeConfig::o2(),
        "s" => OptimizeConfig::o2(),
        _ => OptimizeConfig::o0(),
    };

    if let Some(jit_level) = &matches.value_of("JIT_LEVEL") {
        cfg.jit_level = Some(jit_level.to_string());
    }

    cfg
}
