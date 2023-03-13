use crate::errors::CliError;
use clap::ArgMatches;
use cranefack::OptimizeConfig;
use std::error::Error;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{stdin, ErrorKind, Read};

pub fn read_input(path: &OsStr) -> Result<String, Box<dyn Error>> {
    if path == "-" {
        let mut source = "".to_owned();
        stdin()
            .read_to_string(&mut source)
            .map_err(|err| CliError::SourceReadError(path.to_owned(), err))?;
        Ok(source)
    } else {
        let mut file = File::open(path).map_err(|err| -> Box<dyn Error> {
            match err.kind() {
                ErrorKind::NotFound => CliError::SourceFileNotFound(path.to_owned()).into(),
                ErrorKind::PermissionDenied => {
                    CliError::SourceFilePermissionDenied(path.to_owned()).into()
                }
                _ => err.into(),
            }
        })?;

        let mut source = "".to_owned();
        file.read_to_string(&mut source)
            .map_err(|err| CliError::SourceReadError(path.to_owned(), err))?;
        Ok(source)
    }
}

pub fn get_optimize_config_from_args(matches: &ArgMatches) -> OptimizeConfig {
    let mut cfg = match matches.get_one::<String>("OPT_MODE").map(|s| s.as_str()).unwrap_or("2") {
        "1" => OptimizeConfig::o1(),
        "2" => OptimizeConfig::o2(),
        "3" => OptimizeConfig::o3(),
        "s" => OptimizeConfig::size(),
        "wtf" => OptimizeConfig::wtf(),
        _ => OptimizeConfig::o0(),
    };

    if let Some(jit_level) = &matches.get_one::<u8>("JIT_LEVEL") {
        cfg.jit_level = Some(jit_level.to_string());
    }

    if matches.contains_id("WRAPPING_IS_UB") {
        cfg.wrapping_is_ub = true;
    }

    if matches.contains_id("DEBUG_OPT") {
        cfg.debug = true;
    }

    cfg
}
