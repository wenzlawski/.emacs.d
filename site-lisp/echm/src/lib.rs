extern crate chmlib;
extern crate emacs;

use chmlib::ChmFile;
use emacs::{defun, Env, Result, Value};
use std::path::Path;

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "echm-core")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done loading!")
}

#[defun(user_ptr)]
fn open(env: &Env, file: String) -> Result<ChmFile> {
    let _ = env.message(&file);
    let file = ChmFile::open(Path::new(&file)).expect("Unable to open the file");
    Ok(file)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chmlib::ChmFile;

    #[test]
    fn test_open() {
        let file = ChmFile::open("~/m.wenzlawski/Documents/STEPS.chm").expect("Fuck");
        dbg!(file);
        assert_eq!(5, 5);
    }
}

// #[defun(user_ptr)]
// fn find(env: &Env, file: ChmFile, path: Path)
