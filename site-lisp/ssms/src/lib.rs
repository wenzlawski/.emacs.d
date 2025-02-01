extern crate emacs;

use emacs::{defun, Env, Result, Value};
use std::path::Path;

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "ssms-core")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done loading!")
}

