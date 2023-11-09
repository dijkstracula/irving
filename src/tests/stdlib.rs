#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use crate::{ast::toplevels::Prog, tests::helpers};

    const STDLIB_IMPORT_DIR: &'static str = "src/stdlib/ivy";

    fn typecheck_stdlib_file<S>(name: S) -> Prog
    where
        S: AsRef<Path>,
    {
        let path = PathBuf::from(STDLIB_IMPORT_DIR)
            .join(name)
            .with_extension("ivy");
        helpers::typeinference_from_filename(path.to_str().unwrap())
    }

    #[test]
    fn typecheck() {
        typecheck_stdlib_file("collections");
        typecheck_stdlib_file("nodesets");
        typecheck_stdlib_file("order");
    }
}
