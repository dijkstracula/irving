#[cfg(test)]
mod parser {
    use crate::{tests::helpers, typechecker::inference::TypeChecker, visitor::visitor::Visitable};

    #[test]
    fn test_state_and_actions() {
        let _ast = helpers::prog_from_filename("./programs/001_state_and_actions.ivy");

        // TODO: we still don't handle walking functions for some reason.
        // So we can't typecheck this one yet.
    }

    #[test]
    fn test_safety_and_invariants() {
        let mut ast = helpers::prog_from_filename("programs/002_safety_and_invariants.ivy");
        let mut tc = TypeChecker::new();
        ast.visit(&mut tc).expect("typechecking failed");
    }

    #[test]
    fn test_modules() {
        let _ast = helpers::prog_from_filename("programs/003_modules.ivy");
    }

    #[test]
    fn test_loops() {
        let _ast = helpers::prog_from_filename("programs/004_loops.ivy");
    }

    #[test]
    fn test_isolates() {
        let _ast = helpers::prog_from_filename("programs/005_isolate_and_mixins.ivy");
    }

    #[test]
    fn test_append() {
        let _ast = helpers::prog_from_filename("programs/100_append.ivy");
    }

    #[test]
    fn test_append2() {
        let _ast = helpers::prog_from_filename("programs/101_append2.ivy");
    }
}
