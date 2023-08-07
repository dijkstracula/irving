#[cfg(test)]
mod parser {
    use crate::tests::helpers;

    #[test]
    fn typecheck_001_state_and_actions() {
        helpers::typeinference_from_filename("./programs/001_state_and_actions.ivy");
    }

    #[test]
    fn typecheck_101_append2() {
        helpers::typeinference_from_filename("./programs/101_append2.ivy");
    }

    #[test]
    fn typecheck_200_chainrep() {
        helpers::typeinference_from_filename("./programs/200_chainrep.ivy");
    }
}
