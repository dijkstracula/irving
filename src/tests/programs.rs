#[cfg(test)]
mod parser {
    use crate::tests::helpers;

    #[test]
    fn test_state_and_actions() {
        let _ast = helpers::prog_from_filename("./programs/001_state_and_actions.ivy");
    }

    #[test]
    fn test_safety_and_invariants() {
        let _ast = helpers::prog_from_filename("programs/002_safety_and_invariants.ivy");
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
        let _ast = helpers::typeinference_from_filename("programs/101_append2.ivy");
    }

    #[test]
    fn test_noninf1() {
        let _ast = helpers::typeinference_from_filename("programs/300_noninf1.ivy");
    }
}

#[cfg(test)]
mod accord {
    use crate::tests::helpers;

    #[test]
    fn typecheck_abstract_protocol() {
        let _ast =
            helpers::typeinference_from_filename("programs/accord-ivy/src/abstract_protocol.ivy");
    }

    #[test]
    fn typecheck_messages() {
        let _ast = helpers::typeinference_from_filename("programs/accord-ivy/src/messages.ivy");
    }

    #[test]
    fn typecheck_sorts() {
        let _ast = helpers::typeinference_from_filename("programs/accord-ivy/src/sorts.ivy");
    }

    #[test]
    fn typecheck_temporality() {
        let _ast = helpers::typeinference_from_filename("programs/accord-ivy/src/temporality.ivy");
    }

    #[test]
    fn typecheck_txn() {
        let _ast = helpers::typeinference_from_filename("programs/accord-ivy/src/txn.ivy");
    }

    #[test]
    fn typecheck_protocol() {
        let _ast = helpers::typeinference_from_filename("programs/accord-ivy/src/protocol.ivy");
    }
}
