#[cfg(test)]
mod parser {
    use crate::tests::helpers;

    #[test]
    fn test_state_and_actions() {
        helpers::typeinference_from_filename("./programs/001_state_and_actions.ivy");
    }
}
