#[cfg(test)]
mod parser {
    use crate::tests::helpers;

    #[test]
    fn typecheck_001_state_and_actions() {
        helpers::typeinference_from_filename("./programs/001_state_and_actions.ivy");
    }
}

#[cfg(test)]
mod typechecker {
    use crate::tests::helpers;

    #[test]
    fn typecheck_101_append2() {
        helpers::typeinference_from_filename("./programs/101_append2.ivy");
    }

    #[test]
    fn typecheck_102_counter() {
        helpers::typeinference_from_filename("./programs/102_counter.ivy");
    }

    #[test]
    fn typecheck_200_chainrep() {
        helpers::typeinference_from_filename("./programs/200_chainrep.ivy");
    }
}

#[cfg(test)]
mod extraction {
    use crate::{extraction::ivy::Extractor, tests::helpers, visitor::ast::Visitable};

    #[test]
    fn extract_200_chainrep() {
        let mut ast = helpers::typeinference_from_filename("./programs/200_chainrep.ivy");

        let mut ivy_extractor = Extractor::<String>::new();
        ast.visit(&mut ivy_extractor)
            .expect("ivy extraction failed");

        let mut java_extractor = Extractor::<String>::new();
        ast.visit(&mut java_extractor)
            .expect("java extraction failed");
    }
}
