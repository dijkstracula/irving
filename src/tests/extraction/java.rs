#[cfg(test)]
mod tests {
    use crate::{
        extraction::java::extraction::Extractor,
        tests::helpers,
        typechecker::{
            inference::TypeChecker,
            sorts::{self, IvySort},
        },
        visitor::ast::Visitable,
    };

    #[test]
    fn extract_action_forward_ref() {
        let fragment = "action foo(i: unbounded_sequence, b: bool)";
        let mut ast = helpers::decl_from_src(fragment);

        let mut tc = TypeChecker::new();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(
            "protected Action2<Long, Boolean, Void> foo = new Action2<>()",
            e.pp.out
        );
    }

    #[test]
    fn extract_action_and_decl() {
        let fragment = "action foo(i: unbounded_sequence, b: bool) = { } ";
        let mut ast = helpers::decl_from_src(fragment);

        let mut tc = TypeChecker::new();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(
            "protected Action2<Long, Boolean, Void> foo = new Action2<>((Long i, Boolean b) -> {})",
            e.pp.out.replace('\n', "")
        );
    }

    #[test]
    fn extract_after_decl() {
        let fragment = "after foo(i: unbounded_sequence, b: bool) { count := 0 } ";
        let mut ast = helpers::decl_from_src(fragment);

        let mut tc = TypeChecker::new();
        tc.bindings
            .append(
                "foo".into(),
                IvySort::action_sort(
                    vec!["i".into(), "b".into()],
                    vec![IvySort::Number, IvySort::Bool],
                    sorts::ActionRet::Unit,
                ),
            )
            .unwrap();

        tc.bindings.append("count".into(), IvySort::Number).unwrap();

        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(
            "foo.onAfter((Long i, Boolean b) -> {
    count = 0;
})",
            e.pp.out
        );
    }

    #[test]
    fn extract_after_init() {
        // After init is inlined right in the constructor, rather than
        // emitting a call to an Action object.
        let fragment = "after init { count := 0 } ";
        let mut ast = helpers::decl_from_src(fragment);

        let mut tc = TypeChecker::new();
        tc.bindings
            .append("init".into(), sorts::Module::init_action_sort())
            .unwrap();

        tc.bindings.append("count".into(), IvySort::Number).unwrap();

        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!("count = 0", e.pp.out);
    }

    #[test]
    fn extract_call_not_a_method() {
        let fragment = "inc(1)";
        let mut ast = helpers::expr_from_src(fragment);

        let mut tc = TypeChecker::new();
        tc.bindings
            .append(
                "inc".into(),
                IvySort::action_sort(
                    vec!["i".into()],
                    vec![IvySort::Number],
                    sorts::ActionRet::named("ret".into(), IvySort::Number),
                ),
            )
            .unwrap();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn extract_field_access() {
        let fragment = "a.b";

        let mut ast = helpers::expr_from_src(fragment);

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(fragment, e.pp.out);
    }

    // Terminals

    #[test]
    fn extract_bool() {
        let mut e = Extractor::<String>::new();
        let fragment = "true";
        let mut ast = helpers::expr_from_src(fragment);
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);

        let mut e = Extractor::<String>::new();
        let fragment = "false";
        let mut ast = helpers::expr_from_src(fragment);
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn extract_arith() {
        let mut e = Extractor::<String>::new();

        let fragment = "43 - 1";
        let mut ast = helpers::expr_from_src(fragment);
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn extract_vardecl() {
        let fragment = "var i: unbounded_sequence";
        let mut ast = helpers::decl_from_src(fragment);

        let mut tc = TypeChecker::new();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!("private long i", e.pp.out);
    }
}
