#[cfg(test)]
mod tests {
    use crate::{
        extraction::java::extraction::Extractor,
        tests::helpers,
        typechecker::{
            inference::TypeChecker,
            sorts::{IvySort, Module},
        },
        visitor::ast::Visitable,
    };

    #[test]
    fn extract_call_not_a_method() {
        let fragment = "inc(1)";
        let mut ast = helpers::expr_from_src(fragment);

        let mut tc = TypeChecker::new();
        tc.bindings
            .append(
                "inc".into(),
                IvySort::function_sort(vec![IvySort::Number], IvySort::Number),
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
