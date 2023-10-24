#[cfg(test)]
mod tests {
    use crate::{
        ast::span::Span,
        tests::helpers,
        typechecker::{inference::SortInferer, sorts::IvySort, unifier::ResolverError},
        visitor::ast::Visitable,
    };

    #[test]
    fn if_valid() {
        let fragment = "if 1 < 2 { a := 42; } else { a := 43; }";
        let mut ast = helpers::stmt_from_src(fragment);

        let mut si = SortInferer::new();
        si.bindings.append("a".into(), IvySort::Number).unwrap();

        ast.visit(&mut si).unwrap().modifying(&mut ast);
    }

    #[test]
    fn if_bad_cond() {
        let fragment = "if 1 + 2 { a := 42; }";
        let mut ast = helpers::stmt_from_src(fragment);

        let mut si = SortInferer::new();
        si.bindings.append("a".into(), IvySort::Number).unwrap();

        let err = ast
            .visit(&mut si)
            .expect_err("non-boolean expression in if statement test");
        assert_eq!(
            err,
            ResolverError::UnificationError(IvySort::Number, IvySort::Bool)
                .to_typeerror(&Span::IgnoredForTesting)
        );
    }

    #[test]
    fn while_valid() {
        let fragment = "while a < 100 { a := a + 1; }";
        let mut ast = helpers::stmt_from_src(fragment);

        let mut si = SortInferer::new();
        si.bindings.append("a".into(), IvySort::Number).unwrap();

        ast.visit(&mut si).unwrap().modifying(&mut ast);
    }

    #[test]
    fn logical_assign() {
        let fragment = "link(x,Y) := false";
        let mut ast = helpers::stmt_from_src(fragment);

        let mut si = SortInferer::new();
        si.bindings.append("x".into(), IvySort::Number).unwrap();
        si.bindings
            .append(
                "link".into(),
                IvySort::Map(
                    vec![IvySort::Number, IvySort::Number],
                    Box::new(IvySort::Bool),
                ),
            )
            .unwrap();
        ast.visit(&mut si).unwrap().modifying(&mut ast);
    }
}
