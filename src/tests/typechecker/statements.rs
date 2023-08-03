#[cfg(test)]
mod tests {
    use crate::{
        tests::helpers,
        typechecker::{inference::SortInferer, sorts::IvySort, TypeError},
        visitor::ast::Visitable,
    };

    #[test]
    fn if_valid() {
        let fragment = "if 1 < 2 { a := 42; } else { a := 43; }";
        let mut ast = helpers::stmt_from_src(fragment);

        let mut si = SortInferer::new();
        si.bindings.append("a".into(), IvySort::Number).unwrap();

        ast.visit(&mut si).unwrap().modifying(&mut ast).unwrap();
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
            err.downcast_ref::<TypeError>(),
            Some(&TypeError::UnificationError(IvySort::Number, IvySort::Bool))
        );
    }

    #[test]
    fn while_valid() {
        let fragment = "while a < 100 { a := a + 1; }";
        let mut ast = helpers::stmt_from_src(fragment);

        let mut si = SortInferer::new();
        si.bindings.append("a".into(), IvySort::Number).unwrap();

        ast.visit(&mut si).unwrap().modifying(&mut ast).unwrap();
        println!("{:?}", ast);
    }
}
