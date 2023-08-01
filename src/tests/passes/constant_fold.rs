#[cfg(test)]
mod tests {
    use crate::ast::expressions::*;
    use crate::passes::constant_fold::ConstantFold;
    use crate::tests::helpers;
    use crate::visitor::ast::Visitable;

    #[test]
    fn fold_constants() {
        let expr = "(19 * 2) + 4";
        let mut ast = helpers::rval_from_src(expr);

        let mut cf = ConstantFold;
        ast.visit(&mut cf).unwrap();

        assert_eq!(ast, Expr::Number(42));
    }

    #[test]
    fn fold_zero_add() {
        let expr = "foo + 0";
        let mut ast = helpers::rval_from_src(expr);

        let mut cf = ConstantFold;
        ast.visit(&mut cf).unwrap();

        assert_eq!(ast, Expr::inferred_progsym("foo"));
    }
}
