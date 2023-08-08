#[cfg(test)]
mod tests {
    use crate::ast::expressions::*;
    use crate::ast::span::Span;
    use crate::passes::constant_fold::ConstantFold;
    use crate::tests::helpers;
    use crate::visitor::ast::Visitable;

    #[test]
    fn fold_constants() {
        let expr = "(19 * 2) + 4";
        let mut ast = helpers::rval_from_src(expr);

        let mut cf = ConstantFold;
        ast.visit(&mut cf).unwrap();

        assert_eq!(
            ast,
            Expr::Number {
                span: Span::Optimized,
                val: 42
            }
        );
    }

    #[test]
    fn fold_zero_add() {
        let expr = "foo + 0";
        let mut ast = helpers::rval_from_src(expr);

        let mut cf = ConstantFold;
        ast.visit(&mut cf).unwrap();

        assert_eq!(ast, helpers::inferred_progsym("foo"));
    }

    #[test]
    fn fold_left_assoc() {
        let expr = "(foo + 19) + 4";
        let mut ast = helpers::rval_from_src(expr);

        let mut cf = ConstantFold;
        ast.visit(&mut cf).unwrap();

        let expected = Expr::BinOp {
            span: Span::IgnoredForTesting,
            expr: BinOp {
                lhs: helpers::inferred_progsym("foo").into(),
                op: Verb::Plus,
                rhs: helpers::number(23).into(),
            },
        };
        assert_eq!(ast, expected);
    }
}
