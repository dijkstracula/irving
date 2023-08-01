#[cfg(test)]
mod tests {
    use crate::ast::expressions::*;
    use crate::parser::ivy::{IvyParser, Result, Rule};
    use pest_consume::Parser;

    // Expressions

    fn parse_rval(fragment: &str) -> Result<ExprKind> {
        let res = IvyParser::parse(Rule::rval, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::rval(res)
    }

    #[test]
    fn parse_progsym() {
        let ast = parse_rval("a").unwrap();
        assert_eq!(ast, ExprKind::inferred_progsym("a".into()));
    }

    #[test]
    fn parse_logicvar() {
        parse_rval("X").unwrap();
    }

    #[test]
    fn parse_logicvar_in_subexpr() {
        parse_rval("X + 1").unwrap();
    }

    #[test]
    fn parse_logicvar_in_fnapp() {
        let ast = parse_rval("f(X)").unwrap();

        let arg = ExprKind::LogicSymbol(Symbol {
            id: "X".into(),
            sort: Sort::ToBeInferred,
        });
        let app = AppExpr {
            func: Box::new(ExprKind::ProgramSymbol(Symbol {
                id: "f".into(),
                sort: Sort::ToBeInferred,
            })),
            args: vec![arg],
        };
        let expected = ExprKind::App(app);
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_negation_expr() {
        let _ast = parse_rval("~a").unwrap();
    }

    #[test]
    fn parse_number_expr() {
        let _ast = parse_rval("42").unwrap();
    }

    #[test]
    fn parse_negative_number_expr() {
        let _ast = parse_rval("-42").unwrap();
    }

    #[test]
    fn parse_sub() {
        let _ast = parse_rval("42 - 1").unwrap();
        assert_eq!(
            _ast,
            ExprKind::BinOp(BinOp {
                lhs: Box::new(ExprKind::Number(42)),
                op: Verb::Minus,
                rhs: Box::new(ExprKind::Number(1)),
            })
        );
    }

    #[test]
    fn parse_complex_expr() {
        let fragment = "sock.send(host(1-self).sock.id, val)";
        let _ast = parse_rval(fragment).unwrap();
        println!("{:?}", _ast);
    }

    #[test]
    fn parse_pred() {
        let _ast = parse_rval("i > 0").unwrap();
        assert_eq!(
            _ast,
            ExprKind::BinOp(BinOp {
                lhs: Box::new(ExprKind::inferred_progsym("i".into())),
                op: Verb::Gt,
                rhs: Box::new(ExprKind::Number(0)),
            })
        );
    }

    #[test]
    fn parse_pred_2() {
        let _ast = parse_rval("b = true | b = false").unwrap();
        assert_eq!(
            _ast,
            ExprKind::BinOp(BinOp {
                lhs: Box::new(ExprKind::BinOp(BinOp {
                    lhs: Box::new(ExprKind::inferred_progsym("b".into())),
                    op: Verb::Equals,
                    rhs: Box::new(ExprKind::Boolean(true))
                })),
                op: Verb::Or,
                rhs: Box::new(ExprKind::BinOp(BinOp {
                    lhs: Box::new(ExprKind::inferred_progsym("b".into())),
                    op: Verb::Equals,
                    rhs: Box::new(ExprKind::Boolean(false))
                }))
            })
        );
    }

    #[test]
    fn parse_symbol_with_underscore() {
        assert!(parse_rval("hello_world").is_ok());
    }

    #[test]
    fn parse_dot_expr() {
        let _ast = parse_rval("a.b").unwrap();
        assert_eq!(
            _ast,
            ExprKind::FieldAccess(FieldAccess {
                record: Box::new(ExprKind::inferred_progsym("a".into())),
                field: Symbol {
                    id: "b".into(),
                    sort: Sort::ToBeInferred
                }
            })
        );
    }

    #[test]
    fn parse_unary_fnapp() {
        let _ast = parse_rval("foo(a)").expect("Parsing failed");
    }

    #[test]
    fn parse_fnapp_and_index() {
        let _ast = parse_rval("foo(a).b.c").unwrap();
    }

    #[test]
    fn parse_nested_fnapp() {
        let _ast = parse_rval("sock.send(host(1-self).sock.id, val)").unwrap();
    }

    #[test]
    fn parse_unary_fnapp_and_negation() {
        let _ast = parse_rval("~foo(a)").expect("Parsing failed");
    }

    #[test]
    fn parse_multi_fnapp() {
        let _ast = parse_rval("foo(a, b)").unwrap();
    }

    #[test]
    fn parse_fnapp_in_conj() {
        let _ast = parse_rval("foo(a,b) & b").unwrap();
    }
}
