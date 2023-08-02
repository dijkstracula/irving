#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::ast::expressions::*;
    use crate::ast::span::Span;
    use crate::parser::ivy::{IvyParser, Result, Rule};
    use crate::tests::helpers;
    use pest_consume::Parser;

    // Expressions

    fn parse_rval<S>(fragment: S) -> Result<Expr>
    where
        S: Into<Rc<str>>,
    {
        let rc = fragment.into();
        let res = IvyParser::parse_with_userdata(Rule::rval, &rc, rc.clone())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::rval(res)
    }

    #[test]
    fn parse_progsym() {
        let ast = parse_rval("a").unwrap();
        assert_eq!(ast, helpers::inferred_progsym("a"));
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

        let arg = Expr::LogicSymbol {
            span: Span::IgnoredForTesting,
            sym: Symbol::from("X", Sort::ToBeInferred),
        };
        let app = AppExpr {
            func: Box::new(Expr::ProgramSymbol {
                span: Span::IgnoredForTesting,
                sym: Symbol::from("f", Sort::ToBeInferred),
            }),
            args: vec![arg],
        };
        let expected = Expr::App {
            span: Span::IgnoredForTesting,
            expr: app,
        };
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
        let span = Span::IgnoredForTesting;

        let ast = parse_rval("42 - 1").unwrap();
        assert_eq!(
            ast,
            Expr::BinOp {
                span: Span::IgnoredForTesting,
                expr: BinOp {
                    lhs: Box::new(Expr::Number {
                        span: span.clone(),
                        val: 42
                    }),
                    op: Verb::Minus,
                    rhs: Box::new(Expr::Number {
                        span: span.clone(),
                        val: 1
                    })
                }
            }
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
        let ast = parse_rval("i > 0").unwrap();
        assert_eq!(
            ast,
            Expr::BinOp {
                span: Span::IgnoredForTesting,
                expr: BinOp {
                    lhs: Box::new(helpers::inferred_progsym("i")),
                    op: Verb::Gt,
                    rhs: Box::new(Expr::Number {
                        span: Span::IgnoredForTesting,
                        val: 0
                    })
                }
            }
        );
    }

    #[test]
    fn parse_pred_2() {
        let ast = parse_rval("b = true | b = false").unwrap();
        assert_eq!(
            ast,
            Expr::BinOp {
                span: Span::IgnoredForTesting,
                expr: BinOp {
                    lhs: Box::new(Expr::BinOp {
                        span: Span::IgnoredForTesting,
                        expr: BinOp {
                            lhs: Box::new(helpers::inferred_progsym("b")),
                            op: Verb::Equals,
                            rhs: Box::new(Expr::Boolean {
                                span: Span::IgnoredForTesting,
                                val: true
                            })
                        }
                    }),
                    op: Verb::Or,
                    rhs: Box::new(Expr::BinOp {
                        span: Span::IgnoredForTesting,
                        expr: BinOp {
                            lhs: Box::new(helpers::inferred_progsym("b")),
                            op: Verb::Equals,
                            rhs: Box::new(Expr::Boolean {
                                span: Span::IgnoredForTesting,
                                val: false
                            })
                        }
                    }),
                }
            }
        );
    }

    #[test]
    fn parse_conjunction() {
        let ast = parse_rval("X = Y & Y = Z").unwrap();
        let lhs = BinOp {
            lhs: helpers::inferred_logicsym("X").into(),
            op: Verb::Equals,
            rhs: helpers::inferred_logicsym("Y").into(),
        };
        let rhs = BinOp {
            lhs: helpers::inferred_logicsym("Y").into(),
            op: Verb::Equals,
            rhs: helpers::inferred_logicsym("Z").into(),
        };
        let expected = Expr::BinOp {
            span: Span::IgnoredForTesting,
            expr: BinOp {
                lhs: Expr::BinOp {
                    span: Span::IgnoredForTesting,
                    expr: lhs,
                }
                .into(),
                op: Verb::And,
                rhs: Expr::BinOp {
                    span: Span::IgnoredForTesting,
                    expr: rhs,
                }
                .into(),
            },
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_symbol_with_underscore() {
        assert!(parse_rval("hello_world").is_ok());
    }

    #[test]
    fn parse_dot_expr() {
        let ast = parse_rval("a.b").unwrap();
        assert_eq!(
            ast,
            Expr::FieldAccess {
                span: Span::IgnoredForTesting,
                expr: FieldAccess {
                    record: Box::new(helpers::inferred_progsym("a")),
                    field: Symbol::from("b", Sort::ToBeInferred),
                }
            }
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
