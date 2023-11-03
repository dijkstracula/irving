#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::ast::expressions::*;
    use crate::ast::logic;
    use crate::ast::logic::*;
    use crate::ast::span::Span;
    use crate::parser::ivy::ParserState;
    use crate::parser::ivy::{IvyParser, Result, Rule};
    use crate::tests::helpers;
    use pest_consume::Parser;

    fn parse_fmla<S>(fragment: S) -> Result<Fmla>
    where
        S: Into<String>,
    {
        let fragment = fragment.into();
        let user_data = Rc::new(ParserState::new(file!(), fragment.to_string()));
        let res = IvyParser::parse_with_userdata(Rule::fmla, &fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::fmla(res)
    }

    #[test]
    fn parse_bool() {
        let ast = parse_fmla("true").unwrap();
        let expected = Fmla::Boolean {
            span: Span::IgnoredForTesting,
            val: true,
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_term() {
        let ast = parse_fmla("X").unwrap();
        let expected = helpers::inferred_logicsym("X");
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_number() {
        let ast = parse_fmla("123").unwrap();
        let expected = Fmla::Number {
            span: Span::IgnoredForTesting,
            val: 123,
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_logicvar_in_subexpr() {
        let ast = parse_fmla("X + 1").unwrap();
        assert_eq!(
            ast,
            Fmla::BinOp {
                span: Span::IgnoredForTesting,
                binop: LogicBinOp {
                    lhs: Box::new(helpers::inferred_logicsym("X")),
                    op: Verb::Plus,
                    rhs: Box::new(helpers::logical_number(1))
                }
            }
        )
    }

    #[test]
    fn parse_negation() {
        let ast = parse_fmla("~X").unwrap();
        let expected = Fmla::UnaryOp {
            span: Span::IgnoredForTesting,
            op: Verb::Not,
            fmla: Box::new(helpers::inferred_logicsym("X")),
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_parens_negation() {
        let ast = parse_fmla("~(X)").expect("Parsing failed");
        let expected = Fmla::UnaryOp {
            span: Span::IgnoredForTesting,
            op: Verb::Not,
            fmla: Box::new(helpers::inferred_logicsym("X")),
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_logicvar_in_fnapp() {
        let ast = parse_fmla("f(X)").unwrap();

        let arg = Fmla::LogicSymbol {
            span: Span::IgnoredForTesting,
            sym: Symbol::from("X", Sort::ToBeInferred, Span::IgnoredForTesting),
        };
        let app = LogicApp {
            func: Box::new(Fmla::ProgramSymbol {
                span: Span::IgnoredForTesting,
                sym: Symbol::from("f", Sort::ToBeInferred, Span::IgnoredForTesting),
            }),
            args: vec![arg],
        };
        let expected = Fmla::App {
            span: Span::IgnoredForTesting,
            app,
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_progvar_in_fnapp() {
        let ast = parse_fmla("f(x)").unwrap();

        let arg = Fmla::ProgramSymbol {
            span: Span::IgnoredForTesting,
            sym: Symbol::from("x", Sort::ToBeInferred, Span::IgnoredForTesting),
        };
        let app = LogicApp {
            func: Box::new(Fmla::ProgramSymbol {
                span: Span::IgnoredForTesting,
                sym: Symbol::from("f", Sort::ToBeInferred, Span::IgnoredForTesting),
            }),
            args: vec![arg],
        };
        let expected = Fmla::App {
            span: Span::IgnoredForTesting,
            app,
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_term_with_annotation() {
        parse_fmla("X:int").expect("Parsing failed");
    }

    #[test]
    fn parse_pred_progsym() {
        parse_fmla("end(x)").expect("Parsing failed");
    }

    #[test]
    fn parse_pred_logicsym() {
        parse_fmla("end(X)").expect("Parsing failed");
    }

    #[test]
    fn parse_conjunction() {
        let ast = parse_fmla("X & Y").unwrap();
        let expected = Fmla::BinOp {
            span: Span::IgnoredForTesting,
            binop: LogicBinOp {
                lhs: helpers::inferred_logicsym("X").into(),
                op: Verb::And,
                rhs: helpers::inferred_logicsym("Y").into(),
            },
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_logical_implication() {
        let _ast = parse_fmla("X = Y & Y = Z -> X = Z").unwrap();
    }

    #[test]
    fn parse_nested_implications() {
        let _ast = parse_fmla("forall X, Y. X = 0 -> Y = 0 -> X = Y").unwrap();
    }

    #[test]
    fn parse_nested_implications_2() {
        let fragment = "msg_count = 0 -> 
            host(X).contents.end() = host(Y).contents.end() & 
            (IDX < host(X).contents.end() -> host(X).contents.get(I) = host(Y).contents.get(I))";
        let ast = parse_fmla(fragment).unwrap();

        println!("{:?}", ast);
    }

    #[test]
    fn parse_universal_quant() {
        let _ast = parse_fmla("forall X . X").expect("Parse");
        println!("{:?}", _ast);
    }

    #[test]
    fn parse_universal_quant2() {
        parse_fmla("forall X,Y,Z . X = Y & Y = Z -> X = Z").unwrap();
    }

    #[test]
    fn parse_universal_quant3() {
        let ast = parse_fmla("forall X:node,Y,Z. X=Y & Y=Z -> X=Z").expect("parse");

        let x_equals_y = logic::LogicBinOp {
            lhs: helpers::inferred_logicsym("X").into(),
            op: Verb::Equals,
            rhs: helpers::inferred_logicsym("Y").into(),
        };
        let y_equals_z = logic::LogicBinOp {
            lhs: helpers::inferred_logicsym("Y").into(),
            op: Verb::Equals,
            rhs: helpers::inferred_logicsym("Z").into(),
        };

        let antecedent = Fmla::BinOp {
            span: Span::IgnoredForTesting,
            binop: logic::LogicBinOp {
                lhs: Fmla::BinOp {
                    span: Span::IgnoredForTesting,
                    binop: x_equals_y,
                }
                .into(),
                op: Verb::And,
                rhs: Fmla::BinOp {
                    span: Span::IgnoredForTesting,
                    binop: y_equals_z,
                }
                .into(),
            },
        };

        let consequence = Fmla::BinOp {
            span: Span::IgnoredForTesting,
            binop: LogicBinOp {
                lhs: helpers::inferred_logicsym("X").into(),
                op: Verb::Equals,
                rhs: helpers::inferred_logicsym("Z").into(),
            },
        };

        let implication = Fmla::BinOp {
            span: Span::IgnoredForTesting,
            binop: LogicBinOp {
                lhs: Box::new(antecedent),
                op: Verb::Arrow,
                rhs: Box::new(consequence),
            },
        };

        assert_eq!(
            ast,
            Fmla::Forall {
                span: Span::IgnoredForTesting,
                fmla: Forall {
                    vars: vec![
                        Symbol::from(
                            "X",
                            Sort::Annotated(vec!["node".into()]),
                            Span::IgnoredForTesting
                        ),
                        Symbol::from("Y", Sort::ToBeInferred, Span::IgnoredForTesting),
                        Symbol::from("Z", Sort::ToBeInferred, Span::IgnoredForTesting),
                    ],
                    fmla: Box::new(implication)
                }
            }
        );
    }

    #[test]
    fn parse_existential_quant() {
        assert!(parse_fmla("exists S. S < x").is_ok());
    }

    #[test]
    fn parse_nested_quants() {
        let _ast = parse_fmla("end(X) & forall I. 0 <= I & value(X,I)").unwrap();
        println!("{:?}", _ast);
    }

    #[test]
    fn parse_nested_quants2() {
        let _ast = parse_fmla(
            "end(X) = end(Y) & forall I. 0 <= I & I < end(X) -> value(X,I) = value(Y,I)",
        )
        .expect("Parsing failed");
    }

    #[test]
    fn dot_on_quant() {
        parse_fmla("(forall X . X).bar").expect_err("Can't compose formulae like this");
        parse_fmla("foo.(forall X. X)").expect_err("Can't compose formulae like this");
    }
}
