#[cfg(test)]
mod tests {
    use crate::ast::expressions::*;
    use crate::ast::logic::*;
    use crate::parser::ivy::{IvyParser, Result, Rule};
    use pest_consume::Parser;

    fn parse_fmla(fragment: &str) -> Result<Fmla> {
        let res = IvyParser::parse(Rule::fmla, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::fmla(res)
    }

    #[test]
    fn parse_term() {
        parse_fmla("X").expect("Parsing failed");
    }

    #[test]
    fn parse_negation() {
        parse_fmla("~X").expect("Parsing failed");
    }

    #[test]
    fn parse_parens_negation() {
        parse_fmla("~(X)").expect("Parsing failed");
    }

    #[test]
    fn parse_term_with_annotation() {
        parse_fmla("X:int").expect("Parsing failed");
    }

    #[test]
    fn parse_pred() {
        parse_fmla("end(X)").expect("Parsing failed");
    }

    #[test]
    fn parse_logical_implication() {
        println!("{:?}", parse_fmla("X = Y & Y = Z -> X = Z").unwrap());
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
        let ast = parse_fmla("forall X:node,Y,Z. X=Y & Y=Z -> X=Y").expect("parse");
        assert_eq!(
            ast,
            Fmla::Forall(Forall {
                vars: [
                    Param {
                        id: "X".into(),
                        sort: Some(["node".into()].into())
                    },
                    Param {
                        id: "Y".into(),
                        sort: None
                    },
                    Param {
                        id: "Z".into(),
                        sort: None
                    }
                ]
                .into(),
                fmla: Box::new(Fmla::Pred(Expr::BinOp {
                    lhs: Box::new(Expr::BinOp {
                        lhs: Box::new(Expr::BinOp {
                            lhs: Box::new(Expr::Symbol("X".into())),
                            op: Verb::Equals,
                            rhs: Box::new(Expr::BinOp {
                                lhs: Box::new(Expr::Symbol("Y".into())),
                                op: Verb::And,
                                rhs: Box::new(Expr::Symbol("Y".into()))
                            })
                        }),
                        op: Verb::Equals,
                        rhs: Box::new(Expr::BinOp {
                            lhs: Box::new(Expr::Symbol("Z".into())),
                            op: Verb::Arrow,
                            rhs: Box::new(Expr::Symbol("X".into()))
                        })
                    }),
                    op: Verb::Equals,
                    rhs: Box::new(Expr::Symbol("Y".into()))
                }))
            })
        )
    }

    #[test]
    fn parse_existential_quant() {
        assert!(parse_fmla("exists S. S < x").is_ok());
    }

    #[test]
    fn parse_nested_quants() {
        let _ast = parse_fmla(
            "end(X) = end(Y) & (forall I. 0 <= I & I < end(X) -> value(X,I) = value(Y,I))",
        )
        .unwrap();
    }

    #[test]
    fn parse_nested_quants2() {
        parse_fmla("end(X) = end(Y) & forall I. 0 <= I & I < end(X) -> value(X,I) = value(Y,I)")
            .expect("Parsing failed");
    }
}
