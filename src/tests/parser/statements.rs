#[cfg(test)]
mod tests {
    use crate::{
        ast::{actions::*, expressions::*, logic::*, statements::Stmt},
        parser::ivy::{IvyParser, Rule},
    };
    use pest_consume::Parser;

    // Statements

    #[test]
    fn parse_assign() {
        let fragment = "a := b";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::stmt(res).expect("generate ast");
    }

    #[test]
    fn parse_assign_to_relation() {
        let fragment = "r(X, y) := false";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _ast = IvyParser::stmt(res).expect("generate ast");
        println!("{:?}", _ast);
    }

    #[test]
    fn parse_assign_to_relation_2() {
        let fragment = "r(0) := (1 = 1)";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::stmt(res).expect("generate ast");
    }

    #[test]
    fn parse_assign_with_rhs_logicvar() {
        let fragment = "link(x,Y) := Y = y";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::stmt(res).expect("generate ast");
    }

    #[test]
    fn parse_assign_annot() {
        let fragment = "var a:int := b";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::stmt(res).expect("generate ast");
    }

    #[test]
    fn parse_assign_to_field() {
        let fragment = "foo.bar := false";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let ast = IvyParser::stmt(res).expect("generate ast");
        assert_eq!(
            ast,
            Stmt::ActionSequence(
                [Action::Assign(AssignAction {
                    lhs: Expr::FieldAccess(FieldAccess {
                        record: Box::new(Expr::inferred_progsym("foo")),
                        field: Symbol {
                            id: "bar".into(),
                            sort: Sort::ToBeInferred
                        }
                    }),
                    rhs: Expr::Boolean(false)
                })]
                .into()
            )
        );
    }

    #[test]
    fn parse_assign_to_field_of_this() {
        let fragment = "this.bit := false";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _ast = IvyParser::stmt(res).expect("generate ast");
    }

    #[test]
    fn parse_assert_action() {
        let fragment = "assert x < y";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::stmt(res).unwrap();
    }

    #[test]
    fn parse_assume_action() {
        let fragment = "assume x < y";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::stmt(res).unwrap();
    }

    #[test]
    fn parse_ensure_action() {
        let fragment = "ensure ~failed(y)";
        let _res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
    }

    #[test]
    fn parse_ensure_action_2() {
        let fragment = "ensure msg_count = 0 -> host(0).contents.eq(host(1).contents)";
        let _res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
    }

    #[test]
    fn parse_requires_action() {
        let fragment = "require ~failed(y)";
        let res = IvyParser::parse(Rule::requires_action, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();

        let stmt = IvyParser::requires_action(res).unwrap();
        assert_eq!(
            stmt,
            RequiresAction {
                pred: Fmla::Pred(Expr::UnaryOp {
                    op: Verb::Not,
                    expr: Box::new(Expr::App(AppExpr {
                        func: Box::new(Expr::inferred_progsym("failed".to_owned())),
                        args: [Expr::inferred_progsym("y")].into()
                    }))
                })
            }
        )
    }

    #[test]
    fn parse_requires_action_2() {
        let fragment = "require x >= 0";
        let res = IvyParser::parse(Rule::requires_action, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let stmt = IvyParser::requires_action(res).unwrap();
        assert_eq!(
            stmt,
            RequiresAction {
                pred: Fmla::Pred(Expr::BinOp(BinOp {
                    lhs: Box::new(Expr::inferred_progsym("x")),
                    op: Verb::Ge,
                    rhs: Box::new(Expr::Number(0))
                }))
            }
        );
    }

    #[test]
    fn parse_if_stmt() {
        let fragment = "if 1 < 2 { a := 42; }";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::stmt(res).unwrap();
    }

    #[test]
    fn parse_stmt_block() {
        let fragment = "requires a = 0; a := 42; ensure a = 42";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        println!("{:?}", IvyParser::stmt(res).unwrap());
    }

    #[test]
    fn parse_if_stmt_with_else() {
        let fragment = "if 1 < 2 { a := 42; } else { a := 43; }";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert!(IvyParser::stmt(res).is_ok());
    }

    #[test]
    fn parse_while() {
        let fragment = "while 1 < 2 { a := a + 1; }";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::stmt(res).unwrap();
    }

    #[test]
    fn parse_multi_stmt_while() {
        let fragment = "while i > 0 {
            sum := sum + f(i);
            i := i - 1
        }";

        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let res = IvyParser::stmt(res).unwrap();
        println!("{:?}", res);
    }
}
