#[cfg(test)]
mod tests {
    use crate::{
        ast::expressions::Expr,
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::TypeChecker,
            sorts::{Fargs, IvySort, Module},
            TypeError,
        },
        visitor::visitor::Visitable,
    };
    use pest_consume::Parser;

    #[test]
    fn test_instance_decl_no_args() {
        let prog = "instance socket: net.sock";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        // With an empty context, `net` should produce an unbound identifier error.
        let mut tc = TypeChecker::new();
        let res = decl_ast.visit(&mut tc).expect_err("visit");
        assert_eq!(
            res.downcast::<TypeError>().unwrap(),
            TypeError::UnboundVariable("net".into())
        );

        // If `net` is in the context, it needs to be a module.
        tc.bindings.push_scope();
        tc.bindings.append("net".into(), IvySort::Bool).unwrap();

        let res = decl_ast.visit(&mut tc).expect_err("visit");
        assert_eq!(
            res.downcast::<TypeError>().unwrap(),
            TypeError::NotARecord(IvySort::Bool)
        );
        tc.bindings.pop_scope();

        // If it is module, field lookup needs to succeed.
        tc.bindings.push_scope();
        tc.bindings
            .append(
                "net".into(),
                IvySort::Module(Module {
                    args: vec![],
                    fields: [("not_a_sock".to_owned(), IvySort::Number)].into(),
                }),
            )
            .unwrap();

        let res = decl_ast.visit(&mut tc).expect_err("visit");
        assert_eq!(
            res.downcast::<TypeError>().unwrap(),
            TypeError::UnboundVariable("sock".into())
        );
        tc.bindings.pop_scope();

        // If field lookup succeeds, ensure it's something that can be instantiated (ie. a module or process)
        tc.bindings.push_scope();
        tc.bindings
            .append(
                "net".into(),
                IvySort::Module(Module {
                    args: vec![],
                    fields: [("sock".to_owned(), IvySort::Number)].into(),
                }),
            )
            .unwrap();

        let res = decl_ast.visit(&mut tc).expect_err("visit");
        assert_eq!(
            res.downcast::<TypeError>().unwrap(),
            TypeError::NotInstanceable(IvySort::Number)
        );
        tc.bindings.pop_scope();

        // If field lookup succeeds, and it can be instantiated, do so!
        let sock_mod = IvySort::Module(Module {
            args: vec![],
            fields: [("fd".to_owned(), IvySort::Number)].into(),
        });

        tc.bindings.push_scope();
        tc.bindings
            .append(
                "net".into(),
                IvySort::Module(Module {
                    args: vec![],
                    fields: [("sock".to_owned(), sock_mod.clone())].into(),
                }),
            )
            .unwrap();

        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();

        assert_eq!(res, sock_mod);

        assert_eq!(
            tc.bindings.lookup_sym(&"socket".to_owned()),
            Some(&sock_mod)
        );
        tc.bindings.pop_scope();
    }

    #[test]
    fn test_relation_decl_unannotated() {
        let prog = "relation is_up(X, Y)";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(
            res,
            IvySort::Function(
                Fargs::List(vec!(IvySort::SortVar(1), IvySort::SortVar(2))),
                Box::new(IvySort::Bool)
            )
        );

        assert_eq!(
            tc.bindings.lookup_sym(&"is_up".to_owned()),
            Some(&IvySort::Function(
                Fargs::List(vec!(IvySort::SortVar(1), IvySort::SortVar(2))),
                Box::new(IvySort::Bool)
            ))
        )
    }

    #[test]
    fn test_relation_decl_annotated() {
        let prog = "relation is_up(X: bool, Y: bool)";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(
            res,
            IvySort::Function(
                Fargs::List(vec!(IvySort::Bool, IvySort::Bool)),
                Box::new(IvySort::Bool)
            )
        );

        assert_eq!(
            tc.bindings.lookup_sym(&"is_up".to_owned()),
            Some(&IvySort::Function(
                Fargs::List(vec!(IvySort::Bool, IvySort::Bool)),
                Box::new(IvySort::Bool)
            ))
        )
    }

    #[test]
    fn test_typedecl_uninterp() {
        let prog = "type node";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::Uninterpreted);

        assert_eq!(
            tc.bindings.lookup_sym(&"node".to_owned()),
            Some(&IvySort::Uninterpreted)
        )
    }

    #[test]
    fn test_typedecl_enum() {
        let prog = "type status = {on, off}";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::Enum(["on".into(), "off".into()].into()));

        assert_eq!(
            tc.bindings.lookup_sym(&"status".to_owned()),
            Some(&IvySort::Enum(["on".into(), "off".into()].into()))
        )
    }

    #[test]
    fn test_typedecl_range() {
        let prog = "type numbers = {0..100}";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(
            res,
            IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(100)))
        );

        assert_eq!(
            tc.bindings.lookup_sym(&"numbers".to_owned()),
            Some(&IvySort::Range(
                Box::new(Expr::Number(0)),
                Box::new(Expr::Number(100))
            ))
        )
    }

    #[test]
    fn test_vardecl_inferred() {
        let prog = "var i";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::SortVar(0));

        assert_eq!(
            tc.bindings.lookup_sym(&"i".to_owned()),
            Some(&IvySort::SortVar(0))
        )
    }

    #[test]
    fn test_vardecl_annotated() {
        let prog = "var b: bool";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::Bool);
        assert_eq!(
            tc.bindings.lookup_sym(&"b".to_owned()),
            Some(&IvySort::Bool)
        )
    }
}
