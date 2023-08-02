#[cfg(test)]
mod tests {
    use crate::{
        ast::expressions::Expr,
        parser::ivy::{IvyParser, Rule},
        tests::helpers,
        typechecker::{
            inference::SortInferer,
            sorts::{IvySort, Module},
            TypeError,
        },
        visitor::ast::Visitable,
    };
    use pest_consume::Parser;

    #[test]
    fn test_ensure() {
        let prog = "ensure true";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        decl_ast.visit(&mut tc).expect("sort inference");
    }

    #[test]
    fn test_ensure_bad() {
        let prog = "ensure 1+1";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        decl_ast.visit(&mut tc).expect_err("visit");
    }

    #[test]
    fn test_ensure_quantified() {
        let prog = "ensure forall X . X = X";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        decl_ast.visit(&mut tc).expect_err("visit");
    }

    #[test]
    fn test_require() {
        let prog = "require true";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        decl_ast.visit(&mut tc).expect("sort inference");
    }

    #[test]
    fn test_require_bad() {
        let prog = "require 1+1";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        decl_ast.visit(&mut tc).expect_err("visit");
    }

    #[test]
    fn test_instance_decl_no_args() {
        let prog = "instance socket: net.sock";
        let mut decl_ast = helpers::decl_from_src(prog);

        // With an empty context, `net` should produce an unbound identifier error.
        let mut tc = SortInferer::new();
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
            TypeError::NotARecord(IvySort::Bool.desc())
        );
        tc.bindings.pop_scope();

        // If it is module, field lookup needs to succeed.
        tc.bindings.push_scope();
        tc.bindings
            .append(
                "net".into(),
                IvySort::Module(Module {
                    name: "net".into(),
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
                    name: "net".into(),
                    args: vec![],
                    fields: [("sock".to_owned(), IvySort::Number)].into(),
                }),
            )
            .unwrap();

        let res = decl_ast.visit(&mut tc).expect_err("visit");
        assert_eq!(
            res.downcast::<TypeError>().unwrap(),
            TypeError::NotInstanceable(IvySort::Number.desc())
        );
        tc.bindings.pop_scope();

        // If field lookup succeeds, and it can be instantiated, do so!
        let sock_mod = IvySort::Module(Module {
            name: "socket".into(),
            args: vec![],
            fields: [("fd".to_owned(), IvySort::Number)].into(),
        });

        tc.bindings.push_scope();
        tc.bindings
            .append(
                "net".into(),
                IvySort::Module(Module {
                    name: "net".into(),
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

        assert_eq!(tc.bindings.lookup_sym("socket"), Some(&sock_mod));
        tc.bindings.pop_scope();
    }

    #[test]
    fn test_relation_decl_unannotated() {
        let prog = "relation is_up(X, Y)";

        let parsed = IvyParser::parse(Rule::decl, prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(
            res,
            IvySort::Relation(vec!(IvySort::SortVar(1), IvySort::SortVar(2)))
        );

        assert_eq!(
            tc.bindings.lookup_sym("is_up"),
            Some(&IvySort::Relation(vec!(
                IvySort::SortVar(1),
                IvySort::SortVar(2)
            )))
        )
    }

    #[test]
    fn test_relation_decl_annotated() {
        let prog = "relation is_up(X: bool, Y: bool)";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::Relation(vec!(IvySort::Bool, IvySort::Bool)));

        assert_eq!(
            tc.bindings.lookup_sym("is_up"),
            Some(&IvySort::Relation(vec!(IvySort::Bool, IvySort::Bool)))
        )
    }

    #[test]
    fn test_typedecl_bv() {
        let prog = "type addr = bv[32]";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::BitVec(32));

        assert_eq!(tc.bindings.lookup_sym("addr"), Some(&IvySort::BitVec(32)));

        let prog = "type addr = bv[256]";
        let res = IvyParser::parse(Rule::decl, prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).expect_err("Bit vectors bigger than 255 are disallowed");
    }

    #[test]
    fn test_typedecl_this() {
        let prog = "type this";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::This);
        assert_eq!(tc.bindings.lookup_sym("this"), Some(&IvySort::This))
    }

    #[test]
    fn test_typedecl_uninterp() {
        let prog = "type node";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::SortVar(0));

        assert_eq!(tc.bindings.lookup_sym("node"), Some(&IvySort::SortVar(0)))
    }

    #[test]
    fn test_typedecl_enum() {
        let prog = "type status = {on, off}";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::Enum(["on".into(), "off".into()].into()));

        assert_eq!(
            tc.bindings.lookup_sym("status"),
            Some(&IvySort::Enum(["on".into(), "off".into()].into()))
        )
    }

    #[test]
    fn test_typedecl_range() {
        let prog = "type numbers = {0..100}";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
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
            tc.bindings.lookup_sym("numbers"),
            Some(&IvySort::Range(
                Box::new(Expr::Number(0)),
                Box::new(Expr::Number(100))
            ))
        )
    }

    #[test]
    fn test_vardecl_inferred() {
        let prog = "var i";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::SortVar(0));

        assert_eq!(tc.bindings.lookup_sym("i"), Some(&IvySort::SortVar(0)))
    }

    #[test]
    fn test_vardecl_annotated() {
        let prog = "var b: bool";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::Bool);
        assert_eq!(tc.bindings.lookup_sym("b"), Some(&IvySort::Bool))
    }

    #[test]
    fn test_declblock() {
        let prog = "{
            type client
            type server

            relation link(X:client, Y:server)
            relation semaphore(X:server) # X was previously defined to be a client.
        }";
        let parsed = IvyParser::parse(Rule::decl_block, prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl_block(parsed).expect("AST generation failed");
        println!("{:?}", decl_ast);

        let mut tc = SortInferer::new();
        decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
    }
}
