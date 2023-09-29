#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{
        ast::span::Span,
        parser::ivy::{IvyParser, Rule},
        tests::helpers,
        typechecker::{
            inference::SortInferer,
            sorts::{Class, IvySort, Module},
            subst::SortSubstituter,
            unifier::ResolverError,
            TypeError,
        },
        visitor::ast::Visitable,
    };
    use pest_consume::Parser;

    #[test]
    fn class() {
        let fragment = "class pt = {
            field x: unbounded_sequence
            field y: unbounded_sequence
            action norm(self: pt) returns (n: unbounded_sequence)
        }";
        let mut decl_ast = helpers::decl_from_src(fragment);

        // Phase 1
        let mut tc = SortInferer::new();
        let sort = decl_ast
            .visit(&mut tc)
            .expect("sort inference")
            .modifying(&mut decl_ast);

        // Phase 2
        let mut ss = SortSubstituter::from_inferer(tc);
        decl_ast
            .visit(&mut ss)
            .expect("sort inference")
            .modifying(&mut decl_ast);

        let sort = match sort {
            IvySort::Class(cls) => cls,
            _ => panic!(),
        };

        assert!(matches!(sort, Class { parent: None, .. }));
        assert_eq!(
            sort.fields,
            [("x".into(), IvySort::Number), ("y".into(), IvySort::Number)].into()
        );
    }

    #[test]
    fn subclass_invalid_inheritance() {
        let fragment = "subclass pt of unbounded_sequence = {
            field z: unbounded_sequence
            action norm(self: pt) returns (n: unbounded_sequence)
        }";
        let mut decl_ast = helpers::decl_from_src(fragment);

        let mut tc = SortInferer::new();
        let sort = decl_ast.visit(&mut tc).expect_err("typecheck");

        let err = match sort {
            TypeError::Spanned { inner, .. } => *inner,
            _ => panic!(),
        };
        assert_eq!(err, TypeError::NonClassInheritance(IvySort::Number));
    }

    #[test]
    fn subclass() {
        let fragment = "subclass pt3 of pt = {
            field z: unbounded_sequence
            action norm(self: pt) returns (n: unbounded_sequence)
        }";
        let mut decl_ast = helpers::decl_from_src(fragment);

        // Phase 1
        let mut tc = SortInferer::new();

        tc.bindings
            .append(
                "pt".into(),
                IvySort::Class(Class {
                    parent: None,
                    fields: [("x".into(), IvySort::Number), ("y".into(), IvySort::Number)].into(),
                    actions: [].into(),
                }),
            )
            .unwrap();

        let sort = decl_ast
            .visit(&mut tc)
            .expect("sort inference")
            .modifying(&mut decl_ast);

        // Phase 2
        let mut ss = SortSubstituter::from_inferer(tc);
        decl_ast
            .visit(&mut ss)
            .expect("sort inference")
            .modifying(&mut decl_ast);

        let sort = match sort {
            IvySort::Class(cls) => cls,
            _ => panic!(),
        };

        assert!(matches!(
            sort,
            Class {
                parent: Some(_),
                ..
            }
        ));
        assert_eq!(
            sort.fields,
            [
                ("x".into(), IvySort::Number),
                ("y".into(), IvySort::Number),
                ("z".into(), IvySort::Number)
            ]
            .into()
        );
    }

    #[test]
    fn ensure() {
        let prog = "ensure true";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        decl_ast.visit(&mut tc).expect("sort inference");
    }

    #[test]
    fn ensure_bad() {
        let prog = "ensure 1+1";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        decl_ast.visit(&mut tc).expect_err("visit");
    }

    #[test]
    fn ensure_quantified() {
        let prog = "ensure forall X . X = X";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        decl_ast.visit(&mut tc).expect("visit");
    }

    #[test]
    fn require() {
        let prog = "require true";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        decl_ast.visit(&mut tc).expect("sort inference");
    }

    #[test]
    fn require_bad() {
        let prog = "require 1+1";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        decl_ast.visit(&mut tc).expect_err("visit");
    }

    #[test]
    fn instance_decl_no_args() {
        let prog = "instance socket: net.sock";
        let mut decl_ast = helpers::decl_from_src(prog);

        // With an empty context, `net` should produce an unbound identifier error.
        let mut tc = SortInferer::new();
        let res = decl_ast.visit(&mut tc).expect_err("visit");
        assert_eq!(
            res,
            ResolverError::UnboundVariable("net".into()).to_typeerror(&Span::Todo)
        );

        // If `net` is in the context, it needs to be a module.
        tc.bindings.push_scope();
        tc.bindings.append("net".into(), IvySort::Bool).unwrap();

        let res = decl_ast.visit(&mut tc).expect_err("visit");
        assert_eq!(
            res,
            ResolverError::NotARecord(IvySort::Bool).to_typeerror(&Span::Todo)
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
            res,
            ResolverError::UnboundVariable("sock".into()).to_typeerror(&Span::Todo)
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
            res,
            TypeError::Spanned {
                span: Span::Todo,
                inner: Box::new(TypeError::NotInstanceable(IvySort::Number.desc()))
            }
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
            .modifying(&mut decl_ast);

        assert_eq!(res, sock_mod);

        assert_eq!(tc.bindings.lookup_sym("socket"), Some(&sock_mod));
        tc.bindings.pop_scope();
    }

    #[test]
    fn relation_decl_unannotated() {
        let prog = "relation is_up(X, Y)";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);
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
    fn relation_decl_annotated() {
        let prog = "relation is_up(X: bool, Y: bool)";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);
        assert_eq!(res, IvySort::Relation(vec!(IvySort::Bool, IvySort::Bool)));

        assert_eq!(
            tc.bindings.lookup_sym("is_up"),
            Some(&IvySort::Relation(vec!(IvySort::Bool, IvySort::Bool)))
        )
    }

    #[test]
    fn alias_bv() {
        let prog = "alias addr = bv[32]";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);
        assert_eq!(res, IvySort::BitVec(32));

        assert_eq!(tc.bindings.lookup_sym("addr"), Some(&IvySort::BitVec(32)));

        let prog = "alias addr = bv[256]";
        let res = IvyParser::parse_with_userdata(Rule::decl, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).expect_err("Bit vectors bigger than 255 are disallowed");
    }

    #[test]
    fn typedecl_bv() {
        let prog = "type addr = bv[32]";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);
        assert_eq!(res, IvySort::BitVec(32));

        assert_eq!(tc.bindings.lookup_sym("addr"), Some(&IvySort::BitVec(32)));

        let prog = "type addr = bv[256]";
        let res = IvyParser::parse_with_userdata(Rule::decl, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).expect_err("Bit vectors bigger than 255 are disallowed");
    }

    #[test]
    fn typedecl_this() {
        let prog = "type this";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);
        assert_eq!(res, IvySort::This);
        assert_eq!(tc.bindings.lookup_sym("this"), Some(&IvySort::This))
    }

    #[test]
    fn typedecl_uninterp() {
        let prog = "type node";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);
        assert_eq!(res, IvySort::SortVar(0));

        assert_eq!(tc.bindings.lookup_sym("node"), Some(&IvySort::SortVar(0)))
    }

    #[test]
    fn typedecl_enum() {
        let prog = "type status = {on, off}";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);
        assert_eq!(res, IvySort::Enum(["on".into(), "off".into()].into()));

        assert_eq!(
            tc.bindings.lookup_sym("status"),
            Some(&IvySort::Enum(["on".into(), "off".into()].into()))
        )
    }

    #[test]
    fn typedecl_range() {
        let prog = "type numbers = {0..100}";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);

        assert_eq!(res, IvySort::Range(0, 100));

        assert_eq!(
            tc.bindings.lookup_sym("numbers"),
            Some(&IvySort::Range(0, 100))
        )
    }

    #[test]
    fn vardecl_inferred() {
        let prog = "var i";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);
        assert_eq!(res, IvySort::SortVar(0));

        assert_eq!(tc.bindings.lookup_sym("i"), Some(&IvySort::SortVar(0)))
    }

    #[test]
    fn vardecl_annotated() {
        let prog = "var b: bool";
        let mut decl_ast = helpers::decl_from_src(prog);

        let mut tc = SortInferer::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);
        assert_eq!(res, IvySort::Bool);
        assert_eq!(tc.bindings.lookup_sym("b"), Some(&IvySort::Bool))
    }

    #[test]
    fn declblock() {
        let prog = "{
            type client
            type server

            relation link(X:client, Y:server)
            relation semaphore(X:server) # X was previously defined to be a client.
        }";
        let parsed = IvyParser::parse_with_userdata(Rule::decl_block, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl_block(parsed).expect("AST generation failed");

        let mut tc = SortInferer::new();
        decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast);
    }

    #[test]
    fn var_redefinition() {
        let prog = "{
            var foo: unbounded_sequence
            var foo: bool
        }";
        let parsed = IvyParser::parse_with_userdata(Rule::decl_block, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl_block(parsed).expect("AST generation failed");

        let mut tc = SortInferer::new();
        let err = decl_ast.visit(&mut tc).expect_err("visit");

        assert_eq!(
            err,
            ResolverError::ReboundVariable {
                sym: "foo".into(),
                prev: IvySort::Number,
                new: IvySort::Bool
            }
            .to_typeerror(&Span::IgnoredForTesting)
        );
    }
}
