#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            actions::Action,
            declarations::{ActionDecl, Binding, Decl, ModuleDecl},
            expressions::Sort,
            statements::Stmt,
        },
        tests::helpers,
        typechecker::{
            inference::SortInferer,
            sorts::{self, IvySort, Module},
        },
        visitor::ast::Visitable,
    };

    #[test]
    fn test_empty_module() {
        let mut iso = helpers::module_from_src("module m = { }");

        let sort = IvySort::Module(Module {
            name: "m".into(),
            args: vec![],
            fields: [("init".into(), Module::init_action_sort())].into(),
        });

        let mut tc = SortInferer::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup_sym("m"), Some(&sort));
    }

    #[test]
    fn test_after_init() {
        let mut iso = helpers::module_from_src(
            "module m = { 
            after init { }
        }",
        );

        let sort = IvySort::Module(Module {
            name: "m".into(),
            args: vec![],
            fields: [("init".into(), Module::init_action_sort())].into(),
        });

        let mut tc = SortInferer::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup_sym("m"), Some(&sort));
    }

    #[test]
    fn test_mod_bind_and_alias_this() {
        let mut iso = helpers::module_from_src(
            "module array(domain, range) = { 
            type this
            alias t = this
        }",
        );

        let sort = IvySort::Module(Module {
            name: "array".into(),
            args: vec![
                ("domain".into(), IvySort::SortVar(1)),
                ("range".into(), IvySort::SortVar(2)),
            ],
            fields: [
                ("this".into(), IvySort::This),
                ("t".into(), IvySort::This),
                ("init".into(), Module::init_action_sort()),
            ]
            .into(),
        });

        let mut tc = SortInferer::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup_sym("array"), Some(&sort));

        // `this` should not escape its scope.
        assert_eq!(tc.bindings.lookup_sym("this"), None);
    }

    #[test]
    fn test_mod_actions() {
        let mut iso = helpers::module_from_src(
            "module array(domain, range) = { 
            type this

            action get(a:this,x:domain) returns (y:range)
            action set(a:this,x:domain,y:range) returns (a:this)
        }",
        );

        let sort = IvySort::Module(Module {
            name: "array".into(),
            args: vec![
                ("domain".into(), IvySort::SortVar(1)),
                ("range".into(), IvySort::SortVar(2)),
            ],
            fields: [
                ("this".into(), IvySort::This),
                (
                    "get".into(),
                    IvySort::action_sort(
                        vec!["a".into(), "x".into()],
                        vec![IvySort::This, IvySort::SortVar(1)],
                        sorts::ActionRet::named("y", IvySort::SortVar(2)),
                    ),
                ),
                (
                    "set".into(),
                    IvySort::action_sort(
                        vec!["a".into(), "x".into(), "y".into()],
                        vec![IvySort::This, IvySort::SortVar(1), IvySort::SortVar(2)],
                        sorts::ActionRet::named("a", IvySort::This),
                    ),
                ),
                ("init".into(), Module::init_action_sort()),
            ]
            .into(),
        });

        let mut tc = SortInferer::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso);

        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup_sym("array"), Some(&sort));

        // `this` should not escape its scope.
        assert_eq!(tc.bindings.lookup_sym("this"), None);
    }

    #[test]
    fn module_multiple_instantiations() {
        let mut prog = helpers::prog_from_decls(
            "
        #lang ivy1.8
        module m(t) = { }          # Internally, `t` will be a SortVar at this point.

        instance mint : m(int)     # We need to ensure that we can unify that sortvar...
        instance mbool : m(bool)   # ...with different sorts at different instantiations.
        ",
        );

        let mut tc = SortInferer::new();
        prog.visit(&mut tc).unwrap().modifying(&mut prog);
    }

    #[test]
    fn multimod_instantiation() {
        // Our dependent module.
        let mut tcp = helpers::tcp_moduledecl();
        let mut tc = SortInferer::new();
        tcp.visit(&mut tc).unwrap().modifying(&mut tcp);

        let mut proc = helpers::process_from_decl(
            "process host = { 
                instance net: tcp.net(nat)
            }",
        );

        proc.visit(&mut tc).unwrap().modifying(&mut proc);
        println!(
            "{:?}",
            tc.bindings.lookup_ident(&vec!["host".into(), "net".into()])
        );
        println!("{:?}", tc.bindings.resolve(&IvySort::SortVar(2)));
    }

    #[test]
    fn instantiation_with_overloaded_numerals() {
        // From http://microsoft.github.io/ivy/language.html :
        //
        // The types of numerals are inferred from context. For example, if x
        // has type foo, then in the expression x+1, the numeral 1 is inferred
        // to have type foo."
        let mut proc = helpers::prog_from_decls(
            "
#lang ivy1.8
module counter(t) = {
    individual val : t
    after init { val := 0 }
}

type foo
instance c : counter(foo)",
        );

        let mut tc = SortInferer::new();
        proc.visit(&mut tc).unwrap().modifying(&mut proc);
    }

    #[test]
    fn instantiation_with_invalid_overloaded_numerals() {
        // From http://microsoft.github.io/ivy/language.html :
        //
        // The types of numerals are inferred from context. For example, if x
        // has type foo, then in the expression x+1, the numeral 1 is inferred
        // to have type foo."
        let mut proc = helpers::prog_from_decls(
            "
#lang ivy1.8
module counter(t) = {
    individual val : t
    after init { val := 0 }
}

instance c : counter(bool)",
        );

        let mut tc = SortInferer::new();
        proc.visit(&mut tc)
            .expect_err("bool should not be unifiable with a numeric sort for argument `t`");
    }

    #[test]
    fn module_extraction() {
        let mut proc = helpers::prog_from_decls(
            "
#lang ivy1.8

module a = { 
    action foo(a: nat)

    action bar = { 
        foo(0) # After typechecking, we should have mutated foo()'s `func_sort` field.
    }   
}",
        );

        let mut si = SortInferer::new();
        proc.visit(&mut si)
            .expect("typechecking")
            .modifying(&mut proc);

        // XXX: I hate this!!!  Lenses when
        let bar = match proc.top.body.get(0) {
            Some(crate::ast::declarations::Decl::Module {
                decl:
                    Binding {
                        decl: ModuleDecl { body, .. },
                        ..
                    },
                ..
            }) => body.get(1).unwrap(),
            _ => panic!("unexpected AST"),
        };
        let bar_impl = match bar {
            Decl::Action {
                decl:
                    Binding {
                        decl:
                            ActionDecl {
                                body: Some(stmts), ..
                            },
                        ..
                    },
                ..
            } => stmts.get(0).unwrap(),
            _ => panic!("unexpected AST"),
        };
        let call_action = match bar_impl {
            Stmt::ActionSequence(stmts) => stmts.get(0).unwrap(),
            _ => panic!("unexpected AST"),
        };
        let app_expr = match call_action {
            Action::Call { action, .. } => action,
            _ => panic!("unexpected AST"),
        };
        assert_eq!(
            app_expr.func_sort,
            Sort::Resolved(IvySort::action_sort(
                vec!["a".into()],
                vec![IvySort::Number],
                sorts::ActionRet::Unit
            ))
        );
    }
}
