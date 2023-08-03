#[cfg(test)]
mod tests {
    use std::{collections::BTreeMap, vec};

    use crate::{
        ast::{declarations::Decl, expressions::Expr},
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::SortInferer,
            sorts::{self, IvySort, Module, Object},
            TypeError,
        },
        visitor::ast::Visitable,
    };
    use pest_consume::Parser;

    fn process_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse_with_userdata(Rule::process_decl, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        Decl::Object(IvyParser::process_decl(res).expect("AST generation failed"))
    }

    fn typechecker_with_bindings() -> SortInferer {
        let mut tc = SortInferer::new();

        // type pid: 0..3
        tc.bindings
            .append(
                "pid".into(),
                IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(3))),
            )
            .unwrap();
        // interpret byte -> bv[8]
        tc.bindings
            .append("byte".into(), IvySort::BitVec(8))
            .unwrap();

        tc.bindings
            .append(
                "net".into(),
                IvySort::Module(Module {
                    name: "net".into(),
                    args: [].into(),
                    fields: [(
                        "socket".into(),
                        IvySort::Module(Module {
                            name: "socket".into(),
                            args: vec![],
                            fields: [
                                ("id".into(), tc.bindings.lookup_sym("pid").unwrap().clone()),
                                (
                                    "send".into(),
                                    IvySort::action_sort(
                                        vec!["dst".into()],
                                        vec![tc.bindings.lookup_sym("pid").unwrap().clone()],
                                        sorts::ActionRet::Unit,
                                    ),
                                ),
                                (
                                    "recv".into(),
                                    IvySort::action_sort(
                                        vec!["src".into(), "msg".into()],
                                        vec![IvySort::Bool, IvySort::BitVec(8)],
                                        sorts::ActionRet::Unit,
                                    ),
                                ),
                            ]
                            .into(),
                        }),
                    )]
                    .into(),
                }),
            )
            .unwrap();

        // TODO: this would be a good test for the module instantiator in its own right.
        let vecimpl = "module vec(elems) = {
            type this
            alias t = this

            action empty returns (a: t)

            action append(a:t,e:elems) returns (b:t)
        }";
        let parsed = IvyParser::parse_with_userdata(Rule::module_decl, vecimpl, vecimpl.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        let vecdecl = Decl::Module(IvyParser::module_decl(parsed).expect("AST generation failed"));

        let mut filedecl = vecdecl;
        /*
        let mut mr = ModuleInstantiation::new([("elems".into(), vec!["byte".into()])].into());
        filedecl
            .visit(&mut mr)
            .unwrap()
            .modifying(&mut filedecl)
            .unwrap();
        */

        let filesort = filedecl.visit(&mut tc).unwrap().modifying(&mut filedecl);

        tc.bindings.append("file".into(), filesort).unwrap();
        tc
    }

    #[test]
    fn test_empty_process() {
        let mut iso = process_from_src("process p = { }");

        let sort = IvySort::Object(Object {
            args: BTreeMap::from([]),
            fields: [("init".to_owned(), Module::init_action_sort())].into(),
        });

        let mut tc = SortInferer::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym("p"), Some(&sort));
    }

    #[test]
    fn test_proc_with_params() {
        let mut iso = process_from_src("process host(self:pid) = {}");
        let sort = IvySort::Object(Object {
            args: [(
                "self".into(),
                IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(3))),
            )]
            .into(),
            fields: [("init".to_owned(), Module::init_action_sort())].into(),
        });

        let mut tc = typechecker_with_bindings();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym("host"), Some(&sort));
    }

    #[test]
    fn test_proc_with_implicit_impl() {
        let mut iso = process_from_src(
            "process host(self:pid) = {
            var is_up: bool
        }",
        );
        let sort = IvySort::Object(Object {
            args: [(
                "self".into(),
                IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(3))),
            )]
            .into(),
            fields: [
                ("is_up".into(), IvySort::Bool),
                ("init".to_owned(), Module::init_action_sort()),
            ]
            .into(),
        });

        let mut tc = typechecker_with_bindings();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym("host"), Some(&sort),);
    }

    #[test]
    fn test_proc_with_local_var() {
        let mut iso = process_from_src(
            "process host(self:pid) = {
                var foo: bool;
                action doit = {
                    foo := true;
                }
            }
        ",
        );

        let mut tc = typechecker_with_bindings();
        let _ = iso.visit(&mut tc).unwrap().modifying(&mut iso);

        let mut iso = process_from_src(
            "process host(self:pid) = {
                var foo: bool;
                action doit = {
                    foo := 42; # uh oh!!
                }
            }
        ",
        );
        let _ = iso
            .visit(&mut tc)
            .expect_err("Bool should not be unifiable with Number");
    }

    #[test]
    fn test_bad_parameterized_object_index() {
        let mut iso = process_from_src(
            "process host(self:pid) = {
                var foo: bool;
            
                after init {
                host(true).foo := true;
                }
            }",
        );

        let mut tc = typechecker_with_bindings();
        let _res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
    }

    #[test]
    fn test_append1_host() {
        let mut iso = process_from_src(
            "process host(self:pid) = {
                export action append(val: byte)
                import action show(content: file)
                instance sock: net.socket
                var contents: file

                after init {
                    contents := file.empty();
                }

                implement append {
                    contents := contents.append(val);
                    sock.send(host(1-self).sock.id, val);
                    show(contents);
                }

                implement sock.recv(src: bool, val:byte) {
                    contents := contents.append(val);
                    show(contents);
                }
        }",
        );

        let mut tc = typechecker_with_bindings();
        let _res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
    }

    #[test]
    fn test_relation_inference() {
        let mut iso = process_from_src(
            "process host(self:pid) = {
                # Here, we don't know the types of X and Y...
                relation connected(X, Y)
            
                after init {
                    # ... but here, we know they are unbounded_sequences.
                    connected(0,0) := true;
                    connected(1-1,1+2+3) := (1 = 2);
                }
            }",
        );

        let mut tc = typechecker_with_bindings();
        let _res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
    }
    #[test]
    fn test_relation_inference_bad_arity() {
        let mut iso = process_from_src(
            "process host(self:pid) = {
                relation connected(X, Y)
            
                after init {
                    # connected is a two-tuple
                    connected(0,0,0) := true;
                }
            }",
        );

        let mut tc = typechecker_with_bindings();
        let res = iso.visit(&mut tc).expect_err("Should get a type error");
        let TypeError::SortListMismatch(_, _) = res else {
            unreachable!()
        };
    }

    #[test]
    fn test_relation_inference_bad_assign() {
        let mut iso = process_from_src(
            "process host(self:pid) = {
                relation connected(X, Y)
            
                after init {
                    # We can't assign a non-Boolean expression to a relation.
                    connected(0,0) := 42;
                }
            }",
        );

        let mut tc = typechecker_with_bindings();
        let res = iso.visit(&mut tc).expect_err("Should get a type error");
        assert_eq!(
            res,
            TypeError::UnificationError(IvySort::Bool, IvySort::Number)
        )
    }

    #[test]
    fn test_relation_bad_inference() {
        let mut iso = process_from_src(
            "process host(self:pid) = {
                # Here, we don't know the types of X and Y...
                relation connected(X, Y)
            
                after init {
                    # ... but here, we know they are unbounded_sequences ...
                    connected(0,0) := true;

                    # ... which means they can't also be booleans here!
                    connected(false, true) := true;
                }
            }",
        );

        let mut tc = typechecker_with_bindings();
        let res = iso.visit(&mut tc).expect_err("Should get a type error");
        assert_eq!(
            res,
            TypeError::UnificationError(IvySort::Bool, IvySort::Number)
        )
    }

    #[test]
    fn test_relation_logical_assign() {
        let mut iso = process_from_src(
            "process host(self:pid) = {
                relation connected(X, Y)
            
                after init {
                    connected(X,Y) := false;
                }
            }",
        );

        let mut tc = typechecker_with_bindings();
        let _res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
    }
}
