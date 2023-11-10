#[cfg(test)]
mod tests {
    use std::{rc::Rc, vec};

    use crate::{
        ast::{
            declarations::{Binding, Decl},
            span::Span,
        },
        parser::ivy::{IvyParser, ParserState, Rule},
        tests::helpers,
        typechecker::{
            inference::SortInferer,
            sorts::{self, IvySort, Module, Object},
            unifier::ResolverError,
            TypeError,
        },
        visitor::ast::Visitable,
    };
    use pest_consume::Parser;

    fn typechecker_with_bindings() -> SortInferer {
        // TODO: seems like we should just pull in the actual stdlib.
        let mut tc = SortInferer::new();

        // type pid: 0..3
        tc.bindings
            .append("pid".into(), IvySort::BoundedSequence(0, 3))
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
                                        vec!["dst".into(), "msg".into()],
                                        vec![
                                            tc.bindings.lookup_sym("pid").unwrap().clone(),
                                            IvySort::BitVec(8),
                                        ],
                                        sorts::ActionRet::Unit,
                                    ),
                                ),
                                (
                                    "recv".into(),
                                    IvySort::action_sort(
                                        vec!["src".into(), "msg".into()],
                                        // XXX: why is src a bool?????
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
        let user_data = Rc::new(ParserState::new(file!(), vecimpl));
        let res = IvyParser::parse_with_userdata(Rule::module_decl, vecimpl, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();

        let decl = IvyParser::module_decl(res).expect("Parsing of Vec stub module");
        let vecdecl = Decl::Module { decl };

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
    fn empty_process() {
        let mut iso = helpers::process_from_decl("process p = { }");

        let sort = IvySort::Object(Object {
            args: vec![],
            fields: [("init".to_owned(), Module::init_action_sort())].into(),
        });

        let mut tc = SortInferer::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym("p"), Some(&sort));
    }

    #[test]
    fn proc_with_params() {
        let mut iso = helpers::process_from_decl("process host(self:pid) = {}");
        let sort = IvySort::Object(Object {
            args: [Binding::from(
                "self",
                IvySort::BoundedSequence(0, 3),
                Span::IgnoredForTesting,
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
    fn proc_with_implicit_impl() {
        let mut iso = helpers::process_from_decl(
            "process host(self:pid) = {
            var is_up: bool
        }",
        );
        let sort = IvySort::Object(Object {
            args: [Binding::from(
                "self",
                IvySort::BoundedSequence(0, 3),
                Span::IgnoredForTesting,
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
    fn proc_with_local_var() {
        let mut iso = helpers::process_from_decl(
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

        let mut iso = helpers::process_from_decl(
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
    fn parameterized_object_index() {
        let mut iso = helpers::process_from_decl(
            "process host(self:pid) = {
                var foo: bool;
            
                after init {
                host(0).foo := true;
                }
            }",
        );

        let mut tc = typechecker_with_bindings();
        let _res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
    }

    #[test]
    fn bad_parameterized_object_index() {
        let mut iso = helpers::process_from_decl(
            "process host(self:pid) = {
                var foo: bool;
            
                after init {
                    host(true).foo := true;
                }
            }",
        );

        let mut tc = typechecker_with_bindings();
        let err = iso.visit(&mut tc).unwrap_err();

        assert_eq!(
            err,
            TypeError::Spanned {
                span: Span::IgnoredForTesting,
                inner: Box::new(TypeError::UnificationError(
                    "boolean".into(),
                    "{0..3}".into()
                ))
            }
        );
    }

    #[test]
    fn append1_host() {
        let mut iso = helpers::process_from_decl(
            "process host(self:pid) = {
                alias byte = bv[8]
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
    fn parameterized_obj_index() {
        let mut iso = helpers::process_from_decl(
            "process host(self:pid) = {
                var foo: nat
        }",
        );
        let mut tc = typechecker_with_bindings();
        let iso_sort = iso.visit(&mut tc).unwrap().modifying(&mut iso);

        // Unsurprisingly, this should typecheck to a parameterized object
        assert!(matches!(iso_sort, IvySort::Object(_)));

        // Even though this expression looks like function application, we
        // need to typecheck it as "indexing" into the parameterized object
        // to yield an unparameterized object.
        let mut host_zero = helpers::rval_from_src("host(0)");
        let host_zero_sort = host_zero.visit(&mut tc).unwrap().modifying(&mut host_zero);
        assert!(matches!(host_zero_sort, IvySort::Object(_)));
    }

    #[test]
    fn process_with_ghost_state_assign_lhs() {
        let mut iso = helpers::process_from_decl(
            "process p = {
                specification {
                    var ghost: bool
                }

                after init {
                    # Ghost state should be writable by the implementation.
                    ghost := false;
                }
            }",
        );
        let mut tc = typechecker_with_bindings();
        let _res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
    }

    #[test]
    fn relation_inference() {
        let mut iso = helpers::process_from_decl(
            "process host(self:pid) = {
                # Here, we don't know the types of X and Y...
                relation connected(X, Y)
            
                after init {
                    # ... but here, we know they are nats.
                    connected(0,0) := true;
                    connected(1-1,1+2+3) := (1 = 2);
                }
            }",
        );

        let mut tc = typechecker_with_bindings();
        let _res = iso.visit(&mut tc).unwrap().modifying(&mut iso);
    }
    #[test]
    fn relation_inference_bad_arity() {
        let mut iso = helpers::process_from_decl(
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
        assert_eq!(
            res,
            TypeError::Spanned {
                span: Span::IgnoredForTesting,
                inner: Box::new(TypeError::LenMismatch {
                    expected: 2,
                    actual: 3
                })
            }
        )
    }

    #[test]
    fn relation_inference_bad_assign() {
        let mut iso = helpers::process_from_decl(
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
            ResolverError::UnificationError(IvySort::Bool, IvySort::Number)
                .to_typeerror(&Span::IgnoredForTesting)
        )
    }

    #[test]
    fn relation_bad_inference() {
        let mut iso = helpers::process_from_decl(
            "process host(self:pid) = {
                # Here, we don't know the types of X and Y...
                relation connected(X, Y)
            
                after init {
                    # ... but here, we know they are nats ...
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
            ResolverError::UnificationError(IvySort::Number, IvySort::Bool)
                .to_typeerror(&Span::IgnoredForTesting)
        )
    }

    #[test]
    fn relation_logical_assign() {
        let mut iso = helpers::process_from_decl(
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
