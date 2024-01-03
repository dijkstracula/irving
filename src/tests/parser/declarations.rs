#[cfg(test)]
mod tests {
    use std::{path::PathBuf, rc::Rc};

    use crate::{
        ast::{
            declarations::{self, Binding, Decl, ModuleDecl},
            expressions::Sort,
            span::Span,
        },
        parser::ivy::{IvyParser, ParserState, Rule},
        tests::helpers,
        typechecker::sorts::IvySort,
    };
    use pest_consume::Parser;

    // Declarations

    #[test]
    fn parse_decl_sig() {
        let fragment = "foo(a: int) returns (b: int)";
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), fragment));
        let res = IvyParser::parse_with_userdata(Rule::decl_sig, fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl_sig(res).expect("AST generation failed");
    }

    #[test]
    fn parse_alias_decl() {
        let fragment = "alias byte = bv[8]";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_attribute() {
        let fragment = "attribute bmc[10]";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_attribute_assign() {
        let fragment = "attribute foo.weight = \"0.1\"";
        let ast = helpers::decl_from_src(fragment);
        println!("{:?}", ast);
    }

    #[test]
    fn parse_axiom() {
        let fragment = "axiom X:id < Y";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_axiom_2() {
        let fragment = "axiom host(0).sock.id ~= host(1).sock.id";
        let _ast = helpers::decl_from_src(fragment);
        println!("{:?}", _ast);
    }

    #[test]
    fn parse_class_decl() {
        let fragment = "class pt = {
            field x: nat
            field y: nat
            action norm(self: pt) returns (n: nat)
        }";
        let ast = helpers::decl_from_src(fragment);
        assert!(matches!(
            ast,
            Decl::Class {
                decl: Binding {
                    decl: declarations::ClassDecl { parent: None, .. },
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn parse_subclass_decl() {
        let fragment = "subclass pt3 of pt = {
            field z: nat
            action cross(self: pt3, other: pt3) returns (ortho: pt3)
        }";
        let ast = helpers::decl_from_src(fragment);
        assert!(matches!(
            ast,
            Decl::Subclass {
                decl: Binding {
                    decl: declarations::ClassDecl {
                        parent: Some(_),
                        ..
                    },
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn parse_common_decl() {
        let fragment = "common {
            var msg: count;
            after init { msg_count := 0 }
        }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_function_decl_incomplete() {
        let fragment = "function foo(A: int, B: int)";

        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), fragment));
        let res = IvyParser::parse_with_userdata(Rule::decl, fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).expect_err("Needs return sort or body to infer");
    }

    #[test]
    fn parse_function_decl_uninterp() {
        let fragment = "function foo(A: int, B: int): int";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_function_decl_impl() {
        let fragment = "function foo(A: int, B: int) = A + B";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_function_decl_impl_explicit_ret() {
        let fragment = "function foo(A: int, B: int): int = A + B";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_global_decl() {
        let fragment = "global { instance file : vector(byte) }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_instance_decl() {
        let fragment = "instance c : counter(int)";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_interpret_uninterp_decl() {
        let fragment = "interpret t";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_interpret_annotated_decl() {
        let fragment = "interpret t -> int";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_invariant_decl() {
        let fragment = "invariant X = Z | ~link(X,Y) | ~link(Z,Y)";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_implementation_decl() {
        let fragment = "implementation {
            instance sock : net.socket;
            after init { };
        }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_module_decl_1() {
        let fragment = "module net(pid) = { }";
        let decl = helpers::decl_from_src(fragment);
        assert_eq!(
            decl,
            Decl::Module {
                decl: Binding::from(
                    "net",
                    ModuleDecl {
                        sortsyms: vec!(Binding::inferred("pid")),
                        body: vec!()
                    },
                    Span::IgnoredForTesting
                )
            }
        )
    }

    #[test]
    fn parse_module_decl_2() {
        let fragment = "module net(pid: node) = { 
            action foo(a: int) = { }
        }
        ";
        IvyParser::parse_with_userdata::<Rc<str>>(
            Rule::decl,
            fragment,
            fragment.to_string().into(),
        )
        .expect_err("Parsing succeeded");
    }

    #[test]
    fn parse_object_decl() {
        let fragment = "object timer = { }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_parametrized_decl_err() {
        let fragment = "object timer(x: bool) = { }";
        IvyParser::parse_with_userdata::<Rc<str>>(Rule::decl, fragment, fragment.to_owned().into())
            .expect_err(
                "Raw objects cannot be parameterized in Irving (use `isolate` or `process`)",
            );
    }

    #[test]
    fn parse_process_decl() {
        let proc_fragment = "process timer(x: bool) = { }";
        let isol_fragment = "isolate timer(x: bool) = { }";

        let proc = match helpers::decl_from_src(proc_fragment) {
            Decl::Object {
                decl: Binding { decl, .. },
                ..
            } => decl,
            _ => unreachable!(),
        };
        let isol = match helpers::decl_from_src(isol_fragment) {
            Decl::Object {
                decl: Binding { decl, .. },
                ..
            } => decl,
            _ => unreachable!(),
        };

        // `process` and `isolate` are indistinguishable for us.  (In regular ivy, the distinction
        // comes from an implicit `extract` directive in the former, which we don't consider.)
        // (Note that we extract the object declaration from the enclosing Decl because the latter's
        // Span will be different; it remembers what keyword was used to define the object!)
        assert_eq!(
            proc,
            declarations::ObjectDecl {
                params: [Binding::from(
                    "x",
                    Sort::Annotated(["bool".into()].into()),
                    Span::IgnoredForTesting
                )]
                .into(),
                body: vec!(),
            }
        );
        assert_eq!(
            isol,
            declarations::ObjectDecl {
                params: [Binding::from(
                    "x",
                    Sort::Annotated(["bool".into()].into()),
                    Span::IgnoredForTesting
                )]
                .into(),
                body: vec!(),
            }
        );
    }

    #[test]
    fn parse_relation_decl() {
        let fragment = "relation foo(A: int, B: int)";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_nullary_relation() {
        let fragment = "relation foo";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_specification_decl() {
        let fragment = "specification {
            var msg: count;
            after init { msg_count := 0 }
        }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_var_decl() {
        let fragment = "var a";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn vardecl_declare_and_assign() {
        let decl = "var i := 42";
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), decl));
        IvyParser::parse_with_userdata(Rule::action, decl, user_data)
            .expect_err("Parsing should fail");
    }

    #[test]
    fn parse_type_decl() {
        let fragment = "type int";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_type_decl_this() {
        let fragment = "type this";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_type_decl_with_super() {
        let fragment = "type int of something";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_var_decl_with_type() {
        let fragment = "var a : int";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_type_enum_range() {
        let fragment = "type coolguys = {sammy, nathan, james}";
        let ast = helpers::decl_from_src(fragment);
        assert!(matches!(
            ast,
            Decl::Type {
                decl: Binding {
                    decl: Sort::Resolved(IvySort::Enum(_)),
                    ..
                },
                ..
            }
        ))
    }
    #[test]
    fn parse_type_decl_range() {
        let fragment = "type pid = {0..1}";
        let ast = helpers::decl_from_src(fragment);
        assert!(matches!(
            ast,
            Decl::Type {
                decl: Binding {
                    decl: Sort::Resolved(IvySort::BoundedSequence { .. }),
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn parse_ensure_with_fmla() {
        let fragment = "ensure X -> Y";
        helpers::decl_from_src(fragment);
    }
}
