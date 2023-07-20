#[cfg(test)]
mod tests {
    use crate::{
        ast::declarations::{Binding, Decl, ModuleDecl},
        parser::ivy::{IvyParser, Rule},
    };
    use pest_consume::Parser;

    // Declarations

    #[test]
    fn parse_decl_sig() {
        let fragment = "foo(a: int) returns (b: int)";
        let res = IvyParser::parse(Rule::decl_sig, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert!(IvyParser::decl_sig(res).is_ok());
    }

    #[test]
    fn parse_alias_decl() {
        let fragment = "alias byte = bv[8]";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _res = IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_attribute() {
        let fragment = "attribute bmc[10]";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _res = IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_axiom() {
        let fragment = "axiom X:id < Y";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _res = IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_axiom_2() {
        let fragment = "axiom host(0).sock.id ~= host(1).sock.id";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _res = IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_common_decl() {
        let fragment = "common {
            var msg: count;
            after init { msg_count := 0 }
        }";

        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_function_decl() {
        let fragment = "function foo(a: int, b: int): int";
        let res = IvyParser::parse(Rule::function_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert!(IvyParser::function_decl(res).is_ok());
    }

    #[test]
    fn parse_global_decl() {
        let fragment = "global { instance file : vector(byte) }";
        let res = IvyParser::parse(Rule::global_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::global_decl(res).unwrap();
    }

    #[test]
    fn parse_instance_decl() {
        let fragment = "instance c : counter(int)";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_interpret_uninterp_decl() {
        let fragment = "interpret t";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_interpret_annotated_decl() {
        let fragment = "interpret t -> int";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_invariant_decl() {
        let fragment = "invariant X = Z | ~link(X,Y) | ~link(Z,Y)";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_implementation_decl() {
        let fragment = "implementation {
            instance sock : net.socket;
            after init { };
        }";

        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_module_decl_1() {
        let fragment = "module net(pid) = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert_eq!(
            IvyParser::decl(res).unwrap(),
            Decl::Module(Binding::from(
                "net".into(),
                ModuleDecl {
                    sortsyms: vec!("pid".into()),
                    body: vec!()
                }
            ))
        );
    }

    #[test]
    fn parse_module_decl_2() {
        let fragment = "module net(pid: node) = { 
            action foo(a: int) = { }
        }
        ";
        IvyParser::parse(Rule::decl, fragment).expect_err("Parsing succeeded");
    }

    #[test]
    fn parse_object_decl() {
        let fragment = "object timer = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert!(IvyParser::decl(res).is_ok());
    }

    #[test]
    fn parse_parametrized_decl_err() {
        let fragment = "object timer(x: bool) = { }";
        IvyParser::parse(Rule::decl, fragment).expect_err(
            "Raw objects cannot be parameterized in Irving (use `isolate` or `process`)",
        );
    }

    #[test]
    fn parse_process_decl() {
        let proc_fragment = "process timer(x: bool) = { }";
        let isol_fragment = "isolate timer(x: bool) = { }";

        let res = IvyParser::parse(Rule::decl, proc_fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let proc = IvyParser::decl(res).unwrap();

        let res = IvyParser::parse(Rule::decl, isol_fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let isol = IvyParser::decl(res).unwrap();

        // `process` and `isolate` are indistinguishable for us.  (In regular ivy, the distinction
        // comes from an implicit `extract` directive in the former, which we don't consider.)
        assert_eq!(proc, isol);
    }

    #[test]
    fn parse_relation_decl() {
        let fragment = "relation foo(a: int, b: int)";
        let res = IvyParser::parse(Rule::relation_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert!(IvyParser::relation_decl(res).is_ok());
    }

    #[test]
    fn parse_specification_decl() {
        let fragment = "specification {
            var msg: count;
            after init { msg_count := 0 }
        }";

        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_var_decl() {
        let fragment = "var a";
        let res = IvyParser::parse(Rule::var_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert!(IvyParser::var_decl(res).is_ok());
    }

    #[test]
    fn parse_type_decl() {
        let fragment = "type int";
        let res = IvyParser::parse(Rule::type_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert!(IvyParser::type_decl(res).is_ok());
    }

    #[test]
    fn parse_type_decl_this() {
        let fragment = "type this";
        let res = IvyParser::parse(Rule::type_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::type_decl(res).unwrap();
    }

    #[test]
    fn parse_type_decl_with_super() {
        let fragment = "type int of something";
        let res = IvyParser::parse(Rule::type_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert!(IvyParser::type_decl(res).is_ok());
    }

    #[test]
    fn parse_var_decl_with_type() {
        let fragment = "var a : int";
        let res = IvyParser::parse(Rule::var_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert!(IvyParser::var_decl(res).is_ok());
    }

    #[test]
    fn parse_type_enum_range() {
        let fragment = "type coolguys = {sammy, nathan, james}";
        let res = IvyParser::parse(Rule::type_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::type_decl(res).unwrap();
    }
    #[test]
    fn parse_type_decl_range() {
        let fragment = "type pid = {0..1}";
        let res = IvyParser::parse(Rule::type_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::type_decl(res).unwrap();
    }

    #[test]
    fn parse_ensure_with_fmla() {
        let fragment = "ensure X -> Y";
        let res = IvyParser::parse(Rule::ensure_action, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::ensure_action(res).unwrap();
    }
}
