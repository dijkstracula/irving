#[cfg(test)]
mod tests {
    use pest_consume::Parser;
    use crate::parser::{IvyParser, Rule};


    // Declarations

    #[test]
    fn parse_decl_sig() {
        let fragment = "foo(a: int) returns (b: int)";
        let res = IvyParser::parse(Rule::decl_sig, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::decl_sig(res).is_ok());
    }

    #[test]
    fn parse_action_forward_ref() {
        let fragment = "action foo";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_export_forward_ref() {
        let fragment = "export foo";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_export_action() {
        let fragment = "export action foo(a: int) = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::decl(res).unwrap();
    }


    #[test]
    fn parse_action_decl() {
        let fragment = "action foo(a: int)";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        //assert!(IvyParser::decl(res).is_ok());
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_action_decl_1() {
        let fragment = "action foo(a: int) = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        //assert!(IvyParser::decl(res).is_ok());
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_action_decl_2() {
        let fragment = "action foo(a: int) returns (b: int) = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::decl(res).is_ok());
    }

    #[test]
    fn parse_after_init() {
        let fragment = "after init { i := 0; }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::decl(res).is_ok());
    }

/* TODO: action name needs to be able to be qualified
    #[test]
    fn parse_after_mixin() {
        let fragment = "after sock.recv { i := 0; }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::decl(res).is_ok());
    }
    */


    #[test]
    fn parse_axiom() {
        let fragment = "axiom X:id < Y | X = Y | Y < X";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::decl(res).is_ok());
    }
    #[test]
    fn parse_function_decl() {
        let fragment = "function foo(a: int, b: int): int";
        let res = IvyParser::parse(Rule::function_decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::function_decl(res).is_ok());
    }

    #[test]
    fn parse_instance_decl() {
        let fragment = "instance c : counter(int)";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_invariant_decl() {
        let fragment = "invariant X = Z | ~link(X,Y) | ~link(Z,Y)";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_module_decl() {
        let fragment = "module net(pid: node) = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::decl(res).is_ok());
    }

    #[test]
    fn parse_module_decl_1() {
        let fragment = "module net(pid) = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::decl(res).is_ok());
    }

    #[test]
    fn parse_module_decl_2() {
        let fragment = "module net(pid: node) = { 
            action foo(a: int) = { }
        }
        ";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::decl(res).is_ok());
    }

    #[test]
    fn parse_object_decl() {
        let fragment = "object timer = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::decl(res).is_ok());
    }

    #[test]
    fn parse_relation_decl() {
        let fragment = "relation foo(a: int, b: int)";
        let res = IvyParser::parse(Rule::relation_decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::relation_decl(res).is_ok());
    }

    #[test]
    fn parse_var_decl() {
        let fragment = "var a";
        let res = IvyParser::parse(Rule::var_decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::var_decl(res).is_ok());
    }

    #[test]
    fn parse_type_decl() {
        let fragment = "type int";
        let res = IvyParser::parse(Rule::type_decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::type_decl(res).is_ok());
    }

    #[test]
    fn parse_type_decl_this() {
        // TODO: `this` is not a distinguished token.  Should it be?
        let fragment = "type this";
        let res = IvyParser::parse(Rule::type_decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::type_decl(res).is_ok());
    }

    #[test]
    fn parse_type_decl_with_super() {
        let fragment = "type int of something";
        let res = IvyParser::parse(Rule::type_decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::type_decl(res).is_ok());
    }

    #[test]
    fn parse_var_decl_with_type() {
        let fragment = "var a : int";
        let res = IvyParser::parse(Rule::var_decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::var_decl(res).is_ok());
    }
}