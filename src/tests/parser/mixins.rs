#[cfg(test)]
mod tests {
    use crate::parser::ivy::{IvyParser, Rule};
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
    fn parse_action_forward_ref() {
        let fragment = "action foo";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_export_forward_ref() {
        let fragment = "export foo";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_export_forward_ref_qualified() {
        let fragment = "export foo.bar";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_export_action() {
        let fragment = "export action foo(a: int) = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_action_decl() {
        let fragment = "action foo(a: int)";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_action_decl_1() {
        let fragment = "action foo(a: int) = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_nested_action() {
        let fragment = "action foo(a: int) = { 
            action(bar: b) = {}
        }";
        let res = IvyParser::parse(Rule::action_decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        // This assert indicates that parsing the action stopped when we hit the second action (i.e. what follows is invalid).
        assert!(IvyParser::action_decl(res).unwrap().body == None);
    }

    #[test]
    fn parse_action_decl_2() {
        let fragment = "action foo(a: int) returns (b: int) = { }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert!(IvyParser::decl(res).is_ok());
    }

    #[test]
    fn parse_after_init() {
        let fragment = "after init { i := 0; }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_after_mixin() {
        let fragment = "after sock.recv { i := 0; }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_after_mixin_with_declret() {
        let fragment = "after sock.recv returns (i: int) { i := 0; }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_implement() {
        let fragment = "implement foo { i := 0; }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }

    #[test]
    fn parse_implement_with_declret() {
        let fragment = "implement foo(a: int) returns (b: int) { i := 0; }";
        let res = IvyParser::parse(Rule::decl, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).unwrap();
    }
}
