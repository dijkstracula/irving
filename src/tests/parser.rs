#[cfg(test)]
mod tests {
    use pest_consume::Parser;
    use crate::ast;
    use crate::parser::{IvyParser, Result, Rule};

    #[test]
    fn parse_hashlang_major_minor() {
        let body = "#lang ivy1.8";
        let res = IvyParser::parse(Rule::hashlang, body)
            .expect("Parsing failed")
            .single().unwrap();

        let (major, minor) = IvyParser::langver(res).unwrap();
        assert!(major == 1);
        assert!(minor == 8);
    }

    #[test]
    fn parse_hashlang_major_only() {
        let body = "#lang ivy2";
        let res = IvyParser::parse(Rule::hashlang, body)
            .expect("Parsing failed")
            .single().unwrap();

        let (major, minor) = IvyParser::langver(res).unwrap();
        assert!(major == 2);
        assert!(minor == 0);
    }

    // Expressions

    fn parse_expr(fragment: &str) -> Result<ast::Expr> {
        let res = IvyParser::parse(Rule::expr, fragment)
            .expect("Parsing failed").single().unwrap();
        IvyParser::expr(res)
    }

    #[test]
    fn parse_symbol_expr() {
        assert!(parse_expr("a").is_ok());
    }

    #[test]
    fn parse_symbol_with_underscore() {
        assert!(parse_expr("hello_world").is_ok());
    }

    #[test]
    fn parse_dot_expr() {
        assert!(parse_expr("a.b").is_ok());
    }

    // Statements/Decls

    #[test]
    fn parse_decl_sig() {
        let fragment = "foo(a: int) returns (b: int)";
        let res = IvyParser::parse(Rule::decl_sig, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::decl_sig(res).is_ok());
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
    fn parse_function_decl() {
        let fragment = "function foo(a: int, b: int): int";
        let res = IvyParser::parse(Rule::function_decl, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::function_decl(res).is_ok());
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

    // Statements

    #[test]
    fn parse_assign() {
        let fragment = "a := b";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::stmt(res).expect("generate ast");
    }


    #[test]
    fn parse_assign_annot() {
        let fragment = "a:int := b";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::stmt(res).is_ok());
    }

    #[test]
    fn parse_if_stmt() {
        let fragment = "if 1 < 2 { a := 42; }";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::stmt(res).is_ok());
    }

    #[test]
    fn parse_if_stmt_with_else() {
        let fragment = "if 1 < 2 { a := 42; } else { a := 43; }";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::stmt(res).is_ok());
    }

    #[test]
    fn parse_while() {
        let fragment = "while 1 < 2 { a := a + 1; }";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        assert!(IvyParser::stmt(res).is_ok());
    }

    // Toplevels

    #[test]
    fn parse_trivial_prog() {
        let body = "#lang ivy2";

        let res = IvyParser::parse(Rule::prog, body)
            .expect("Parsing failed")
            .single().unwrap();

        let prog = IvyParser::prog(res).unwrap();
        assert!(prog.major_version == 2);
        assert!(prog.minor_version == 0);
    }


    #[test]
    fn parse_less_trivial_prog() {
        let body = "
#lang ivy2
module net(pid: node) = { 
    action send(msg: msg_t) = {
        a := 41 + 2;
    }
}";

        let res = IvyParser::parse(Rule::prog, body)
            .expect("Parsing failed")
            .single().unwrap();

        let prog = IvyParser::prog(res).unwrap();
        assert!(prog.major_version == 2);
        assert!(prog.minor_version == 0);
    }

}