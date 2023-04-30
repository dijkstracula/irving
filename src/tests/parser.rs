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
    fn parse_dot_expr() {
        assert!(parse_expr("a.b.c.d").is_ok());
    }

    // Statements/Decls

    #[test]
    fn parse_include_decl() {
//        let fragment = "include collections";

//        let res = IvyParser::parse(Rule::body)
 //           .expect("Parsing failed")
  //          .single().unwrap();
    }

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
}