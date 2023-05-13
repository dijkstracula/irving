#[cfg(test)]
mod tests {
    use pest_consume::Parser;
    use crate::parser::ivy::{IvyParser, Rule};

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
    fn parse_assign_to_relation() {
        let fragment = "r(X) := false";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::stmt(res).expect("generate ast");
    }


    #[test]
    fn parse_assign_annot() {
        let fragment = "var a:int := b";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::stmt(res).expect("generate ast");
    }

    #[test]
    fn parse_assert_action() {
        let fragment = "assert x < y";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::stmt(res).unwrap();
    }

    #[test]
    fn parse_assume_action() {
        let fragment = "assume x < y";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::stmt(res).unwrap();
    }

    #[test]
    fn parse_ensure_action() {
        let fragment = "ensure ~failed(y)";
        let _res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
    }

    #[test]
    fn parse_requires_action() {
        let fragment = "require ~failed(y)";
        let _res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
    }

    #[test]
    fn parse_if_stmt() {
        let fragment = "if 1 < 2 { a := 42; }";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        IvyParser::stmt(res).unwrap();
    }

    #[test]
    fn parse_stmt_block() {
        let fragment = "requires a = 0; a := 42; ensure a = 42";
        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        println!("{:?}", IvyParser::stmt(res).unwrap());
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
        IvyParser::stmt(res).unwrap();
    }

    #[test]
    fn parse_multi_stmt_while() {
        let fragment = "while i > 0 {
            sum := sum + f(i);
            i := i - 1
        }";

        let res = IvyParser::parse(Rule::stmt, fragment)
            .expect("Parsing failed")
            .single().unwrap();
        let res = IvyParser::stmt(res).unwrap();
        println!("{:?}", res);
    }
}