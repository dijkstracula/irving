#[cfg(test)]
mod tests {
    use pest_consume::Parser;
    use crate::parser::{IvyParser, Rule};

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
        IvyParser::stmt(res).unwrap();
    }
}