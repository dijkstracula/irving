#[cfg(test)]
mod tests {
    use pest_consume::Parser;
    use crate::parser::IvyParser;
    use crate::parser::Rule;

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
}