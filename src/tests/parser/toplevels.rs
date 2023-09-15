#[cfg(test)]
mod tests {
    use crate::parser::ivy::{IvyParser, Rule};
    use pest_consume::Parser;

    #[test]
    fn parse_hashlang_major_minor() {
        let body = "#lang ivy1.8";
        let res = IvyParser::parse_with_userdata(Rule::hashlang, body, body.into())
            .expect("Parsing failed")
            .single()
            .unwrap();

        let (major, minor) = IvyParser::langver(res).unwrap();
        assert!(major == 1);
        assert!(minor == 8);
    }

    #[test]
    fn parse_hashlang_major_only() {
        let body = "#lang ivy2";
        let res = IvyParser::parse_with_userdata(Rule::hashlang, body, body.into())
            .expect("Parsing failed")
            .single()
            .unwrap();

        let (major, minor) = IvyParser::langver(res).unwrap();
        assert!(major == 2);
        assert!(minor == 0);
    }

    // Toplevels

    #[test]
    fn parse_trivial_prog() {
        let body = "#lang ivy2";

        let res = IvyParser::parse_with_userdata(Rule::prog, body, body.into())
            .expect("Parsing failed")
            .single()
            .unwrap();

        let prog = IvyParser::prog(res).unwrap();
        assert!(prog.major_version == 2);
        assert!(prog.minor_version == 0);
    }

    #[test]
    fn parse_less_trivial_prog() {
        let body = "
#lang ivy2
isolate net(pid: node) = { 
    action send(msg: msg_t) = {
        a := 41 + 2;
    }
}";

        let res = IvyParser::parse_with_userdata(Rule::prog, body, body.to_owned().into())
            .expect("Parsing failed")
            .single()
            .unwrap();

        let prog = IvyParser::prog(res).unwrap();
        assert!(prog.major_version == 2);
        assert!(prog.minor_version == 0);
    }

    #[test]
    fn logical_call_to_action() {
        let iso = "process foo = {
            type node
            action doit(x: node)

            action uhoh = {
                doit(X) # We should expect this line to be invalid.
            }
        } ";
        IvyParser::parse_with_userdata(Rule::process_decl, iso, iso)
            .expect_err("Can't call an action with a logical variable");
    }

    #[test]
    fn non_top_include() {
        let iso = "process foo = {
            include number
        } ";
        IvyParser::parse_with_userdata(Rule::process_decl, iso, iso)
            .expect_err("includes must be at the top level");
    }
}
