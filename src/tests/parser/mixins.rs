#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        ast::{declarations::*, expressions::*, span::Span},
        parser::ivy::{IvyParser, ParserState, Rule},
        tests::helpers,
    };
    use pest_consume::Parser;

    // Declarations

    #[test]
    fn parse_action_forward_ref() {
        let fragment = "action foo";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_export_forward_ref() {
        let fragment = "export foo";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_export_forward_ref_qualified() {
        let fragment = "export foo.bar";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_export_action() {
        let fragment = "export action foo(a: int) = { }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_action_decl_no_ret_nor_body() {
        let fragment = "action foo(a: int)";
        let user_data = Rc::new(ParserState::new(file!(), fragment));
        let res = IvyParser::parse_with_userdata(Rule::action_decl, fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();

        assert_eq!(
            IvyParser::action_decl(res),
            Ok(Binding::from(
                "foo",
                ActionDecl {
                    params: vec![Symbol::from(
                        "a",
                        Sort::Annotated(vec!["int".into()]),
                        Span::IgnoredForTesting
                    )],
                    ret: None,
                    body: None,
                },
                Span::IgnoredForTesting
            ))
        );
    }

    #[test]
    fn parse_action_decl_no_ret_but_body() {
        let fragment = "action foo(a: int) = { }";
        let user_data = Rc::new(ParserState::new(file!(), fragment));
        let res = IvyParser::parse_with_userdata(Rule::action_decl, fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        assert_eq!(
            IvyParser::action_decl(res),
            Ok(Binding::from(
                "foo",
                ActionDecl {
                    params: vec![Symbol::from(
                        "a",
                        Sort::Annotated(vec!["int".into()]),
                        Span::IgnoredForTesting
                    )],
                    ret: None,
                    body: Some(vec![])
                },
                Span::IgnoredForTesting
            ))
        );
    }

    #[test]
    fn parse_nested_action() {
        let fragment = "action foo(a: int) = { 
            action(bar: b) = {}
        }";
        let user_data = Rc::new(ParserState::new(file!(), fragment));
        let res = IvyParser::parse_with_userdata(Rule::action_decl, fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        // This assert indicates that parsing the action stopped when we hit the second action (i.e. what follows is invalid).
        assert!(IvyParser::action_decl(res).unwrap().decl.body.is_none());
    }

    #[test]
    fn parse_action_decl_with_ret_no_body() {
        let fragment = "action foo(a: int) returns (b: int)";
        let user_data = Rc::new(ParserState::new(file!(), fragment));
        let res = IvyParser::parse_with_userdata(Rule::action_decl, fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();

        assert_eq!(
            IvyParser::action_decl(res),
            Ok(Binding::from(
                "foo",
                ActionDecl {
                    params: vec![Symbol::from(
                        "a",
                        Sort::Annotated(vec!["int".into()]),
                        Span::IgnoredForTesting
                    )],
                    ret: Some(Symbol::from(
                        "b",
                        Sort::Annotated(vec!["int".into()]),
                        Span::IgnoredForTesting
                    )),
                    body: None
                },
                Span::IgnoredForTesting
            ))
        );
    }

    #[test]
    fn parse_action_decl_with_ret_and_body() {
        let fragment = "action foo(a: int) returns (b: int) = { }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_after_init() {
        let fragment = "after init { i := 0; }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_after_mixin() {
        let fragment = "after sock.recv { i := 0; }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_after_mixin_with_declret() {
        let fragment = "after sock.recv returns (i: int) { i := 0; }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_implement() {
        let fragment = "implement foo { i := 0; }";
        helpers::decl_from_src(fragment);
    }

    #[test]
    fn parse_implement_with_declret() {
        let fragment = "implement foo(a: int) returns (b: int) { i := 0; }";
        helpers::decl_from_src(fragment);
    }
}
