#[cfg(test)]
mod tests {
    use crate::ast;
    use crate::ast::declarations::*;
    use crate::ast::expressions::Param;
    use crate::parser::ivy::{IvyParser, Rule};
    use crate::rewriter;
    use pest_consume::{Error, Parser};

    pub type Result<T> = std::result::Result<T, Error<Rule>>;

    const ACTION_FOO: &'static str = "action foo(a: int) returns (b: int) = { }";

    //const BEFORE_FOO_NO_FUNCDECL: &'static str = "before foo { }";
    //const BEFORE_FOO: &'static str = "before foo(a: int) { }";

    //const AFTER_FOO_NO_FUNCDECL: &'static str = "after foo(a: int) { }";
    const AFTER_FOO: &'static str = "after foo(a: int) returns (b: int) { }";

    fn ast_to_decls(decls: Vec<&str>) -> Result<Vec<ast::declarations::Decl>> {
        let parsed = decls
            .into_iter()
            .map(|decl| {
                let res = IvyParser::parse(Rule::decl, decl)
                    .expect("Parsing failed")
                    .single()
                    .unwrap();
                IvyParser::decl(res)
            })
            .collect::<Result<Vec<_>>>();
        parsed
    }

    #[test]
    fn simple_action() {
        let mut decls = ast_to_decls(vec![ACTION_FOO]).expect("Parsing");
        assert!(decls.len() == 1);

        let decl = match decls.pop() {
            Some(ast::declarations::Decl::Action(action_decl)) => action_decl,
            _ => unreachable!(),
        };

        assert_eq!(
            decl,
            ActionDecl {
                name: vec!("foo".to_owned()),
                params: vec!(Param {
                    id: "a".to_owned(),
                    sort: Some(vec!("int".to_owned()))
                }),
                ret: Some(Param {
                    id: "b".to_owned(),
                    sort: Some(vec!("int".to_owned()))
                }),
                body: Some(vec!())
            }
        );

        let mixin = rewriter::Mixin::from_action(decl);
        assert_eq!(
            mixin,
            rewriter::Mixin {
                name: "foo".to_owned(),
                params: Some(vec!(Param {
                    id: "a".to_owned(),
                    sort: Some(vec!("int".to_owned()))
                })),
                ret: Some(Param {
                    id: "b".to_owned(),
                    sort: Some(vec!("int".to_owned()))
                }),
                pre: None,
                body: Some(vec!()),
                post: None,
            }
        );
    }

    #[test]
    fn action_and_post_correct() {
        let decls = ast_to_decls(vec![ACTION_FOO, AFTER_FOO]).expect("Parsing");

        let mut mixin = rewriter::Mixin::new("foo".into());
        for decl in decls {
            match decl {
                Decl::Action(ad) => {
                    mixin.mix_action(ad).unwrap();
                }
                Decl::AfterAction(post) => {
                    mixin.mix_after(post).unwrap();
                }
                Decl::BeforeAction(pre) => {
                    mixin.mix_before(pre).unwrap();
                }
                _ => unimplemented!(),
            }
        }

        assert_eq!(
            mixin,
            rewriter::Mixin {
                name: "foo".to_owned(),
                params: Some(vec!(Param {
                    id: "a".to_owned(),
                    sort: Some(vec!("int".to_owned()))
                })),
                ret: Some(Param {
                    id: "b".to_owned(),
                    sort: Some(vec!("int".to_owned()))
                }),
                pre: None,
                body: Some(vec!()),
                post: Some(vec!()),
            }
        );
    }

    #[test]
    fn action_and_post_incorrect_params() {
        const AFTER_FOO: &'static str = "after foo(xyz: int) returns (b: int) { }";

        let mut decls = ast_to_decls(vec![ACTION_FOO, AFTER_FOO]).expect("Parsing");
        let mut mixin = rewriter::Mixin::from_decl(decls.remove(0));
        let post = match decls.remove(0) {
            Decl::AfterAction(post) => post,
            _ => unreachable!(),
        };
        assert!(mixin.mix_after(post).is_err());
    }

    #[test]
    fn action_and_post_incorrect_ret() {
        const AFTER_FOO: &'static str = "after foo(a: int) returns (xyz: int) { }";

        let mut decls = ast_to_decls(vec![ACTION_FOO, AFTER_FOO]).expect("Parsing");
        let mut mixin = rewriter::Mixin::from_decl(decls.remove(0));
        let post = match decls.remove(0) {
            Decl::AfterAction(post) => post,
            _ => unreachable!(),
        };
        assert!(mixin.mix_after(post).is_err());
    }
}
