#[cfg(test)]
mod tests {
    use crate::ast::declarations::{Binding, Decl, NormalizedIsolateDecl};
    use crate::passes::isolate_normalizer::{IsolateNormalizer, NormalizerError};
    use crate::tests::helpers;
    use crate::visitor::ast::Visitable;

    #[test]
    fn normalize_empty_module() {
        let mut ast = helpers::decl_from_src("isolate foo = {}");
        let mut mn = IsolateNormalizer::new();

        let expected_ast = Decl::NormalizedIsolate(Binding::from(
            "foo".into(),
            NormalizedIsolateDecl {
                params: vec![],
                impl_decls: vec![],
                spec_decls: vec![],
                common_spec_decls: vec![],
                common_impl_decls: vec![],
            },
        ));
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn normalize_simple_module() {
        let mut ast = helpers::decl_from_src(
            "isolate foo = {
            var is_up: bool;
            instance net: sock.net;
        }",
        );
        let expected_ast = Decl::NormalizedIsolate(Binding::from(
            "foo".into(),
            NormalizedIsolateDecl {
                params: vec![],
                impl_decls: [
                    helpers::decl_from_src("var is_up: bool"),
                    helpers::decl_from_src("instance net: sock.net"),
                ]
                .into(),
                spec_decls: vec![],
                common_spec_decls: vec![],
                common_impl_decls: vec![],
            },
        ));

        let mut mn = IsolateNormalizer::new();
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn normalize_impl_module() {
        let mut ast = helpers::decl_from_src(
            "isolate foo = {
            instance net: sock.net;

            implementation {
                var is_up: bool;
            }
        }",
        );
        let expected_ast = Decl::NormalizedIsolate(Binding::from(
            "foo".into(),
            NormalizedIsolateDecl {
                params: vec![],
                impl_decls: [
                    helpers::decl_from_src("var is_up: bool"),
                    helpers::decl_from_src("instance net: sock.net"),
                ]
                .into(),
                spec_decls: vec![],
                common_spec_decls: vec![],
                common_impl_decls: vec![],
            },
        ));

        let mut mn = IsolateNormalizer::new();
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn normalize_multiple_impls() {
        let mut ast = helpers::decl_from_src(
            "isolate foo = {
            implementation {
                instance net: sock.net;
            }

            implementation {
                var is_up: bool;
            }
        }",
        );
        let expected_ast = Decl::NormalizedIsolate(Binding::from(
            "foo".into(),
            NormalizedIsolateDecl {
                params: vec![],
                impl_decls: [
                    helpers::decl_from_src("instance net: sock.net"),
                    helpers::decl_from_src("var is_up: bool"),
                ]
                .into(),
                spec_decls: vec![],
                common_spec_decls: vec![],
                common_impl_decls: vec![],
            },
        ));

        let mut mn = IsolateNormalizer::new();
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn normalize_nested_decls() {
        let mut ast = helpers::decl_from_src(
            "isolate foo = {
            implementation {
                implementation {
                    instance net: sock.net;
                }
            }

            implementation {
                var is_up: bool;
            }
        }",
        );

        let mut mn = IsolateNormalizer::new();
        let res = ast.visit(&mut mn).expect_err("traversal");
        assert_eq!(
            res.downcast::<NormalizerError>().unwrap(),
            NormalizerError::BadNesting {
                inner: "implementation",
                outer: "implementation"
            }
        );
    }

    #[test]
    fn normalize_simple_common() {
        let mut ast = helpers::decl_from_src(
            "isolate foo = {
            common {
                var is_up: bool;
                instance net: sock.net;
            }
        }",
        );
        let expected_ast = Decl::NormalizedIsolate(Binding::from(
            "foo".into(),
            NormalizedIsolateDecl {
                params: vec![],
                impl_decls: vec![],
                spec_decls: vec![],
                common_spec_decls: vec![],
                common_impl_decls: [
                    helpers::decl_from_src("var is_up: bool"),
                    helpers::decl_from_src("instance net: sock.net"),
                ]
                .into(),
            },
        ));
        let mut mn = IsolateNormalizer::new();
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn normalize_common() {
        let mut ast = helpers::decl_from_src(
            "isolate foo = {
            common {
                specification {
                    var is_up: bool;
                }
                implementation {
                    instance net: sock.net;
                }
            }
        }",
        );
        let expected_ast = Decl::NormalizedIsolate(Binding::from(
            "foo".into(),
            NormalizedIsolateDecl {
                params: vec![],
                impl_decls: vec![],
                spec_decls: vec![],
                common_spec_decls: [helpers::decl_from_src("var is_up: bool")].into(),
                common_impl_decls: [helpers::decl_from_src("instance net: sock.net")].into(),
            },
        ));

        let mut mn = IsolateNormalizer::new();
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }
}
