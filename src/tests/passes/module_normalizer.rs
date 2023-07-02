#[cfg(test)]
mod tests {
    use crate::ast::declarations::{Decl, NormalizedModuleDecl};
    use crate::parser::ivy::{IvyParser, Rule};
    use crate::passes::module_normalizer::{ModuleNormalizer, NormalizerError};
    use crate::visitor::visitor::Visitable;
    use pest_consume::Parser;

    fn decl_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).expect("AST generation failed")
    }

    #[test]
    fn normalize_empty_module() {
        let mut ast = decl_from_src("module foo = {}");
        let mut mn = ModuleNormalizer::new();

        let expected_ast = Decl::NormalizedModule(NormalizedModuleDecl {
            name: "foo".into(),
            params: vec![],
            impl_decls: vec![],
            spec_decls: vec![],
            common_spec_decls: vec![],
            common_impl_decls: vec![],
        });
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn normalize_simple_module() {
        let mut ast = decl_from_src(
            "module foo = {
            var is_up: bool;
            instance net: sock.net;
        }",
        );
        let expected_ast = Decl::NormalizedModule(NormalizedModuleDecl {
            name: "foo".into(),
            params: vec![],
            impl_decls: [
                decl_from_src("var is_up: bool"),
                decl_from_src("instance net: sock.net"),
            ]
            .into(),
            spec_decls: vec![],
            common_spec_decls: vec![],
            common_impl_decls: vec![],
        });

        let mut mn = ModuleNormalizer::new();
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn normalize_impl_module() {
        let mut ast = decl_from_src(
            "module foo = {
            instance net: sock.net;

            implementation {
                var is_up: bool;
            }
        }",
        );
        let expected_ast = Decl::NormalizedModule(NormalizedModuleDecl {
            name: "foo".into(),
            params: vec![],
            impl_decls: [
                decl_from_src("instance net: sock.net"),
                decl_from_src("var is_up: bool"),
            ]
            .into(),
            spec_decls: vec![],
            common_spec_decls: vec![],
            common_impl_decls: vec![],
        });

        let mut mn = ModuleNormalizer::new();
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn normalize_multiple_impls() {
        let mut ast = decl_from_src(
            "module foo = {
            implementation {
                instance net: sock.net;
            }

            implementation {
                var is_up: bool;
            }
        }",
        );
        let expected_ast = Decl::NormalizedModule(NormalizedModuleDecl {
            name: "foo".into(),
            params: vec![],
            impl_decls: [
                decl_from_src("instance net: sock.net"),
                decl_from_src("var is_up: bool"),
            ]
            .into(),
            spec_decls: vec![],
            common_spec_decls: vec![],
            common_impl_decls: vec![],
        });

        let mut mn = ModuleNormalizer::new();
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn normalize_nested_decls() {
        let mut ast = decl_from_src(
            "module foo = {
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

        let mut mn = ModuleNormalizer::new();
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
        let mut ast = decl_from_src(
            "module foo = {
            common {
                var is_up: bool;
                instance net: sock.net;
            }
        }",
        );
        let expected_ast = Decl::NormalizedModule(NormalizedModuleDecl {
            name: "foo".into(),
            params: vec![],
            impl_decls: vec![],
            spec_decls: vec![],
            common_spec_decls: vec![],
            common_impl_decls: [
                decl_from_src("var is_up: bool"),
                decl_from_src("instance net: sock.net"),
            ]
            .into(),
        });
        let mut mn = ModuleNormalizer::new();
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn normalize_common() {
        let mut ast = decl_from_src(
            "module foo = {
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
        let expected_ast = Decl::NormalizedModule(NormalizedModuleDecl {
            name: "foo".into(),
            params: vec![],
            impl_decls: vec![],
            spec_decls: vec![],
            common_spec_decls: [decl_from_src("var is_up: bool")].into(),
            common_impl_decls: [decl_from_src("instance net: sock.net")].into(),
        });

        let mut mn = ModuleNormalizer::new();
        ast.visit(&mut mn).expect("traversal");
        assert_eq!(ast, expected_ast);
    }
}
