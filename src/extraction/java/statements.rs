use std::fmt::Write;

use crate::{
    ast::{statements, toplevels},
    visitor::ast::{Visitable, Visitor},
};

use super::extraction::Extractor;

impl<W> Extractor<W>
where
    W: Write,
{
    pub fn emit_prog(&mut self, ast: &mut toplevels::Prog) -> Result<(), std::fmt::Error> {
        let imports = include_str!("templates/imports.txt");
        self.pp.write_str(imports)?;
        self.pp.write_str("\n")?;

        for inc in ast.includes.iter_mut() {
            self.begin_include_decl(&mut inc.name)?
                .and_then(|_| self.finish_include_decl(&mut inc.name))?;
            self.pp.write_str("\n")?;
        }
        self.pp.write_str("\n")?;

        self.pp
            .write_str("public class Extracted extends Protocol {\n")?;

        self.pp
            .write_str("public Extracted(MelinaContext ctx) { super(ctx); }\n")?;

        for decl in &mut ast.top {
            decl.visit(self)?.modifying(decl);
            self.pp.write_str("\n")?;
        }
        self.pp.write_str("\n}\n")?;
        Ok(())
    }

    pub fn emit_if(&mut self, ast: &mut statements::If) -> Result<(), std::fmt::Error> {
        self.pp.write_str("if (")?;
        ast.tst.visit(self)?;
        self.pp.write_str(") {\n")?;

        for stmt in &mut ast.thn {
            stmt.visit(self)?.modifying(stmt);
            self.pp.write_str(";\n")?;
        }

        if let Some(stmts) = &mut ast.els {
            self.pp.write_str("} else {\n")?;
            for stmt in stmts {
                stmt.visit(self)?.modifying(stmt);
                self.pp.write_str(";\n")?;
            }
        }
        self.pp.write_str("}\n")?;

        Ok(())
    }

    pub fn emit_while(&mut self, ast: &mut statements::While) -> Result<(), std::fmt::Error> {
        self.pp.write_str("while ")?;
        ast.test.visit(self)?;

        self.pp.write_str(" {\n")?;
        for stmt in &mut ast.doit {
            stmt.visit(self)?.modifying(stmt);
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("}\n")?;

        Ok(())
    }
}
