use std::fmt::{Error, Write};

use crate::ast::{
    declarations::{self, Binding},
    expressions::{self, Sort},
};

use super::{extraction::Extractor, types::JavaType};

/// Class extraction for Ivy is slightly more convoluted. The rough
/// procedure is that for a class C, we generate a metaclass in Java
/// with that the name Ivy_CFactory, which will extend irving.IvyClass.
/// Then we generate the actual class C.
///
/// TODO: When we are mixing in actions for C, we need to actually redirect
/// those assignmetns to the CFactory.

impl<W> Extractor<W>
where
    W: Write,
{
    pub fn factory_classname(name: &str) -> String {
        format!("Ivy_{}_Factory", name)
    }

    pub fn emit_class_definition(
        &mut self,
        name: &mut expressions::Token,
        ast: &mut declarations::ClassDecl,
    ) -> Result<(), Error> {
        self.pp.write_fmt(format_args!("class {} {{\n", name))?;

        self.pp.write_str("// Fields\n")?;
        for Binding { name, decl, .. } in &ast.fields {
            let Sort::Resolved(sort) = decl else {
                panic!()
            };
            let javatype: JavaType = sort.into();
            self.pp
                .write_fmt(format_args!("public {} {};\n", javatype.as_jval(), name))?;
        }

        self.pp.write_str("\n")?;

        self.pp.write_str("// Actions\n")?;
        for Binding {
            name: actname,
            decl,
            span,
        } in &mut ast.actions
        {
            self.emit_curried_delegate_action(span, name, actname, decl)?;
        }

        self.pp.write_str("\n}\n")?;
        Ok(())
    }

    pub fn emit_class_factory(
        &mut self,
        name: &mut expressions::Token,
        ast: &mut declarations::ClassDecl,
    ) -> Result<(), Error> {
        self.pp.write_fmt(format_args!(
            "class {} extends IvyClass<{}> {{\n",
            Self::factory_classname(name),
            name
        ))?;

        self.pp.write_str("// Actions\n")?;
        for Binding { name, decl, span } in &mut ast.actions {
            self.pp.write_str("public static ")?;
            self.emit_action_object_declaration(span, name, decl)?;
            self.pp.write_str(";\n")?;
        }

        self.pp.write_str("\n\n")?;

        self.pp.write_str("// Fields\n")?;
        for Binding { name, decl, .. } in &ast.fields {
            let Sort::Resolved(sort) = decl else {
                panic!()
            };
            let javatype: JavaType = sort.into();

            self.pp.write_str("IvySort<")?;
            self.pp.write_str(&javatype.as_jref())?;
            self.pp.write_str(", ?> ")?; // XXX: Will we care about the Z3 type?
            self.pp.write_str(name)?;
            self.pp.write_str(";\n")?;
        }

        // Constructor
        self.pp.write_str("\n\n")?;
        self.pp.write_fmt(format_args!(
            "public {}(MelinaContext ctx) {{\n",
            Self::factory_classname(name)
        ))?;
        self.pp
            .write_fmt(format_args!("super(\"{}\", ctx);\n", name))?;

        for Binding { name, decl, .. } in &ast.fields {
            let Sort::Resolved(sort) = decl else {
                panic!()
            };
            let javatype: JavaType = sort.into();

            self.pp
                .write_fmt(format_args!("{} = {};", name, javatype.melina_generator()))?;
        }
        self.pp.write_str("\n}\n")?;

        // make()
        self.pp.write_str("\n")?;
        self.pp.write_str("@Override\n")?;
        self.pp.write_fmt(format_args!(
            "public {} make() {{ return new {}(); }}\n",
            name, name
        ))?;

        // generator()
        self.pp.write_str("\n")?;
        self.pp.write_str("@Override\n")?;
        self.pp
            .write_fmt(format_args!("public Supplier<{}> generator() {{\n", name))?;

        for Binding { name, decl, .. } in &ast.fields {
            let Sort::Resolved(sort) = decl else {
                panic!()
            };
            let javatype: JavaType = sort.into();
            self.pp.write_fmt(format_args!(
                "Supplier<{}> {}_gen = {}.generator();\n",
                javatype.as_jref(),
                name,
                name
            ))?;
        }

        self.pp.write_str("return () -> {\n")?;
        self.pp
            .write_fmt(format_args!("{} ret = make();\n", name))?;
        for Binding { name, .. } in &ast.fields {
            self.pp
                .write_fmt(format_args!("ret.{} = {}_gen.get();\n", name, name))?;
        }
        self.pp.write_str("return ret;\n")?;
        self.pp.write_str("};")?;
        self.pp.write_str("\n}\n")?;

        self.pp.write_str("\n}\n")?;
        Ok(())
    }
}
