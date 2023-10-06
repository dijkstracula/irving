use std::fmt::{Error, Write};

use crate::{
    ast::{declarations, expressions, span::Span},
    typechecker::sorts::IvySort,
    visitor::ast::Visitor,
};

use super::extraction::Extractor;

impl<W> Extractor<W>
where
    W: Write,
{
    pub fn emit_curried_delegate_action(
        &mut self,
        span: &Span,
        classname: &mut expressions::Token,
        actname: &mut expressions::Token,
        decl: &mut declarations::ActionDecl,
    ) -> Result<(), Error> {
        // Note that we curry out the first parameter in this case.
        let arity = decl.params.len() - 1;
        let mut ret = decl.ret.clone().or(Some(expressions::Symbol {
            name: "_void".into(),
            decl: expressions::Sort::Resolved(IvySort::Unit),
            span: span.clone(),
        }));
        self.pp.write_str("public ")?;

        self.pp.write_fmt(format_args!("Action{}<", arity))?;
        let mut sorts: Vec<expressions::Sort> = decl
            .params
            .iter_mut()
            .chain(ret.iter_mut())
            .skip(1)
            .map(|sym| sym.decl.clone())
            .collect::<_>();
        self.write_separated(&mut sorts, ", ")?;
        self.pp
            .write_fmt(format_args!("> {actname} = new Action{arity}<>(("))?;

        decl.params
            .iter_mut()
            .skip(1)
            .enumerate()
            .map(|(i, b)| {
                if i > 0 {
                    self.pp.write_str(", ")?;
                }
                self.param(b)?.modifying(b);
                Ok(())
            })
            .collect::<Result<Vec<_>, Error>>()?;
        self.pp.write_str(") -> ")?;
        self.pp.write_fmt(format_args!(
            "{}.{}.apply(this",
            Self::factory_classname(classname),
            actname
        ))?;
        decl.params
            .iter_mut()
            .skip(1)
            .map(|b| {
                self.pp.write_fmt(format_args!(", {}", b.name))?;
                Ok(())
            })
            .collect::<Result<Vec<_>, Error>>()?;
        self.pp.write_str("));\n")?;

        Ok(())
    }

    pub fn emit_action_object_declaration(
        &mut self,
        span: &Span,
        name: &mut expressions::Token,
        ast: &mut declarations::ActionDecl,
    ) -> Result<(), Error> {
        let arity = ast.params.len();
        let mut ret = ast.ret.clone().or(Some(expressions::Symbol {
            name: "_void".into(),
            decl: expressions::Sort::Resolved(IvySort::Unit),
            span: span.clone(),
        }));

        self.pp.write_fmt(format_args!("Action{}<", arity))?;
        let mut sorts: Vec<expressions::Sort> = ast
            .params
            .iter_mut()
            .chain(ret.iter_mut())
            .map(|sym| sym.decl.clone())
            .collect::<_>();
        self.write_separated(&mut sorts, ", ")?;
        self.pp
            .write_fmt(format_args!("> {name} = new Action{arity}<>("))?;

        if let Some(ref mut body) = &mut ast.body {
            self.write_lambda(&mut ast.params, &mut ast.ret, body)?;
        }

        self.pp.write_str(")")?;
        Ok(())
    }
}
