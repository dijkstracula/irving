#![allow(dead_code)]

/// Corresponds to a file/line pairing, and possibly additionally docstrings to
/// be reconstructed in the extracted code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation<'a> {
    docstring: Vec<String>,
    file: &'a str,
    line: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ident {
    /// Used for all symbols occurring in the source, including operators like
    /// `+` and `&`.
    Str {
        val: String,
        subscripts: Vec<Ident>
    },

    /// Numerical identifiers are used as temporaries internally.    
    Num {
        val: u64
    },

    /// Represents members of a namespace.
    Dot {
        namesp: Box<Ident>,
        member: Box<Ident>
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Verb {
    Iff, Or, And, Lt, Leq, Gt, Gtq, Equals, Notequals,
    Plus, Minus, Times, Div,
    Empty, True, False, 
    Colon, Comma, Dot
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Expr<'a> {
    App {
        func: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
        ann: Annotation<'a>
    },

    Pi {
        vars: Vec<Expr<'a>>,
        body: Box<Expr<'a>>,
        ann: Annotation<'a>
    },

    Symbol {
        name: Ident,
        vrb: Verb,
        ann: Annotation<'a>
    },

    /// Used internally only as placeholders
    Variable {
        idx: u64,
        ann: Annotation<'a>
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActionKind {
    Internal,
    External,
    Imported,
    Exported
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Stmt {

}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Decl<'a> {
    Action {
        name: String,
        kind: ActionKind,
        inputs: Vec<Expr<'a>>,
        outputs: Vec<Expr<'a>>,
        body: Option<Stmt>,

        ann: Annotation<'a>,
    },

    Header {
        file: &'a str,
        ann: Annotation<'a>, 
    },

    Group {
        decls: Vec<Decl<'a>>,

        ann: Annotation<'a>
    },

    Include {
        file: Expr<'a>,
        ann: Annotation<'a>,
    },

    Init {
        body: Stmt,
        ann: Annotation<'a>,
    },

    Instance {
        objname: Expr<'a>,
        modname: Expr<'a>,
        prms: Vec<Expr<'a>>,
    },

    Instantiate {
        name: Expr<'a>,
        prms: Vec<Expr<'a>>,
        ann: Annotation<'a>, 
    },

    Interpretation {
        itype: Expr<'a>,
        ctype: Expr<'a>,
        ann: Annotation<'a>,
    },

    Module {
        name: Expr<'a>,
        prms: Vec<Expr<'a>>,
        body: Box<Decl<'a>>,
        ann: Annotation<'a>
    },

    Object {
        name: Expr<'a>,
        body: Box<Decl<'a>>,
        ann: Annotation<'a>
    },

    Type {
        sort: Expr<'a>,
        supr: Option<Expr<'a>>,
        /* spec: TypeSpec */
        ann: Annotation<'a>
    },

    Var {
        typing: Expr<'a>,
        is_destructor: bool,
        def: Option<Expr<'a>>,
        ann: Annotation<'a>
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prog<'a> {
    major_version: u8,
    minor_version: u8,

    decls: Vec<Decl<'a>>
}