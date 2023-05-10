#![allow(dead_code)]

/// Corresponds to a file/line pairing, and possibly additionally docstrings to
/// be reconstructed in the extracted code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annotation {
    docstring: Vec<String>,
    file: String,
    line: u32,
}

type Ident = Vec<Symbol>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Verb {
    Iff, Or, And, Lt, Le, Gt, Ge, Equals, Notequals, Not, Arrow,
    Plus, Minus, Times, Div,
    Empty, True, False, 
    Colon, Comma, Dot
}

pub type Symbol = String;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AppExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexExpr {
    pub lhs: Box<Expr>,
    pub idx: Box<Expr>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum Formula {
    Forall {
        params: Vec<Param>,
        expr: Box<Expr>
    },
    Exists {
        params: Vec<Param>,
        expr: Box<Expr>
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum Expr {
    App(AppExpr),

    BinOp {
        lhs: Box<Expr>,
        op: Verb,
        rhs: Box<Expr>
    },

    Formula(Formula),

    Identifier(Ident),

    Index(IndexExpr),

    Number(i64),

    Pi {
        terms: Vec<Expr>,
        body: Box<Expr>,
    },

    Subscript {
        val: Box<Expr>,
        subscripts: Vec<Expr>
    },

    UnaryOp{op: Verb, expr: Box<Expr>},

    Term(Term)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ActionKind {
    Internal,
    External,
    Imported,
    Exported
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeclSig {
    pub name: Vec<Symbol>,
    pub params: Vec<Param>,
}

pub type DeclRet = Option<Term>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ActionDecl {
    pub name: Vec<String>,
    pub kind: ActionKind,
    pub params: Vec<Param>,
    pub ret: Option<Param>,
    pub body: Option<Vec<Decl>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AfterDecl {
    pub name: Vec<String>,
    pub params: Option<Vec<Param>>,
    pub ret:  Option<Param>,
    pub body: Vec<Decl>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BeforeDecl {
    pub name: Vec<String>,
    pub params: Option<Vec<Param>>,
    pub body: Vec<Decl>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDecl {
    pub name: Vec<String>,
    pub params: Vec<Param>,
    pub ret: Symbol
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExportDecl {
    Action(ActionDecl),
    ForwardRef(Vec<Symbol>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportDecl {
    pub name: Symbol,
    pub params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstanceDecl {
    pub name: Vec<Symbol>,
    pub sort: Vec<Symbol>,
    pub args: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IsolateDecl {
    pub name: Symbol,
    pub params: Vec<Param>,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleDecl {
    pub name: Vec<String>,
    pub params: Vec<Param>,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjectDecl {
    pub name: Vec<String>,
    pub params: Vec<Param>,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub id: Symbol,
    pub sort: Option<Ident>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Relation {
    pub name: Vec<String>,
    pub params: Vec<Param>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Sort {
    Range(Box<Expr>, Box<Expr>),
    Enum(Vec<Symbol>),
    Subclass(Symbol),
    Uninterpreted,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub name: Symbol,
    pub sort: Sort
    /* spec: TypeSpec */
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Term {
    pub id: Ident,
    pub sort: Option<Ident>,
    //is_destructor: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum Decl {

    Action(ActionDecl),

    AfterAction(AfterDecl),

    Alias(Symbol, Expr),

    Axiom(Expr),

    BeforeAction(BeforeDecl),

    Export(ExportDecl),

    Import(ImportDecl),

    Isolate(IsolateDecl),

    Function(FunctionDecl),

    Header {
        file: String,
    },

    Globals(Vec<Decl>),

    Group {
        decls: Vec<Decl>,
    },

    Include(Symbol),

    Instance(InstanceDecl),

    Instantiate {
        name: Expr,
        prms: Vec<Expr>,
    },

    Interpretation {
        itype: Expr,
        ctype: Expr,
    },
    
    Invariant(Expr),

    Module(ModuleDecl),

    Object(ObjectDecl),

    Relation(Relation),

    Stmts(Vec<Stmt>),

    Var(Term),

    Type(Type),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub struct If {
    pub tst: Expr,
    pub thn: Vec<Stmt>,
    pub els: Option<Vec<Stmt>>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub struct While {
    pub test: Expr,
    pub doit: Vec<Stmt>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum Stmt {
    CompoundActions(Vec<Action>),
    If(If),
    While(While),
    Expr(Expr),
}

// Actions

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub struct AssignAction {
    pub lhs: Expr,
    pub rhs: Expr
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub struct AssertAction{
    pub pred: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssumeAction{
    pub pred: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnsureAction{
    pub pred: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RequiresAction{
    pub pred: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Action {
    Assert(AssertAction),
    Assign(AssignAction),
    Assume(AssumeAction),
    Ensure(EnsureAction),
    Requires(RequiresAction),
}


// Top levels


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prog {
    pub major_version: u8,
    pub minor_version: u8,

    pub top: IsolateDecl,
}