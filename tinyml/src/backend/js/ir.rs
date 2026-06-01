//! TypeScript backend IR.
//!
//! The IR is a single algebra for runtime declarations and type declarations so
//! the emitter can produce `.ts` output without coordinating separate JS and DTS
//! modules.

/// A complete JS-backend module containing runtime JavaScript and TypeScript declaration items.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// Top-level JS-backend items. The emitter can project these into `.js`, `.d.ts`, or `.ts` output.
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    /// `import { imported as local } from "module";`
    Import {
        names: Vec<ImportName>,
        from: String,
    },
    /// `const name: Type = value;`
    Const {
        name: String,
        ty: Option<TsType>,
        value: Expr,
    },
    /// `function name(param, ...): Type { ... }`
    Function {
        name: String,
        params: Vec<Param>,
        return_type: Option<TsType>,
        body: Vec<Stmt>,
    },
    /// `export { name, ... };`
    Export { names: Vec<String> },
    /// `export declare const name: type;`
    TypeValue {
        name: String,
        ty: TsType,
        exported: bool,
    },
    /// `export type Name<T> = type;`
    TypeAlias {
        name: String,
        params: Vec<String>,
        ty: TsType,
        exported: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    /// JavaScript/TypeScript parameter name.
    pub name: String,
    /// Optional TypeScript annotation for the parameter.
    pub ty: Option<TsType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportName {
    /// Imported binding name as written by the source module.
    pub imported: String,
    /// Local JavaScript binding name.
    pub local: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// `const name = value;`
    Const { name: String, value: Expr },
    /// `expr;`
    Expr(Expr),
    /// `return expr;`
    Return(Expr),
    /// `{ ... }`
    Block(Vec<Stmt>),
    /// `if (condition) { ... } else { ... }`
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Vec<Stmt>,
    },
    /// `switch (discriminant) { case test: ... default: ... }`
    Switch {
        discriminant: Expr,
        cases: Vec<SwitchCase>,
        default: Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchCase {
    /// `case test:` expression.
    pub test: Expr,
    /// Statements emitted for this case.
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Numeric literal, emitted exactly as stored.
    Number(String),
    /// JavaScript string literal.
    String(String),
    /// `true` or `false`.
    Bool(bool),
    /// `undefined`.
    Undefined,
    /// `null`.
    Null,
    /// JavaScript identifier reference.
    Var(String),
    /// Arrow function expression: `(params) => { body }`.
    Function {
        params: Vec<String>,
        body: Vec<Stmt>,
    },
    /// Call expression: `callee(arg, ...)`.
    Call { callee: Box<Expr>, args: Vec<Expr> },
    /// Binary expression: `(left op right)`.
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Array literal: `[item, ...]`.
    Array(Vec<Expr>),
    /// Object literal: `{ key: value, ... }`.
    Object(Vec<Property>),
    /// Member access: `object.property` or `object["property"]`.
    Member { object: Box<Expr>, property: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    /// Object property name.
    pub name: String,
    /// Object property value expression.
    pub value: Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
    /// `===`
    Eq,
    /// `!==`
    Ne,
    /// `<`
    Lt,
    /// `<=`
    Lte,
    /// `>`
    Gt,
    /// `>=`
    Gte,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TsType {
    /// `number`.
    Number,
    /// `boolean`.
    Boolean,
    /// `string`.
    String,
    /// `undefined`.
    Undefined,
    /// `void`.
    Void,
    /// `unknown`.
    Unknown,
    /// Generic type parameter reference, e.g. `T`.
    TypeParam(String),
    /// Readonly tuple type, e.g. `readonly [A, B]`.
    Tuple(Vec<TsType>),
    /// Function type, e.g. `(arg0: A) => B`.
    Function {
        params: Vec<TsType>,
        result: Box<TsType>,
    },
    /// Object type literal, e.g. `{ readonly x: number }`.
    Object(Vec<TsField>),
    /// Union type, e.g. `A | B`.
    Union(Vec<TsType>),
    /// Named type application, e.g. `Option<T>`.
    Named { name: String, args: Vec<TsType> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TsField {
    /// Field name in an object type literal.
    pub name: String,
    /// Whether the field is emitted as optional with `?`.
    pub optional: bool,
    /// Field TypeScript type.
    pub ty: TsType,
}
