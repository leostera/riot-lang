use std::collections::{HashMap, HashSet};

use crate::{
    checker::{interface::ModuleInterface, scheme::Scheme, tst::Type},
    lambda::ir as lambda,
    parser::{ast::Literal, ident::Ident},
    prelude,
};

use super::ir;

pub struct Lowerer<'a> {
    interface: &'a ModuleInterface,
    exported: HashSet<String>,
}

impl<'a> Lowerer<'a> {
    pub fn new(interface: &'a ModuleInterface) -> Self {
        let exported = interface
            .values
            .iter()
            .map(|value| js_name(&value.name))
            .collect();
        Self {
            interface,
            exported,
        }
    }

    pub fn lower_module(&self, module: &lambda::Module) -> ir::Module {
        let mut items = Vec::new();
        let mut exports = Vec::new();

        for value in &module.values {
            let name = js_name(&value.name);
            if self.exported.contains(&name) {
                exports.push(name.clone());
            }
            match &value.body.kind {
                lambda::ExprKind::Function { params, body } => {
                    let param_names = params
                        .iter()
                        .enumerate()
                        .map(pattern_param_name)
                        .collect::<Vec<_>>();
                    items.push(ir::Item::Function {
                        name,
                        params: param_names
                            .iter()
                            .cloned()
                            .zip(
                                params
                                    .iter()
                                    .map(|param| Some(self.lower_type(&param.ty, false))),
                            )
                            .map(|(name, ty)| ir::Param { name, ty })
                            .collect(),
                        return_type: Some(self.lower_type(&body.ty, true)),
                        body: self.lower_function_body_with_params(params, &param_names, body),
                    })
                }
                _ => items.push(ir::Item::Const {
                    name,
                    ty: Some(self.lower_type(&value.body.ty, false)),
                    value: self.lower_expr(&value.body),
                }),
            }
        }

        if !exports.is_empty() {
            items.push(ir::Item::Export { names: exports });
        }

        items.extend(self.lower_interface_types());
        ir::Module { items }
    }

    fn lower_expr(&self, expr: &lambda::Expr) -> ir::Expr {
        self.lower_expr_with_env(expr, &HashMap::new())
    }

    fn lower_expr_with_env(
        &self,
        expr: &lambda::Expr,
        env: &HashMap<String, ir::Expr>,
    ) -> ir::Expr {
        match &expr.kind {
            lambda::ExprKind::Constant(constant) => lower_constant(constant),
            lambda::ExprKind::Var { name, .. } => env
                .get(&js_name(name))
                .cloned()
                .unwrap_or_else(|| ir::Expr::Var(js_name(name))),
            lambda::ExprKind::Construct { name, args, .. } => self.lower_construct(name, args),
            lambda::ExprKind::Function { params, body } => {
                let param_names = params
                    .iter()
                    .enumerate()
                    .map(pattern_param_name)
                    .collect::<Vec<_>>();
                ir::Expr::Function {
                    params: param_names.clone(),
                    body: self.lower_function_body_with_params(params, &param_names, body),
                }
            }
            lambda::ExprKind::Let { bind, body, .. } => ir::Expr::Call {
                callee: Box::new(ir::Expr::Function {
                    params: Vec::new(),
                    body: vec![
                        ir::Stmt::Const {
                            name: pattern_param_name((0, bind)),
                            value: ir::Expr::Undefined,
                        },
                        ir::Stmt::Return(self.lower_expr(body)),
                    ],
                }),
                args: Vec::new(),
            },
            lambda::ExprKind::Apply { callee, args } => ir::Expr::Call {
                callee: Box::new(self.lower_expr_with_env(callee, env)),
                args: args
                    .iter()
                    .map(|arg| self.lower_expr_with_env(arg, env))
                    .collect(),
            },
            lambda::ExprKind::Prim { op, args } => self.lower_primitive(op, args),
            lambda::ExprKind::Tuple(items) => {
                ir::Expr::Array(items.iter().map(|item| self.lower_expr(item)).collect())
            }
            lambda::ExprKind::Record { fields, .. } => ir::Expr::Object(
                fields
                    .iter()
                    .map(|(name, value)| ir::Property {
                        name: js_name(name),
                        value: self.lower_expr_with_env(value, env),
                    })
                    .collect(),
            ),
            lambda::ExprKind::Block(exprs) => self.lower_block_expr(exprs),
            lambda::ExprKind::Match { scrutinee, arms } => self.lower_match_expr(scrutinee, arms),
        }
    }

    fn lower_function_body_with_params(
        &self,
        params: &[lambda::Pattern],
        param_names: &[String],
        body: &lambda::Expr,
    ) -> Vec<ir::Stmt> {
        let mut stmts = Vec::new();
        let mut env = HashMap::new();
        for (pattern, name) in params.iter().zip(param_names) {
            self.bind_pattern_env(&mut env, pattern, ir::Expr::Var(name.clone()));
        }
        stmts.extend(self.lower_function_body_with_env(body, &env));
        stmts
    }

    fn lower_function_body_with_env(
        &self,
        body: &lambda::Expr,
        env: &HashMap<String, ir::Expr>,
    ) -> Vec<ir::Stmt> {
        match &body.kind {
            lambda::ExprKind::Block(exprs) => self.lower_block_returning_with_env(exprs, env),
            lambda::ExprKind::Match { scrutinee, arms } => {
                vec![self.lower_match_stmt(scrutinee, arms)]
            }
            _ => vec![ir::Stmt::Return(self.lower_expr_with_env(body, env))],
        }
    }

    fn lower_block_returning(&self, exprs: &[lambda::Expr]) -> Vec<ir::Stmt> {
        self.lower_block_returning_with_env(exprs, &HashMap::new())
    }

    fn lower_block_returning_with_env(
        &self,
        exprs: &[lambda::Expr],
        env: &HashMap<String, ir::Expr>,
    ) -> Vec<ir::Stmt> {
        let mut stmts = Vec::new();
        for (index, expr) in exprs.iter().enumerate() {
            match &expr.kind {
                lambda::ExprKind::Let { bind, body, .. } => stmts.push(ir::Stmt::Const {
                    name: pattern_param_name((0, bind)),
                    value: self.lower_expr_with_env(body, env),
                }),
                _ if index + 1 == exprs.len() => {
                    stmts.push(ir::Stmt::Return(self.lower_expr_with_env(expr, env)));
                }
                _ => {
                    stmts.push(ir::Stmt::Expr(self.lower_expr_with_env(expr, env)));
                }
            }
        }
        if stmts.is_empty() {
            stmts.push(ir::Stmt::Return(ir::Expr::Undefined));
        }
        stmts
    }

    fn lower_block_expr(&self, exprs: &[lambda::Expr]) -> ir::Expr {
        ir::Expr::Call {
            callee: Box::new(ir::Expr::Function {
                params: Vec::new(),
                body: self.lower_block_returning(exprs),
            }),
            args: Vec::new(),
        }
    }

    fn lower_construct(&self, name: &Ident, args: &[lambda::Expr]) -> ir::Expr {
        let tag = name_string(name);
        if args.is_empty() {
            return ir::Expr::Object(vec![ir::Property {
                name: "type".into(),
                value: ir::Expr::String(tag),
            }]);
        }

        let mut properties = vec![ir::Property {
            name: "type".into(),
            value: ir::Expr::String(tag),
        }];
        if args.len() == 1 {
            properties.push(ir::Property {
                name: "value".into(),
                value: self.lower_expr(&args[0]),
            });
        } else {
            properties.push(ir::Property {
                name: "value".into(),
                value: ir::Expr::Array(args.iter().map(|arg| self.lower_expr(arg)).collect()),
            });
        }
        ir::Expr::Object(properties)
    }

    fn lower_primitive(&self, op: &lambda::Primitive, args: &[lambda::Expr]) -> ir::Expr {
        let bin = match op {
            lambda::Primitive::Add { .. } => Some(ir::BinaryOp::Add),
            lambda::Primitive::Sub { .. } => Some(ir::BinaryOp::Sub),
            lambda::Primitive::Mul { .. } => Some(ir::BinaryOp::Mul),
            lambda::Primitive::Div { .. } => Some(ir::BinaryOp::Div),
            lambda::Primitive::Rem { .. } => Some(ir::BinaryOp::Rem),
            lambda::Primitive::Named(_) => None,
        };
        if let (Some(op), [left, right]) = (bin, args) {
            ir::Expr::Binary {
                op,
                left: Box::new(self.lower_expr(left)),
                right: Box::new(self.lower_expr(right)),
            }
        } else {
            ir::Expr::Call {
                callee: Box::new(ir::Expr::Var(match op {
                    lambda::Primitive::Named(name) => js_name(name),
                    _ => "__tinyml_primitive".into(),
                })),
                args: args.iter().map(|arg| self.lower_expr(arg)).collect(),
            }
        }
    }

    fn lower_match_expr(&self, scrutinee: &lambda::Expr, arms: &[lambda::MatchArm]) -> ir::Expr {
        ir::Expr::Call {
            callee: Box::new(ir::Expr::Function {
                params: Vec::new(),
                body: vec![self.lower_match_stmt(scrutinee, arms)],
            }),
            args: Vec::new(),
        }
    }

    fn lower_match_stmt(&self, scrutinee: &lambda::Expr, arms: &[lambda::MatchArm]) -> ir::Stmt {
        if let Some(switch) = self.lower_switch_match(scrutinee, arms) {
            return switch;
        }

        let scrutinee_expr = self.lower_expr(scrutinee);
        let mut current_else = Vec::new();
        for arm in arms.iter().rev() {
            let branch = self.lower_match_branch(&arm.pattern, &arm.body, &scrutinee_expr);
            if matches!(arm.pattern.kind, lambda::PatternKind::Wildcard) {
                current_else = branch;
            } else {
                current_else = vec![ir::Stmt::If {
                    condition: pattern_test_expr(&arm.pattern, scrutinee_expr.clone()),
                    then_branch: branch,
                    else_branch: current_else,
                }];
            }
        }
        ir::Stmt::Block(current_else)
    }

    fn lower_switch_match(
        &self,
        scrutinee: &lambda::Expr,
        arms: &[lambda::MatchArm],
    ) -> Option<ir::Stmt> {
        let mut constructor_match = false;
        let mut literal_match = false;
        let mut cases = Vec::new();
        let mut default = Vec::new();
        let scrutinee_expr = self.lower_expr(scrutinee);

        for arm in arms {
            match &arm.pattern.kind {
                lambda::PatternKind::Constant(constant) => {
                    literal_match = true;
                    cases.push(ir::SwitchCase {
                        test: lower_constant(constant),
                        body: self.lower_match_branch(&arm.pattern, &arm.body, &scrutinee_expr),
                    });
                }
                lambda::PatternKind::Construct { name, .. } => {
                    constructor_match = true;
                    cases.push(ir::SwitchCase {
                        test: ir::Expr::String(name_string(name)),
                        body: self.lower_match_branch(&arm.pattern, &arm.body, &scrutinee_expr),
                    });
                }
                lambda::PatternKind::Wildcard => {
                    default = self.lower_match_branch(&arm.pattern, &arm.body, &scrutinee_expr);
                }
                _ => return None,
            }
        }

        if constructor_match && literal_match {
            return None;
        }
        let discriminant = if constructor_match {
            ir::Expr::Member {
                object: Box::new(scrutinee_expr),
                property: "type".into(),
            }
        } else {
            scrutinee_expr
        };
        Some(ir::Stmt::Switch {
            discriminant,
            cases,
            default,
        })
    }

    fn lower_match_branch(
        &self,
        pattern: &lambda::Pattern,
        body: &lambda::Expr,
        scrutinee: &ir::Expr,
    ) -> Vec<ir::Stmt> {
        let mut env = HashMap::new();
        self.bind_pattern_env(&mut env, pattern, scrutinee.clone());
        vec![ir::Stmt::Return(self.lower_expr_with_env(body, &env))]
    }

    fn bind_pattern_env(
        &self,
        env: &mut HashMap<String, ir::Expr>,
        pattern: &lambda::Pattern,
        value: ir::Expr,
    ) {
        match &pattern.kind {
            lambda::PatternKind::Var { name, .. } => {
                env.insert(js_name(name), value);
            }
            lambda::PatternKind::Record { fields, .. } => {
                for (field, pattern) in fields {
                    self.bind_pattern_env(
                        env,
                        pattern,
                        ir::Expr::Member {
                            object: Box::new(value.clone()),
                            property: js_name(field),
                        },
                    );
                }
            }
            lambda::PatternKind::Tuple(items) => {
                for (index, item) in items.iter().enumerate() {
                    self.bind_pattern_env(
                        env,
                        item,
                        ir::Expr::Member {
                            object: Box::new(value.clone()),
                            property: index.to_string(),
                        },
                    );
                }
            }
            lambda::PatternKind::Construct { args, .. } => {
                if args.len() == 1 {
                    self.bind_pattern_env(
                        env,
                        &args[0],
                        ir::Expr::Member {
                            object: Box::new(value),
                            property: "value".into(),
                        },
                    );
                } else {
                    for (index, arg) in args.iter().enumerate() {
                        self.bind_pattern_env(
                            env,
                            arg,
                            ir::Expr::Member {
                                object: Box::new(ir::Expr::Member {
                                    object: Box::new(value.clone()),
                                    property: "value".into(),
                                }),
                                property: index.to_string(),
                            },
                        );
                    }
                }
            }
            _ => {}
        }
    }

    fn lower_interface_types(&self) -> Vec<ir::Item> {
        let mut declarations = Vec::new();
        for ty in &self.interface.types {
            declarations.push(ir::Item::TypeAlias {
                name: name_string(&ty.name),
                params: ty.params.iter().map(|param| param.name.clone()).collect(),
                ty: self.lower_type(&ty.body, false),
                exported: true,
            });
        }
        for value in &self.interface.values {
            declarations.push(ir::Item::TypeValue {
                name: js_name(&value.name),
                ty: self.lower_scheme(&value.scheme),
                exported: true,
            });
        }
        declarations
    }

    fn lower_scheme(&self, scheme: &Scheme) -> ir::TsType {
        self.lower_type(&scheme.body, false)
    }

    fn lower_type(&self, ty: &Type, _return_position: bool) -> ir::TsType {
        match ty {
            Type::Var(_) => ir::TsType::Unknown,
            Type::Generic(var) => ir::TsType::TypeParam(format!("T{}", var.index())),
            Type::Tuple(items) => ir::TsType::Tuple(
                items
                    .iter()
                    .map(|item| self.lower_type(item, false))
                    .collect(),
            ),
            Type::Arrow { .. } => self.lower_function_type(ty),
            Type::Apply { ident, arguments } => match ident.as_name() {
                Some(
                    "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "f8" | "f16"
                    | "f32" | "f64",
                ) => ir::TsType::Number,
                Some("bool") => ir::TsType::Boolean,
                Some("string") => ir::TsType::String,
                Some("unit") => ir::TsType::Undefined,
                Some(name) if prelude::PRIMITIVE_TYPES.contains(&name) => ir::TsType::Unknown,
                _ => ir::TsType::Named {
                    name: name_string(ident),
                    args: arguments
                        .iter()
                        .map(|arg| self.lower_type(arg, false))
                        .collect(),
                },
            },
        }
    }

    fn lower_function_type(&self, ty: &Type) -> ir::TsType {
        let mut params = Vec::new();
        let mut current = ty;
        while let Type::Arrow { parameter, result } = current {
            params.push(self.lower_type(parameter, false));
            current = result;
        }
        ir::TsType::Function {
            params,
            result: Box::new(self.lower_type(current, true)),
        }
    }
}

fn lower_constant(constant: &lambda::Constant) -> ir::Expr {
    match constant {
        lambda::Constant::Unit => ir::Expr::Undefined,
        lambda::Constant::Literal(Literal::Int(value) | Literal::Float(value)) => {
            ir::Expr::Number(value.clone())
        }
        lambda::Constant::Literal(Literal::String(value)) => {
            ir::Expr::String(value.trim_matches('"').to_string())
        }
        lambda::Constant::Literal(Literal::Bool(value)) => ir::Expr::Bool(*value),
    }
}

fn pattern_test_expr(pattern: &lambda::Pattern, scrutinee: ir::Expr) -> ir::Expr {
    match &pattern.kind {
        lambda::PatternKind::Constant(constant) => ir::Expr::Binary {
            op: ir::BinaryOp::Eq,
            left: Box::new(scrutinee),
            right: Box::new(lower_constant(constant)),
        },
        lambda::PatternKind::Construct { name, .. } => ir::Expr::Binary {
            op: ir::BinaryOp::Eq,
            left: Box::new(ir::Expr::Member {
                object: Box::new(scrutinee),
                property: "type".into(),
            }),
            right: Box::new(ir::Expr::String(name_string(name))),
        },
        lambda::PatternKind::Wildcard | lambda::PatternKind::Var { .. } => ir::Expr::Bool(true),
        _ => ir::Expr::Bool(false),
    }
}

fn pattern_param_name((index, pattern): (usize, &lambda::Pattern)) -> String {
    match &pattern.kind {
        lambda::PatternKind::Var { name, .. } => js_name(name),
        _ if index == 0 => "$x".into(),
        _ => format!("$x{index}"),
    }
}

fn js_name(ident: &Ident) -> String {
    let name = name_string(ident);
    if is_valid_js_ident(&name) && !is_reserved(&name) {
        name
    } else {
        name.chars()
            .map(|ch| {
                if ch == '_' || ch == '$' || ch.is_ascii_alphanumeric() {
                    ch
                } else {
                    '_'
                }
            })
            .collect()
    }
}

fn name_string(ident: &Ident) -> String {
    match ident {
        Ident::Name { name, .. } => name.clone(),
        Ident::Qualified { name, rest, .. } => format!("{}_{}", name, name_string(rest)),
    }
}

fn is_valid_js_ident(name: &str) -> bool {
    let mut chars = name.chars();
    chars
        .next()
        .is_some_and(|ch| ch == '_' || ch == '$' || ch.is_ascii_alphabetic())
        && chars.all(|ch| ch == '_' || ch == '$' || ch.is_ascii_alphanumeric())
}

fn is_reserved(name: &str) -> bool {
    matches!(
        name,
        "break"
            | "case"
            | "catch"
            | "class"
            | "const"
            | "continue"
            | "debugger"
            | "default"
            | "delete"
            | "do"
            | "else"
            | "export"
            | "extends"
            | "finally"
            | "for"
            | "function"
            | "if"
            | "import"
            | "in"
            | "instanceof"
            | "new"
            | "return"
            | "super"
            | "switch"
            | "this"
            | "throw"
            | "try"
            | "typeof"
            | "var"
            | "void"
            | "while"
            | "with"
            | "yield"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        checker::{
            interface::ValueDescription,
            scheme::Scheme,
            tst::{BindingId, EntityId},
        },
        parser::lexer::Span,
    };

    fn ident(name: &str) -> Ident {
        Ident::Name {
            name: name.into(),
            span: Span::new(0, 0),
        }
    }

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn unit_type() -> Type {
        Type::Apply {
            ident: ident("unit"),
            arguments: Vec::new(),
        }
    }

    fn int_expr(value: &str) -> lambda::Expr {
        lambda::Expr {
            ty: unit_type(),
            kind: lambda::ExprKind::Constant(lambda::Constant::Literal(Literal::Int(value.into()))),
            span: span(),
        }
    }

    fn module_with_value(name: &str, body: lambda::Expr) -> lambda::Module {
        lambda::Module {
            values: vec![lambda::ValueDecl {
                binding: BindingId::new(0),
                entity: EntityId::new(0),
                name: ident(name),
                ty: unit_type(),
                body,
                span: span(),
            }],
        }
    }

    fn interface_with_value(name: &str) -> ModuleInterface {
        ModuleInterface {
            values: vec![ValueDescription {
                name: ident(name),
                scheme: Scheme::monomorphic(unit_type()),
            }],
            constructors: Vec::new(),
            record_fields: Vec::new(),
            types: Vec::new(),
            modules: Vec::new(),
        }
    }

    #[test]
    fn preserves_valid_js_names() {
        assert_eq!(js_name(&ident("answer")), "answer");
        assert_eq!(js_name(&ident("with-hyphen")), "with_hyphen");
    }

    #[test]
    fn lowers_tuple_item_count() {
        let module = module_with_value(
            "pair",
            lambda::Expr {
                ty: unit_type(),
                kind: lambda::ExprKind::Tuple(vec![int_expr("1"), int_expr("2")]),
                span: span(),
            },
        );
        let js = Lowerer::new(&interface_with_value("pair")).lower_module(&module);
        assert!(matches!(
            &js.items[0],
            ir::Item::Const {
                value: ir::Expr::Array(items),
                ..
            } if items.len() == 2
        ));
    }

    #[test]
    fn lowers_record_field_names() {
        let module = module_with_value(
            "point",
            lambda::Expr {
                ty: unit_type(),
                kind: lambda::ExprKind::Record {
                    name: ident("Point"),
                    fields: vec![(ident("x"), int_expr("1")), (ident("y"), int_expr("2"))],
                },
                span: span(),
            },
        );
        let js = Lowerer::new(&interface_with_value("point")).lower_module(&module);
        assert!(matches!(
            &js.items[0],
            ir::Item::Const {
                value: ir::Expr::Object(fields),
                ..
            } if fields.iter().map(|field| field.name.as_str()).collect::<Vec<_>>() == ["x", "y"]
        ));
    }

    #[test]
    fn lowers_multi_field_constructor_to_tuple_value_field() {
        let module = module_with_value(
            "pair",
            lambda::Expr {
                ty: unit_type(),
                kind: lambda::ExprKind::Construct {
                    name: ident("Pair"),
                    entity: EntityId::new(0),
                    args: vec![int_expr("1"), int_expr("2")],
                },
                span: span(),
            },
        );
        let js = Lowerer::new(&interface_with_value("pair")).lower_module(&module);
        assert!(matches!(
            &js.items[0],
            ir::Item::Const {
                value: ir::Expr::Object(fields),
                ..
            } if fields.len() == 2
                && fields[0].name == "type"
                && fields[1].name == "value"
                && matches!(&fields[1].value, ir::Expr::Array(items) if items.len() == 2)
        ));
    }

    #[test]
    fn lowers_function_arity_and_export_declaration() {
        let body = lambda::Expr {
            ty: unit_type(),
            kind: lambda::ExprKind::Function {
                params: vec![
                    lambda::Pattern {
                        ty: unit_type(),
                        kind: lambda::PatternKind::Var {
                            name: ident("left"),
                            binding: BindingId::new(1),
                        },
                        span: span(),
                    },
                    lambda::Pattern {
                        ty: unit_type(),
                        kind: lambda::PatternKind::Var {
                            name: ident("right"),
                            binding: BindingId::new(2),
                        },
                        span: span(),
                    },
                ],
                body: Box::new(int_expr("1")),
            },
            span: span(),
        };
        let js = Lowerer::new(&interface_with_value("add"))
            .lower_module(&module_with_value("add", body));
        assert!(matches!(
            &js.items[0],
            ir::Item::Function { params, .. }
                if params.iter().map(|param| param.name.as_str()).collect::<Vec<_>>() == ["left", "right"]
        ));
        assert!(matches!(
            &js.items[1],
            ir::Item::Export { names } if names == &["add"]
        ));
    }

    #[test]
    fn lowers_constructor_match_to_switch_on_type() {
        let scrutinee = lambda::Expr {
            ty: unit_type(),
            kind: lambda::ExprKind::Var {
                name: ident("tree"),
                entity: EntityId::new(0),
            },
            span: span(),
        };
        let body = lambda::Expr {
            ty: unit_type(),
            kind: lambda::ExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms: vec![lambda::MatchArm {
                    pattern: lambda::Pattern {
                        ty: unit_type(),
                        kind: lambda::PatternKind::Construct {
                            name: ident("Leaf"),
                            entity: EntityId::new(1),
                            args: Vec::new(),
                        },
                        span: span(),
                    },
                    body: int_expr("0"),
                    span: span(),
                }],
            },
            span: span(),
        };
        let js = Lowerer::new(&interface_with_value("size"))
            .lower_module(&module_with_value("size", body));
        assert!(matches!(
            &js.items[0],
            ir::Item::Const {
                value: ir::Expr::Call { callee, .. },
                ..
            } if matches!(callee.as_ref(), ir::Expr::Function { body, .. }
                if matches!(&body[0], ir::Stmt::Switch {
                    discriminant: ir::Expr::Member { property, .. },
                    cases,
                    ..
                } if property == "type" && cases.len() == 1))
        ));
    }

    #[test]
    fn lowers_ts_primitive_values() {
        let interface = ModuleInterface {
            values: vec![ValueDescription {
                name: ident("flag"),
                scheme: Scheme::monomorphic(Type::Apply {
                    ident: ident("bool"),
                    arguments: Vec::new(),
                }),
            }],
            constructors: Vec::new(),
            record_fields: Vec::new(),
            types: Vec::new(),
            modules: Vec::new(),
        };

        let module = Lowerer::new(&interface).lower_module(&lambda::Module { values: Vec::new() });
        assert!(matches!(
            &module.items[0],
            ir::Item::TypeValue {
                name,
                ty: ir::TsType::Boolean,
                exported: true
            } if name == "flag"
        ));
    }
}
