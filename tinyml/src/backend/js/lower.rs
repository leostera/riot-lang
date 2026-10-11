//! Lambda-to-TypeScript backend lowering.
//!
//! Lowering here is representation work only: it translates existing Lambda
//! values and checker-provided type information into JS-backend IR. It does not
//! infer new types.

use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
};

use crate::{
    checker::{interface::ModuleInterface, scheme::Scheme, tir::Type},
    lambda::ir as lambda,
    parser::{ast::Literal, ident::Ident},
    prelude,
};

use super::ir;

pub struct Lowerer<'a> {
    interface: &'a ModuleInterface,
    exported: HashSet<String>,
    variant_type_names: HashSet<String>,
    match_counter: Cell<usize>,
}

impl<'a> Lowerer<'a> {
    pub fn new(interface: &'a ModuleInterface) -> Self {
        Self::with_imported_constructors(interface, &[])
    }

    pub fn with_imported_constructors(
        interface: &'a ModuleInterface,
        imported_constructors: &[crate::checker::interface::ConstructorDescription],
    ) -> Self {
        let exported = interface
            .values
            .iter()
            .map(|value| js_name(&value.name))
            .collect();
        let mut constructor_counts = HashMap::new();
        for constructor in interface.constructors.iter().chain(imported_constructors) {
            if let Some(owner) = constructor.owner.as_name() {
                *constructor_counts.entry(owner.to_string()).or_insert(0) += 1;
            }
        }
        let variant_type_names = constructor_counts
            .into_iter()
            .filter_map(|(owner, count)| (count > 1).then_some(owner))
            .collect();
        Self {
            interface,
            exported,
            variant_type_names,
            match_counter: Cell::new(0),
        }
    }

    fn is_variant_record_constructor(&self, name: &Ident, ty: &Type) -> bool {
        let Some(owner) = crate::checker::builtin::type_name(ty) else {
            return false;
        };
        name.as_name() != Some(owner) || self.variant_type_names.contains(owner)
    }

    pub fn lower_module(&self, module: &lambda::Module) -> ir::Module {
        self.lower_module_with_imports(module, &[])
    }

    pub fn lower_module_with_imports(
        &self,
        module: &lambda::Module,
        imports: &[crate::parser::ast::UseDecl],
    ) -> ir::Module {
        self.match_counter.set(0);
        let mut items = lower_imports(imports);

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
                    let ir::Expr::Function {
                        params: outer_params,
                        body: outer_body,
                    } = self.lower_curried_function(params, &param_names, body, &HashMap::new())
                    else {
                        unreachable!("a function value lowers to a JavaScript function")
                    };
                    let result_type = if params.len() == 1 {
                        &body.ty
                    } else {
                        match &value.body.ty {
                            Type::Arrow { result, .. } => result.as_ref(),
                            _ => &body.ty,
                        }
                    };
                    let outer_param = params
                        .first()
                        .expect("function declarations have at least one parameter");
                    items.push(ir::Item::Function {
                        name,
                        params: outer_params
                            .into_iter()
                            .map(|name| ir::Param {
                                name,
                                ty: Some(self.lower_type(&outer_param.ty, false)),
                            })
                            .collect(),
                        return_type: Some(self.lower_type(result_type, true)),
                        body: outer_body,
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
        items.push(ir::Item::IntegerRuntime);
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
            lambda::ExprKind::Construct { name, args, .. } => {
                self.lower_construct(name, args, &expr.ty, env)
            }
            lambda::ExprKind::Function { params, body } => {
                let param_names = params
                    .iter()
                    .enumerate()
                    .map(pattern_param_name)
                    .collect::<Vec<_>>();
                self.lower_curried_function(params, &param_names, body, env)
            }
            // A standalone let expression carries its value in TIR; block
            // lowering handles the binding and extends the environment.
            lambda::ExprKind::Let { body, .. } => self.lower_expr_with_env(body, env),
            lambda::ExprKind::Apply { callee, args } => {
                args.iter()
                    .fold(self.lower_expr_with_env(callee, env), |callee, arg| {
                        ir::Expr::Call {
                            callee: Box::new(callee),
                            args: vec![self.lower_expr_with_env(arg, env)],
                        }
                    })
            }
            lambda::ExprKind::Prim { op, args } => self.lower_primitive(op, args, env),
            lambda::ExprKind::Tuple(items) => ir::Expr::Array(
                items
                    .iter()
                    .map(|item| self.lower_expr_with_env(item, env))
                    .collect(),
            ),
            lambda::ExprKind::Record { name, fields } => {
                let mut properties = Vec::with_capacity(fields.len() + 1);
                if self.is_variant_record_constructor(name, &expr.ty) {
                    properties.push(ir::Property {
                        name: "type".into(),
                        value: ir::Expr::String(name_string(name)),
                    });
                }
                properties.extend(fields.iter().map(|(name, value)| ir::Property {
                    name: js_property_name(name),
                    value: self.lower_expr_with_env(value, env),
                }));
                ir::Expr::Object(properties)
            }
            lambda::ExprKind::Field { base, field, .. } => ir::Expr::Member {
                object: Box::new(self.lower_expr_with_env(base, env)),
                property: js_property_name(field),
            },
            lambda::ExprKind::Block(exprs) => self.lower_block_expr_with_env(exprs, env),
            lambda::ExprKind::Match { scrutinee, arms } => {
                self.lower_match_expr(scrutinee, arms, env)
            }
        }
    }

    fn lower_curried_function(
        &self,
        params: &[lambda::Pattern],
        param_names: &[String],
        body: &lambda::Expr,
        outer_env: &HashMap<String, ir::Expr>,
    ) -> ir::Expr {
        let (Some(pattern), Some(name)) = (params.first(), param_names.first()) else {
            return self.lower_expr_with_env(body, outer_env);
        };
        let mut env = outer_env.clone();
        self.bind_pattern_env(&mut env, pattern, ir::Expr::Var(name.clone()));
        let function_body = if params.len() == 1 {
            self.lower_function_body_with_env(body, &env)
        } else {
            vec![ir::Stmt::Return(self.lower_curried_function(
                &params[1..],
                &param_names[1..],
                body,
                &env,
            ))]
        };
        ir::Expr::Function {
            params: vec![name.clone()],
            body: function_body,
        }
    }

    fn lower_function_body_with_env(
        &self,
        body: &lambda::Expr,
        env: &HashMap<String, ir::Expr>,
    ) -> Vec<ir::Stmt> {
        match &body.kind {
            lambda::ExprKind::Block(exprs) => self.lower_block_returning_with_env(exprs, env),
            _ => vec![ir::Stmt::Return(self.lower_expr_with_env(body, env))],
        }
    }

    fn lower_block_returning_with_env(
        &self,
        exprs: &[lambda::Expr],
        env: &HashMap<String, ir::Expr>,
    ) -> Vec<ir::Stmt> {
        let mut stmts = Vec::new();
        let mut env = env.clone();
        for (index, expr) in exprs.iter().enumerate() {
            match &expr.kind {
                lambda::ExprKind::Let { bind, body, .. } => {
                    let value = self.lower_expr_with_env(body, &env);
                    let name = pattern_param_name((index, bind));
                    stmts.push(ir::Stmt::Const {
                        name: name.clone(),
                        value: value.clone(),
                    });
                    self.bind_pattern_env(&mut env, bind, ir::Expr::Var(name));
                }
                _ if index + 1 == exprs.len() => {
                    stmts.push(ir::Stmt::Return(self.lower_expr_with_env(expr, &env)));
                }
                _ => {
                    stmts.push(ir::Stmt::Expr(self.lower_expr_with_env(expr, &env)));
                }
            }
        }
        if stmts.is_empty() {
            stmts.push(ir::Stmt::Return(ir::Expr::Undefined));
        }
        stmts
    }

    fn lower_block_expr_with_env(
        &self,
        exprs: &[lambda::Expr],
        env: &HashMap<String, ir::Expr>,
    ) -> ir::Expr {
        ir::Expr::Call {
            callee: Box::new(ir::Expr::Function {
                params: Vec::new(),
                body: self.lower_block_returning_with_env(exprs, env),
            }),
            args: Vec::new(),
        }
    }

    fn lower_construct(
        &self,
        name: &Ident,
        args: &[lambda::Expr],
        ty: &Type,
        env: &HashMap<String, ir::Expr>,
    ) -> ir::Expr {
        let tag = name_string(name);
        if is_std_option_type(ty) {
            return match (tag.as_str(), args) {
                ("None", []) => ir::Expr::Null,
                ("Some", [value]) => self.lower_expr_with_env(value, env),
                _ => ir::Expr::Null,
            };
        }
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
                value: self.lower_expr_with_env(&args[0], env),
            });
        } else {
            properties.push(ir::Property {
                name: "value".into(),
                value: ir::Expr::Array(
                    args.iter()
                        .map(|arg| self.lower_expr_with_env(arg, env))
                        .collect(),
                ),
            });
        }
        ir::Expr::Object(properties)
    }

    fn lower_primitive(
        &self,
        op: &lambda::Primitive,
        args: &[lambda::Expr],
        env: &HashMap<String, ir::Expr>,
    ) -> ir::Expr {
        let operand_type = match op {
            lambda::Primitive::Add { operand }
            | lambda::Primitive::Sub { operand }
            | lambda::Primitive::Mul { operand }
            | lambda::Primitive::Div { operand }
            | lambda::Primitive::Rem { operand }
            | lambda::Primitive::Eq { operand }
            | lambda::Primitive::Ne { operand }
            | lambda::Primitive::Lt { operand }
            | lambda::Primitive::Lte { operand }
            | lambda::Primitive::Gt { operand }
            | lambda::Primitive::Gte { operand } => Some(operand),
            lambda::Primitive::Named(_) => None,
        };
        let [left, right] = args else {
            return ir::Expr::Call {
                callee: Box::new(ir::Expr::Var("__tinyml_primitive".into())),
                args: args
                    .iter()
                    .map(|arg| self.lower_expr_with_env(arg, env))
                    .collect(),
            };
        };
        let left = Box::new(self.lower_expr_with_env(left, env));
        let right = Box::new(self.lower_expr_with_env(right, env));

        if let Some(operand_type) = operand_type
            && let Some((bits, signed)) = integer_width(operand_type)
            && let Some(op) = match op {
                lambda::Primitive::Add { .. } => Some(ir::IntegerOp::Add),
                lambda::Primitive::Sub { .. } => Some(ir::IntegerOp::Sub),
                lambda::Primitive::Mul { .. } => Some(ir::IntegerOp::Mul),
                lambda::Primitive::Div { .. } => Some(ir::IntegerOp::Div),
                lambda::Primitive::Rem { .. } => Some(ir::IntegerOp::Rem),
                _ => None,
            }
        {
            return ir::Expr::IntegerOp {
                op,
                bits,
                signed,
                left,
                right,
            };
        }

        let binary = match op {
            lambda::Primitive::Add { operand }
                if crate::checker::builtin::type_name(operand) == Some("string") =>
            {
                Some(ir::BinaryOp::Add)
            }
            lambda::Primitive::Eq { .. } => Some(ir::BinaryOp::Eq),
            lambda::Primitive::Ne { .. } => Some(ir::BinaryOp::Ne),
            lambda::Primitive::Lt { .. } => Some(ir::BinaryOp::Lt),
            lambda::Primitive::Lte { .. } => Some(ir::BinaryOp::Lte),
            lambda::Primitive::Gt { .. } => Some(ir::BinaryOp::Gt),
            lambda::Primitive::Gte { .. } => Some(ir::BinaryOp::Gte),
            _ => None,
        };
        if let Some(op) = binary {
            ir::Expr::Binary { op, left, right }
        } else {
            ir::Expr::Call {
                callee: Box::new(ir::Expr::Var(match op {
                    lambda::Primitive::Named(name) => js_name(name),
                    _ => "__tinyml_primitive".into(),
                })),
                args: vec![*left, *right],
            }
        }
    }

    fn lower_match_expr(
        &self,
        scrutinee: &lambda::Expr,
        arms: &[lambda::MatchArm],
        env: &HashMap<String, ir::Expr>,
    ) -> ir::Expr {
        let match_id = self.match_counter.get();
        self.match_counter.set(match_id + 1);
        let value_name = if match_id == 0 {
            "$tinyml_match".to_string()
        } else {
            format!("$tinyml_match_{match_id}")
        };
        ir::Expr::Call {
            callee: Box::new(ir::Expr::Function {
                params: vec![value_name.clone()],
                body: vec![self.lower_match_stmt(
                    ir::Expr::Var(value_name),
                    &scrutinee.ty,
                    arms,
                    env,
                )],
            }),
            args: vec![self.lower_expr_with_env(scrutinee, env)],
        }
    }

    fn lower_match_stmt(
        &self,
        scrutinee: ir::Expr,
        scrutinee_type: &Type,
        arms: &[lambda::MatchArm],
        env: &HashMap<String, ir::Expr>,
    ) -> ir::Stmt {
        if let Some(switch) = self.lower_switch_match(&scrutinee, scrutinee_type, arms, env) {
            return switch;
        }

        let scrutinee_expr = scrutinee;
        let mut current_else = Vec::new();
        for arm in arms.iter().rev() {
            let branch = self.lower_match_branch(&arm.pattern, &arm.body, &scrutinee_expr, env);
            if matches!(arm.pattern.kind, lambda::PatternKind::Wildcard) {
                current_else = branch;
            } else {
                current_else = vec![ir::Stmt::If {
                    condition: pattern_test_expr(self, &arm.pattern, scrutinee_expr.clone()),
                    then_branch: branch,
                    else_branch: current_else,
                }];
            }
        }
        ir::Stmt::Block(current_else)
    }

    fn lower_switch_match(
        &self,
        scrutinee: &ir::Expr,
        scrutinee_type: &Type,
        arms: &[lambda::MatchArm],
        env: &HashMap<String, ir::Expr>,
    ) -> Option<ir::Stmt> {
        if is_std_option_type(scrutinee_type) {
            return None;
        }
        let mut constructor_match = false;
        let mut literal_match = false;
        let mut cases = Vec::new();
        let mut default = Vec::new();
        let scrutinee_expr = scrutinee.clone();

        for arm in arms {
            match &arm.pattern.kind {
                lambda::PatternKind::Constant(constant) => {
                    literal_match = true;
                    cases.push(ir::SwitchCase {
                        test: lower_constant(constant),
                        body: self.lower_match_branch(
                            &arm.pattern,
                            &arm.body,
                            &scrutinee_expr,
                            env,
                        ),
                    });
                }
                lambda::PatternKind::Construct { name, args, .. }
                    if args.iter().all(is_irrefutable) =>
                {
                    constructor_match = true;
                    cases.push(ir::SwitchCase {
                        test: ir::Expr::String(name_string(name)),
                        body: self.lower_match_branch(
                            &arm.pattern,
                            &arm.body,
                            &scrutinee_expr,
                            env,
                        ),
                    });
                }
                lambda::PatternKind::Construct { .. } => return None,
                lambda::PatternKind::Wildcard | lambda::PatternKind::Var { .. } => {
                    default =
                        self.lower_match_branch(&arm.pattern, &arm.body, &scrutinee_expr, env);
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
        outer_env: &HashMap<String, ir::Expr>,
    ) -> Vec<ir::Stmt> {
        let mut env = outer_env.clone();
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
                            property: js_property_name(field),
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
            lambda::PatternKind::Construct { name, args, .. }
                if is_std_option_type(&pattern.ty) =>
            {
                if name.as_name() == Some("Some")
                    && let [arg] = args.as_slice()
                {
                    self.bind_pattern_env(env, arg, value);
                }
            }
            lambda::PatternKind::Construct { args, .. } => {
                if let [arg] = args.as_slice() {
                    self.bind_pattern_env(
                        env,
                        arg,
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
            Type::Apply { ident, arguments } => match crate::checker::builtin::type_name(ty) {
                Some("u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64") => {
                    ir::TsType::BigInt
                }
                Some("f8" | "f16" | "f32" | "f64") => ir::TsType::Number,
                Some("bool") => ir::TsType::Boolean,
                Some("string") => ir::TsType::String,
                Some("unit") => ir::TsType::Undefined,
                Some("Option")
                    if arguments.len() == 1 && ident.is_qualified_as("Std", "Option") =>
                {
                    ir::TsType::Union(vec![
                        self.lower_type(&arguments[0], false),
                        ir::TsType::Null,
                    ])
                }
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

fn is_std_option_type(ty: &Type) -> bool {
    crate::checker::builtin::is_std_type(ty, "Option")
}

fn lower_constant(constant: &lambda::Constant) -> ir::Expr {
    match constant {
        lambda::Constant::Unit => ir::Expr::Undefined,
        lambda::Constant::Literal(Literal::Int(value)) => {
            ir::Expr::BigInt(integer_literal_as_decimal(value))
        }
        lambda::Constant::Literal(Literal::Float(value)) => ir::Expr::Number(value.clone()),
        lambda::Constant::Literal(Literal::String(value)) => {
            ir::Expr::String(decode_string_literal(value))
        }
        lambda::Constant::Literal(Literal::Bool(value)) => ir::Expr::Bool(*value),
    }
}

fn integer_literal_as_decimal(value: &str) -> String {
    let (negative, magnitude) = value
        .strip_prefix('-')
        .map_or((false, value), |magnitude| (true, magnitude));
    let (radix, digits) = if let Some(digits) = magnitude.strip_prefix("0x") {
        (16, digits)
    } else if let Some(digits) = magnitude.strip_prefix("0o") {
        (8, digits)
    } else if let Some(digits) = magnitude.strip_prefix("0b") {
        (2, digits)
    } else {
        (10, magnitude)
    };
    let magnitude = u128::from_str_radix(digits, radix)
        .expect("the checker validates integer literal syntax and range before lowering");
    if negative {
        format!("-{magnitude}")
    } else {
        magnitude.to_string()
    }
}

fn decode_string_literal(value: &str) -> String {
    let body = value
        .strip_prefix('"')
        .and_then(|value| value.strip_suffix('"'))
        .unwrap_or(value);
    let mut decoded = String::with_capacity(body.len());
    let mut chars = body.chars();
    while let Some(character) = chars.next() {
        if character != '\\' {
            decoded.push(character);
            continue;
        }
        match chars.next() {
            Some('n') => decoded.push('\n'),
            Some('r') => decoded.push('\r'),
            Some('t') => decoded.push('\t'),
            Some('0') => decoded.push('\0'),
            Some('\\') => decoded.push('\\'),
            Some('"') => decoded.push('"'),
            Some('b') => decoded.push('\u{0008}'),
            Some('f') => decoded.push('\u{000c}'),
            Some('v') => decoded.push('\u{000b}'),
            Some(other) => {
                decoded.push('\\');
                decoded.push(other);
            }
            None => decoded.push('\\'),
        }
    }
    decoded
}

fn integer_width(ty: &Type) -> Option<(u8, bool)> {
    match crate::checker::builtin::type_name(ty)? {
        "i8" => Some((8, true)),
        "i16" => Some((16, true)),
        "i32" => Some((32, true)),
        "i64" => Some((64, true)),
        "u8" => Some((8, false)),
        "u16" => Some((16, false)),
        "u32" => Some((32, false)),
        "u64" => Some((64, false)),
        _ => None,
    }
}

fn pattern_test_expr(
    lowerer: &Lowerer<'_>,
    pattern: &lambda::Pattern,
    scrutinee: ir::Expr,
) -> ir::Expr {
    match &pattern.kind {
        lambda::PatternKind::Constant(constant) => ir::Expr::Binary {
            op: ir::BinaryOp::Eq,
            left: Box::new(scrutinee),
            right: Box::new(lower_constant(constant)),
        },
        lambda::PatternKind::Construct { name, args, .. } if is_std_option_type(&pattern.ty) => {
            match (name.as_name(), args.as_slice()) {
                (Some("None"), []) => ir::Expr::Binary {
                    op: ir::BinaryOp::Eq,
                    left: Box::new(scrutinee),
                    right: Box::new(ir::Expr::Null),
                },
                (Some("Some"), [arg]) => and_all([
                    ir::Expr::Binary {
                        op: ir::BinaryOp::Ne,
                        left: Box::new(scrutinee.clone()),
                        right: Box::new(ir::Expr::Null),
                    },
                    pattern_test_expr(lowerer, arg, scrutinee),
                ]),
                _ => ir::Expr::Bool(false),
            }
        }
        lambda::PatternKind::Construct { name, args, .. } => {
            let tag_test = ir::Expr::Binary {
                op: ir::BinaryOp::Eq,
                left: Box::new(ir::Expr::Member {
                    object: Box::new(scrutinee.clone()),
                    property: "type".into(),
                }),
                right: Box::new(ir::Expr::String(name_string(name))),
            };
            let non_null = and_all([
                ir::Expr::Binary {
                    op: ir::BinaryOp::Ne,
                    left: Box::new(scrutinee.clone()),
                    right: Box::new(ir::Expr::Null),
                },
                ir::Expr::Binary {
                    op: ir::BinaryOp::Ne,
                    left: Box::new(scrutinee.clone()),
                    right: Box::new(ir::Expr::Undefined),
                },
            ]);
            let payload_tests = args.iter().enumerate().map(|(index, arg)| {
                let payload = if args.len() == 1 {
                    ir::Expr::Member {
                        object: Box::new(scrutinee.clone()),
                        property: "value".into(),
                    }
                } else {
                    ir::Expr::Member {
                        object: Box::new(ir::Expr::Member {
                            object: Box::new(scrutinee.clone()),
                            property: "value".into(),
                        }),
                        property: index.to_string(),
                    }
                };
                pattern_test_expr(lowerer, arg, payload)
            });
            and_all(
                std::iter::once(non_null)
                    .chain(std::iter::once(tag_test))
                    .chain(payload_tests),
            )
        }
        lambda::PatternKind::Tuple(items) => {
            let is_array = ir::Expr::Call {
                callee: Box::new(ir::Expr::Member {
                    object: Box::new(ir::Expr::Var("Array".into())),
                    property: "isArray".into(),
                }),
                args: vec![scrutinee.clone()],
            };
            let has_expected_length = ir::Expr::Binary {
                op: ir::BinaryOp::Eq,
                left: Box::new(ir::Expr::Member {
                    object: Box::new(scrutinee.clone()),
                    property: "length".into(),
                }),
                right: Box::new(ir::Expr::Number(items.len().to_string())),
            };
            and_all([is_array, has_expected_length].into_iter().chain(
                items.iter().enumerate().map(|(index, item)| {
                    pattern_test_expr(
                        lowerer,
                        item,
                        ir::Expr::Member {
                            object: Box::new(scrutinee.clone()),
                            property: index.to_string(),
                        },
                    )
                }),
            ))
        }
        lambda::PatternKind::Record { name, fields } => {
            let non_null = and_all([
                ir::Expr::Binary {
                    op: ir::BinaryOp::Ne,
                    left: Box::new(scrutinee.clone()),
                    right: Box::new(ir::Expr::Null),
                },
                ir::Expr::Binary {
                    op: ir::BinaryOp::Ne,
                    left: Box::new(scrutinee.clone()),
                    right: Box::new(ir::Expr::Undefined),
                },
            ]);
            let variant_tag = lowerer
                .is_variant_record_constructor(name, &pattern.ty)
                .then(|| ir::Expr::Binary {
                    op: ir::BinaryOp::Eq,
                    left: Box::new(ir::Expr::Member {
                        object: Box::new(scrutinee.clone()),
                        property: "type".into(),
                    }),
                    right: Box::new(ir::Expr::String(name_string(name))),
                });
            and_all(
                std::iter::once(non_null)
                    .chain(variant_tag)
                    .chain(fields.iter().map(|(field, pattern)| {
                        pattern_test_expr(
                            lowerer,
                            pattern,
                            ir::Expr::Member {
                                object: Box::new(scrutinee.clone()),
                                property: js_property_name(field),
                            },
                        )
                    })),
            )
        }
        lambda::PatternKind::Wildcard | lambda::PatternKind::Var { .. } => ir::Expr::Bool(true),
    }
}

fn and_all(expressions: impl IntoIterator<Item = ir::Expr>) -> ir::Expr {
    expressions
        .into_iter()
        .reduce(|left, right| ir::Expr::Binary {
            op: ir::BinaryOp::And,
            left: Box::new(left),
            right: Box::new(right),
        })
        .unwrap_or(ir::Expr::Bool(true))
}

fn is_irrefutable(pattern: &lambda::Pattern) -> bool {
    match &pattern.kind {
        lambda::PatternKind::Wildcard | lambda::PatternKind::Var { .. } => true,
        lambda::PatternKind::Tuple(items) => items.iter().all(is_irrefutable),
        lambda::PatternKind::Record { fields, .. } => {
            fields.iter().all(|(_, pattern)| is_irrefutable(pattern))
        }
        lambda::PatternKind::Constant(_) | lambda::PatternKind::Construct { .. } => false,
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
        return name;
    }

    let mut encoded = String::from("$tinyml$");
    for character in name.chars() {
        encoded.push_str(&format!("{:x}$", character as u32));
    }
    encoded
}

fn js_property_name(ident: &Ident) -> String {
    name_string(ident)
}

fn lower_imports(imports: &[crate::parser::ast::UseDecl]) -> Vec<ir::Item> {
    imports
        .iter()
        .map(|import| match import {
            crate::parser::ast::UseDecl::Concrete {
                module, members, ..
            } => ir::Item::Import {
                names: members
                    .iter()
                    .map(|member| ir::ImportName {
                        imported: js_name(member),
                        local: js_name(member),
                    })
                    .collect(),
                from: format!("./{}.mjs", name_string(module)),
            },
            crate::parser::ast::UseDecl::Wildcard { module, .. } => ir::Item::ImportAll {
                local: js_name(module),
                from: format!("./{}.mjs", name_string(module)),
            },
        })
        .collect()
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
        "arguments"
            | "await"
            | "break"
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
            | "enum"
            | "eval"
            | "export"
            | "extends"
            | "finally"
            | "for"
            | "function"
            | "if"
            | "import"
            | "implements"
            | "interface"
            | "let"
            | "null"
            | "package"
            | "private"
            | "protected"
            | "public"
            | "static"
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
            | "true"
            | "false"
            | "of"
            | "async"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        checker::{
            interface::ValueDescription,
            scheme::Scheme,
            tir::{BindingId, EntityId},
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
                binding: BindingId::synthetic(0),
                entity: EntityId::synthetic_value(0),
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
        assert_eq!(
            js_name(&ident("with-hyphen")),
            "$tinyml$77$69$74$68$2d$68$79$70$68$65$6e$"
        );
        assert_eq!(
            js_name(&ident("arguments")),
            "$tinyml$61$72$67$75$6d$65$6e$74$73$"
        );
        assert_ne!(js_name(&ident("default")), js_name(&ident("_default")));
        assert_ne!(js_name(&ident("foo'")), js_name(&ident("foo_")));
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
                ty: Type::Apply {
                    ident: ident("Point"),
                    arguments: Vec::new(),
                },
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
                    entity: EntityId::synthetic_value(0),
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
    fn lowers_functions_as_curried_unary_calls_and_exports() {
        let body = lambda::Expr {
            ty: unit_type(),
            kind: lambda::ExprKind::Function {
                params: vec![
                    lambda::Pattern {
                        ty: unit_type(),
                        kind: lambda::PatternKind::Var {
                            name: ident("left"),
                            binding: BindingId::synthetic(1),
                        },
                        span: span(),
                    },
                    lambda::Pattern {
                        ty: unit_type(),
                        kind: lambda::PatternKind::Var {
                            name: ident("right"),
                            binding: BindingId::synthetic(2),
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
            ir::Item::Function { params, body, .. }
                if params.iter().map(|param| param.name.as_str()).collect::<Vec<_>>() == ["left"]
                    && matches!(body.as_slice(), [ir::Stmt::Return(ir::Expr::Function { params, .. })]
                        if params == &["right"])
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
                entity: EntityId::synthetic_value(0),
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
                            entity: EntityId::synthetic_value(1),
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
    fn std_option_lowers_to_null_union_but_user_option_does_not() {
        let interface = ModuleInterface::empty();
        let lowerer = Lowerer::new(&interface);
        let number = Type::Apply {
            ident: crate::checker::builtin::canonical_type_ident("i32"),
            arguments: Vec::new(),
        };
        let std_option = Type::Apply {
            ident: crate::parser::ident::Ident::qualified("Std", "Option"),
            arguments: vec![number.clone()],
        };
        let user_option = Type::Apply {
            ident: ident("Option"),
            arguments: vec![number],
        };

        assert!(matches!(
            lowerer.lower_type(&std_option, false),
            ir::TsType::Union(items)
                if matches!(items.as_slice(), [ir::TsType::BigInt, ir::TsType::Null])
        ));
        assert!(matches!(
            lowerer.lower_type(&user_option, false),
            ir::TsType::Named { name, .. } if name == "Option"
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
