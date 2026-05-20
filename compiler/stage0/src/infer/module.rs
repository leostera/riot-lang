use std::collections::BTreeMap;

use thiserror::Error;

use super::env::Env;
use super::scheme::TypeScheme;
use super::state::State;
use super::types::Type;
use super::unifier::UnifyError;
use crate::ast::{AstBlock, AstDecl, AstExpr, AstFnDecl, AstProgram, AstStmt, TextSpan};
use crate::signature::{
    ImportedSignatures, Rsig, RsigExport, RsigType, RsigTypeScheme, parse_type,
    parse_type_signature,
};

#[derive(Debug, Error, PartialEq, Eq)]
pub(crate) enum InferError {
    #[error("cannot infer expression type: {0}")]
    Unsupported(&'static str),
    #[error("unknown value `{0}`")]
    UnknownValue(String),
    #[error("{0}")]
    Unify(#[from] UnifyError),
    #[error("{error}")]
    At {
        span: TextSpan,
        error: Box<InferError>,
    },
}

impl InferError {
    pub(crate) fn span(&self) -> Option<TextSpan> {
        match self {
            InferError::At { span, .. } => Some(*span),
            InferError::Unsupported(_) | InferError::UnknownValue(_) | InferError::Unify(_) => None,
        }
    }

    fn at(self, span: TextSpan) -> Self {
        match self {
            InferError::At { .. } => self,
            error => InferError::At {
                span,
                error: Box::new(error),
            },
        }
    }

    fn root(&self) -> &InferError {
        match self {
            InferError::At { error, .. } => error.root(),
            error => error,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct InferredModule {
    pub(crate) env: Env,
    pub(crate) expression_types: BTreeMap<TextSpan, Type>,
    pub(crate) binding_schemes: BTreeMap<TextSpan, TypeScheme>,
}

pub(crate) fn infer_program(
    program: &AstProgram,
    imports: &ImportedSignatures,
) -> Result<InferredModule, InferError> {
    let mut state = State::default();
    let mut expression_types = BTreeMap::new();
    let mut binding_schemes = BTreeMap::new();
    install_prelude(&mut state);
    install_imports(&mut state, program, imports);
    for decl in &program.decls {
        infer_decl(
            &mut state,
            decl,
            &mut expression_types,
            &mut binding_schemes,
        )?;
    }
    Ok(InferredModule {
        env: state.into_env(),
        expression_types,
        binding_schemes,
    })
}

impl InferredModule {
    pub(crate) fn function_signatures(
        &self,
        program: &AstProgram,
    ) -> BTreeMap<String, (Vec<RsigType>, RsigType)> {
        let function_arities = program
            .decls
            .iter()
            .filter_map(|decl| match decl {
                AstDecl::Function(function) => Some((function.name.clone(), function.params.len())),
                _ => None,
            })
            .collect::<BTreeMap<_, _>>();
        let mut signatures = BTreeMap::new();
        for (name, arity) in function_arities {
            let Some(scheme) = self.env.get_value(&name) else {
                continue;
            };
            if let Some((params, result)) = peel_source_function_type(&scheme.body, arity) {
                signatures.insert(
                    name,
                    (
                        params.iter().map(infer_type_to_rsig_type).collect(),
                        infer_type_to_rsig_type(&result),
                    ),
                );
            }
        }
        signatures
    }

    pub(crate) fn expression_rsig_types(&self) -> BTreeMap<TextSpan, RsigType> {
        self.expression_types
            .iter()
            .map(|(span, type_)| (*span, infer_type_to_rsig_type(type_)))
            .collect()
    }

    pub(crate) fn binding_rsig_schemes(&self) -> BTreeMap<TextSpan, RsigTypeScheme> {
        self.binding_schemes
            .iter()
            .map(|(span, scheme)| (*span, infer_scheme_to_rsig_scheme(scheme)))
            .collect()
    }
}

pub(crate) fn infer_function_signatures(
    program: &AstProgram,
    imports: &ImportedSignatures,
) -> Result<BTreeMap<String, (Vec<RsigType>, RsigType)>, InferError> {
    let inferred = infer_program(program, imports)?;
    Ok(inferred.function_signatures(program))
}

fn install_prelude(state: &mut State) {
    let dbg_message = state.fresh_var();
    state.add_prelude_value(
        "dbg",
        state.generalize(Type::arrow(dbg_message, Type::Unit)),
    );
    state.add_prelude_value(
        "println",
        TypeScheme::monomorphic(Type::arrow(Type::String, Type::Unit)),
    );

    let send_message = state.fresh_var();
    state.add_prelude_value(
        "send",
        state.generalize(Type::arrows(
            vec![Type::ActorId(Box::new(send_message.clone())), send_message],
            Type::Unit,
        )),
    );

    let actor_message = state.fresh_var();
    let actor_action = state.generalize(Type::arrow(
        Type::ActorId(Box::new(actor_message)),
        Type::Unit,
    ));
    state.add_prelude_value("monitor", actor_action.clone());
    state.add_prelude_value("link", actor_action);

    let list_item = state.fresh_var();
    state.add_prelude_value(
        "list_len",
        state.generalize(Type::arrow(Type::List(Box::new(list_item)), Type::I64)),
    );

    let list_get_item = state.fresh_var();
    state.add_prelude_value(
        "list_get",
        state.generalize(Type::arrows(
            vec![Type::List(Box::new(list_get_item.clone())), Type::I64],
            list_get_item,
        )),
    );

    state.add_prelude_value(
        "string_len",
        TypeScheme::monomorphic(Type::arrow(Type::String, Type::I64)),
    );
    state.add_prelude_value(
        "string_concat",
        TypeScheme::monomorphic(Type::arrows(vec![Type::String, Type::String], Type::String)),
    );
}

fn install_imports(state: &mut State, program: &AstProgram, imports: &ImportedSignatures) {
    for decl in &program.decls {
        let AstDecl::Use(use_) = decl else {
            continue;
        };
        let Some(rsig) = imports.get(&use_.name) else {
            continue;
        };
        install_import_signature(state, &use_.name, rsig);
    }
}

fn install_import_signature(state: &mut State, module_name: &str, rsig: &Rsig) {
    for export in &rsig.exports {
        let (name, scheme) = match export {
            RsigExport::Function(function) => (&function.name, &function.scheme),
            RsigExport::External(external) => (&external.name, &external.scheme),
        };
        let scheme = rsig_scheme_to_infer_scheme(scheme, state);
        state.add_prelude_value(format!("{module_name}.{name}"), scheme);
    }
}

fn source_function_type(params: Vec<Type>, result: Type) -> Type {
    if params.is_empty() {
        Type::arrow(Type::Unit, result)
    } else {
        Type::arrows(params, result)
    }
}

fn peel_source_function_type(type_: &Type, arity: usize) -> Option<(Vec<Type>, Type)> {
    let mut params = Vec::new();
    let mut current = type_;

    if arity == 0 {
        let Type::Arrow { parameter, result } = current else {
            return None;
        };
        if parameter.as_ref() != &Type::Unit {
            return None;
        }
        return Some((params, result.as_ref().clone()));
    }

    for _ in 0..arity {
        let Type::Arrow { parameter, result } = current else {
            return None;
        };
        params.push(parameter.as_ref().clone());
        current = result;
    }

    Some((params, current.clone()))
}

fn infer_decl(
    state: &mut State,
    decl: &AstDecl,
    expression_types: &mut BTreeMap<TextSpan, Type>,
    binding_schemes: &mut BTreeMap<TextSpan, TypeScheme>,
) -> Result<(), InferError> {
    match decl {
        AstDecl::Use(_) => Ok(()),
        AstDecl::External(external) => {
            let (params, result) = parse_type_signature(&external.type_text);
            let type_ = rsig_signature_to_infer_type(&params, &result, state);
            state.add_value(external.name.clone(), state.generalize(type_));
            Ok(())
        }
        AstDecl::Function(function) => {
            let type_ = infer_function(state, function, expression_types, binding_schemes)?;
            state.add_value(function.name.clone(), state.generalize(type_));
            Ok(())
        }
    }
}

fn infer_function(
    state: &mut State,
    function: &AstFnDecl,
    expression_types: &mut BTreeMap<TextSpan, Type>,
    binding_schemes: &mut BTreeMap<TextSpan, TypeScheme>,
) -> Result<Type, InferError> {
    let params = function
        .params
        .iter()
        .enumerate()
        .map(|(index, _param)| {
            let type_ = function
                .param_types
                .get(index)
                .and_then(|annotation| annotation.as_ref())
                .map(|annotation| rsig_type_to_infer_type(&parse_type(&annotation.text), state))
                .unwrap_or_else(|| state.fresh_var());
            type_
        })
        .collect::<Vec<_>>();
    let result_constraint = function
        .return_type
        .as_ref()
        .map(|annotation| rsig_type_to_infer_type(&parse_type(&annotation.text), state))
        .unwrap_or_else(|| state.fresh_var());
    let self_type = source_function_type(params.clone(), result_constraint.clone());

    state.push_scope();
    state.add_value(function.name.clone(), state.monomorphic(self_type));
    for (param, type_) in function.params.iter().zip(&params) {
        state.add_value(param.clone(), state.monomorphic(type_.clone()));
    }
    let result = infer_block(state, &function.body, expression_types, binding_schemes)?;
    state.unify(&result_constraint, &result)?;
    let result = state.resolve(&result_constraint);
    state.pop_scope();
    Ok(source_function_type(params, state.resolve(&result)))
}

fn infer_block(
    state: &mut State,
    block: &AstBlock,
    expression_types: &mut BTreeMap<TextSpan, Type>,
    binding_schemes: &mut BTreeMap<TextSpan, TypeScheme>,
) -> Result<Type, InferError> {
    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                name_span,
                type_annotation,
                value,
                ..
            } => {
                let inferred = infer_expr(state, value, expression_types, binding_schemes)?;
                let type_ = if let Some(annotation) = type_annotation {
                    let expected = rsig_type_to_infer_type(&parse_type(&annotation.text), state);
                    state.unify(&expected, &inferred)?;
                    expected
                } else {
                    inferred
                };
                let scheme = state.generalize(type_);
                binding_schemes.insert(*name_span, scheme.clone());
                state.add_value(name.clone(), scheme);
            }
            AstStmt::Expr(expr) => {
                infer_expr(state, expr, expression_types, binding_schemes)?;
            }
        }
    }

    if let Some(tail) = &block.tail {
        infer_expr(state, tail, expression_types, binding_schemes)
    } else {
        Ok(Type::Unit)
    }
}

fn infer_expr(
    state: &mut State,
    expr: &AstExpr,
    expression_types: &mut BTreeMap<TextSpan, Type>,
    binding_schemes: &mut BTreeMap<TextSpan, TypeScheme>,
) -> Result<Type, InferError> {
    let span = expr_span(expr);
    let inferred = infer_expr_kind(state, expr, expression_types, binding_schemes)
        .map_err(|error| error.at(span))?;
    let resolved = state.resolve(&inferred);
    expression_types.insert(span, resolved.clone());
    Ok(resolved)
}

fn infer_expr_kind(
    state: &mut State,
    expr: &AstExpr,
    expression_types: &mut BTreeMap<TextSpan, Type>,
    binding_schemes: &mut BTreeMap<TextSpan, TypeScheme>,
) -> Result<Type, InferError> {
    match expr {
        AstExpr::Bool { .. } => Ok(Type::Bool),
        AstExpr::Char { .. } => Ok(Type::Char),
        AstExpr::Float { .. } => Ok(Type::F64),
        AstExpr::Int { .. } => Ok(Type::I64),
        AstExpr::String { .. } => Ok(Type::String),
        AstExpr::Unit { .. } => Ok(Type::Unit),
        AstExpr::Lambda {
            params: names,
            param_types,
            body,
            ..
        } => {
            let parameter_types = names
                .iter()
                .enumerate()
                .map(|(index, _param)| {
                    param_types
                        .get(index)
                        .and_then(|annotation| annotation.as_ref())
                        .map(|annotation| {
                            rsig_type_to_infer_type(&parse_type(&annotation.text), state)
                        })
                        .unwrap_or_else(|| state.fresh_var())
                })
                .collect::<Vec<_>>();

            state.push_scope();
            for (name, type_) in names.iter().zip(&parameter_types) {
                state.add_value(name.clone(), state.monomorphic(type_.clone()));
            }
            let result = infer_block(state, body, expression_types, binding_schemes)?;
            state.pop_scope();
            Ok(source_function_type(
                parameter_types,
                state.resolve(&result),
            ))
        }
        AstExpr::Path { path, .. } if path.segments.len() == 1 => {
            let name = &path.segments[0];
            let scheme = state
                .get_value(name)
                .cloned()
                .ok_or_else(|| InferError::UnknownValue(name.clone()))?;
            Ok(state.instantiate(&scheme))
        }
        AstExpr::Add { lhs, rhs, .. }
        | AstExpr::Sub { lhs, rhs, .. }
        | AstExpr::Mul { lhs, rhs, .. }
        | AstExpr::Div { lhs, rhs, .. }
        | AstExpr::Mod { lhs, rhs, .. } => {
            let lhs = infer_expr(state, lhs, expression_types, binding_schemes)?;
            let rhs = infer_expr(state, rhs, expression_types, binding_schemes)?;
            state.unify(&Type::I64, &lhs)?;
            state.unify(&Type::I64, &rhs)?;
            Ok(Type::I64)
        }
        AstExpr::Neg { expr, .. } => {
            let actual = infer_expr(state, expr, expression_types, binding_schemes)?;
            match state.resolve(&actual) {
                Type::I64 => Ok(Type::I64),
                Type::F64 => Ok(Type::F64),
                Type::Var(_) => {
                    state.unify(&Type::I64, &actual)?;
                    Ok(Type::I64)
                }
                actual => Err(UnifyError::TypeMismatch {
                    lhs: Type::I64,
                    rhs: actual,
                }
                .into()),
            }
        }
        AstExpr::Eq { lhs, rhs, .. } => {
            let lhs = infer_expr(state, lhs, expression_types, binding_schemes)?;
            let rhs = infer_expr(state, rhs, expression_types, binding_schemes)?;
            state.unify(&lhs, &rhs)?;
            Ok(Type::Bool)
        }
        AstExpr::Lt { lhs, rhs, .. } => {
            let lhs = infer_expr(state, lhs, expression_types, binding_schemes)?;
            let rhs = infer_expr(state, rhs, expression_types, binding_schemes)?;
            state.unify(&lhs, &rhs)?;
            match state.resolve(&lhs) {
                Type::I64 | Type::String | Type::Var(_) => Ok(Type::Bool),
                actual => Err(UnifyError::TypeMismatch {
                    lhs: Type::I64,
                    rhs: actual,
                }
                .into()),
            }
        }
        AstExpr::And { lhs, rhs, .. } | AstExpr::Or { lhs, rhs, .. } => {
            let lhs = infer_expr(state, lhs, expression_types, binding_schemes)?;
            let rhs = infer_expr(state, rhs, expression_types, binding_schemes)?;
            state.unify(&Type::Bool, &lhs)?;
            state.unify(&Type::Bool, &rhs)?;
            Ok(Type::Bool)
        }
        AstExpr::Not { expr, .. } => {
            let actual = infer_expr(state, expr, expression_types, binding_schemes)?;
            state.unify(&Type::Bool, &actual)?;
            Ok(Type::Bool)
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            let condition = infer_expr(state, condition, expression_types, binding_schemes)?;
            state.unify(&Type::Bool, &condition)?;
            let then_type = infer_expr(state, then_branch, expression_types, binding_schemes)?;
            let else_type = infer_expr(state, else_branch, expression_types, binding_schemes)?;
            state.unify(&then_type, &else_type)?;
            Ok(state.resolve(&then_type))
        }
        AstExpr::Tuple { items, .. } => Ok(Type::Tuple(
            items
                .iter()
                .map(|item| infer_expr(state, item, expression_types, binding_schemes))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        AstExpr::List { items, .. } => {
            let element = state.fresh_var();
            for item in items {
                let actual = infer_expr(state, item, expression_types, binding_schemes)?;
                state.unify(&element, &actual)?;
            }
            Ok(Type::List(Box::new(state.resolve(&element))))
        }
        AstExpr::Call { callee, args, .. } if callee.segments.len() == 1 => {
            let name = &callee.segments[0];
            let scheme = state
                .get_value(name)
                .cloned()
                .ok_or_else(|| InferError::UnknownValue(name.clone()))?;
            let callee_type = state.instantiate(&scheme);
            let arg_types = args
                .iter()
                .map(|arg| infer_expr(state, arg, expression_types, binding_schemes))
                .collect::<Result<Vec<_>, _>>()?;
            apply_call(state, callee_type, arg_types)
        }
        AstExpr::Apply { callee, args, .. } => {
            let callee_type = infer_expr(state, callee, expression_types, binding_schemes)?;
            let arg_types = args
                .iter()
                .map(|arg| infer_expr(state, arg, expression_types, binding_schemes))
                .collect::<Result<Vec<_>, _>>()?;
            apply_call(state, callee_type, arg_types)
        }
        AstExpr::Spawn { body, .. } => {
            state.push_scope();
            infer_block(state, body, expression_types, binding_schemes)?;
            state.pop_scope();
            Ok(Type::ActorId(Box::new(state.fresh_var())))
        }
        AstExpr::Record { path, .. } => Ok(Type::Record(path.segments.join("."))),
        AstExpr::TupleIndex { base, index, .. } => {
            let inferred_base = infer_expr(state, base, expression_types, binding_schemes)?;
            let base_type = state.resolve(&inferred_base);
            match base_type {
                Type::Tuple(items) => items
                    .get(*index)
                    .cloned()
                    .ok_or(InferError::Unsupported("tuple projection index")),
                Type::Var(_) => Ok(state.fresh_var()),
                _ => Err(InferError::Unsupported("tuple projection")),
            }
        }
        AstExpr::Field { base, field, .. } => {
            if let AstExpr::Record { fields, .. } = base.as_ref() {
                fields
                    .iter()
                    .find_map(|(name, value)| {
                        (name == field)
                            .then(|| infer_expr(state, value, expression_types, binding_schemes))
                    })
                    .unwrap_or_else(|| Err(InferError::Unsupported("record field")))
            } else {
                infer_expr(state, base, expression_types, binding_schemes)?;
                Ok(state.fresh_var())
            }
        }
        AstExpr::Receive { binder, body, .. } => {
            let message = state.fresh_var();
            state.push_scope();
            state.add_value(binder.clone(), state.monomorphic(message));
            infer_expr(state, body, expression_types, binding_schemes)?;
            state.pop_scope();
            Ok(Type::Unit)
        }
        AstExpr::Call { callee, args, .. } if callee.segments.len() == 2 => {
            let name = callee.segments.join(".");
            let scheme = state
                .get_value(&name)
                .cloned()
                .ok_or_else(|| InferError::UnknownValue(name.clone()))?;
            let callee_type = state.instantiate(&scheme);
            let arg_types = args
                .iter()
                .map(|arg| infer_expr(state, arg, expression_types, binding_schemes))
                .collect::<Result<Vec<_>, _>>()?;
            apply_call(state, callee_type, arg_types)
        }
        AstExpr::Path { path, .. } if path.segments.len() == 2 => {
            let name = path.segments.join(".");
            let scheme = state
                .get_value(&name)
                .cloned()
                .ok_or_else(|| InferError::UnknownValue(name.clone()))?;
            Ok(state.instantiate(&scheme))
        }
        AstExpr::Call { .. } | AstExpr::Path { .. } => Err(InferError::Unsupported("path")),
    }
}

fn apply_call(
    state: &mut State,
    callee_type: Type,
    arg_types: Vec<Type>,
) -> Result<Type, InferError> {
    let mut callee_type = callee_type;
    if arg_types.is_empty() {
        let result = state.fresh_var();
        state.unify(&callee_type, &Type::arrow(Type::Unit, result.clone()))?;
        return Ok(state.resolve(&result));
    }

    for arg_type in arg_types {
        let result = state.fresh_var();
        state.unify(&callee_type, &Type::arrow(arg_type, result.clone()))?;
        callee_type = state.resolve(&result);
    }

    Ok(callee_type)
}

fn expr_span(expr: &AstExpr) -> TextSpan {
    match expr {
        AstExpr::Add { span, .. }
        | AstExpr::Sub { span, .. }
        | AstExpr::Mul { span, .. }
        | AstExpr::Div { span, .. }
        | AstExpr::Mod { span, .. }
        | AstExpr::Neg { span, .. }
        | AstExpr::Eq { span, .. }
        | AstExpr::Lt { span, .. }
        | AstExpr::And { span, .. }
        | AstExpr::Or { span, .. }
        | AstExpr::Not { span, .. }
        | AstExpr::If { span, .. }
        | AstExpr::Bool { span, .. }
        | AstExpr::Call { span, .. }
        | AstExpr::Apply { span, .. }
        | AstExpr::Lambda { span, .. }
        | AstExpr::Spawn { span, .. }
        | AstExpr::Receive { span, .. }
        | AstExpr::Unit { span, .. }
        | AstExpr::Tuple { span, .. }
        | AstExpr::List { span, .. }
        | AstExpr::Record { span, .. }
        | AstExpr::Field { span, .. }
        | AstExpr::TupleIndex { span, .. }
        | AstExpr::Char { span, .. }
        | AstExpr::Float { span, .. }
        | AstExpr::Int { span, .. }
        | AstExpr::Path { span, .. }
        | AstExpr::String { span, .. } => *span,
    }
}

fn rsig_signature_to_infer_type(params: &[RsigType], result: &RsigType, state: &mut State) -> Type {
    source_function_type(
        params
            .iter()
            .map(|param| rsig_type_to_infer_type(param, state))
            .collect(),
        rsig_type_to_infer_type(result, state),
    )
}

fn rsig_type_to_infer_type(type_: &RsigType, state: &mut State) -> Type {
    match type_ {
        RsigType::ActorId(message) => {
            Type::ActorId(Box::new(rsig_type_to_infer_type(message, state)))
        }
        RsigType::Arrow { parameter, result } => Type::arrow(
            rsig_type_to_infer_type(parameter, state),
            rsig_type_to_infer_type(result, state),
        ),
        RsigType::Bool => Type::Bool,
        RsigType::Char => Type::Char,
        RsigType::F64 => Type::F64,
        RsigType::I64 => Type::I64,
        RsigType::List(element) => Type::List(Box::new(rsig_type_to_infer_type(element, state))),
        RsigType::Record(name) => Type::Record(name.clone()),
        RsigType::String => Type::String,
        RsigType::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|item| rsig_type_to_infer_type(item, state))
                .collect(),
        ),
        RsigType::Unit => Type::Unit,
        RsigType::Unknown | RsigType::Var(_) => state.fresh_var(),
    }
}

fn rsig_scheme_to_infer_scheme(scheme: &RsigTypeScheme, state: &mut State) -> TypeScheme {
    let mut vars = BTreeMap::new();
    let mut quantifiers = Vec::new();
    for name in &scheme.quantifiers {
        let fresh = state.fresh_var();
        if let Type::Var(var) = fresh {
            quantifiers.push(var);
            vars.insert(name.clone(), Type::Var(var));
        }
    }
    TypeScheme {
        quantifiers,
        body: rsig_type_to_infer_type_with_vars(&scheme.body, state, &mut vars),
    }
}

fn rsig_type_to_infer_type_with_vars(
    type_: &RsigType,
    state: &mut State,
    vars: &mut BTreeMap<String, Type>,
) -> Type {
    match type_ {
        RsigType::ActorId(message) => Type::ActorId(Box::new(rsig_type_to_infer_type_with_vars(
            message, state, vars,
        ))),
        RsigType::Arrow { parameter, result } => Type::arrow(
            rsig_type_to_infer_type_with_vars(parameter, state, vars),
            rsig_type_to_infer_type_with_vars(result, state, vars),
        ),
        RsigType::Bool => Type::Bool,
        RsigType::Char => Type::Char,
        RsigType::F64 => Type::F64,
        RsigType::I64 => Type::I64,
        RsigType::List(element) => Type::List(Box::new(rsig_type_to_infer_type_with_vars(
            element, state, vars,
        ))),
        RsigType::Record(name) => Type::Record(name.clone()),
        RsigType::String => Type::String,
        RsigType::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|item| rsig_type_to_infer_type_with_vars(item, state, vars))
                .collect(),
        ),
        RsigType::Unit => Type::Unit,
        RsigType::Var(name) => vars
            .entry(name.clone())
            .or_insert_with(|| state.fresh_var())
            .clone(),
        RsigType::Unknown => state.fresh_var(),
    }
}

fn infer_type_to_rsig_type(type_: &Type) -> RsigType {
    match type_ {
        Type::ActorId(message) => RsigType::ActorId(Box::new(infer_type_to_rsig_type(message))),
        Type::Bool => RsigType::Bool,
        Type::Char => RsigType::Char,
        Type::F64 => RsigType::F64,
        Type::Arrow { parameter, result } => RsigType::Arrow {
            parameter: Box::new(infer_type_to_rsig_type(parameter)),
            result: Box::new(infer_type_to_rsig_type(result)),
        },
        Type::I64 => RsigType::I64,
        Type::List(element) => RsigType::List(Box::new(infer_type_to_rsig_type(element))),
        Type::Record(name) => RsigType::Record(name.clone()),
        Type::String => RsigType::String,
        Type::Tuple(items) => RsigType::Tuple(items.iter().map(infer_type_to_rsig_type).collect()),
        Type::Unit => RsigType::Unit,
        Type::Var(var) => RsigType::Var(format!("'t{}", var.index())),
    }
}

fn infer_scheme_to_rsig_scheme(scheme: &TypeScheme) -> RsigTypeScheme {
    RsigTypeScheme {
        quantifiers: scheme
            .quantifiers
            .iter()
            .map(|var| format!("'t{}", var.index()))
            .collect(),
        body: infer_type_to_rsig_type(&scheme.body),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        AstBlock, AstDecl, AstExpr, AstFnDecl, AstPath, AstProgram, AstStmt, TextSpan,
    };
    use crate::infer::module::{
        InferError, InferredModule, infer_function_signatures, infer_program,
    };
    use crate::infer::scheme::TypeScheme;
    use crate::infer::types::Type;
    use crate::signature::{ImportedSignatures, RsigType};

    fn span() -> TextSpan {
        TextSpan::new(0, 0)
    }

    fn int(value: i64) -> AstExpr {
        AstExpr::Int {
            value,
            span: span(),
        }
    }

    fn bool_(value: bool) -> AstExpr {
        AstExpr::Bool {
            value,
            span: span(),
        }
    }

    fn path(name: &str) -> AstExpr {
        AstExpr::Path {
            path: AstPath {
                segments: vec![name.to_owned()],
            },
            span: span(),
        }
    }

    fn add(lhs: AstExpr, rhs: AstExpr) -> AstExpr {
        AstExpr::Add {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: span(),
        }
    }

    fn call(name: &str, args: Vec<AstExpr>) -> AstExpr {
        AstExpr::Call {
            callee: AstPath {
                segments: vec![name.to_owned()],
            },
            args,
            span: span(),
        }
    }

    fn lambda(params: Vec<&str>, tail: AstExpr) -> AstExpr {
        AstExpr::Lambda {
            params: params.into_iter().map(str::to_owned).collect(),
            param_types: Vec::new(),
            body: Box::new(AstBlock {
                statements: Vec::new(),
                tail: Some(tail),
                span: span(),
            }),
            span: span(),
        }
    }

    fn apply(callee: AstExpr, args: Vec<AstExpr>) -> AstExpr {
        AstExpr::Apply {
            callee: Box::new(callee),
            args,
            span: span(),
        }
    }

    fn function(name: &str, params: Vec<&str>, statements: Vec<AstStmt>, tail: AstExpr) -> AstDecl {
        AstDecl::Function(AstFnDecl {
            name: name.to_owned(),
            name_span: span(),
            params: params.into_iter().map(str::to_owned).collect(),
            param_types: Vec::new(),
            return_type: None,
            body: AstBlock {
                statements,
                tail: Some(tail),
                span: span(),
            },
            span: span(),
        })
    }

    fn infer(program: &AstProgram) -> Result<InferredModule, InferError> {
        infer_program(program, &ImportedSignatures::new())
    }

    fn signatures(
        program: &AstProgram,
    ) -> Result<std::collections::BTreeMap<String, (Vec<RsigType>, RsigType)>, InferError> {
        infer_function_signatures(program, &ImportedSignatures::new())
    }

    #[test]
    fn scans_module_declarations_top_to_bottom() {
        let program = AstProgram {
            decls: vec![
                function("one", vec![], Vec::new(), int(1)),
                function(
                    "two",
                    vec![],
                    Vec::new(),
                    add(call("one", Vec::new()), int(1)),
                ),
            ],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(exports.len(), 2);
        assert_eq!(exports[0].0, "one");
        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::I64))
        );
        assert_eq!(exports[1].0, "two");
        assert_eq!(
            exports[1].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::I64))
        );
    }

    #[test]
    fn later_declarations_cannot_see_future_values() {
        let program = AstProgram {
            decls: vec![function("two", vec![], Vec::new(), call("one", Vec::new()))],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(
            &crate::infer::module::InferError::UnknownValue("one".to_owned()),
            err.root()
        );
    }

    #[test]
    fn lexical_let_rebinding_uses_the_latest_binding() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                vec![
                    AstStmt::Let {
                        name: "a".to_owned(),
                        name_span: span(),
                        type_annotation: None,
                        value: int(0),
                        span: span(),
                    },
                    AstStmt::Let {
                        name: "a".to_owned(),
                        name_span: span(),
                        type_annotation: None,
                        value: bool_(true),
                        span: span(),
                    },
                ],
                path("a"),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::Bool))
        );
    }

    #[test]
    fn function_parameters_are_generalized_at_the_top_level() {
        let program = AstProgram {
            decls: vec![function("id", vec!["x"], Vec::new(), path("x"))],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();
        let var = exports[0].1.quantifiers[0];

        assert_eq!(exports[0].0, "id");
        assert_eq!(exports[0].1.quantifiers.len(), 1);
        assert_eq!(
            exports[0].1,
            TypeScheme {
                quantifiers: vec![var],
                body: Type::arrow(Type::Var(var), Type::Var(var)),
            }
        );
    }

    #[test]
    fn lambda_expressions_infer_unary_arrow_chains() {
        let program = AstProgram {
            decls: vec![function(
                "make_adder",
                vec!["n"],
                Vec::new(),
                lambda(vec!["x"], add(path("x"), path("n"))),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::I64, Type::arrow(Type::I64, Type::I64)))
        );
    }

    #[test]
    fn apply_expressions_infer_from_the_callee_arrow() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                apply(lambda(vec!["x"], path("x")), vec![int(1)]),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::I64))
        );
    }

    #[test]
    fn let_bound_lambdas_can_be_called_by_name() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                vec![AstStmt::Let {
                    name: "id".to_owned(),
                    name_span: span(),
                    type_annotation: None,
                    value: lambda(vec!["x"], path("x")),
                    span: span(),
                }],
                call("id", vec![int(1)]),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::I64))
        );
    }

    #[test]
    fn let_bound_lambdas_are_instantiated_at_each_use() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                vec![
                    AstStmt::Let {
                        name: "id".to_owned(),
                        name_span: span(),
                        type_annotation: None,
                        value: lambda(vec!["x"], path("x")),
                        span: span(),
                    },
                    AstStmt::Expr(call("id", vec![int(1)])),
                ],
                call("id", vec![bool_(true)]),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::Bool))
        );
    }

    #[test]
    fn prelude_values_are_visible_but_not_exported() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                call("dbg", vec![int(1)]),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(exports.len(), 1);
        assert_eq!(exports[0].0, "main");
        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::Unit))
        );
    }

    #[test]
    fn function_signatures_project_concrete_parameter_types() {
        let program = AstProgram {
            decls: vec![function(
                "add",
                vec!["n", "x"],
                Vec::new(),
                add(path("x"), path("n")),
            )],
        };

        let signatures = signatures(&program).unwrap();

        assert_eq!(
            signatures.get("add"),
            Some(&(vec![RsigType::I64, RsigType::I64], RsigType::I64))
        );
    }

    #[test]
    fn function_signatures_project_arrow_parameter_types() {
        let program = AstProgram {
            decls: vec![function(
                "apply_i64",
                vec!["f", "x"],
                Vec::new(),
                add(call("f", vec![add(path("x"), int(0))]), int(1)),
            )],
        };

        let signatures = signatures(&program).unwrap();

        assert_eq!(
            signatures.get("apply_i64"),
            Some(&(
                vec![
                    RsigType::Arrow {
                        parameter: Box::new(RsigType::I64),
                        result: Box::new(RsigType::I64),
                    },
                    RsigType::I64,
                ],
                RsigType::I64,
            ))
        );
    }
}
