use thiserror::Error;

use super::env::Env;
use super::scheme::TypeScheme;
use super::state::State;
use super::types::Type;
use super::unifier::UnifyError;
use crate::ast::{AstBlock, AstDecl, AstExpr, AstFnDecl, AstProgram, AstStmt};
use crate::signature::{RsigType, parse_type, parse_type_signature};

#[derive(Debug, Error, PartialEq, Eq)]
pub(crate) enum InferError {
    #[error("cannot infer expression type: {0}")]
    Unsupported(&'static str),
    #[error("unknown value `{0}`")]
    UnknownValue(String),
    #[error("{0}")]
    Unify(#[from] UnifyError),
}

#[derive(Debug, Clone)]
pub(crate) struct InferredModule {
    pub(crate) env: Env,
}

pub(crate) fn infer_program(program: &AstProgram) -> Result<InferredModule, InferError> {
    let mut state = State::default();
    install_prelude(&mut state);
    for decl in &program.decls {
        infer_decl(&mut state, decl)?;
    }
    Ok(InferredModule {
        env: state.into_env(),
    })
}

fn install_prelude(state: &mut State) {
    let dbg_message = state.fresh_var();
    state.add_prelude_value(
        "dbg",
        state.generalize(Type::Function(vec![dbg_message], Box::new(Type::Unit))),
    );
    state.add_prelude_value(
        "println",
        TypeScheme::monomorphic(Type::Function(vec![Type::String], Box::new(Type::Unit))),
    );

    let send_message = state.fresh_var();
    state.add_prelude_value(
        "send",
        state.generalize(Type::Function(
            vec![Type::ActorId(Box::new(send_message.clone())), send_message],
            Box::new(Type::Unit),
        )),
    );

    let actor_message = state.fresh_var();
    let actor_action = state.generalize(Type::Function(
        vec![Type::ActorId(Box::new(actor_message))],
        Box::new(Type::Unit),
    ));
    state.add_prelude_value("monitor", actor_action.clone());
    state.add_prelude_value("link", actor_action);

    let list_item = state.fresh_var();
    state.add_prelude_value(
        "list_len",
        state.generalize(Type::Function(
            vec![Type::List(Box::new(list_item))],
            Box::new(Type::I64),
        )),
    );

    let list_get_item = state.fresh_var();
    state.add_prelude_value(
        "list_get",
        state.generalize(Type::Function(
            vec![Type::List(Box::new(list_get_item.clone())), Type::I64],
            Box::new(list_get_item),
        )),
    );

    state.add_prelude_value(
        "string_len",
        TypeScheme::monomorphic(Type::Function(vec![Type::String], Box::new(Type::I64))),
    );
    state.add_prelude_value(
        "string_concat",
        TypeScheme::monomorphic(Type::Function(
            vec![Type::String, Type::String],
            Box::new(Type::String),
        )),
    );
}

fn infer_decl(state: &mut State, decl: &AstDecl) -> Result<(), InferError> {
    match decl {
        AstDecl::Use(_) => Ok(()),
        AstDecl::External(external) => {
            let (params, result) = parse_type_signature(&external.type_text);
            let type_ = Type::Function(
                params
                    .iter()
                    .map(|param| rsig_type_to_infer_type(param, state))
                    .collect(),
                Box::new(rsig_type_to_infer_type(&result, state)),
            );
            state.add_value(external.name.clone(), state.generalize(type_));
            Ok(())
        }
        AstDecl::Function(function) => {
            let type_ = infer_function(state, function)?;
            state.add_value(function.name.clone(), state.generalize(type_));
            Ok(())
        }
    }
}

fn infer_function(state: &mut State, function: &AstFnDecl) -> Result<Type, InferError> {
    state.push_scope();
    let params = function
        .params
        .iter()
        .enumerate()
        .map(|(index, param)| {
            let type_ = function
                .param_types
                .get(index)
                .and_then(|annotation| annotation.as_ref())
                .map(|annotation| rsig_type_to_infer_type(&parse_type(&annotation.text), state))
                .unwrap_or_else(|| state.fresh_var());
            state.add_value(param.clone(), state.monomorphic(type_.clone()));
            type_
        })
        .collect::<Vec<_>>();
    let result = infer_block(state, &function.body)?;
    let result = if let Some(annotation) = &function.return_type {
        let expected = rsig_type_to_infer_type(&parse_type(&annotation.text), state);
        state.unify(&expected, &result)?;
        expected
    } else {
        result
    };
    state.pop_scope();
    Ok(Type::Function(params, Box::new(state.resolve(&result))))
}

fn infer_block(state: &mut State, block: &AstBlock) -> Result<Type, InferError> {
    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                let inferred = infer_expr(state, value)?;
                let type_ = if let Some(annotation) = type_annotation {
                    let expected = rsig_type_to_infer_type(&parse_type(&annotation.text), state);
                    state.unify(&expected, &inferred)?;
                    expected
                } else {
                    inferred
                };
                state.add_value(name.clone(), state.generalize(type_));
            }
            AstStmt::Expr(expr) => {
                infer_expr(state, expr)?;
            }
        }
    }

    if let Some(tail) = &block.tail {
        infer_expr(state, tail)
    } else {
        Ok(Type::Unit)
    }
}

fn infer_expr(state: &mut State, expr: &AstExpr) -> Result<Type, InferError> {
    match expr {
        AstExpr::Bool { .. } => Ok(Type::Bool),
        AstExpr::Char { .. } => Ok(Type::Char),
        AstExpr::Float { .. } => Ok(Type::F64),
        AstExpr::Int { .. } => Ok(Type::I64),
        AstExpr::String { .. } => Ok(Type::String),
        AstExpr::Unit { .. } => Ok(Type::Unit),
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
            let lhs = infer_expr(state, lhs)?;
            let rhs = infer_expr(state, rhs)?;
            state.unify(&Type::I64, &lhs)?;
            state.unify(&Type::I64, &rhs)?;
            Ok(Type::I64)
        }
        AstExpr::Neg { expr, .. } => {
            let actual = infer_expr(state, expr)?;
            state.unify(&Type::I64, &actual)?;
            Ok(Type::I64)
        }
        AstExpr::Eq { lhs, rhs, .. } => {
            let lhs = infer_expr(state, lhs)?;
            let rhs = infer_expr(state, rhs)?;
            state.unify(&lhs, &rhs)?;
            Ok(Type::Bool)
        }
        AstExpr::Lt { lhs, rhs, .. } => {
            let lhs = infer_expr(state, lhs)?;
            let rhs = infer_expr(state, rhs)?;
            state.unify(&Type::I64, &lhs)?;
            state.unify(&Type::I64, &rhs)?;
            Ok(Type::Bool)
        }
        AstExpr::And { lhs, rhs, .. } | AstExpr::Or { lhs, rhs, .. } => {
            let lhs = infer_expr(state, lhs)?;
            let rhs = infer_expr(state, rhs)?;
            state.unify(&Type::Bool, &lhs)?;
            state.unify(&Type::Bool, &rhs)?;
            Ok(Type::Bool)
        }
        AstExpr::Not { expr, .. } => {
            let actual = infer_expr(state, expr)?;
            state.unify(&Type::Bool, &actual)?;
            Ok(Type::Bool)
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            let condition = infer_expr(state, condition)?;
            state.unify(&Type::Bool, &condition)?;
            let then_type = infer_expr(state, then_branch)?;
            let else_type = infer_expr(state, else_branch)?;
            state.unify(&then_type, &else_type)?;
            Ok(state.resolve(&then_type))
        }
        AstExpr::Tuple { items, .. } => Ok(Type::Tuple(
            items
                .iter()
                .map(|item| infer_expr(state, item))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        AstExpr::List { items, .. } => {
            let element = state.fresh_var();
            for item in items {
                let actual = infer_expr(state, item)?;
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
                .map(|arg| infer_expr(state, arg))
                .collect::<Result<Vec<_>, _>>()?;
            let result = state.fresh_var();
            state.unify(
                &callee_type,
                &Type::Function(arg_types, Box::new(result.clone())),
            )?;
            Ok(state.resolve(&result))
        }
        AstExpr::Spawn { .. } => Ok(Type::ActorId(Box::new(state.fresh_var()))),
        AstExpr::Record { path, .. } => Ok(Type::Record(path.segments.join("."))),
        AstExpr::TupleIndex { base, index, .. } => {
            if let AstExpr::Tuple { items, .. } = base.as_ref() {
                items
                    .get(*index)
                    .map(|item| infer_expr(state, item))
                    .unwrap_or_else(|| Err(InferError::Unsupported("tuple projection index")))
            } else {
                Err(InferError::Unsupported("tuple projection"))
            }
        }
        AstExpr::Field { base, field, .. } => {
            if let AstExpr::Record { fields, .. } = base.as_ref() {
                fields
                    .iter()
                    .find_map(|(name, value)| (name == field).then(|| infer_expr(state, value)))
                    .unwrap_or_else(|| Err(InferError::Unsupported("record field")))
            } else {
                Err(InferError::Unsupported("record field"))
            }
        }
        AstExpr::Receive { .. } => Ok(Type::Unit),
        AstExpr::Call { .. } | AstExpr::Path { .. } => Err(InferError::Unsupported("path")),
    }
}

fn rsig_type_to_infer_type(type_: &RsigType, state: &mut State) -> Type {
    match type_ {
        RsigType::ActorId(message) => {
            Type::ActorId(Box::new(rsig_type_to_infer_type(message, state)))
        }
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

#[cfg(test)]
mod tests {
    use crate::ast::{
        AstBlock, AstDecl, AstExpr, AstFnDecl, AstPath, AstProgram, AstStmt, TextSpan,
    };
    use crate::infer::module::infer_program;
    use crate::infer::scheme::TypeScheme;
    use crate::infer::types::Type;

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

        let inferred = infer_program(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(exports.len(), 2);
        assert_eq!(exports[0].0, "one");
        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::Function(Vec::new(), Box::new(Type::I64)))
        );
        assert_eq!(exports[1].0, "two");
        assert_eq!(
            exports[1].1,
            TypeScheme::monomorphic(Type::Function(Vec::new(), Box::new(Type::I64)))
        );
    }

    #[test]
    fn later_declarations_cannot_see_future_values() {
        let program = AstProgram {
            decls: vec![function("two", vec![], Vec::new(), call("one", Vec::new()))],
        };

        let err = infer_program(&program).unwrap_err();

        assert_eq!(
            crate::infer::module::InferError::UnknownValue("one".to_owned()),
            err
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

        let inferred = infer_program(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::Function(Vec::new(), Box::new(Type::Bool)))
        );
    }

    #[test]
    fn function_parameters_are_generalized_at_the_top_level() {
        let program = AstProgram {
            decls: vec![function("id", vec!["x"], Vec::new(), path("x"))],
        };

        let inferred = infer_program(&program).unwrap();
        let exports = inferred.env.exported_values();
        let var = exports[0].1.quantifiers[0];

        assert_eq!(exports[0].0, "id");
        assert_eq!(exports[0].1.quantifiers.len(), 1);
        assert_eq!(
            exports[0].1,
            TypeScheme {
                quantifiers: vec![var],
                body: Type::Function(vec![Type::Var(var)], Box::new(Type::Var(var))),
            }
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

        let inferred = infer_program(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(exports.len(), 1);
        assert_eq!(exports[0].0, "main");
        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::Function(Vec::new(), Box::new(Type::Unit)))
        );
    }
}
