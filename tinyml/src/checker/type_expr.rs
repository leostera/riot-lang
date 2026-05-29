use crate::parser::{ast, ident::Ident};

use super::{state::State, tst::Type};

impl Type {
    pub fn from_type_expr(state: &mut State, annotation: &ast::TypeExpr) -> Self {
        match annotation {
            ast::TypeExpr::Var { var, .. } => state
                .get_type_param_var(var)
                .cloned()
                .unwrap_or_else(|| state.fresh_var()),
            ast::TypeExpr::App { name, args, .. } => Type::Apply {
                ident: name.clone(),
                arguments: args
                    .iter()
                    .map(|arg| Type::from_type_expr(state, arg))
                    .collect(),
            },
            ast::TypeExpr::Fn { params, result, .. } => {
                let result = Type::from_type_expr(state, result);
                params
                    .iter()
                    .rev()
                    .fold(result, |result, param| Type::Arrow {
                        parameter: Box::new(Type::from_type_expr(state, param)),
                        result: Box::new(result),
                    })
            }
            ast::TypeExpr::Tuple { items, .. } => Type::Tuple(
                items
                    .iter()
                    .map(|item| Type::from_type_expr(state, item))
                    .collect(),
            ),
            ast::TypeExpr::Record { name, .. } => Type::Apply {
                ident: name.clone(),
                arguments: Vec::new(),
            },
            ast::TypeExpr::Variant { .. } => Type::Apply {
                ident: Ident::from_string("variant"),
                arguments: Vec::new(),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        checker::{state::State, tst::Type},
        parser::{ast, ident::Ident, lexer::Span},
    };

    fn ident(name: &str) -> Ident {
        Ident::Name {
            name: name.into(),
            span: Span::new(0, name.len()),
        }
    }

    fn app(name: &str) -> Type {
        Type::Apply {
            ident: ident(name),
            arguments: Vec::new(),
        }
    }

    #[test]
    fn lowers_type_identifier_to_application() {
        let mut state = State::new();
        let ty = ast::TypeExpr::App {
            name: ident("i32"),
            args: Vec::new(),
            span: Span::new(0, 3),
        };
        assert_eq!(Type::from_type_expr(&mut state, &ty), app("i32"));
    }

    #[test]
    fn lowers_function_types_to_nested_unary_arrows() {
        let mut state = State::new();
        let ty = ast::TypeExpr::Fn {
            params: vec![
                ast::TypeExpr::App {
                    name: ident("i32"),
                    args: Vec::new(),
                    span: Span::new(0, 3),
                },
                ast::TypeExpr::App {
                    name: ident("i32"),
                    args: Vec::new(),
                    span: Span::new(0, 3),
                },
            ],
            result: Box::new(ast::TypeExpr::App {
                name: ident("bool"),
                args: Vec::new(),
                span: Span::new(0, 4),
            }),
            span: Span::new(0, 10),
        };
        assert_eq!(
            Type::from_type_expr(&mut state, &ty),
            Type::Arrow {
                parameter: Box::new(app("i32")),
                result: Box::new(Type::Arrow {
                    parameter: Box::new(app("i32")),
                    result: Box::new(app("bool")),
                }),
            }
        );
    }
}
