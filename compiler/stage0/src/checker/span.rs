use crate::ast::{AstExpr, TextSpan};

pub(super) fn expr_span(expr: &AstExpr) -> TextSpan {
    match expr {
        AstExpr::Add {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Sub {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Mul {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Div {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Mod {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Neg { expr: _, span }
        | AstExpr::Eq {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Lt {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::And {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Or {
            lhs: _,
            rhs: _,
            span,
        }
        | AstExpr::Not { expr: _, span }
        | AstExpr::If {
            condition: _,
            then_branch: _,
            else_branch: _,
            span,
        }
        | AstExpr::Match {
            scrutinee: _,
            arms: _,
            span,
        }
        | AstExpr::Block { block: _, span } => *span,
        AstExpr::Call {
            callee: _,
            args: _,
            span,
        }
        | AstExpr::Apply {
            callee: _,
            args: _,
            span,
        }
        | AstExpr::Lambda {
            params: _,
            param_types: _,
            body: _,
            span,
        }
        | AstExpr::Spawn { body: _, span }
        | AstExpr::Receive {
            binder: _,
            binder_span: _,
            body: _,
            span,
        } => *span,
        AstExpr::Bool { value: _, span }
        | AstExpr::Char { value: _, span }
        | AstExpr::Unit { span }
        | AstExpr::Tuple { items: _, span }
        | AstExpr::List { items: _, span }
        | AstExpr::Record {
            path: _,
            fields: _,
            span,
        }
        | AstExpr::Field {
            base: _,
            field: _,
            span,
        }
        | AstExpr::TupleIndex {
            base: _,
            index: _,
            span,
        }
        | AstExpr::Float { value: _, span }
        | AstExpr::Int { value: _, span }
        | AstExpr::Path { path: _, span }
        | AstExpr::String { value: _, span } => *span,
    }
}
