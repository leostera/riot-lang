use super::ir::{LambdaExpr, LambdaPath};

pub(super) fn lower_prelude_operator(callee: LambdaPath, args: Vec<LambdaExpr>) -> LambdaExpr {
    let operator = callee.as_slice().last().map(String::as_str);
    match (operator, args.as_slice()) {
        (Some("(+)"), [_, _]) => {
            LambdaExpr::Add(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
        (Some("(-)"), [_, _]) => {
            LambdaExpr::Sub(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
        (Some("(-)"), [_]) => LambdaExpr::Neg(Box::new(args[0].clone())),
        (Some("(*)"), [_, _]) => {
            LambdaExpr::Mul(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
        (Some("(/)"), [_, _]) => {
            LambdaExpr::Div(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
        (Some("(%)"), [_, _]) => {
            LambdaExpr::Mod(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
        (Some("(==)"), [_, _]) => {
            LambdaExpr::Eq(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
        (Some("(<)"), [_, _]) => {
            LambdaExpr::Lt(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
        (Some("(&&)"), [_, _]) => {
            LambdaExpr::And(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
        (Some("(||)"), [_, _]) => {
            LambdaExpr::Or(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
        (Some("(!)"), [_]) => LambdaExpr::Not(Box::new(args[0].clone())),
        _ => LambdaExpr::Call {
            callee: callee.into_segments(),
            args,
        },
    }
}
