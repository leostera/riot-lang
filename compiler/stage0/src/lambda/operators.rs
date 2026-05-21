use super::ir::{RirExpr, RirPath};

pub(super) fn lower_prelude_operator(callee: RirPath, args: Vec<RirExpr>) -> RirExpr {
    let operator = callee.as_slice().last().map(String::as_str);
    match (operator, args.as_slice()) {
        (Some("(+)"), [_, _]) => RirExpr::Add(Box::new(args[0].clone()), Box::new(args[1].clone())),
        (Some("(-)"), [_, _]) => RirExpr::Sub(Box::new(args[0].clone()), Box::new(args[1].clone())),
        (Some("(-)"), [_]) => RirExpr::Neg(Box::new(args[0].clone())),
        (Some("(*)"), [_, _]) => RirExpr::Mul(Box::new(args[0].clone()), Box::new(args[1].clone())),
        (Some("(/)"), [_, _]) => RirExpr::Div(Box::new(args[0].clone()), Box::new(args[1].clone())),
        (Some("(%)"), [_, _]) => RirExpr::Mod(Box::new(args[0].clone()), Box::new(args[1].clone())),
        (Some("(==)"), [_, _]) => RirExpr::Eq(Box::new(args[0].clone()), Box::new(args[1].clone())),
        (Some("(<)"), [_, _]) => RirExpr::Lt(Box::new(args[0].clone()), Box::new(args[1].clone())),
        (Some("(&&)"), [_, _]) => {
            RirExpr::And(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
        (Some("(||)"), [_, _]) => RirExpr::Or(Box::new(args[0].clone()), Box::new(args[1].clone())),
        (Some("(!)"), [_]) => RirExpr::Not(Box::new(args[0].clone())),
        _ => RirExpr::Call {
            callee: callee.into_segments(),
            args,
        },
    }
}
