#[derive(Debug, Clone)]
pub(crate) struct TypedProgram {
    pub(crate) main: TypedFunction,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedFunction {
    pub(crate) body: TypedExpr,
}

#[derive(Debug, Clone)]
pub(crate) enum TypedExpr {
    Println { message: String },
}

#[derive(Debug, Clone)]
pub(crate) struct RirProgram {
    pub(crate) entry: RirFunction,
}

#[derive(Debug, Clone)]
pub(crate) struct RirFunction {
    pub(crate) instructions: Vec<RirInstruction>,
}

#[derive(Debug, Clone)]
pub(crate) enum RirInstruction {
    RuntimePrintln { message: String },
}

pub(crate) fn lower_to_rir(program: TypedProgram) -> RirProgram {
    let instruction = match program.main.body {
        TypedExpr::Println { message } => RirInstruction::RuntimePrintln { message },
    };

    RirProgram {
        entry: RirFunction {
            instructions: vec![instruction],
        },
    }
}
