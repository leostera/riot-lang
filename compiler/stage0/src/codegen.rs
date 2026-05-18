use camino::Utf8Path;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};

use crate::ir::{RirInstruction, RirProgram};

pub(crate) fn emit_object(
    program: &RirProgram,
    object_path: &Utf8Path,
    llvm_ir_path: Option<&Utf8Path>,
) -> miette::Result<()> {
    Target::initialize_native(&InitializationConfig::default())
        .map_err(|error| miette::miette!("failed to initialize native LLVM target: {error}"))?;

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple)
        .map_err(|error| miette::miette!("failed to load LLVM target: {error}"))?;
    let target_machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .ok_or_else(|| miette::miette!("failed to create LLVM target machine"))?;

    let context = Context::create();
    let module = context.create_module("stage0_main");
    module.set_triple(&triple);
    module.set_data_layout(&target_machine.get_target_data().get_data_layout());

    let builder = context.create_builder();
    let i32_type = context.i32_type();
    let i64_type = context.i64_type();
    let void_type = context.void_type();
    let ptr_type = context.ptr_type(AddressSpace::default());

    let init_fn = module.add_function("riot_rt_init", void_type.fn_type(&[], false), None);
    let shutdown_fn = module.add_function("riot_rt_shutdown", void_type.fn_type(&[], false), None);
    let println_fn = module.add_function(
        "riot_rt_println",
        void_type.fn_type(&[ptr_type.into(), i64_type.into()], false),
        None,
    );
    let dbg_fn = module.add_function(
        "riot_rt_dbg",
        void_type.fn_type(&[ptr_type.into(), i64_type.into()], false),
        None,
    );

    let main_fn = module.add_function("main", i32_type.fn_type(&[], false), None);
    let entry = context.append_basic_block(main_fn, "entry");
    builder.position_at_end(entry);

    builder
        .build_call(init_fn, &[], "riot_rt_init")
        .map_err(|error| miette::miette!("failed to emit runtime init call: {error}"))?;

    for instruction in &program.entry.instructions {
        match instruction {
            RirInstruction::RuntimeDbg { message } => {
                emit_string_runtime_call(
                    &builder,
                    dbg_fn,
                    i64_type,
                    message,
                    "riot_rt_dbg",
                    "failed to emit dbg call",
                )?;
            }
            RirInstruction::RuntimePrintln { message } => {
                emit_string_runtime_call(
                    &builder,
                    println_fn,
                    i64_type,
                    message,
                    "riot_rt_println",
                    "failed to emit println call",
                )?;
            }
        }
    }

    builder
        .build_call(shutdown_fn, &[], "riot_rt_shutdown")
        .map_err(|error| miette::miette!("failed to emit runtime shutdown call: {error}"))?;
    builder
        .build_return(Some(&i32_type.const_zero()))
        .map_err(|error| miette::miette!("failed to emit main return: {error}"))?;

    module
        .verify()
        .map_err(|error| miette::miette!("generated invalid LLVM module: {error}"))?;

    if let Some(path) = llvm_ir_path {
        module
            .print_to_file(path.as_std_path())
            .map_err(|error| miette::miette!("failed to write LLVM IR: {error}"))?;
    }

    target_machine
        .write_to_file(&module, FileType::Object, object_path.as_std_path())
        .map_err(|error| miette::miette!("failed to emit object file: {error}"))?;

    Ok(())
}

fn emit_string_runtime_call<'ctx>(
    builder: &inkwell::builder::Builder<'ctx>,
    function: inkwell::values::FunctionValue<'ctx>,
    i64_type: inkwell::types::IntType<'ctx>,
    message: &str,
    call_name: &str,
    error_context: &'static str,
) -> miette::Result<()> {
    let value = builder
        .build_global_string_ptr(message, "str")
        .map_err(|error| miette::miette!("failed to emit string literal: {error}"))?;
    let len = i64_type.const_int(message.len() as u64, false);
    builder
        .build_call(
            function,
            &[value.as_pointer_value().into(), len.into()],
            call_name,
        )
        .map_err(|error| miette::miette!("{error_context}: {error}"))?;
    Ok(())
}
