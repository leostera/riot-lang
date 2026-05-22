use std::collections::HashMap;

use camino::Utf8Path;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};
use miette::bail;

use crate::actor::StacklessActorLowerer;
use crate::actor::air::{
    ActorFrameOp, ActorFrameSlot, ActorFrameState, ActorIrActor, ActorIrProgram, ActorStateNext,
};
use crate::lambda::ir::{
    Capture, LambdaBlock, LambdaExpr, LambdaExternal, LambdaExternalTable, LambdaMatchArm,
    LambdaPattern, LambdaProgram, LambdaStmt, Param,
};
use crate::signature::{ConstructorName, ImportedSignatures, RsigExport, RsigType, TypeName};
use crate::stdlib::Stdlib;

mod abi;
mod static_eval;

use abi::{AbiType, ExternalAbi, FunctionAbi};
use static_eval::{StaticEvaluator, StaticFunctionTable, StaticValue};

const POLL_CONSUMED: u32 = 1;
const POLL_DONE: u32 = 2;
const POLL_PROGRESS: u32 = 4;
const POLL_WAITING: u32 = 16;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CodegenMode {
    Executable,
    Module,
}

#[derive(Debug, Default)]
pub(crate) struct LlvmBackend;

impl LlvmBackend {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn emit_executable_object(
        &self,
        program: &LambdaProgram,
        imports: &ImportedSignatures,
        object_path: &Utf8Path,
        llvm_ir_path: Option<&Utf8Path>,
    ) -> miette::Result<()> {
        self.emit_object_with_mode(
            program,
            imports,
            object_path,
            llvm_ir_path,
            CodegenMode::Executable,
        )
    }

    pub(crate) fn emit_module_object(
        &self,
        program: &LambdaProgram,
        imports: &ImportedSignatures,
        object_path: &Utf8Path,
        llvm_ir_path: Option<&Utf8Path>,
    ) -> miette::Result<()> {
        self.emit_object_with_mode(
            program,
            imports,
            object_path,
            llvm_ir_path,
            CodegenMode::Module,
        )
    }

    pub(crate) fn emit_llvm_text(
        &self,
        program: &LambdaProgram,
        imports: &ImportedSignatures,
        mode: CodegenMode,
    ) -> miette::Result<String> {
        let context = Context::create();
        let target_machine = Self::create_target_machine()?;
        let module = Self::build_program_module(&context, &target_machine, program, imports, mode)?;
        Ok(module.print_to_string().to_string())
    }

    pub(crate) fn emit_assembly_text(
        &self,
        program: &LambdaProgram,
        imports: &ImportedSignatures,
        mode: CodegenMode,
    ) -> miette::Result<String> {
        let context = Context::create();
        let target_machine = Self::create_target_machine()?;
        let module = Self::build_program_module(&context, &target_machine, program, imports, mode)?;
        let buffer = target_machine
            .write_to_memory_buffer(&module, FileType::Assembly)
            .map_err(|error| miette::miette!("failed to emit assembly: {error}"))?;
        Ok(String::from_utf8_lossy(buffer.as_slice()).into_owned())
    }

    fn emit_object_with_mode(
        &self,
        program: &LambdaProgram,
        imports: &ImportedSignatures,
        object_path: &Utf8Path,
        llvm_ir_path: Option<&Utf8Path>,
        mode: CodegenMode,
    ) -> miette::Result<()> {
        let context = Context::create();
        let target_machine = Self::create_target_machine()?;
        let module = Self::build_program_module(&context, &target_machine, program, imports, mode)?;

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

    fn create_target_machine() -> miette::Result<TargetMachine> {
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|error| miette::miette!("failed to initialize native LLVM target: {error}"))?;

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)
            .map_err(|error| miette::miette!("failed to load LLVM target: {error}"))?;
        target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| miette::miette!("failed to create LLVM target machine"))
    }

    fn build_program_module<'ctx>(
        context: &'ctx Context,
        target_machine: &TargetMachine,
        program: &LambdaProgram,
        imports: &ImportedSignatures,
        mode: CodegenMode,
    ) -> miette::Result<Module<'ctx>> {
        let triple = TargetMachine::get_default_triple();
        let module = context.create_module(program.module_name.as_str());
        module.set_triple(&triple);
        module.set_data_layout(&target_machine.get_target_data().get_data_layout());

        let externals = codegen_externals(program)?;
        let codegen = Codegen {
            context,
            module,
            builder: context.create_builder(),
            program,
            imports,
            functions: FunctionValueTable::new(),
            function_abis: infer_function_abis(program, &externals),
            function_map: StaticFunctionTable::from_functions(&program.functions),
            externals,
            actor_ir: StacklessActorLowerer::new(imports).lower(program),
            string_counter: 0,
        };
        codegen.emit_program(mode)
    }
}

#[derive(Debug, Clone)]
struct FrameSlot {
    name: String,
    abi: AbiType,
    field_index: u32,
}

#[derive(Debug, Clone)]
struct ActorShape {
    slots: Vec<FrameSlot>,
    captures: Vec<FrameSlot>,
    states: Vec<ActorFrameState>,
    size_bytes: usize,
    align: usize,
}

#[derive(Clone)]
enum CgValue<'ctx> {
    Unit,
    I64(IntValue<'ctx>),
    Bool(IntValue<'ctx>),
    ActorId(IntValue<'ctx>),
    Value(IntValue<'ctx>),
    Static(StaticValue),
}

impl<'ctx> CgValue<'ctx> {
    fn runtime_root(&self) -> Option<IntValue<'ctx>> {
        match self {
            CgValue::Value(value) => Some(*value),
            _ => None,
        }
    }

    fn abi(&self) -> AbiType {
        match self {
            CgValue::Unit => AbiType::Unit,
            CgValue::I64(_) => AbiType::I64,
            CgValue::Bool(_) => AbiType::Bool,
            CgValue::ActorId(_) => AbiType::ActorId,
            CgValue::Value(_) | CgValue::Static(_) => AbiType::Value,
        }
    }
}

#[derive(Clone, Default)]
struct Env<'ctx> {
    values: HashMap<String, CgValue<'ctx>>,
    statics: HashMap<String, StaticValue>,
}

impl Env<'_> {
    fn value_abis(&self) -> HashMap<String, AbiType> {
        self.values
            .iter()
            .map(|(name, value)| (name.clone(), value.abi()))
            .collect()
    }
}

struct Codegen<'ctx, 'a> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    program: &'a LambdaProgram,
    imports: &'a ImportedSignatures,
    functions: FunctionValueTable<'ctx>,
    function_abis: FunctionAbiTable,
    function_map: StaticFunctionTable<'a>,
    externals: LambdaExternalTable,
    actor_ir: ActorIrProgram,
    string_counter: usize,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct FunctionAbiTable {
    abis: HashMap<String, FunctionAbi>,
}

impl FunctionAbiTable {
    fn new() -> Self {
        Self::default()
    }

    fn insert(&mut self, name: impl Into<String>, abi: FunctionAbi) {
        self.abis.insert(name.into(), abi);
    }

    fn get(&self, name: impl AsRef<str>) -> Option<&FunctionAbi> {
        self.abis.get(name.as_ref())
    }
}

#[derive(Debug, Default)]
struct FunctionValueTable<'ctx> {
    values: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> FunctionValueTable<'ctx> {
    fn new() -> Self {
        Self::default()
    }

    fn insert(&mut self, name: impl Into<String>, value: FunctionValue<'ctx>) {
        self.values.insert(name.into(), value);
    }

    fn get(&self, name: impl AsRef<str>) -> Option<FunctionValue<'ctx>> {
        self.values.get(name.as_ref()).copied()
    }

    fn contains_key(&self, name: impl AsRef<str>) -> bool {
        self.values.contains_key(name.as_ref())
    }
}

impl<'ctx> Codegen<'ctx, '_> {
    fn emit_program(mut self, mode: CodegenMode) -> miette::Result<Module<'ctx>> {
        self.declare_local_functions()?;
        self.define_local_functions()?;
        if mode == CodegenMode::Executable {
            self.emit_c_main()?;
        }
        self.module
            .verify()
            .map_err(|error| miette::miette!("generated invalid LLVM module: {error}"))?;
        Ok(self.module)
    }

    fn declare_local_functions(&mut self) -> miette::Result<()> {
        for function in &self.program.functions {
            if function.name == "main" {
                continue;
            }
            let Some(abi) = self.function_abis.get(&function.name) else {
                continue;
            };
            if !abi.is_supported_local() {
                continue;
            }
            let fn_type = self.local_function_type(abi)?;
            let function_value = self.module.add_function(&function.symbol, fn_type, None);
            self.functions.insert(function.name.clone(), function_value);
        }
        Ok(())
    }

    fn define_local_functions(&mut self) -> miette::Result<()> {
        let functions = self
            .program
            .functions
            .iter()
            .filter(|function| function.name != "main")
            .cloned()
            .collect::<Vec<_>>();
        for function in functions {
            let Some(function_value) = self.functions.get(&function.name) else {
                continue;
            };
            let Some(abi) = self.function_abis.get(&function.name).cloned() else {
                continue;
            };
            let entry = self.context.append_basic_block(function_value, "entry");
            self.builder.position_at_end(entry);

            let mut env = Env::default();
            for (index, (param, param_abi)) in function.params.iter().zip(&abi.params).enumerate() {
                let value = function_value
                    .get_nth_param(index as u32)
                    .ok_or_else(|| miette::miette!("missing LLVM parameter {index}"))?;
                let value = match param_abi {
                    AbiType::I64 => CgValue::I64(value.into_int_value()),
                    AbiType::Bool => CgValue::Bool(value.into_int_value()),
                    AbiType::ActorId => CgValue::ActorId(value.into_int_value()),
                    AbiType::Value => CgValue::Value(value.into_int_value()),
                    AbiType::Unit | AbiType::Unknown => CgValue::Unit,
                };
                env.values.insert(param.as_str().to_owned(), value);
            }

            let result = self.emit_block(&function.body, &mut env)?;
            self.emit_return_value(result, abi.result)?;
        }
        Ok(())
    }

    fn emit_c_main(&mut self) -> miette::Result<()> {
        let Some(main) = self
            .program
            .functions
            .iter()
            .find(|function| function.name == "main")
            .cloned()
        else {
            bail!("stage0 compile requires one entrypoint named main");
        };

        let i32_type = self.context.i32_type();
        let main_type = match main.params.as_slice() {
            [] => i32_type.fn_type(&[], false),
            [_] => i32_type.fn_type(&[i32_type.into(), self.ptr_type().into()], false),
            _ => bail!("stage0 main supports at most one argument list"),
        };
        let main_fn = self.module.add_function("main", main_type, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        self.call_void_runtime("riot_rt_init", &[])?;
        let mut env = Env::default();
        if let [args] = main.params.as_slice() {
            let argc = main_fn
                .get_nth_param(0)
                .ok_or_else(|| miette::miette!("missing argc parameter"))?
                .into_int_value();
            let argv = main_fn
                .get_nth_param(1)
                .ok_or_else(|| miette::miette!("missing argv parameter"))?
                .into_pointer_value();
            let args_value =
                self.call_runtime_value("riot_rt_argv", &[argc.into(), argv.into()])?;
            env.values
                .insert(args.as_str().to_owned(), CgValue::Value(args_value));
        }
        let result = self.emit_block(&main.body, &mut env)?;
        let exit_code = self.emit_main_exit_code(result, &main.result)?;
        self.call_void_runtime("riot_rt_shutdown", &[])?;
        self.builder
            .build_return(Some(&exit_code))
            .map_err(|error| miette::miette!("failed to emit main return: {error}"))?;
        Ok(())
    }

    fn emit_main_exit_code(
        &mut self,
        result: CgValue<'ctx>,
        result_type: &RsigType,
    ) -> miette::Result<IntValue<'ctx>> {
        let i32_type = self.context.i32_type();
        match result_type {
            RsigType::Unit | RsigType::Unknown => Ok(i32_type.const_zero()),
            RsigType::I32 | RsigType::I64 => {
                let value = self.value_as_i64(result)?;
                self.builder
                    .build_int_truncate(value, i32_type, "main_exit_code")
                    .map_err(|error| miette::miette!("failed to emit main exit code: {error}"))
            }
            type_ if is_result_unit_i32(type_) => {
                let value = self.value_as_runtime(result)?;
                Ok(self
                    .call_runtime_basic("riot_rt_result_exit_code", &[value.into()])?
                    .into_int_value())
            }
            other => bail!(
                "unsupported main return type `{}` for executable codegen",
                other.canonical()
            ),
        }
    }

    fn emit_block(
        &mut self,
        block: &LambdaBlock,
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let mut rooted_values = self.push_env_runtime_roots(env)?;
        for stmt in &block.statements {
            match stmt {
                LambdaStmt::Let { name, value } => {
                    if let Some(static_value) = self.static_eval(value, &env.statics) {
                        env.statics.insert(name.as_str().to_owned(), static_value);
                    }
                    let value = self.emit_expr(value, env)?;
                    if let Some(root) = value.runtime_root() {
                        self.push_runtime_root(root)?;
                        rooted_values += 1;
                    }
                    env.values.insert(name.as_str().to_owned(), value);
                }
                LambdaStmt::Expr(expr) => {
                    self.emit_expr(expr, env)?;
                }
            }
        }

        let result = if let Some(tail) = &block.tail {
            self.emit_expr(tail, env)
        } else {
            Ok(CgValue::Unit)
        }?;
        self.pop_runtime_roots(rooted_values)?;
        Ok(result)
    }

    fn emit_expr(
        &mut self,
        expr: &LambdaExpr,
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        match expr {
            LambdaExpr::If {
                condition,
                then_branch,
                else_branch,
            } => self.emit_if(condition, then_branch, else_branch, env),
            LambdaExpr::Match { scrutinee, arms } => self.emit_match(scrutinee, arms, env),
            LambdaExpr::Block(block) => {
                let mut scoped = env.clone();
                self.emit_block(block, &mut scoped)
            }
            LambdaExpr::Bool(value) => Ok(CgValue::Bool(
                self.context.bool_type().const_int(u64::from(*value), false),
            )),
            LambdaExpr::Call {
                callee,
                args,
                result,
                ..
            } => self.emit_call(callee, args, result, env),
            LambdaExpr::Apply {
                callee,
                args,
                result,
            } => self.emit_apply(callee, args, result, env),
            LambdaExpr::Lambda {
                params,
                captures,
                body,
            } => self.emit_lambda_value(params, captures, body, env),
            LambdaExpr::Unit => Ok(CgValue::Unit),
            LambdaExpr::Tuple(items) => self.emit_value_sequence("riot_rt_value_tuple", items, env),
            LambdaExpr::List(items) => self.emit_value_sequence("riot_rt_value_list", items, env),
            LambdaExpr::Record { path, fields } => self.emit_record_value(path, fields, env),
            LambdaExpr::Field { base, field } => {
                let base = self.emit_expr(base, env)?;
                let base = self.value_as_runtime(base)?;
                self.push_runtime_root(base)?;
                let (field_ptr, field_len) = self.string_literal(field)?;
                let value = self.call_runtime_value(
                    "riot_rt_value_record_get",
                    &[base.into(), field_ptr.into(), field_len.into()],
                )?;
                self.pop_runtime_roots(1)?;
                Ok(CgValue::Value(value))
            }
            LambdaExpr::TupleIndex { base, index } => {
                let base = self.emit_expr(base, env)?;
                let base = self.value_as_runtime(base)?;
                self.push_runtime_root(base)?;
                let value = self.call_runtime_value(
                    "riot_rt_value_tuple_get",
                    &[
                        base.into(),
                        self.context
                            .i64_type()
                            .const_int(*index as u64, false)
                            .into(),
                    ],
                )?;
                self.pop_runtime_roots(1)?;
                Ok(CgValue::Value(value))
            }
            LambdaExpr::Variant {
                type_name,
                constructor,
                payload,
            } => self.emit_variant_value(type_name, constructor, payload, env),
            LambdaExpr::Char(_) | LambdaExpr::Float(_) => self
                .static_eval(expr, &env.statics)
                .map(|value| self.emit_static_value(value))
                .transpose()?
                .ok_or_else(|| miette::miette!("unsupported non-scalar expression")),
            LambdaExpr::Int(value) => Ok(CgValue::I64(self.i64_const(*value))),
            LambdaExpr::Path(path) => self.emit_path(path.as_slice(), env),
            LambdaExpr::Local(binding) => self.emit_path(&[binding.as_str().to_owned()], env),
            LambdaExpr::String(value) => {
                let (ptr, len) = self.string_literal(value)?;
                Ok(CgValue::Value(self.call_runtime_value(
                    "riot_rt_value_string",
                    &[ptr.into(), len.into()],
                )?))
            }
            LambdaExpr::Spawn { actor_id, .. } => self.emit_spawn(*actor_id, env),
            LambdaExpr::Receive { .. } => bail!("receive cannot be lowered outside an actor frame"),
        }
    }

    fn emit_variant_value(
        &mut self,
        type_name: &TypeName,
        constructor: &ConstructorName,
        payload: &[LambdaExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let runtime_type_name = runtime_variant_type_name(type_name);
        let (type_ptr, type_len) = self.string_literal(&runtime_type_name)?;
        let (constructor_ptr, constructor_len) = self.string_literal(constructor.as_str())?;
        if payload.is_empty() {
            return Ok(CgValue::Value(self.call_runtime_value(
                "riot_rt_value_variant",
                &[
                    type_ptr.into(),
                    type_len.into(),
                    constructor_ptr.into(),
                    constructor_len.into(),
                ],
            )?));
        }

        let payload = self.emit_variant_payload(payload, env)?;
        self.push_runtime_root(payload)?;
        let variant = self.call_runtime_value(
            "riot_rt_value_variant_payload",
            &[
                type_ptr.into(),
                type_len.into(),
                constructor_ptr.into(),
                constructor_len.into(),
                payload.into(),
            ],
        )?;
        self.pop_runtime_roots(1)?;
        Ok(CgValue::Value(variant))
    }

    fn emit_variant_payload(
        &mut self,
        payload: &[LambdaExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<IntValue<'ctx>> {
        match payload {
            [] => self.call_runtime_value("riot_rt_value_unit", &[]),
            [value] => {
                let value = self.emit_expr(value, env)?;
                self.value_as_runtime(value)
            }
            values => {
                let tuple = self.emit_value_sequence("riot_rt_value_tuple", values, env)?;
                self.value_as_runtime(tuple)
            }
        }
    }

    fn emit_if(
        &mut self,
        condition: &LambdaExpr,
        then_branch: &LambdaExpr,
        else_branch: &LambdaExpr,
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let Some(parent) = self.current_function() else {
            bail!("if expression emitted without an active function");
        };
        let local_abis = env.value_abis();
        let result_abi = unify_abi(
            infer_expr_abi(
                then_branch,
                &local_abis,
                &self.function_abis,
                &self.externals,
            ),
            infer_expr_abi(
                else_branch,
                &local_abis,
                &self.function_abis,
                &self.externals,
            ),
        );
        if result_abi == AbiType::Unknown {
            return self
                .static_eval(
                    &LambdaExpr::If {
                        condition: Box::new(condition.clone()),
                        then_branch: Box::new(then_branch.clone()),
                        else_branch: Box::new(else_branch.clone()),
                    },
                    &env.statics,
                )
                .map(CgValue::Static)
                .ok_or_else(|| miette::miette!("unsupported if expression result"));
        }

        let condition = self.emit_bool(condition, env)?;
        let then_block = self.context.append_basic_block(parent, "if.then");
        let else_block = self.context.append_basic_block(parent, "if.else");
        let cont_block = self.context.append_basic_block(parent, "if.cont");
        self.builder
            .build_conditional_branch(condition, then_block, else_block)
            .map_err(|error| miette::miette!("failed to emit if branch: {error}"))?;

        self.builder.position_at_end(then_block);
        let mut then_env = env.clone();
        let then_value = self.emit_expr(then_branch, &mut then_env)?;
        let then_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| miette::miette!("missing then block"))?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|error| miette::miette!("failed to emit then branch: {error}"))?;

        self.builder.position_at_end(else_block);
        let mut else_env = env.clone();
        let else_value = self.emit_expr(else_branch, &mut else_env)?;
        let else_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| miette::miette!("missing else block"))?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|error| miette::miette!("failed to emit else branch: {error}"))?;

        self.builder.position_at_end(cont_block);
        match result_abi {
            AbiType::I64 | AbiType::ActorId | AbiType::Value => {
                let ty = self.context.i64_type();
                let phi = self
                    .builder
                    .build_phi(ty, "iftmp")
                    .map_err(|error| miette::miette!("failed to emit if phi: {error}"))?;
                let then_value = if result_abi == AbiType::Value {
                    self.value_as_runtime(then_value)?
                } else {
                    self.value_as_i64(then_value)?
                };
                let else_value = if result_abi == AbiType::Value {
                    self.value_as_runtime(else_value)?
                } else {
                    self.value_as_i64(else_value)?
                };
                phi.add_incoming(&[(&then_value, then_end), (&else_value, else_end)]);
                match result_abi {
                    AbiType::ActorId => Ok(CgValue::ActorId(phi.as_basic_value().into_int_value())),
                    AbiType::Value => Ok(CgValue::Value(phi.as_basic_value().into_int_value())),
                    _ => Ok(CgValue::I64(phi.as_basic_value().into_int_value())),
                }
            }
            AbiType::Bool => {
                let ty = self.context.bool_type();
                let phi = self
                    .builder
                    .build_phi(ty, "iftmp")
                    .map_err(|error| miette::miette!("failed to emit if phi: {error}"))?;
                let then_value = self.value_as_bool(then_value)?;
                let else_value = self.value_as_bool(else_value)?;
                phi.add_incoming(&[(&then_value, then_end), (&else_value, else_end)]);
                Ok(CgValue::Bool(phi.as_basic_value().into_int_value()))
            }
            AbiType::Unit => Ok(CgValue::Unit),
            AbiType::Unknown => unreachable!(),
        }
    }

    fn emit_match(
        &mut self,
        scrutinee: &LambdaExpr,
        arms: &[LambdaMatchArm],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if arms.is_empty() {
            bail!("cannot lower empty match expression");
        }
        let Some(parent) = self.current_function() else {
            bail!("match expression emitted without an active function");
        };
        let local_abis = env.value_abis();
        let result_abi = arms
            .iter()
            .map(|arm| infer_match_arm_abi(arm, &local_abis, &self.function_abis, &self.externals))
            .fold(AbiType::Unknown, unify_abi);
        if result_abi == AbiType::Unknown {
            bail!("cannot lower match expression with unknown result ABI");
        }

        let scrutinee = self.emit_expr(scrutinee, env)?;
        let scrutinee_root = self.push_optional_runtime_root(&scrutinee)?;
        let cont_block = self.context.append_basic_block(parent, "match.cont");
        let test_blocks = (0..arms.len())
            .map(|index| {
                self.context
                    .append_basic_block(parent, &format!("match.test.{index}"))
            })
            .collect::<Vec<_>>();
        let arm_blocks = (0..arms.len())
            .map(|index| {
                self.context
                    .append_basic_block(parent, &format!("match.arm.{index}"))
            })
            .collect::<Vec<_>>();
        self.builder
            .build_unconditional_branch(test_blocks[0])
            .map_err(|error| miette::miette!("failed to enter match: {error}"))?;

        for (index, arm) in arms.iter().enumerate() {
            self.builder.position_at_end(test_blocks[index]);
            if index + 1 == arms.len() || pattern_is_irrefutable(&arm.pattern) {
                self.builder
                    .build_unconditional_branch(arm_blocks[index])
                    .map_err(|error| miette::miette!("failed to emit match arm: {error}"))?;
            } else {
                let next = test_blocks.get(index + 1).copied().unwrap_or(cont_block);
                let condition = self.emit_pattern_test(&scrutinee, &arm.pattern)?;
                self.builder
                    .build_conditional_branch(condition, arm_blocks[index], next)
                    .map_err(|error| miette::miette!("failed to emit match test: {error}"))?;
            }
        }

        let mut incoming = Vec::new();
        for (index, arm) in arms.iter().enumerate() {
            self.builder.position_at_end(arm_blocks[index]);
            let mut arm_env = env.clone();
            let arm_roots = self.bind_pattern_env(&mut arm_env, &arm.pattern, &scrutinee)?;
            let value = self.emit_expr(&arm.body, &mut arm_env)?;
            let incoming_value = self.match_incoming_value(value, result_abi)?;
            self.pop_runtime_roots(arm_roots)?;
            let end_block = self
                .builder
                .get_insert_block()
                .ok_or_else(|| miette::miette!("missing match arm block"))?;
            self.builder
                .build_unconditional_branch(cont_block)
                .map_err(|error| miette::miette!("failed to leave match arm: {error}"))?;
            if let Some(value) = incoming_value {
                incoming.push((value, end_block));
            }
        }

        self.builder.position_at_end(cont_block);
        let result = self.match_result_value(result_abi, incoming)?;
        self.pop_runtime_roots(scrutinee_root)?;
        Ok(result)
    }

    fn emit_pattern_test(
        &mut self,
        scrutinee: &CgValue<'ctx>,
        pattern: &LambdaPattern,
    ) -> miette::Result<IntValue<'ctx>> {
        let bool_type = self.context.bool_type();
        match pattern {
            LambdaPattern::Wildcard | LambdaPattern::Bind { .. } => {
                Ok(bool_type.const_int(1, false))
            }
            LambdaPattern::Unit => {
                let scrutinee = self.value_as_runtime(scrutinee.clone())?;
                let unit = self.call_runtime_value("riot_rt_value_unit", &[])?;
                Ok(self
                    .builder
                    .build_int_compare(IntPredicate::EQ, scrutinee, unit, "match_unit")
                    .map_err(|error| miette::miette!("failed to emit unit pattern: {error}"))?)
            }
            LambdaPattern::Bool(expected) => {
                let actual = self.value_as_bool(scrutinee.clone())?;
                Ok(self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        actual,
                        bool_type.const_int(u64::from(*expected), false),
                        "match_bool",
                    )
                    .map_err(|error| miette::miette!("failed to emit bool pattern: {error}"))?)
            }
            LambdaPattern::Int(expected) => {
                let actual = self.value_as_i64(scrutinee.clone())?;
                Ok(self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        actual,
                        self.i64_const(*expected),
                        "match_i64",
                    )
                    .map_err(|error| miette::miette!("failed to emit int pattern: {error}"))?)
            }
            LambdaPattern::String(expected) => {
                let scrutinee = self.value_as_runtime(scrutinee.clone())?;
                let (ptr, len) = self.string_literal(expected)?;
                let pattern =
                    self.call_runtime_value("riot_rt_value_string", &[ptr.into(), len.into()])?;
                self.push_runtime_root(pattern)?;
                let result = self
                    .call_runtime_value("riot_rt_value_eq", &[scrutinee.into(), pattern.into()])?;
                self.pop_runtime_roots(1)?;
                Ok(result)
            }
            LambdaPattern::Constructor {
                type_name,
                constructor,
                payload,
            } => {
                let scrutinee = self.value_as_runtime(scrutinee.clone())?;
                let runtime_type_name = runtime_variant_type_name(type_name);
                let (type_ptr, type_len) = self.string_literal(&runtime_type_name)?;
                let (constructor_ptr, constructor_len) =
                    self.string_literal(constructor.as_str())?;
                let tag_matches = self.call_runtime_value(
                    "riot_rt_value_variant_is",
                    &[
                        scrutinee.into(),
                        type_ptr.into(),
                        type_len.into(),
                        constructor_ptr.into(),
                        constructor_len.into(),
                    ],
                )?;
                if payload.is_empty() {
                    return Ok(tag_matches);
                }
                self.emit_constructor_payload_pattern_test(scrutinee, tag_matches, payload)
            }
            LambdaPattern::Tuple(items) => self.emit_tuple_pattern_test(scrutinee, items),
            LambdaPattern::List { prefix, tail } => {
                self.emit_list_pattern_test(scrutinee, prefix, tail.as_deref())
            }
            LambdaPattern::Record { type_name, fields } => {
                self.emit_record_pattern_test(scrutinee, type_name, fields)
            }
        }
    }

    fn emit_record_pattern_test(
        &mut self,
        scrutinee: &CgValue<'ctx>,
        type_name: &TypeName,
        fields: &[(String, LambdaPattern)],
    ) -> miette::Result<IntValue<'ctx>> {
        let bool_type = self.context.bool_type();
        let record = self.value_as_runtime(scrutinee.clone())?;
        let runtime_type_name = runtime_record_type_name(type_name);
        let (path_ptr, path_len) = self.string_literal(&runtime_type_name)?;
        let record_matches = self.call_runtime_value(
            "riot_rt_value_record_is",
            &[record.into(), path_ptr.into(), path_len.into()],
        )?;

        if fields
            .iter()
            .all(|(_, pattern)| pattern_is_irrefutable(pattern))
        {
            return Ok(record_matches);
        }

        let parent = self
            .builder
            .get_insert_block()
            .and_then(|block| block.get_parent())
            .ok_or_else(|| miette::miette!("missing function for record pattern"))?;
        let start_block = self
            .builder
            .get_insert_block()
            .ok_or_else(|| miette::miette!("missing record pattern block"))?;
        let field_block = self
            .context
            .append_basic_block(parent, "match.record.fields");
        let cont_block = self.context.append_basic_block(parent, "match.record.cont");
        self.builder
            .build_conditional_branch(record_matches, field_block, cont_block)
            .map_err(|error| miette::miette!("failed to emit record pattern branch: {error}"))?;

        self.builder.position_at_end(field_block);
        let mut aggregate = bool_type.const_int(1, false);
        for (field, pattern) in fields {
            let (field_ptr, field_len) = self.string_literal(field)?;
            let value = self.call_runtime_value(
                "riot_rt_value_record_get",
                &[record.into(), field_ptr.into(), field_len.into()],
            )?;
            let field_matches = self.emit_pattern_test(&CgValue::Value(value), pattern)?;
            aggregate = self
                .builder
                .build_and(aggregate, field_matches, "match_record_field")
                .map_err(|error| miette::miette!("failed to emit record field test: {error}"))?;
        }
        let field_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| miette::miette!("missing record pattern field block"))?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|error| miette::miette!("failed to leave record pattern test: {error}"))?;

        self.builder.position_at_end(cont_block);
        let phi = self
            .builder
            .build_phi(bool_type, "match_record")
            .map_err(|error| miette::miette!("failed to emit record pattern phi: {error}"))?;
        phi.add_incoming(&[
            (&bool_type.const_int(0, false), start_block),
            (&aggregate, field_end),
        ]);
        Ok(phi.as_basic_value().into_int_value())
    }

    fn emit_constructor_payload_pattern_test(
        &mut self,
        scrutinee: IntValue<'ctx>,
        tag_matches: IntValue<'ctx>,
        payload: &[LambdaPattern],
    ) -> miette::Result<IntValue<'ctx>> {
        let bool_type = self.context.bool_type();
        let parent = self
            .builder
            .get_insert_block()
            .and_then(|block| block.get_parent())
            .ok_or_else(|| miette::miette!("missing function for constructor payload pattern"))?;
        let start_block = self
            .builder
            .get_insert_block()
            .ok_or_else(|| miette::miette!("missing constructor payload pattern block"))?;
        let payload_block = self
            .context
            .append_basic_block(parent, "match.variant.payload");
        let cont_block = self
            .context
            .append_basic_block(parent, "match.variant.cont");
        self.builder
            .build_conditional_branch(tag_matches, payload_block, cont_block)
            .map_err(|error| {
                miette::miette!("failed to emit constructor payload branch: {error}")
            })?;

        self.builder.position_at_end(payload_block);
        let payload_value =
            self.call_runtime_value("riot_rt_value_variant_get_payload", &[scrutinee.into()])?;
        self.push_runtime_root(payload_value)?;
        let payload_matches = match payload {
            [] => bool_type.const_int(1, false),
            [pattern] => self.emit_pattern_test(&CgValue::Value(payload_value), pattern)?,
            patterns => {
                let mut aggregate = bool_type.const_int(1, false);
                for (index, pattern) in patterns.iter().enumerate() {
                    let item = self.call_runtime_value(
                        "riot_rt_value_tuple_get",
                        &[payload_value.into(), self.i64_const(index as i64).into()],
                    )?;
                    let item_matches = self.emit_pattern_test(&CgValue::Value(item), pattern)?;
                    aggregate = self
                        .builder
                        .build_and(aggregate, item_matches, "match_variant_payload")
                        .map_err(|error| {
                            miette::miette!("failed to combine constructor payload tests: {error}")
                        })?;
                }
                aggregate
            }
        };
        self.pop_runtime_roots(1)?;
        let payload_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| miette::miette!("missing constructor payload end block"))?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|error| {
                miette::miette!("failed to leave constructor payload test: {error}")
            })?;

        self.builder.position_at_end(cont_block);
        let phi = self
            .builder
            .build_phi(bool_type, "match_variant")
            .map_err(|error| miette::miette!("failed to emit constructor payload phi: {error}"))?;
        phi.add_incoming(&[
            (&bool_type.const_int(0, false), start_block),
            (&payload_matches, payload_end),
        ]);
        Ok(phi.as_basic_value().into_int_value())
    }

    fn emit_tuple_pattern_test(
        &mut self,
        scrutinee: &CgValue<'ctx>,
        items: &[LambdaPattern],
    ) -> miette::Result<IntValue<'ctx>> {
        let bool_type = self.context.bool_type();
        let tuple = self.value_as_runtime(scrutinee.clone())?;
        let arity_matches = self.call_runtime_value(
            "riot_rt_value_tuple_arity_is",
            &[
                tuple.into(),
                self.context
                    .i64_type()
                    .const_int(items.len() as u64, false)
                    .into(),
            ],
        )?;

        if items.iter().all(pattern_is_irrefutable) {
            return Ok(arity_matches);
        }

        let parent = self
            .builder
            .get_insert_block()
            .and_then(|block| block.get_parent())
            .ok_or_else(|| miette::miette!("missing function for tuple pattern"))?;
        let start_block = self
            .builder
            .get_insert_block()
            .ok_or_else(|| miette::miette!("missing tuple pattern block"))?;
        let item_block = self.context.append_basic_block(parent, "match.tuple.items");
        let cont_block = self.context.append_basic_block(parent, "match.tuple.cont");
        self.builder
            .build_conditional_branch(arity_matches, item_block, cont_block)
            .map_err(|error| miette::miette!("failed to emit tuple pattern branch: {error}"))?;

        self.builder.position_at_end(item_block);
        let mut aggregate = bool_type.const_int(1, false);
        for (index, pattern) in items.iter().enumerate() {
            let item = self.call_runtime_value(
                "riot_rt_value_tuple_get",
                &[tuple.into(), self.i64_const(index as i64).into()],
            )?;
            let item_matches = self.emit_pattern_test(&CgValue::Value(item), pattern)?;
            aggregate = self
                .builder
                .build_and(aggregate, item_matches, "match_tuple_item")
                .map_err(|error| miette::miette!("failed to emit tuple item test: {error}"))?;
        }
        let item_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| miette::miette!("missing tuple pattern item block"))?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|error| miette::miette!("failed to leave tuple pattern test: {error}"))?;

        self.builder.position_at_end(cont_block);
        let phi = self
            .builder
            .build_phi(bool_type, "match_tuple")
            .map_err(|error| miette::miette!("failed to emit tuple pattern phi: {error}"))?;
        phi.add_incoming(&[
            (&bool_type.const_int(0, false), start_block),
            (&aggregate, item_end),
        ]);
        Ok(phi.as_basic_value().into_int_value())
    }

    fn emit_list_pattern_test(
        &mut self,
        scrutinee: &CgValue<'ctx>,
        prefix: &[LambdaPattern],
        tail: Option<&LambdaPattern>,
    ) -> miette::Result<IntValue<'ctx>> {
        let bool_type = self.context.bool_type();
        let list = self.value_as_runtime(scrutinee.clone())?;
        let len = self
            .call_runtime_basic("riot_rt_value_list_len", &[list.into()])?
            .into_int_value();
        let expected_len = self.i64_const(prefix.len() as i64);
        let len_matches = if tail.is_some() {
            self.builder.build_int_compare(
                IntPredicate::UGE,
                len,
                expected_len,
                "match_list_min_len",
            )
        } else {
            self.builder
                .build_int_compare(IntPredicate::EQ, len, expected_len, "match_list_len")
        }
        .map_err(|error| miette::miette!("failed to emit list pattern length test: {error}"))?;

        if prefix.is_empty() && tail.is_none_or(pattern_is_irrefutable) {
            return Ok(len_matches);
        }

        let parent = self
            .builder
            .get_insert_block()
            .and_then(|block| block.get_parent())
            .ok_or_else(|| miette::miette!("missing function for list pattern"))?;
        let start_block = self
            .builder
            .get_insert_block()
            .ok_or_else(|| miette::miette!("missing list pattern block"))?;
        let item_block = self.context.append_basic_block(parent, "match.list.items");
        let cont_block = self.context.append_basic_block(parent, "match.list.cont");
        self.builder
            .build_conditional_branch(len_matches, item_block, cont_block)
            .map_err(|error| miette::miette!("failed to emit list pattern branch: {error}"))?;

        self.builder.position_at_end(item_block);
        let mut aggregate = bool_type.const_int(1, false);
        for (index, pattern) in prefix.iter().enumerate() {
            let item = self.call_runtime_value(
                "riot_rt_value_list_get",
                &[list.into(), self.i64_const(index as i64).into()],
            )?;
            let item_matches = self.emit_pattern_test(&CgValue::Value(item), pattern)?;
            aggregate = self
                .builder
                .build_and(aggregate, item_matches, "match_list_item")
                .map_err(|error| miette::miette!("failed to emit list item test: {error}"))?;
        }
        if let Some(tail) = tail
            && !pattern_is_irrefutable(tail)
        {
            let tail_value = self.call_runtime_value(
                "riot_rt_value_list_drop",
                &[list.into(), self.i64_const(prefix.len() as i64).into()],
            )?;
            self.push_runtime_root(tail_value)?;
            let tail_matches = self.emit_pattern_test(&CgValue::Value(tail_value), tail)?;
            self.pop_runtime_roots(1)?;
            aggregate = self
                .builder
                .build_and(aggregate, tail_matches, "match_list_tail")
                .map_err(|error| miette::miette!("failed to emit list tail test: {error}"))?;
        }
        let item_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| miette::miette!("missing list pattern item block"))?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|error| miette::miette!("failed to leave list pattern test: {error}"))?;

        self.builder.position_at_end(cont_block);
        let phi = self
            .builder
            .build_phi(bool_type, "match_list")
            .map_err(|error| miette::miette!("failed to emit list pattern phi: {error}"))?;
        phi.add_incoming(&[
            (&bool_type.const_int(0, false), start_block),
            (&aggregate, item_end),
        ]);
        Ok(phi.as_basic_value().into_int_value())
    }

    fn bind_pattern_env(
        &mut self,
        env: &mut Env<'ctx>,
        pattern: &LambdaPattern,
        scrutinee: &CgValue<'ctx>,
    ) -> miette::Result<usize> {
        match pattern {
            LambdaPattern::Bind { binding, type_ } => {
                let value = match scrutinee {
                    CgValue::Value(value) => self.runtime_value_as_type(*value, type_)?,
                    _ => scrutinee.clone(),
                };
                let roots = if let Some(root) = value.runtime_root() {
                    self.push_runtime_root(root)?;
                    1
                } else {
                    0
                };
                env.values.insert(binding.as_str().to_owned(), value);
                Ok(roots)
            }
            LambdaPattern::Constructor { payload, .. } if !payload.is_empty() => {
                let scrutinee = self.value_as_runtime(scrutinee.clone())?;
                let payload_value = self
                    .call_runtime_value("riot_rt_value_variant_get_payload", &[scrutinee.into()])?;
                self.bind_payload_patterns(env, payload, payload_value)
            }
            LambdaPattern::Tuple(items) => {
                let tuple = self.value_as_runtime(scrutinee.clone())?;
                self.bind_payload_patterns(env, items, tuple)
            }
            LambdaPattern::List { prefix, tail } => {
                self.bind_list_patterns(env, prefix, tail.as_deref(), scrutinee)
            }
            LambdaPattern::Record { fields, .. } => {
                let record = self.value_as_runtime(scrutinee.clone())?;
                self.bind_record_patterns(env, fields, record)
            }
            LambdaPattern::Wildcard
            | LambdaPattern::Constructor { .. }
            | LambdaPattern::Unit
            | LambdaPattern::Bool(_)
            | LambdaPattern::Int(_)
            | LambdaPattern::String(_) => Ok(0),
        }
    }

    fn bind_record_patterns(
        &mut self,
        env: &mut Env<'ctx>,
        fields: &[(String, LambdaPattern)],
        record: IntValue<'ctx>,
    ) -> miette::Result<usize> {
        let mut roots = 0;
        for (field, pattern) in fields {
            let (field_ptr, field_len) = self.string_literal(field)?;
            let value = self.call_runtime_value(
                "riot_rt_value_record_get",
                &[record.into(), field_ptr.into(), field_len.into()],
            )?;
            roots += self.bind_pattern_env(env, pattern, &CgValue::Value(value))?;
        }
        Ok(roots)
    }

    fn bind_payload_patterns(
        &mut self,
        env: &mut Env<'ctx>,
        patterns: &[LambdaPattern],
        payload: IntValue<'ctx>,
    ) -> miette::Result<usize> {
        match patterns {
            [] => Ok(0),
            [pattern] => self.bind_pattern_env(env, pattern, &CgValue::Value(payload)),
            patterns => {
                let mut roots = 0;
                for (index, pattern) in patterns.iter().enumerate() {
                    let item = self.call_runtime_value(
                        "riot_rt_value_tuple_get",
                        &[
                            payload.into(),
                            self.context
                                .i64_type()
                                .const_int(index as u64, false)
                                .into(),
                        ],
                    )?;
                    roots += self.bind_pattern_env(env, pattern, &CgValue::Value(item))?;
                }
                Ok(roots)
            }
        }
    }

    fn bind_list_patterns(
        &mut self,
        env: &mut Env<'ctx>,
        prefix: &[LambdaPattern],
        tail: Option<&LambdaPattern>,
        scrutinee: &CgValue<'ctx>,
    ) -> miette::Result<usize> {
        let list = self.value_as_runtime(scrutinee.clone())?;
        let mut roots = 0;
        for (index, pattern) in prefix.iter().enumerate() {
            let item = self.call_runtime_value(
                "riot_rt_value_list_get",
                &[list.into(), self.i64_const(index as i64).into()],
            )?;
            roots += self.bind_pattern_env(env, pattern, &CgValue::Value(item))?;
        }
        if let Some(tail) = tail {
            let tail_value = self.call_runtime_value(
                "riot_rt_value_list_drop",
                &[list.into(), self.i64_const(prefix.len() as i64).into()],
            )?;
            roots += self.bind_pattern_env(env, tail, &CgValue::Value(tail_value))?;
        }
        Ok(roots)
    }

    fn match_incoming_value(
        &mut self,
        value: CgValue<'ctx>,
        abi: AbiType,
    ) -> miette::Result<Option<BasicValueEnum<'ctx>>> {
        Ok(match abi {
            AbiType::Unit => None,
            AbiType::I64 | AbiType::ActorId => Some(self.value_as_i64(value)?.into()),
            AbiType::Bool => Some(self.value_as_bool(value)?.into()),
            AbiType::Value => Some(self.value_as_runtime(value)?.into()),
            AbiType::Unknown => bail!("cannot lower match value with unknown ABI"),
        })
    }

    fn match_result_value(
        &self,
        abi: AbiType,
        incoming: Vec<(BasicValueEnum<'ctx>, BasicBlock<'ctx>)>,
    ) -> miette::Result<CgValue<'ctx>> {
        match abi {
            AbiType::Unit => Ok(CgValue::Unit),
            AbiType::I64 | AbiType::ActorId | AbiType::Bool | AbiType::Value => {
                let ty = self.basic_type_for_abi(abi)?;
                let phi = self
                    .builder
                    .build_phi(ty, "matchtmp")
                    .map_err(|error| miette::miette!("failed to emit match phi: {error}"))?;
                let incoming_refs = incoming
                    .iter()
                    .map(|(value, block)| (value as &dyn inkwell::values::BasicValue<'ctx>, *block))
                    .collect::<Vec<_>>();
                phi.add_incoming(&incoming_refs);
                let value = phi.as_basic_value();
                match abi {
                    AbiType::I64 => Ok(CgValue::I64(value.into_int_value())),
                    AbiType::ActorId => Ok(CgValue::ActorId(value.into_int_value())),
                    AbiType::Bool => Ok(CgValue::Bool(value.into_int_value())),
                    AbiType::Value => Ok(CgValue::Value(value.into_int_value())),
                    AbiType::Unit | AbiType::Unknown => unreachable!(),
                }
            }
            AbiType::Unknown => bail!("cannot lower match with unknown ABI"),
        }
    }

    fn emit_static_value(&mut self, value: StaticValue) -> miette::Result<CgValue<'ctx>> {
        match value {
            StaticValue::Int(value) => Ok(CgValue::Value(
                self.call_runtime_value("riot_rt_value_i64", &[self.i64_const(value).into()])?,
            )),
            StaticValue::Bool(value) => Ok(CgValue::Value(
                self.call_runtime_value(
                    "riot_rt_value_bool",
                    &[self
                        .context
                        .bool_type()
                        .const_int(u64::from(value), false)
                        .into()],
                )?,
            )),
            StaticValue::Unit => Ok(CgValue::Value(
                self.call_runtime_value("riot_rt_value_unit", &[])?,
            )),
            StaticValue::String(value) | StaticValue::Float(value) => {
                let (ptr, len) = self.string_literal(&value)?;
                Ok(CgValue::Value(self.call_runtime_value(
                    "riot_rt_value_string",
                    &[ptr.into(), len.into()],
                )?))
            }
            StaticValue::Char(value) => {
                let rendered = value.to_string();
                let (ptr, len) = self.string_literal(&rendered)?;
                Ok(CgValue::Value(self.call_runtime_value(
                    "riot_rt_value_string",
                    &[ptr.into(), len.into()],
                )?))
            }
            StaticValue::Tuple(items) => self.emit_static_sequence("riot_rt_value_tuple", items),
            StaticValue::List(items) => self.emit_static_sequence("riot_rt_value_list", items),
            StaticValue::Record { path, fields } => self.emit_static_record(&path, fields),
            StaticValue::Variant {
                type_name,
                constructor,
                payload,
            } => self.emit_static_variant(&type_name, &constructor, *payload),
        }
    }

    fn emit_static_sequence(
        &mut self,
        symbol: &str,
        items: Vec<StaticValue>,
    ) -> miette::Result<CgValue<'ctx>> {
        let mut values = Vec::with_capacity(items.len());
        let mut rooted_values = 0;
        for item in items {
            let value = self.emit_static_value(item)?;
            let value = self.value_as_runtime(value)?;
            self.push_runtime_root(value)?;
            rooted_values += 1;
            values.push(value);
        }
        let result = self.build_value_array_call(symbol, &values)?;
        self.pop_runtime_roots(rooted_values)?;
        Ok(CgValue::Value(result))
    }

    fn emit_static_record(
        &mut self,
        path: &str,
        fields: Vec<(String, StaticValue)>,
    ) -> miette::Result<CgValue<'ctx>> {
        let (path_ptr, path_len) = self.string_literal(path)?;
        let record = self.call_runtime_value(
            "riot_rt_value_record_begin",
            &[
                path_ptr.into(),
                path_len.into(),
                self.context
                    .i64_type()
                    .const_int(fields.len() as u64, false)
                    .into(),
            ],
        )?;
        self.push_runtime_root(record)?;
        for (index, (name, value)) in fields.into_iter().enumerate() {
            let value = self.emit_static_value(value)?;
            let value = self.value_as_runtime(value)?;
            self.push_runtime_root(value)?;
            let (name_ptr, name_len) = self.string_literal(&name)?;
            self.call_void_runtime(
                "riot_rt_value_record_set",
                &[
                    record.into(),
                    self.context
                        .i64_type()
                        .const_int(index as u64, false)
                        .into(),
                    name_ptr.into(),
                    name_len.into(),
                    value.into(),
                ],
            )?;
            self.pop_runtime_roots(1)?;
        }
        self.pop_runtime_roots(1)?;
        Ok(CgValue::Value(record))
    }

    fn emit_static_variant(
        &mut self,
        type_name: &TypeName,
        constructor: &ConstructorName,
        payload: StaticValue,
    ) -> miette::Result<CgValue<'ctx>> {
        let runtime_type_name = runtime_variant_type_name(type_name);
        let (type_ptr, type_len) = self.string_literal(&runtime_type_name)?;
        let (constructor_ptr, constructor_len) = self.string_literal(constructor.as_str())?;
        if matches!(payload, StaticValue::Unit) {
            return Ok(CgValue::Value(self.call_runtime_value(
                "riot_rt_value_variant",
                &[
                    type_ptr.into(),
                    type_len.into(),
                    constructor_ptr.into(),
                    constructor_len.into(),
                ],
            )?));
        }

        let payload = self.emit_static_value(payload)?;
        let payload = self.value_as_runtime(payload)?;
        self.push_runtime_root(payload)?;
        let variant = self.call_runtime_value(
            "riot_rt_value_variant_payload",
            &[
                type_ptr.into(),
                type_len.into(),
                constructor_ptr.into(),
                constructor_len.into(),
                payload.into(),
            ],
        )?;
        self.pop_runtime_roots(1)?;
        Ok(CgValue::Value(variant))
    }

    fn emit_value_sequence(
        &mut self,
        symbol: &str,
        items: &[LambdaExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let mut values = Vec::with_capacity(items.len());
        let mut rooted_values = 0;
        for item in items {
            let value = self.emit_expr(item, env)?;
            let value = self.value_as_runtime(value)?;
            self.push_runtime_root(value)?;
            rooted_values += 1;
            values.push(value);
        }
        let result = self.build_value_array_call(symbol, &values)?;
        self.pop_runtime_roots(rooted_values)?;
        Ok(CgValue::Value(result))
    }

    fn emit_lambda_value(
        &mut self,
        params: &[Param],
        captures: &[Capture],
        body: &LambdaBlock,
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let apply = self.declare_lambda_apply_function(params, captures, body)?;
        let mut values = Vec::with_capacity(captures.len());
        let mut rooted_values = 0;
        for capture in captures {
            let Some(value) = env.values.get(capture.as_str()).cloned() else {
                bail!(
                    "lambda capture `{}` is not available in the current environment",
                    capture.as_str()
                );
            };
            let value = self.value_as_runtime(value)?;
            self.push_runtime_root(value)?;
            rooted_values += 1;
            values.push(value);
        }
        let captures_ptr = self.build_value_array_ptr(&values)?;
        let len = self
            .context
            .i64_type()
            .const_int(values.len() as u64, false);
        let closure = self.call_runtime_value(
            "riot_rt_value_closure",
            &[apply.into(), captures_ptr.into(), len.into()],
        )?;
        self.pop_runtime_roots(rooted_values)?;
        Ok(CgValue::Value(closure))
    }

    fn emit_apply(
        &mut self,
        callee: &LambdaExpr,
        args: &[LambdaExpr],
        result_type: &RsigType,
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let [arg] = args else {
            bail!("stage0 closure apply expects exactly one argument after currying")
        };
        let closure = self.emit_expr(callee, env)?;
        let closure = self.value_as_runtime(closure)?;
        self.push_runtime_root(closure)?;
        let arg = self.emit_expr(arg, env)?;
        let arg = self.value_as_runtime(arg)?;
        self.push_runtime_root(arg)?;
        let result =
            self.call_runtime_value("riot_rt_value_apply", &[closure.into(), arg.into()])?;
        self.pop_runtime_roots(2)?;
        self.runtime_value_as_type(result, result_type)
    }

    fn build_value_array_call(
        &mut self,
        symbol: &str,
        values: &[IntValue<'ctx>],
    ) -> miette::Result<IntValue<'ctx>> {
        let i64_type = self.context.i64_type();
        let len = i64_type.const_int(values.len() as u64, false);
        let ptr = self.build_value_array_ptr(values)?;
        self.call_runtime_value(symbol, &[ptr.into(), len.into()])
    }

    fn build_value_array_ptr(
        &mut self,
        values: &[IntValue<'ctx>],
    ) -> miette::Result<PointerValue<'ctx>> {
        let i64_type = self.context.i64_type();
        let len = i64_type.const_int(values.len() as u64, false);
        let ptr = if values.is_empty() {
            self.ptr_type().const_null()
        } else {
            let ptr = self
                .builder
                .build_array_alloca(i64_type, len, "rt_values")
                .map_err(|error| miette::miette!("failed to allocate value array: {error}"))?;
            for (index, value) in values.iter().enumerate() {
                let element = unsafe {
                    self.builder
                        .build_in_bounds_gep(
                            i64_type,
                            ptr,
                            &[i64_type.const_int(index as u64, false)],
                            "rt_value_slot",
                        )
                        .map_err(|error| {
                            miette::miette!("failed to address value array slot: {error}")
                        })?
                };
                self.builder.build_store(element, *value).map_err(|error| {
                    miette::miette!("failed to store value array slot: {error}")
                })?;
            }
            ptr
        };
        Ok(ptr)
    }

    fn declare_lambda_apply_function(
        &mut self,
        params: &[Param],
        captures: &[Capture],
        body: &LambdaBlock,
    ) -> miette::Result<PointerValue<'ctx>> {
        let index = self.string_counter;
        self.string_counter += 1;
        let name = format!("riot_lambda_apply_{index}");
        let i64_type = self.context.i64_type();
        let apply_type = self.context.i64_type().fn_type(
            &[self.ptr_type().into(), i64_type.into(), i64_type.into()],
            false,
        );
        let function = self.module.add_function(&name, apply_type, None);
        let current_block = self.builder.get_insert_block();
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        let captures_ptr = function
            .get_nth_param(0)
            .ok_or_else(|| miette::miette!("missing closure captures parameter"))?
            .into_pointer_value();
        let argument = function
            .get_nth_param(2)
            .ok_or_else(|| miette::miette!("missing closure argument parameter"))?
            .into_int_value();
        let mut env = Env::default();
        for (index, capture) in captures.iter().enumerate() {
            let slot = unsafe {
                self.builder
                    .build_in_bounds_gep(
                        i64_type,
                        captures_ptr,
                        &[i64_type.const_int(index as u64, false)],
                        "closure_capture",
                    )
                    .map_err(|error| {
                        miette::miette!("failed to address closure capture: {error}")
                    })?
            };
            let value = self
                .builder
                .build_load(i64_type, slot, capture.as_str())
                .map_err(|error| miette::miette!("failed to load closure capture: {error}"))?
                .into_int_value();
            env.values
                .insert(capture.as_str().to_owned(), CgValue::Value(value));
        }

        let result = if let Some((param, rest)) = params.split_first() {
            env.values
                .insert(param.as_str().to_owned(), CgValue::Value(argument));
            if rest.is_empty() {
                self.emit_block(body, &mut env)?
            } else {
                let mut nested_captures = captures.to_vec();
                nested_captures.push(Capture::from_key(param.key().clone()));
                let nested = LambdaExpr::Lambda {
                    params: rest.to_vec(),
                    captures: nested_captures,
                    body: Box::new(body.clone()),
                };
                self.emit_expr(&nested, &mut env)?
            }
        } else {
            self.emit_block(body, &mut env)?
        };
        let result = self.value_as_runtime(result)?;
        self.builder
            .build_return(Some(&result))
            .map_err(|error| miette::miette!("failed to emit lambda apply return: {error}"))?;
        if let Some(block) = current_block {
            self.builder.position_at_end(block);
        }
        Ok(function.as_global_value().as_pointer_value())
    }

    fn emit_record_value(
        &mut self,
        path: &[String],
        fields: &[(String, LambdaExpr)],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let path = runtime_record_path_name(path);
        let (path_ptr, path_len) = self.string_literal(&path)?;
        let record = self.call_runtime_value(
            "riot_rt_value_record_begin",
            &[
                path_ptr.into(),
                path_len.into(),
                self.context
                    .i64_type()
                    .const_int(fields.len() as u64, false)
                    .into(),
            ],
        )?;
        self.push_runtime_root(record)?;
        for (index, (name, expr)) in fields.iter().enumerate() {
            let value = self.emit_expr(expr, env)?;
            let value = self.value_as_runtime(value)?;
            self.push_runtime_root(value)?;
            let (name_ptr, name_len) = self.string_literal(name)?;
            self.call_void_runtime(
                "riot_rt_value_record_set",
                &[
                    record.into(),
                    self.context
                        .i64_type()
                        .const_int(index as u64, false)
                        .into(),
                    name_ptr.into(),
                    name_len.into(),
                    value.into(),
                ],
            )?;
            self.pop_runtime_roots(1)?;
        }
        self.pop_runtime_roots(1)?;
        Ok(CgValue::Value(record))
    }

    fn emit_path(&self, path: &[String], env: &Env<'ctx>) -> miette::Result<CgValue<'ctx>> {
        let Some((head, tail)) = path.split_first() else {
            bail!("empty path");
        };
        if tail.is_empty()
            && let Some(value) = env.values.get(head)
        {
            return Ok(value.clone());
        }
        if let Some(value) = StaticEvaluator::new(&self.function_map, &self.externals)
            .resolve_path(path, &env.statics)
        {
            return Ok(CgValue::Static(value));
        }
        bail!("unknown value `{}`", path.join("."))
    }

    fn emit_call(
        &mut self,
        callee: &[String],
        args: &[LambdaExpr],
        result: &RsigType,
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if let Some(name) = Stdlib::prelude_member_name(callee) {
            if let Some(static_value) = self.static_eval_call(name, args, &env.statics) {
                return Ok(CgValue::Static(static_value));
            }
            if let Some(external) = self.externals.get(name).cloned() {
                let value = self.emit_external_call(
                    &external.abi,
                    &external.params,
                    &external.result,
                    args,
                    env,
                )?;
                return self.coerce_call_result(value, result);
            }
        }
        match callee {
            [name] if self.functions.contains_key(name) => self.emit_local_call(name, args, env),
            [name] => {
                if let Some(static_value) = self.static_eval_call(name, args, &env.statics) {
                    Ok(CgValue::Static(static_value))
                } else {
                    bail!("cannot lower call to `{name}` with the currently inferred ABI")
                }
            }
            [module, name] => self.emit_imported_call(module, name, args, result, env),
            _ => bail!("unsupported call path `{}`", callee.join(".")),
        }
    }

    fn coerce_call_result(
        &self,
        value: CgValue<'ctx>,
        result: &RsigType,
    ) -> miette::Result<CgValue<'ctx>> {
        let target = AbiType::from_rsig(result);
        if target == AbiType::Unknown || value.abi() == target {
            return Ok(value);
        }
        match value {
            CgValue::Value(value) => self.runtime_value_as_type(value, result),
            value => Ok(value),
        }
    }

    fn emit_local_call(
        &mut self,
        name: &str,
        args: &[LambdaExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let function = self
            .functions
            .get(name)
            .ok_or_else(|| miette::miette!("missing LLVM function for `{name}`"))?;
        let abi = self
            .function_abis
            .get(name)
            .cloned()
            .ok_or_else(|| miette::miette!("missing ABI for function `{name}`"))?;
        if abi.params.len() != args.len() {
            bail!("function `{name}` called with wrong arity");
        }
        let mut call_args = Vec::new();
        let mut rooted_values = 0;
        for (arg, param_abi) in args.iter().zip(&abi.params) {
            let (arg, roots) = self.emit_basic_arg(arg, *param_abi, env)?;
            rooted_values += roots;
            call_args.push(arg);
        }
        let call = self
            .builder
            .build_call(function, &call_args, &format!("call_{name}"))
            .map_err(|error| miette::miette!("failed to emit call to `{name}`: {error}"))?;
        let result = self.call_result(call.try_as_basic_value().basic(), abi.result);
        self.pop_runtime_roots(rooted_values)?;
        result
    }

    fn emit_imported_call(
        &mut self,
        module: &str,
        name: &str,
        args: &[LambdaExpr],
        checked_result: &RsigType,
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let export = self
            .imports
            .get(module)
            .and_then(|rsig| rsig.find(name))
            .ok_or_else(|| miette::miette!("module `{module}` does not export `{name}`"))?
            .clone();
        match export {
            RsigExport::Function(function) => self.emit_declared_function_call(
                &function.symbol,
                &function.params,
                &function.result,
                args,
                env,
            ),
            RsigExport::External(external) => {
                let value = self.emit_external_call(
                    &external.abi,
                    &external.params,
                    &external.result,
                    args,
                    env,
                )?;
                self.coerce_call_result(value, checked_result)
            }
        }
    }

    fn emit_declared_function_call(
        &mut self,
        symbol: &str,
        params: &[RsigType],
        result: &RsigType,
        args: &[LambdaExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if params.len() != args.len() {
            bail!("imported function `{symbol}` called with wrong arity");
        }
        let abi = FunctionAbi {
            params: params.iter().map(AbiType::from_rsig).collect(),
            result: AbiType::from_rsig(result),
        };
        if !abi.is_supported_local() {
            bail!("cannot lower imported call `{symbol}` because its .rsig ABI is not concrete");
        }
        let function = self.get_or_add_function(symbol, self.local_function_type(&abi)?);
        let mut call_args = Vec::new();
        let mut rooted_values = 0;
        for (arg, param_abi) in args.iter().zip(&abi.params) {
            let (arg, roots) = self.emit_basic_arg(arg, *param_abi, env)?;
            rooted_values += roots;
            call_args.push(arg);
        }
        let call = self
            .builder
            .build_call(function, &call_args, &format!("call_{symbol}"))
            .map_err(|error| miette::miette!("failed to emit imported call `{symbol}`: {error}"))?;
        let result = self.call_result(call.try_as_basic_value().basic(), abi.result);
        self.pop_runtime_roots(rooted_values)?;
        result
    }

    fn emit_external_call(
        &mut self,
        symbol: &str,
        params: &[RsigType],
        result: &RsigType,
        args: &[LambdaExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if params.len() != args.len() {
            bail!("external `{symbol}` called with wrong arity");
        }
        let function =
            self.get_or_add_function(symbol, self.external_function_type(symbol, params, result)?);
        let mut call_args = Vec::new();
        let mut rooted_values = 0;
        for (arg, param) in args.iter().zip(params) {
            rooted_values += self.push_external_arg(symbol, &mut call_args, arg, param, env)?;
        }
        let call = self
            .builder
            .build_call(function, &call_args, &format!("call_{symbol}"))
            .map_err(|error| miette::miette!("failed to emit external call `{symbol}`: {error}"))?;
        let result = self.call_result(
            call.try_as_basic_value().basic(),
            ExternalAbi::new(symbol).result_abi(result),
        );
        self.pop_runtime_roots(rooted_values)?;
        result
    }

    fn emit_spawn(
        &mut self,
        actor_id: usize,
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let restore_block = self.builder.get_insert_block();
        let actor = self.define_actor(actor_id)?;
        if let Some(block) = restore_block {
            self.builder.position_at_end(block);
        }

        let ptr_type = self.ptr_type();
        let i64_type = self.context.i64_type();
        let alloc_fn = self.get_or_add_function(
            "riot_rt_alloc_frame_v2",
            ptr_type.fn_type(&[i64_type.into(), i64_type.into(), ptr_type.into()], false),
        );
        let size = i64_type.const_int(actor.size_bytes as u64, false);
        let align = i64_type.const_int(actor.align as u64, false);
        let drop_fn = ptr_type.const_null();
        let frame = self
            .builder
            .build_call(
                alloc_fn,
                &[size.into(), align.into(), drop_fn.into()],
                "actor_frame",
            )
            .map_err(|error| miette::miette!("failed to emit actor frame allocation: {error}"))?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| miette::miette!("actor frame allocation returned void"))?
            .into_pointer_value();

        self.store_frame_i64(
            actor.frame_type,
            frame,
            0,
            self.context.i64_type().const_zero(),
        )?;
        for capture in &actor.captures {
            let value = env
                .values
                .get(&capture.name)
                .cloned()
                .ok_or_else(|| miette::miette!("missing actor capture `{}`", capture.name))?;
            self.store_frame_value(actor.frame_type, frame, capture, value)?;
        }

        let spawn_fn = self.get_or_add_function(
            "riot_rt_spawn_actor_v2",
            i64_type.fn_type(
                &[
                    ptr_type.into(),
                    ptr_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                    ptr_type.into(),
                ],
                false,
            ),
        );
        let resume_ptr = actor.resume.as_global_value().as_pointer_value();
        let actor_id = self
            .builder
            .build_call(
                spawn_fn,
                &[
                    frame.into(),
                    resume_ptr.into(),
                    size.into(),
                    align.into(),
                    drop_fn.into(),
                ],
                "actor_id",
            )
            .map_err(|error| miette::miette!("failed to emit actor spawn: {error}"))?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| miette::miette!("actor spawn returned void"))?
            .into_int_value();
        self.emit_actor_frame_roots(actor_id, &actor.root_offsets)?;
        Ok(CgValue::ActorId(actor_id))
    }

    fn define_actor(&mut self, actor_id: usize) -> miette::Result<ActorDef<'ctx>> {
        let actor_ir = self
            .actor_ir
            .actors
            .iter()
            .find(|actor| actor.id == actor_id)
            .cloned()
            .ok_or_else(|| miette::miette!("missing Actor IR for actor #{actor_id}"))?;
        let shape = actor_shape_from_ir(&actor_ir);
        let frame_type = self.actor_frame_type(shape.slots.len());
        let ptr_type = self.ptr_type();
        let i64_type = self.context.i64_type();
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[ptr_type.into(), i64_type.into(), ptr_type.into()], false);
        let name = format!(
            "riot_actor_{}_{}_resume",
            self.program.module_name, actor_id
        );
        let root_offsets = actor_frame_root_offsets(&shape);
        if let Some(resume) = self.module.get_function(&name) {
            return Ok(ActorDef {
                resume,
                frame_type,
                captures: shape.captures,
                size_bytes: shape.size_bytes,
                align: shape.align,
                root_offsets,
            });
        }
        let resume = self.module.add_function(&name, fn_type, None);

        let restore_block = self.builder.get_insert_block();
        let frame = resume.get_first_param().unwrap().into_pointer_value();
        let message = resume.get_nth_param(2).unwrap().into_pointer_value();
        let entry = self.context.append_basic_block(resume, "entry");
        let done = self.context.append_basic_block(resume, "done");
        let op_blocks = (0..shape.states.len())
            .map(|index| {
                self.context
                    .append_basic_block(resume, &format!("state_{index}"))
            })
            .collect::<Vec<_>>();

        self.builder.position_at_end(entry);
        let state = self.load_frame_i64(frame_type, frame, 0, "state")?;
        let cases = op_blocks
            .iter()
            .enumerate()
            .map(|(index, block)| (i64_type.const_int(index as u64, false), *block))
            .collect::<Vec<_>>();
        self.builder
            .build_switch(state, done, &cases)
            .map_err(|error| miette::miette!("failed to emit actor dispatch: {error}"))?;

        self.builder.position_at_end(done);
        self.return_poll(POLL_DONE)?;

        for (state, block) in shape.states.iter().zip(&op_blocks) {
            self.builder.position_at_end(*block);
            let mut env = self.load_actor_env(frame_type, frame, &shape)?;
            let next_state = match state.next {
                ActorStateNext::State(index) => index,
                ActorStateNext::Done => shape.states.len(),
            };
            match &state.op {
                ActorFrameOp::Let { name, value } => {
                    let value = self.emit_expr(value, &mut env)?;
                    let slot = shape
                        .slots
                        .iter()
                        .find(|slot| slot.name == name.as_str())
                        .ok_or_else(|| miette::miette!("missing actor slot `{}`", name.as_str()))?;
                    self.store_frame_value(frame_type, frame, slot, value)?;
                    self.store_frame_i64(
                        frame_type,
                        frame,
                        0,
                        i64_type.const_int(next_state as u64, false),
                    )?;
                    self.return_poll(progress_flags(next_state, shape.states.len()))?;
                }
                ActorFrameOp::Expr(expr) => {
                    self.emit_expr(expr, &mut env)?;
                    self.store_frame_i64(
                        frame_type,
                        frame,
                        0,
                        i64_type.const_int(next_state as u64, false),
                    )?;
                    self.return_poll(progress_flags(next_state, shape.states.len()))?;
                }
                ActorFrameOp::Receive { arms } => {
                    let wait = self.context.append_basic_block(resume, "receive.wait");
                    let consume = self.context.append_basic_block(resume, "receive.consume");
                    let is_null = self
                        .builder
                        .build_is_null(message, "message_is_null")
                        .map_err(|error| {
                            miette::miette!("failed to emit receive check: {error}")
                        })?;
                    self.builder
                        .build_conditional_branch(is_null, wait, consume)
                        .map_err(|error| {
                            miette::miette!("failed to emit receive branch: {error}")
                        })?;

                    self.builder.position_at_end(wait);
                    self.return_poll(POLL_WAITING)?;

                    self.builder.position_at_end(consume);
                    let message_value =
                        self.call_runtime_value("riot_rt_msg_value", &[message.into()])?;
                    self.push_runtime_root(message_value)?;

                    let mut next_arm = consume;
                    for (arm_index, arm) in arms.iter().enumerate() {
                        self.builder.position_at_end(next_arm);
                        let matched =
                            self.emit_pattern_test(&CgValue::Value(message_value), &arm.pattern)?;
                        let arm_block = self
                            .context
                            .append_basic_block(resume, &format!("receive.arm.{arm_index}"));
                        let miss_block = self
                            .context
                            .append_basic_block(resume, &format!("receive.next.{arm_index}"));
                        self.builder
                            .build_conditional_branch(matched, arm_block, miss_block)
                            .map_err(|error| {
                                miette::miette!("failed to emit receive pattern branch: {error}")
                            })?;

                        self.builder.position_at_end(arm_block);
                        let mut arm_env = env.clone();
                        let arm_roots = self.bind_pattern_env(
                            &mut arm_env,
                            &arm.pattern,
                            &CgValue::Value(message_value),
                        )?;
                        self.emit_expr(&arm.body, &mut arm_env)?;
                        self.store_frame_i64(
                            frame_type,
                            frame,
                            0,
                            i64_type.const_int(next_state as u64, false),
                        )?;
                        self.pop_runtime_roots(arm_roots)?;
                        self.pop_runtime_roots(1)?;
                        self.return_poll(
                            POLL_CONSUMED | progress_flags(next_state, shape.states.len()),
                        )?;

                        next_arm = miss_block;
                    }

                    self.builder.position_at_end(next_arm);
                    self.pop_runtime_roots(1)?;
                    self.return_poll(POLL_WAITING)?;
                }
            }
        }

        if let Some(block) = restore_block {
            self.builder.position_at_end(block);
        }

        Ok(ActorDef {
            resume,
            frame_type,
            captures: shape.captures,
            size_bytes: shape.size_bytes,
            align: shape.align,
            root_offsets,
        })
    }

    fn emit_actor_frame_roots(
        &mut self,
        actor_id: IntValue<'ctx>,
        offsets: &[usize],
    ) -> miette::Result<()> {
        if offsets.is_empty() {
            return Ok(());
        }
        let i64_type = self.context.i64_type();
        let len = i64_type.const_int(offsets.len() as u64, false);
        let ptr = self
            .builder
            .build_array_alloca(i64_type, len, "actor_frame_root_offsets")
            .map_err(|error| {
                miette::miette!("failed to allocate actor frame root offsets: {error}")
            })?;
        for (index, offset) in offsets.iter().enumerate() {
            let slot = unsafe {
                self.builder
                    .build_in_bounds_gep(
                        i64_type,
                        ptr,
                        &[i64_type.const_int(index as u64, false)],
                        "actor_frame_root_offset",
                    )
                    .map_err(|error| {
                        miette::miette!("failed to address actor frame root offset: {error}")
                    })?
            };
            self.builder
                .build_store(slot, i64_type.const_int(*offset as u64, false))
                .map_err(|error| {
                    miette::miette!("failed to store actor frame root offset: {error}")
                })?;
        }
        self.call_void_runtime(
            "riot_rt_actor_frame_roots",
            &[actor_id.into(), ptr.into(), len.into()],
        )
    }

    fn load_actor_env(
        &self,
        frame_type: StructType<'ctx>,
        frame: PointerValue<'ctx>,
        shape: &ActorShape,
    ) -> miette::Result<Env<'ctx>> {
        let mut env = Env::default();
        for slot in &shape.slots {
            let raw = self.load_frame_i64(frame_type, frame, slot.field_index, &slot.name)?;
            let value = match slot.abi {
                AbiType::I64 => CgValue::I64(raw),
                AbiType::Bool => {
                    let zero = self.context.i64_type().const_zero();
                    CgValue::Bool(
                        self.builder
                            .build_int_compare(IntPredicate::NE, raw, zero, "bool_slot")
                            .map_err(|error| {
                                miette::miette!("failed to load actor bool slot: {error}")
                            })?,
                    )
                }
                AbiType::ActorId => CgValue::ActorId(raw),
                AbiType::Value => CgValue::Value(raw),
                AbiType::Unit | AbiType::Unknown => CgValue::Unit,
            };
            env.values.insert(slot.name.clone(), value);
        }
        Ok(env)
    }

    fn store_frame_value(
        &mut self,
        frame_type: StructType<'ctx>,
        frame: PointerValue<'ctx>,
        slot: &FrameSlot,
        value: CgValue<'ctx>,
    ) -> miette::Result<()> {
        let value = match (slot.abi, value) {
            (AbiType::I64, CgValue::I64(value)) | (AbiType::ActorId, CgValue::ActorId(value)) => {
                value
            }
            (AbiType::Value, value) => self.value_as_runtime(value)?,
            (AbiType::Bool, CgValue::Bool(value)) => self
                .builder
                .build_int_z_extend(value, self.context.i64_type(), "bool_to_i64")
                .map_err(|error| miette::miette!("failed to store actor bool slot: {error}"))?,
            (AbiType::I64, CgValue::Static(value)) => self.i64_const(
                value
                    .as_int()
                    .ok_or_else(|| miette::miette!("actor slot `{}` expects i64", slot.name))?,
            ),
            (AbiType::Bool, CgValue::Static(value)) => {
                let value = value
                    .as_bool()
                    .ok_or_else(|| miette::miette!("actor slot `{}` expects bool", slot.name))?;
                self.context.i64_type().const_int(u64::from(value), false)
            }
            (_, CgValue::ActorId(value)) if slot.abi == AbiType::I64 => value,
            (_, CgValue::I64(value)) if slot.abi == AbiType::ActorId => value,
            _ => bail!("unsupported value for actor slot `{}`", slot.name),
        };
        self.store_frame_i64(frame_type, frame, slot.field_index, value)
    }

    fn store_frame_i64(
        &self,
        frame_type: StructType<'ctx>,
        frame: PointerValue<'ctx>,
        field_index: u32,
        value: IntValue<'ctx>,
    ) -> miette::Result<()> {
        let field = self.frame_field(frame_type, frame, field_index)?;
        self.builder
            .build_store(field, value)
            .map_err(|error| miette::miette!("failed to store actor frame field: {error}"))?;
        Ok(())
    }

    fn load_frame_i64(
        &self,
        frame_type: StructType<'ctx>,
        frame: PointerValue<'ctx>,
        field_index: u32,
        name: &str,
    ) -> miette::Result<IntValue<'ctx>> {
        let field = self.frame_field(frame_type, frame, field_index)?;
        Ok(self
            .builder
            .build_load(self.context.i64_type(), field, name)
            .map_err(|error| miette::miette!("failed to load actor frame field: {error}"))?
            .into_int_value())
    }

    fn frame_field(
        &self,
        frame_type: StructType<'ctx>,
        frame: PointerValue<'ctx>,
        field_index: u32,
    ) -> miette::Result<PointerValue<'ctx>> {
        self.builder
            .build_struct_gep(frame_type, frame, field_index, "frame_field")
            .map_err(|error| miette::miette!("failed to address actor frame field: {error}"))
    }

    fn emit_bool(
        &mut self,
        expr: &LambdaExpr,
        env: &mut Env<'ctx>,
    ) -> miette::Result<IntValue<'ctx>> {
        let value = self.emit_expr(expr, env)?;
        self.value_as_bool(value)
    }

    fn value_as_i64(&self, value: CgValue<'ctx>) -> miette::Result<IntValue<'ctx>> {
        match value {
            CgValue::I64(value) | CgValue::ActorId(value) => Ok(value),
            CgValue::Bool(value) => self
                .builder
                .build_int_z_extend(value, self.context.i64_type(), "bool_i64")
                .map_err(|error| miette::miette!("failed to extend bool: {error}")),
            CgValue::Static(value) => value
                .as_int()
                .map(|value| self.i64_const(value))
                .ok_or_else(|| miette::miette!("expected i64 value")),
            CgValue::Value(value) => Ok(self
                .call_runtime_basic("riot_rt_value_as_i64", &[value.into()])?
                .into_int_value()),
            _ => bail!("expected i64 value"),
        }
    }

    fn value_as_actor_id(&self, value: CgValue<'ctx>) -> miette::Result<IntValue<'ctx>> {
        match value {
            CgValue::ActorId(value) => Ok(value),
            CgValue::Value(value) => Ok(self
                .call_runtime_basic("riot_rt_value_as_actor_id", &[value.into()])?
                .into_int_value()),
            _ => bail!("expected actor id value"),
        }
    }

    fn value_as_bool(&self, value: CgValue<'ctx>) -> miette::Result<IntValue<'ctx>> {
        match value {
            CgValue::Bool(value) => Ok(value),
            CgValue::Static(value) => value
                .as_bool()
                .map(|value| self.context.bool_type().const_int(u64::from(value), false))
                .ok_or_else(|| miette::miette!("expected bool value")),
            CgValue::Value(value) => Ok(self
                .call_runtime_basic("riot_rt_value_as_bool", &[value.into()])?
                .into_int_value()),
            _ => bail!("expected bool value"),
        }
    }

    fn emit_basic_arg(
        &mut self,
        arg: &LambdaExpr,
        abi: AbiType,
        env: &mut Env<'ctx>,
    ) -> miette::Result<(BasicMetadataValueEnum<'ctx>, usize)> {
        let value = self.emit_expr(arg, env)?;
        Ok(match abi {
            AbiType::I64 => (self.value_as_i64(value)?.into(), 0),
            AbiType::ActorId => (self.value_as_actor_id(value)?.into(), 0),
            AbiType::Bool => (self.value_as_bool(value)?.into(), 0),
            AbiType::Value => {
                let value = self.value_as_runtime(value)?;
                self.push_runtime_root(value)?;
                (value.into(), 1)
            }
            AbiType::Unit | AbiType::Unknown => bail!("unsupported function parameter ABI"),
        })
    }

    fn push_external_arg(
        &mut self,
        symbol: &str,
        output: &mut Vec<BasicMetadataValueEnum<'ctx>>,
        arg: &LambdaExpr,
        type_: &RsigType,
        env: &mut Env<'ctx>,
    ) -> miette::Result<usize> {
        match type_ {
            type_ if ExternalAbi::new(symbol).param_is_boxed(type_) => {
                let value = self.emit_expr(arg, env)?;
                let value = self.value_as_runtime(value)?;
                self.push_runtime_root(value)?;
                output.push(value.into());
                Ok(1)
            }
            RsigType::String => {
                let value = self.emit_expr(arg, env)?;
                let roots = self.push_optional_runtime_root(&value)?;
                let (ptr, len) = self.value_as_bytes(value)?;
                output.push(ptr.into());
                output.push(len.into());
                Ok(roots)
            }
            RsigType::I32 | RsigType::I64 => {
                let value = self.emit_expr(arg, env)?;
                output.push(self.value_as_i64(value)?.into());
                Ok(0)
            }
            RsigType::Bool => {
                let value = self.emit_expr(arg, env)?;
                output.push(self.value_as_bool(value)?.into());
                Ok(0)
            }
            RsigType::ActorId(_) => {
                let value = self.emit_expr(arg, env)?;
                output.push(self.value_as_actor_id(value)?.into());
                Ok(0)
            }
            _ => bail!(
                "external argument type `{}` is not supported by stage0 codegen yet",
                type_.canonical()
            ),
        }
    }

    fn value_as_bytes(
        &mut self,
        value: CgValue<'ctx>,
    ) -> miette::Result<(PointerValue<'ctx>, IntValue<'ctx>)> {
        match value {
            CgValue::Value(value) => {
                let ptr = self
                    .call_runtime_basic("riot_rt_value_bytes_ptr", &[value.into()])?
                    .into_pointer_value();
                let len = self
                    .call_runtime_basic("riot_rt_value_bytes_len", &[value.into()])?
                    .into_int_value();
                Ok((ptr, len))
            }
            CgValue::Static(value) => {
                let rendered = value.to_print_string();
                self.string_literal(&rendered)
            }
            _ => bail!("expected string-compatible value"),
        }
    }

    fn value_as_runtime(&mut self, value: CgValue<'ctx>) -> miette::Result<IntValue<'ctx>> {
        match value {
            CgValue::Value(value) => Ok(value),
            CgValue::I64(value) => self.call_runtime_value("riot_rt_value_i64", &[value.into()]),
            CgValue::Bool(value) => self.call_runtime_value("riot_rt_value_bool", &[value.into()]),
            CgValue::ActorId(value) => {
                self.call_runtime_value("riot_rt_value_actor_id", &[value.into()])
            }
            CgValue::Static(value) => match self.emit_static_value(value)? {
                CgValue::Value(value) => Ok(value),
                other => self.value_as_runtime(other),
            },
            CgValue::Unit => self.call_runtime_value("riot_rt_value_unit", &[]),
        }
    }

    fn runtime_value_as_type(
        &self,
        value: IntValue<'ctx>,
        type_: &RsigType,
    ) -> miette::Result<CgValue<'ctx>> {
        Ok(match AbiType::from_rsig(type_) {
            AbiType::I64 => CgValue::I64(
                self.call_runtime_basic("riot_rt_value_as_i64", &[value.into()])?
                    .into_int_value(),
            ),
            AbiType::Bool => CgValue::Bool(
                self.call_runtime_basic("riot_rt_value_as_bool", &[value.into()])?
                    .into_int_value(),
            ),
            AbiType::Unit => CgValue::Unit,
            AbiType::ActorId => CgValue::ActorId(
                self.call_runtime_basic("riot_rt_value_as_actor_id", &[value.into()])?
                    .into_int_value(),
            ),
            AbiType::Value | AbiType::Unknown => CgValue::Value(value),
        })
    }

    fn call_result(
        &self,
        value: Option<BasicValueEnum<'ctx>>,
        abi: AbiType,
    ) -> miette::Result<CgValue<'ctx>> {
        match abi {
            AbiType::Unit => Ok(CgValue::Unit),
            AbiType::I64 => Ok(CgValue::I64(
                value
                    .ok_or_else(|| miette::miette!("call returned void"))?
                    .into_int_value(),
            )),
            AbiType::Bool => Ok(CgValue::Bool(
                value
                    .ok_or_else(|| miette::miette!("call returned void"))?
                    .into_int_value(),
            )),
            AbiType::ActorId => Ok(CgValue::ActorId(
                value
                    .ok_or_else(|| miette::miette!("call returned void"))?
                    .into_int_value(),
            )),
            AbiType::Value => Ok(CgValue::Value(
                value
                    .ok_or_else(|| miette::miette!("call returned void"))?
                    .into_int_value(),
            )),
            AbiType::Unknown => bail!("cannot lower call with unknown ABI"),
        }
    }

    fn emit_return_value(&mut self, value: CgValue<'ctx>, abi: AbiType) -> miette::Result<()> {
        match abi {
            AbiType::Unit => {
                self.builder
                    .build_return(None)
                    .map_err(|error| miette::miette!("failed to emit return: {error}"))?;
            }
            AbiType::I64 | AbiType::ActorId => {
                let value = self.value_as_i64(value)?;
                self.builder
                    .build_return(Some(&value))
                    .map_err(|error| miette::miette!("failed to emit return: {error}"))?;
            }
            AbiType::Bool => {
                let value = self.value_as_bool(value)?;
                self.builder
                    .build_return(Some(&value))
                    .map_err(|error| miette::miette!("failed to emit return: {error}"))?;
            }
            AbiType::Value => {
                let value = self.value_as_runtime(value)?;
                self.builder
                    .build_return(Some(&value))
                    .map_err(|error| miette::miette!("failed to emit return: {error}"))?;
            }
            AbiType::Unknown => bail!("cannot return value with unknown ABI"),
        }
        Ok(())
    }

    fn return_poll(&self, flags: u32) -> miette::Result<()> {
        let value = self.context.i32_type().const_int(flags as u64, false);
        self.builder
            .build_return(Some(&value))
            .map_err(|error| miette::miette!("failed to emit actor poll return: {error}"))?;
        Ok(())
    }

    fn push_env_runtime_roots(&self, env: &Env<'ctx>) -> miette::Result<usize> {
        let mut count = 0;
        let mut values = env.values.iter().collect::<Vec<_>>();
        values.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
        for (_, value) in values {
            if let Some(root) = value.runtime_root() {
                self.push_runtime_root(root)?;
                count += 1;
            }
        }
        Ok(count)
    }

    fn push_optional_runtime_root(&self, value: &CgValue<'ctx>) -> miette::Result<usize> {
        if let Some(root) = value.runtime_root() {
            self.push_runtime_root(root)?;
            Ok(1)
        } else {
            Ok(0)
        }
    }

    fn push_runtime_root(&self, value: IntValue<'ctx>) -> miette::Result<()> {
        self.call_void_runtime("riot_rt_root_push", &[value.into()])
    }

    fn pop_runtime_roots(&self, count: usize) -> miette::Result<()> {
        if count == 0 {
            return Ok(());
        }
        self.call_void_runtime(
            "riot_rt_root_pop",
            &[self
                .context
                .i64_type()
                .const_int(count as u64, false)
                .into()],
        )
    }

    fn call_void_runtime(
        &self,
        symbol: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> miette::Result<()> {
        let fn_type = self.runtime_function_type(symbol)?;
        let function = self.get_or_add_function(symbol, fn_type);
        self.builder
            .build_call(function, args, symbol)
            .map_err(|error| miette::miette!("failed to emit runtime call `{symbol}`: {error}"))?;
        Ok(())
    }

    fn call_runtime_basic(
        &self,
        symbol: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> miette::Result<BasicValueEnum<'ctx>> {
        let fn_type = self.runtime_function_type(symbol)?;
        let function = self.get_or_add_function(symbol, fn_type);
        self.builder
            .build_call(function, args, symbol)
            .map_err(|error| miette::miette!("failed to emit runtime call `{symbol}`: {error}"))?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| miette::miette!("runtime call `{symbol}` returned void"))
    }

    fn call_runtime_value(
        &self,
        symbol: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> miette::Result<IntValue<'ctx>> {
        Ok(self.call_runtime_basic(symbol, args)?.into_int_value())
    }

    fn runtime_function_type(&self, symbol: &str) -> miette::Result<FunctionType<'ctx>> {
        let void_type = self.context.void_type();
        let i64_type = self.context.i64_type();
        let bool_type = self.context.bool_type();
        let ptr_type = self.ptr_type();
        Ok(match symbol {
            "riot_rt_init" | "riot_rt_shutdown" => void_type.fn_type(&[], false),
            "riot_rt_argv" => {
                i64_type.fn_type(&[self.context.i32_type().into(), ptr_type.into()], false)
            }
            "riot_rt_result_exit_code" => {
                self.context.i32_type().fn_type(&[i64_type.into()], false)
            }
            "riot_rt_root_push" | "riot_rt_root_pop" => {
                void_type.fn_type(&[i64_type.into()], false)
            }
            "riot_rt_actor_frame_roots" => {
                void_type.fn_type(&[i64_type.into(), ptr_type.into(), i64_type.into()], false)
            }
            "riot_rt_dbg" | "riot_rt_println" | "riot_prim_println" => {
                void_type.fn_type(&[ptr_type.into(), i64_type.into()], false)
            }
            "riot_rt_dbg_value" | "riot_rt_println_value" => {
                void_type.fn_type(&[i64_type.into()], false)
            }
            "riot_rt_dbg_i64" | "riot_rt_println_i64" | "riot_rt_monitor" | "riot_rt_link" => {
                void_type.fn_type(&[i64_type.into()], false)
            }
            "riot_rt_value_unit" => i64_type.fn_type(&[], false),
            "riot_rt_value_i64" | "riot_rt_value_actor_id" => {
                i64_type.fn_type(&[i64_type.into()], false)
            }
            "riot_rt_value_bool" => i64_type.fn_type(&[bool_type.into()], false),
            "riot_rt_value_string" => i64_type.fn_type(&[ptr_type.into(), i64_type.into()], false),
            "riot_rt_value_variant" => i64_type.fn_type(
                &[
                    ptr_type.into(),
                    i64_type.into(),
                    ptr_type.into(),
                    i64_type.into(),
                ],
                false,
            ),
            "riot_rt_value_variant_payload" => i64_type.fn_type(
                &[
                    ptr_type.into(),
                    i64_type.into(),
                    ptr_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                ],
                false,
            ),
            "riot_rt_value_tuple" | "riot_rt_value_list" => {
                i64_type.fn_type(&[ptr_type.into(), i64_type.into()], false)
            }
            "riot_rt_value_closure" => {
                i64_type.fn_type(&[ptr_type.into(), ptr_type.into(), i64_type.into()], false)
            }
            "riot_rt_value_apply" => i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            "riot_rt_msg_value" => i64_type.fn_type(&[ptr_type.into()], false),
            "riot_rt_value_as_i64" | "riot_rt_value_as_actor_id" => {
                i64_type.fn_type(&[i64_type.into()], false)
            }
            "riot_rt_value_as_bool" => bool_type.fn_type(&[i64_type.into()], false),
            "riot_rt_value_record_begin" => {
                i64_type.fn_type(&[ptr_type.into(), i64_type.into(), i64_type.into()], false)
            }
            "riot_rt_value_record_set" => void_type.fn_type(
                &[
                    i64_type.into(),
                    i64_type.into(),
                    ptr_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                ],
                false,
            ),
            "riot_rt_value_record_get" => {
                i64_type.fn_type(&[i64_type.into(), ptr_type.into(), i64_type.into()], false)
            }
            "riot_rt_value_record_is" => {
                bool_type.fn_type(&[i64_type.into(), ptr_type.into(), i64_type.into()], false)
            }
            "riot_rt_value_variant_get_payload" => i64_type.fn_type(&[i64_type.into()], false),
            "riot_rt_value_tuple_get"
            | "riot_rt_value_list_get"
            | "riot_rt_value_list_drop"
            | "riot_rt_value_list_cons" => {
                i64_type.fn_type(&[i64_type.into(), i64_type.into()], false)
            }
            "riot_rt_value_tuple_arity_is" => {
                bool_type.fn_type(&[i64_type.into(), i64_type.into()], false)
            }
            "riot_rt_value_list_len" | "riot_rt_value_string_len" => {
                i64_type.fn_type(&[i64_type.into()], false)
            }
            "riot_rt_value_string_concat" => {
                i64_type.fn_type(&[i64_type.into(), i64_type.into()], false)
            }
            "riot_rt_value_bytes_ptr" => ptr_type.fn_type(&[i64_type.into()], false),
            "riot_rt_value_bytes_len" => i64_type.fn_type(&[i64_type.into()], false),
            "riot_rt_value_eq" | "riot_rt_value_lt" => {
                bool_type.fn_type(&[i64_type.into(), i64_type.into()], false)
            }
            "riot_rt_value_variant_is" => bool_type.fn_type(
                &[
                    i64_type.into(),
                    ptr_type.into(),
                    i64_type.into(),
                    ptr_type.into(),
                    i64_type.into(),
                ],
                false,
            ),
            "riot_rt_send" => {
                void_type.fn_type(&[i64_type.into(), ptr_type.into(), i64_type.into()], false)
            }
            "riot_rt_send_i64" | "riot_rt_send_actor_id" => {
                void_type.fn_type(&[i64_type.into(), i64_type.into()], false)
            }
            "riot_rt_send_unit" => void_type.fn_type(&[i64_type.into()], false),
            "riot_rt_send_bool" => void_type.fn_type(&[i64_type.into(), bool_type.into()], false),
            "riot_rt_send_value" => void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            "riot_rt_send_msg" => void_type.fn_type(&[i64_type.into(), ptr_type.into()], false),
            "riot_rt_dbg_msg" => void_type.fn_type(&[ptr_type.into()], false),
            _ => bail!("unknown runtime function `{symbol}`"),
        })
    }

    fn get_or_add_function(&self, name: &str, fn_type: FunctionType<'ctx>) -> FunctionValue<'ctx> {
        self.module
            .get_function(name)
            .unwrap_or_else(|| self.module.add_function(name, fn_type, None))
    }

    fn local_function_type(&self, abi: &FunctionAbi) -> miette::Result<FunctionType<'ctx>> {
        let params = abi
            .params
            .iter()
            .map(|param| self.basic_type_for_abi(*param).map(Into::into))
            .collect::<miette::Result<Vec<BasicMetadataTypeEnum<'ctx>>>>()?;
        Ok(match abi.result {
            AbiType::Unit => self.context.void_type().fn_type(&params, false),
            AbiType::I64 | AbiType::ActorId | AbiType::Bool | AbiType::Value => {
                self.basic_type_for_abi(abi.result)?.fn_type(&params, false)
            }
            AbiType::Unknown => bail!("cannot build function type for unknown ABI"),
        })
    }

    fn external_function_type(
        &self,
        symbol: &str,
        params: &[RsigType],
        result: &RsigType,
    ) -> miette::Result<FunctionType<'ctx>> {
        let mut llvm_params = Vec::new();
        for param in params {
            match param {
                type_ if ExternalAbi::new(symbol).param_is_boxed(type_) => {
                    llvm_params.push(self.context.i64_type().into())
                }
                RsigType::String => {
                    llvm_params.push(self.ptr_type().into());
                    llvm_params.push(self.context.i64_type().into());
                }
                RsigType::I32 | RsigType::I64 | RsigType::ActorId(_) => {
                    llvm_params.push(self.context.i64_type().into())
                }
                RsigType::Bool => llvm_params.push(self.context.bool_type().into()),
                _ => bail!(
                    "unsupported external parameter type `{}`",
                    param.canonical()
                ),
            }
        }
        let result_abi = ExternalAbi::new(symbol).result_abi(result);
        match result_abi {
            AbiType::Unit => Ok(self.context.void_type().fn_type(&llvm_params, false)),
            AbiType::I64 | AbiType::ActorId | AbiType::Bool | AbiType::Value => Ok(self
                .basic_type_for_abi(result_abi)?
                .fn_type(&llvm_params, false)),
            AbiType::Unknown => bail!("unsupported external result type `{}`", result.canonical()),
        }
    }

    fn basic_type_for_abi(&self, abi: AbiType) -> miette::Result<BasicTypeEnum<'ctx>> {
        Ok(match abi {
            AbiType::I64 | AbiType::ActorId | AbiType::Value => self.context.i64_type().into(),
            AbiType::Bool => self.context.bool_type().into(),
            AbiType::Unit | AbiType::Unknown => bail!("ABI type does not have a basic LLVM type"),
        })
    }

    fn actor_frame_type(&self, slots: usize) -> StructType<'ctx> {
        let fields = (0..=slots)
            .map(|_| self.context.i64_type().into())
            .collect::<Vec<BasicTypeEnum<'ctx>>>();
        self.context.struct_type(&fields, false)
    }

    fn ptr_type(&self) -> inkwell::types::PointerType<'ctx> {
        self.context.ptr_type(AddressSpace::default())
    }

    fn current_function(&self) -> Option<FunctionValue<'ctx>> {
        self.builder.get_insert_block()?.get_parent()
    }

    fn string_literal(
        &mut self,
        value: &str,
    ) -> miette::Result<(PointerValue<'ctx>, IntValue<'ctx>)> {
        let index = self.string_counter;
        self.string_counter += 1;
        let global = self
            .builder
            .build_global_string_ptr(value, &format!("str_{index}"))
            .map_err(|error| miette::miette!("failed to emit string literal: {error}"))?;
        Ok((
            global.as_pointer_value(),
            self.context.i64_type().const_int(value.len() as u64, false),
        ))
    }

    fn i64_const(&self, value: i64) -> IntValue<'ctx> {
        self.context.i64_type().const_int(value as u64, true)
    }

    fn static_eval(
        &self,
        expr: &LambdaExpr,
        bindings: &HashMap<String, StaticValue>,
    ) -> Option<StaticValue> {
        StaticEvaluator::new(&self.function_map, &self.externals).eval_expr(expr, bindings)
    }

    fn static_eval_call(
        &self,
        name: &str,
        args: &[LambdaExpr],
        bindings: &HashMap<String, StaticValue>,
    ) -> Option<StaticValue> {
        StaticEvaluator::new(&self.function_map, &self.externals).eval_call(name, args, bindings)
    }
}

struct ActorDef<'ctx> {
    resume: FunctionValue<'ctx>,
    frame_type: StructType<'ctx>,
    captures: Vec<FrameSlot>,
    size_bytes: usize,
    align: usize,
    root_offsets: Vec<usize>,
}

fn actor_shape_from_ir(actor: &ActorIrActor) -> ActorShape {
    ActorShape {
        slots: actor
            .frame
            .slots
            .iter()
            .map(frame_slot_from_actor_ir)
            .collect(),
        captures: actor
            .frame
            .captures
            .iter()
            .map(frame_slot_from_actor_ir)
            .collect(),
        states: actor.states.clone(),
        size_bytes: actor.frame.size_bytes,
        align: actor.frame.align,
    }
}

fn frame_slot_from_actor_ir(slot: &ActorFrameSlot) -> FrameSlot {
    FrameSlot {
        name: slot.name.as_str().to_owned(),
        abi: AbiType::from_actor_slot(slot.type_),
        field_index: slot.field_index,
    }
}

fn actor_frame_root_offsets(shape: &ActorShape) -> Vec<usize> {
    shape
        .slots
        .iter()
        .filter(|slot| slot.abi == AbiType::Value)
        .map(|slot| slot.field_index as usize * 8)
        .collect()
}

fn progress_flags(next_index: usize, len: usize) -> u32 {
    if next_index >= len {
        POLL_PROGRESS | POLL_DONE
    } else {
        POLL_PROGRESS
    }
}

fn pattern_is_irrefutable(pattern: &LambdaPattern) -> bool {
    matches!(
        pattern,
        LambdaPattern::Wildcard | LambdaPattern::Bind { .. }
    )
}

fn is_result_unit_i32(type_: &RsigType) -> bool {
    match type_ {
        RsigType::VariantApp { name, args } if name.as_str() == "Result" => {
            matches!(args.as_slice(), [RsigType::Unit, RsigType::I32])
        }
        _ => false,
    }
}

fn infer_function_abis(
    program: &LambdaProgram,
    externals: &LambdaExternalTable,
) -> FunctionAbiTable {
    let mut abis = FunctionAbiTable::new();
    for function in &program.functions {
        abis.insert(
            function.name.clone(),
            FunctionAbi {
                params: function
                    .param_types
                    .iter()
                    .map(AbiType::from_rsig)
                    .collect(),
                result: AbiType::from_rsig(&function.result),
            },
        );
    }

    for _ in 0..4 {
        let previous = abis.clone();
        for function in &program.functions {
            let mut locals = HashMap::new();
            let mut params = previous
                .get(&function.name)
                .map(|abi| abi.params.clone())
                .unwrap_or_else(|| vec![AbiType::Unknown; function.params.len()]);
            for (name, abi) in function.params.iter().zip(&params) {
                locals.insert(name.as_str().to_owned(), *abi);
            }
            let result = infer_block_abi(
                &function.body,
                &function.params,
                &mut locals,
                &mut params,
                &previous,
                externals,
            );
            abis.insert(
                function.name.clone(),
                FunctionAbi {
                    params,
                    result: unify_abi(
                        previous
                            .get(&function.name)
                            .map(|abi| abi.result)
                            .unwrap_or(AbiType::Unknown),
                        result,
                    ),
                },
            );
        }
        if abis == previous {
            break;
        }
    }
    abis
}

fn infer_block_abi(
    block: &LambdaBlock,
    params: &[Param],
    locals: &mut HashMap<String, AbiType>,
    param_types: &mut [AbiType],
    functions: &FunctionAbiTable,
    externals: &LambdaExternalTable,
) -> AbiType {
    for stmt in &block.statements {
        match stmt {
            LambdaStmt::Let { name, value } => {
                let abi = infer_expr_abi(value, locals, functions, externals);
                locals.insert(name.as_str().to_owned(), abi);
            }
            LambdaStmt::Expr(expr) => {
                mark_expr_constraints(expr, params, locals, param_types, functions, externals);
            }
        }
    }
    if let Some(tail) = &block.tail {
        mark_expr_constraints(tail, params, locals, param_types, functions, externals);
        infer_expr_abi(tail, locals, functions, externals)
    } else {
        AbiType::Unit
    }
}

fn infer_expr_abi(
    expr: &LambdaExpr,
    locals: &HashMap<String, AbiType>,
    functions: &FunctionAbiTable,
    externals: &LambdaExternalTable,
) -> AbiType {
    match expr {
        LambdaExpr::Int(_) => AbiType::I64,
        LambdaExpr::Bool(_) => AbiType::Bool,
        LambdaExpr::If {
            then_branch,
            else_branch,
            ..
        } => unify_abi(
            infer_expr_abi(then_branch, locals, functions, externals),
            infer_expr_abi(else_branch, locals, functions, externals),
        ),
        LambdaExpr::Match { arms, .. } => arms
            .iter()
            .map(|arm| infer_match_arm_abi(arm, locals, functions, externals))
            .fold(AbiType::Unknown, unify_abi),
        LambdaExpr::Block(block) => infer_block_expr_abi(block, locals, functions, externals),
        LambdaExpr::Call { result, .. } => AbiType::from_rsig(result),
        LambdaExpr::Unit => AbiType::Unit,
        LambdaExpr::Apply { result, .. } => AbiType::from_rsig(result),
        LambdaExpr::Lambda { .. } => AbiType::Value,
        LambdaExpr::Path(path) => path
            .first()
            .and_then(|name| locals.get(name))
            .copied()
            .unwrap_or(AbiType::Unknown),
        LambdaExpr::Local(binding) => locals
            .get(binding.as_str())
            .copied()
            .unwrap_or(AbiType::Unknown),
        LambdaExpr::Tuple(_)
        | LambdaExpr::List(_)
        | LambdaExpr::Record { .. }
        | LambdaExpr::Variant { .. }
        | LambdaExpr::Field { .. }
        | LambdaExpr::TupleIndex { .. }
        | LambdaExpr::String(_) => AbiType::Value,
        LambdaExpr::Spawn { .. } => AbiType::ActorId,
        LambdaExpr::Receive { .. } | LambdaExpr::Char(_) | LambdaExpr::Float(_) => AbiType::Unknown,
    }
}

fn infer_match_arm_abi(
    arm: &LambdaMatchArm,
    outer_locals: &HashMap<String, AbiType>,
    functions: &FunctionAbiTable,
    externals: &LambdaExternalTable,
) -> AbiType {
    let mut locals = outer_locals.clone();
    bind_pattern_abis(&arm.pattern, &mut locals);
    infer_expr_abi(&arm.body, &locals, functions, externals)
}

fn bind_pattern_abis(pattern: &LambdaPattern, locals: &mut HashMap<String, AbiType>) {
    match pattern {
        LambdaPattern::Bind { binding, type_ } => {
            locals.insert(binding.as_str().to_owned(), AbiType::from_rsig(type_));
        }
        LambdaPattern::Constructor { payload, .. } | LambdaPattern::Tuple(payload) => {
            for pattern in payload {
                bind_pattern_abis(pattern, locals);
            }
        }
        LambdaPattern::List { prefix, tail } => {
            for pattern in prefix {
                bind_pattern_abis(pattern, locals);
            }
            if let Some(tail) = tail {
                bind_pattern_abis(tail, locals);
            }
        }
        LambdaPattern::Record { fields, .. } => {
            for (_, pattern) in fields {
                bind_pattern_abis(pattern, locals);
            }
        }
        LambdaPattern::Wildcard
        | LambdaPattern::Unit
        | LambdaPattern::Bool(_)
        | LambdaPattern::Int(_)
        | LambdaPattern::String(_) => {}
    }
}

fn infer_block_expr_abi(
    block: &LambdaBlock,
    outer_locals: &HashMap<String, AbiType>,
    functions: &FunctionAbiTable,
    externals: &LambdaExternalTable,
) -> AbiType {
    let mut locals = outer_locals.clone();
    let mut param_types = Vec::new();
    infer_block_abi(
        block,
        &[],
        &mut locals,
        &mut param_types,
        functions,
        externals,
    )
}

fn mark_expr_constraints(
    expr: &LambdaExpr,
    params: &[Param],
    locals: &mut HashMap<String, AbiType>,
    param_types: &mut [AbiType],
    functions: &FunctionAbiTable,
    externals: &LambdaExternalTable,
) {
    match expr {
        LambdaExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            mark_expr_constraints(condition, params, locals, param_types, functions, externals);
            mark_expr_constraints(
                then_branch,
                params,
                locals,
                param_types,
                functions,
                externals,
            );
            mark_expr_constraints(
                else_branch,
                params,
                locals,
                param_types,
                functions,
                externals,
            );
        }
        LambdaExpr::Match { scrutinee, arms } => {
            mark_expr_constraints(scrutinee, params, locals, param_types, functions, externals);
            for arm in arms {
                mark_expr_constraints(&arm.body, params, locals, param_types, functions, externals);
            }
        }
        LambdaExpr::Block(block) => {
            let mut nested_locals = locals.clone();
            let mut nested_params = Vec::new();
            infer_block_abi(
                block,
                &[],
                &mut nested_locals,
                &mut nested_params,
                functions,
                externals,
            );
        }
        LambdaExpr::Call {
            args, arg_types, ..
        } => {
            for (arg, type_) in args.iter().zip(arg_types) {
                let abi = AbiType::from_rsig(type_);
                if abi != AbiType::Unknown {
                    mark_expr_as(params, locals, param_types, arg, abi);
                }
                mark_expr_constraints(arg, params, locals, param_types, functions, externals);
            }
        }
        LambdaExpr::Apply { callee, args, .. } => {
            mark_expr_constraints(callee, params, locals, param_types, functions, externals);
            for arg in args {
                mark_expr_constraints(arg, params, locals, param_types, functions, externals);
            }
        }
        LambdaExpr::Lambda {
            params: lambda_params,
            body,
            ..
        } => {
            let mut nested_locals = locals.clone();
            for param in lambda_params {
                nested_locals.insert(param.as_str().to_owned(), AbiType::Unknown);
            }
            let mut nested_params = vec![AbiType::Unknown; lambda_params.len()];
            infer_block_abi(
                body,
                lambda_params,
                &mut nested_locals,
                &mut nested_params,
                functions,
                externals,
            );
        }
        LambdaExpr::Tuple(items) | LambdaExpr::List(items) => {
            for item in items {
                mark_expr_constraints(item, params, locals, param_types, functions, externals);
            }
        }
        LambdaExpr::Record { fields, .. } => {
            for (_, value) in fields {
                mark_expr_constraints(value, params, locals, param_types, functions, externals);
            }
        }
        LambdaExpr::Field { base, .. } | LambdaExpr::TupleIndex { base, .. } => {
            mark_expr_constraints(base, params, locals, param_types, functions, externals);
        }
        LambdaExpr::Receive { arms } => {
            for arm in arms {
                mark_expr_constraints(&arm.body, params, locals, param_types, functions, externals);
            }
        }
        LambdaExpr::Spawn { body, .. } => {
            let mut nested_locals = HashMap::new();
            let mut nested_params = Vec::new();
            infer_block_abi(
                body,
                &[],
                &mut nested_locals,
                &mut nested_params,
                functions,
                externals,
            );
        }
        LambdaExpr::Variant { payload, .. } => {
            for value in payload {
                mark_expr_constraints(value, params, locals, param_types, functions, externals);
            }
        }
        LambdaExpr::Bool(_)
        | LambdaExpr::Unit
        | LambdaExpr::Char(_)
        | LambdaExpr::Float(_)
        | LambdaExpr::Int(_)
        | LambdaExpr::Path(_)
        | LambdaExpr::Local(_)
        | LambdaExpr::String(_) => {}
    }
}

fn mark_expr_as(
    params: &[Param],
    locals: &mut HashMap<String, AbiType>,
    param_types: &mut [AbiType],
    expr: &LambdaExpr,
    abi: AbiType,
) {
    if let LambdaExpr::Block(block) = expr {
        if let Some(tail) = &block.tail {
            mark_expr_as(params, locals, param_types, tail, abi);
        }
        return;
    }

    let name = match expr {
        LambdaExpr::Path(path) => {
            let [name] = path.as_slice() else {
                return;
            };
            name.as_str()
        }
        LambdaExpr::Local(binding) => binding.as_str(),
        _ => return,
    };

    locals.insert(name.to_owned(), abi);
    if let Some(index) = params.iter().position(|param| param.as_str() == name) {
        param_types[index] = unify_abi(param_types[index], abi);
    }
}

fn unify_abi(lhs: AbiType, rhs: AbiType) -> AbiType {
    match (lhs, rhs) {
        (AbiType::Unknown, value) | (value, AbiType::Unknown) => value,
        (lhs, rhs) if lhs == rhs => lhs,
        (AbiType::Value, value) | (value, AbiType::Value) if value != AbiType::Unit => {
            AbiType::Value
        }
        _ => AbiType::Unknown,
    }
}

fn runtime_variant_type_name(type_name: &TypeName) -> String {
    runtime_type_name(type_name.as_str())
}

fn runtime_record_type_name(type_name: &TypeName) -> String {
    runtime_type_name(type_name.as_str())
}

fn runtime_record_path_name(path: &[String]) -> String {
    runtime_type_name(&path.join("."))
}

fn runtime_type_name(name: &str) -> String {
    name.rsplit('.').next().unwrap_or(name).to_owned()
}

fn codegen_externals(program: &LambdaProgram) -> miette::Result<LambdaExternalTable> {
    let mut externals = LambdaExternalTable::new();
    for export in crate::stdlib::Stdlib::new().prelude_signature()?.exports {
        let crate::signature::RsigExport::External(external) = export else {
            continue;
        };
        externals.insert(LambdaExternal {
            name: external.name,
            params: external.params,
            result: external.result,
            abi: external.abi,
        });
    }

    for external in &program.externals {
        externals.insert(external.clone());
    }

    Ok(externals)
}

#[cfg(test)]
mod tests {
    use crate::lambda::ir::{
        BindingKey, Capture, LambdaBlock, LambdaExpr, LambdaFunction, LambdaProgram, LambdaStmt,
        Param,
    };
    use crate::signature::{ImportedSignatures, ModuleName, RsigType};

    use super::{CodegenMode, LlvmBackend};

    #[test]
    fn lambda_values_lower_to_runtime_closure_calls() {
        let n = BindingKey::new("n");
        let x = BindingKey::new("x");
        let program = LambdaProgram {
            module_name: ModuleName::new("ClosureTest"),
            uses: Vec::new(),
            externals: Vec::new(),
            functions: vec![LambdaFunction {
                name: "main".to_owned(),
                params: Vec::new(),
                param_types: Vec::new(),
                result: RsigType::Unknown,
                body: LambdaBlock {
                    statements: vec![LambdaStmt::Let {
                        name: n.clone(),
                        value: LambdaExpr::Int(1),
                    }],
                    tail: Some(LambdaExpr::Lambda {
                        params: vec![Param::from_key(x)],
                        captures: vec![Capture::from_key(n.clone())],
                        body: Box::new(LambdaBlock {
                            statements: Vec::new(),
                            tail: Some(LambdaExpr::Local(n)),
                        }),
                    }),
                },
                symbol: "riot_mod_ClosureTest_main".to_owned(),
            }],
        };

        let llvm = LlvmBackend::new()
            .emit_llvm_text(
                &program,
                &ImportedSignatures::new(),
                CodegenMode::Executable,
            )
            .unwrap();

        assert!(llvm.contains("riot_rt_value_closure"));
        assert!(llvm.contains("riot_lambda_apply_"));
    }

    #[test]
    fn lambda_apply_lowers_to_runtime_apply_function() {
        let x = BindingKey::new("x");
        let program = LambdaProgram {
            module_name: ModuleName::new("ClosureApplyTest"),
            uses: Vec::new(),
            externals: Vec::new(),
            functions: vec![LambdaFunction {
                name: "main".to_owned(),
                params: Vec::new(),
                param_types: Vec::new(),
                result: RsigType::Unknown,
                body: LambdaBlock {
                    statements: Vec::new(),
                    tail: Some(LambdaExpr::Apply {
                        callee: Box::new(LambdaExpr::Lambda {
                            params: vec![Param::from_key(x.clone())],
                            captures: Vec::new(),
                            body: Box::new(LambdaBlock {
                                statements: Vec::new(),
                                tail: Some(LambdaExpr::Call {
                                    callee: vec!["(+)".to_owned()],
                                    args: vec![LambdaExpr::Local(x), LambdaExpr::Int(1)],
                                    arg_types: vec![RsigType::I64, RsigType::I64],
                                    result: RsigType::I64,
                                }),
                            }),
                        }),
                        args: vec![LambdaExpr::Int(41)],
                        result: RsigType::I64,
                    }),
                },
                symbol: "riot_mod_ClosureApplyTest_main".to_owned(),
            }],
        };

        let llvm = LlvmBackend::new()
            .emit_llvm_text(
                &program,
                &ImportedSignatures::new(),
                CodegenMode::Executable,
            )
            .unwrap();

        assert!(llvm.contains("riot_rt_value_apply"));
        assert!(llvm.contains("riot_rt_value_as_i64"));
        assert!(llvm.contains("riot_lambda_apply_"));
    }
}
