use std::collections::{BTreeMap, HashMap};

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

use crate::ir::{
    ActorFrameOp, ActorIrActor, ActorIrProgram, Capture, Param, RirBlock, RirExpr, RirFunction,
    RirMatchArm, RirPattern, RirProgram, RirStmt, lower_rir_to_actor_ir,
};
use crate::signature::{ConstructorName, ImportedSignatures, RsigExport, RsigType, TypeName};

mod abi;
mod static_eval;

use abi::{AbiType, FunctionAbi};
use static_eval::{
    StaticValue, eval_call as static_eval_call, eval_expr as static_eval_expr,
    resolve_path as resolve_static_path,
};

const POLL_CONSUMED: u32 = 1;
const POLL_DONE: u32 = 2;
const POLL_PROGRESS: u32 = 4;
const POLL_WAITING: u32 = 16;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CodegenMode {
    Executable,
    Module,
}

pub(crate) fn emit_executable_object(
    program: &RirProgram,
    imports: &ImportedSignatures,
    object_path: &Utf8Path,
    llvm_ir_path: Option<&Utf8Path>,
) -> miette::Result<()> {
    emit_object_with_mode(
        program,
        imports,
        object_path,
        llvm_ir_path,
        CodegenMode::Executable,
    )
}

pub(crate) fn emit_module_object(
    program: &RirProgram,
    imports: &ImportedSignatures,
    object_path: &Utf8Path,
    llvm_ir_path: Option<&Utf8Path>,
) -> miette::Result<()> {
    emit_object_with_mode(
        program,
        imports,
        object_path,
        llvm_ir_path,
        CodegenMode::Module,
    )
}

pub(crate) fn emit_llvm_text(
    program: &RirProgram,
    imports: &ImportedSignatures,
    mode: CodegenMode,
) -> miette::Result<String> {
    let context = Context::create();
    let target_machine = create_target_machine()?;
    let module = build_program_module(&context, &target_machine, program, imports, mode)?;
    Ok(module.print_to_string().to_string())
}

pub(crate) fn emit_assembly_text(
    program: &RirProgram,
    imports: &ImportedSignatures,
    mode: CodegenMode,
) -> miette::Result<String> {
    let context = Context::create();
    let target_machine = create_target_machine()?;
    let module = build_program_module(&context, &target_machine, program, imports, mode)?;
    let buffer = target_machine
        .write_to_memory_buffer(&module, FileType::Assembly)
        .map_err(|error| miette::miette!("failed to emit assembly: {error}"))?;
    Ok(String::from_utf8_lossy(buffer.as_slice()).into_owned())
}

fn emit_object_with_mode(
    program: &RirProgram,
    imports: &ImportedSignatures,
    object_path: &Utf8Path,
    llvm_ir_path: Option<&Utf8Path>,
    mode: CodegenMode,
) -> miette::Result<()> {
    let context = Context::create();
    let target_machine = create_target_machine()?;
    let module = build_program_module(&context, &target_machine, program, imports, mode)?;

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
    program: &RirProgram,
    imports: &ImportedSignatures,
    mode: CodegenMode,
) -> miette::Result<Module<'ctx>> {
    let triple = TargetMachine::get_default_triple();
    let module = context.create_module(program.module_name.as_str());
    module.set_triple(&triple);
    module.set_data_layout(&target_machine.get_target_data().get_data_layout());

    let codegen = Codegen {
        context,
        module,
        builder: context.create_builder(),
        program,
        imports,
        functions: HashMap::new(),
        function_abis: infer_function_abis(program),
        function_map: program
            .functions
            .iter()
            .map(|function| (function.name.as_str(), function))
            .collect(),
        externals: program
            .externals
            .iter()
            .map(|external| (external.name.as_str(), external))
            .collect(),
        actor_ir: lower_rir_to_actor_ir(program, imports),
        string_counter: 0,
    };
    codegen.emit_program(mode)
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
    states: Vec<crate::ir::ActorFrameState>,
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
}

#[derive(Clone, Default)]
struct Env<'ctx> {
    values: HashMap<String, CgValue<'ctx>>,
    statics: HashMap<String, StaticValue>,
}

struct Codegen<'ctx, 'a> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    program: &'a RirProgram,
    imports: &'a ImportedSignatures,
    functions: HashMap<String, FunctionValue<'ctx>>,
    function_abis: HashMap<String, FunctionAbi>,
    function_map: HashMap<&'a str, &'a RirFunction>,
    externals: BTreeMap<&'a str, &'a crate::ir::RirExternal>,
    actor_ir: ActorIrProgram,
    string_counter: usize,
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
            let Some(function_value) = self.functions.get(&function.name).copied() else {
                continue;
            };
            let abi = self.function_abis[&function.name].clone();
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
        let main_fn = self
            .module
            .add_function("main", i32_type.fn_type(&[], false), None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        self.call_void_runtime("riot_rt_init", &[])?;
        let mut env = Env::default();
        self.emit_block(&main.body, &mut env)?;
        self.call_void_runtime("riot_rt_shutdown", &[])?;
        self.builder
            .build_return(Some(&i32_type.const_zero()))
            .map_err(|error| miette::miette!("failed to emit main return: {error}"))?;
        Ok(())
    }

    fn emit_block(
        &mut self,
        block: &RirBlock,
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let mut rooted_values = self.push_env_runtime_roots(env)?;
        for stmt in &block.statements {
            match stmt {
                RirStmt::Let { name, value } => {
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
                RirStmt::Expr(expr) => {
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

    fn emit_expr(&mut self, expr: &RirExpr, env: &mut Env<'ctx>) -> miette::Result<CgValue<'ctx>> {
        match expr {
            RirExpr::Add(lhs, rhs) => {
                let lhs = self.emit_i64(lhs, env)?;
                let rhs = self.emit_i64(rhs, env)?;
                Ok(CgValue::I64(
                    self.builder
                        .build_int_add(lhs, rhs, "addtmp")
                        .map_err(|error| miette::miette!("failed to emit add: {error}"))?,
                ))
            }
            RirExpr::Sub(lhs, rhs) => {
                let lhs = self.emit_i64(lhs, env)?;
                let rhs = self.emit_i64(rhs, env)?;
                Ok(CgValue::I64(
                    self.builder
                        .build_int_sub(lhs, rhs, "subtmp")
                        .map_err(|error| miette::miette!("failed to emit sub: {error}"))?,
                ))
            }
            RirExpr::Mul(lhs, rhs) => {
                let lhs = self.emit_i64(lhs, env)?;
                let rhs = self.emit_i64(rhs, env)?;
                Ok(CgValue::I64(
                    self.builder
                        .build_int_mul(lhs, rhs, "multmp")
                        .map_err(|error| miette::miette!("failed to emit mul: {error}"))?,
                ))
            }
            RirExpr::Div(lhs, rhs) => {
                let lhs = self.emit_i64(lhs, env)?;
                let rhs = self.emit_i64(rhs, env)?;
                Ok(CgValue::I64(
                    self.builder
                        .build_int_signed_div(lhs, rhs, "divtmp")
                        .map_err(|error| miette::miette!("failed to emit div: {error}"))?,
                ))
            }
            RirExpr::Mod(lhs, rhs) => {
                let lhs = self.emit_i64(lhs, env)?;
                let rhs = self.emit_i64(rhs, env)?;
                Ok(CgValue::I64(
                    self.builder
                        .build_int_signed_rem(lhs, rhs, "modtmp")
                        .map_err(|error| miette::miette!("failed to emit mod: {error}"))?,
                ))
            }
            RirExpr::Neg(value) => {
                if let Some(value) = self.static_eval(expr, &env.statics) {
                    return Ok(CgValue::Static(value));
                }
                let value = self.emit_i64(value, env)?;
                Ok(CgValue::I64(
                    self.builder
                        .build_int_neg(value, "negtmp")
                        .map_err(|error| miette::miette!("failed to emit negation: {error}"))?,
                ))
            }
            RirExpr::Eq(lhs, rhs) => {
                let lhs_value = self.emit_expr(lhs, env)?;
                let lhs_root = self.push_optional_runtime_root(&lhs_value)?;
                let rhs_value = self.emit_expr(rhs, env)?;
                let rhs_root = self.push_optional_runtime_root(&rhs_value)?;
                let result = match (lhs_value, rhs_value) {
                    (CgValue::I64(lhs), CgValue::I64(rhs)) => Ok(CgValue::Bool(
                        self.builder
                            .build_int_compare(IntPredicate::EQ, lhs, rhs, "eqtmp")
                            .map_err(|error| miette::miette!("failed to emit equality: {error}"))?,
                    )),
                    (CgValue::Bool(lhs), CgValue::Bool(rhs)) => Ok(CgValue::Bool(
                        self.builder
                            .build_int_compare(IntPredicate::EQ, lhs, rhs, "eqtmp")
                            .map_err(|error| miette::miette!("failed to emit equality: {error}"))?,
                    )),
                    (lhs, rhs) => {
                        let lhs = self.value_as_runtime(lhs)?;
                        self.push_runtime_root(lhs)?;
                        let rhs = self.value_as_runtime(rhs)?;
                        self.push_runtime_root(rhs)?;
                        let result = Ok(CgValue::Bool(
                            self.call_runtime_value("riot_rt_value_eq", &[lhs.into(), rhs.into()])?,
                        ));
                        self.pop_runtime_roots(2)?;
                        result
                    }
                };
                self.pop_runtime_roots(lhs_root + rhs_root)?;
                result
            }
            RirExpr::Lt(lhs, rhs) => {
                let lhs_value = self.emit_expr(lhs, env)?;
                let lhs_root = self.push_optional_runtime_root(&lhs_value)?;
                let rhs_value = self.emit_expr(rhs, env)?;
                let rhs_root = self.push_optional_runtime_root(&rhs_value)?;
                let result = match (lhs_value, rhs_value) {
                    (CgValue::I64(lhs), CgValue::I64(rhs)) => Ok(CgValue::Bool(
                        self.builder
                            .build_int_compare(IntPredicate::SLT, lhs, rhs, "lttmp")
                            .map_err(|error| {
                                miette::miette!("failed to emit comparison: {error}")
                            })?,
                    )),
                    (lhs, rhs) => {
                        let lhs = self.value_as_runtime(lhs)?;
                        self.push_runtime_root(lhs)?;
                        let rhs = self.value_as_runtime(rhs)?;
                        self.push_runtime_root(rhs)?;
                        let result = Ok(CgValue::Bool(
                            self.call_runtime_value("riot_rt_value_lt", &[lhs.into(), rhs.into()])?,
                        ));
                        self.pop_runtime_roots(2)?;
                        result
                    }
                };
                self.pop_runtime_roots(lhs_root + rhs_root)?;
                result
            }
            RirExpr::And(lhs, rhs) => {
                let lhs = self.emit_bool(lhs, env)?;
                let rhs = self.emit_bool(rhs, env)?;
                Ok(CgValue::Bool(
                    self.builder
                        .build_and(lhs, rhs, "andtmp")
                        .map_err(|error| miette::miette!("failed to emit and: {error}"))?,
                ))
            }
            RirExpr::Or(lhs, rhs) => {
                let lhs = self.emit_bool(lhs, env)?;
                let rhs = self.emit_bool(rhs, env)?;
                Ok(CgValue::Bool(
                    self.builder
                        .build_or(lhs, rhs, "ortmp")
                        .map_err(|error| miette::miette!("failed to emit or: {error}"))?,
                ))
            }
            RirExpr::Not(value) => {
                let value = self.emit_bool(value, env)?;
                Ok(CgValue::Bool(
                    self.builder
                        .build_not(value, "nottmp")
                        .map_err(|error| miette::miette!("failed to emit not: {error}"))?,
                ))
            }
            RirExpr::If {
                condition,
                then_branch,
                else_branch,
            } => self.emit_if(condition, then_branch, else_branch, env),
            RirExpr::Match { scrutinee, arms } => self.emit_match(scrutinee, arms, env),
            RirExpr::Block(block) => {
                let mut scoped = env.clone();
                self.emit_block(block, &mut scoped)
            }
            RirExpr::Bool(value) => Ok(CgValue::Bool(
                self.context.bool_type().const_int(u64::from(*value), false),
            )),
            RirExpr::Call { callee, args } => self.emit_call(callee, args, env),
            RirExpr::Apply {
                callee,
                args,
                result,
            } => self.emit_apply(callee, args, result, env),
            RirExpr::Lambda {
                params,
                captures,
                body,
            } => self.emit_lambda_value(params, captures, body, env),
            RirExpr::Unit => Ok(CgValue::Unit),
            RirExpr::Tuple(items) => self.emit_value_sequence("riot_rt_value_tuple", items, env),
            RirExpr::List(items) => self.emit_value_sequence("riot_rt_value_list", items, env),
            RirExpr::Record { path, fields } => self.emit_record_value(path, fields, env),
            RirExpr::Field { base, field } => {
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
            RirExpr::TupleIndex { base, index } => {
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
            RirExpr::Variant {
                type_name,
                constructor,
                payload,
            } => self.emit_variant_value(type_name, constructor, payload, env),
            RirExpr::Char(_) | RirExpr::Float(_) => self
                .static_eval(expr, &env.statics)
                .map(|value| self.emit_static_value(value))
                .transpose()?
                .ok_or_else(|| miette::miette!("unsupported non-scalar expression")),
            RirExpr::Int(value) => Ok(CgValue::I64(self.i64_const(*value))),
            RirExpr::Path(path) => self.emit_path(path, env),
            RirExpr::String(value) => {
                let (ptr, len) = self.string_literal(value)?;
                Ok(CgValue::Value(self.call_runtime_value(
                    "riot_rt_value_string",
                    &[ptr.into(), len.into()],
                )?))
            }
            RirExpr::Spawn { actor_id, .. } => self.emit_spawn(*actor_id, env),
            RirExpr::Receive { .. } => bail!("receive cannot be lowered outside an actor frame"),
        }
    }

    fn emit_variant_value(
        &mut self,
        type_name: &TypeName,
        constructor: &ConstructorName,
        payload: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let (type_ptr, type_len) = self.string_literal(type_name.as_str())?;
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
        payload: &[RirExpr],
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
        condition: &RirExpr,
        then_branch: &RirExpr,
        else_branch: &RirExpr,
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let Some(parent) = self.current_function() else {
            bail!("if expression emitted without an active function");
        };
        let result_abi = unify_abi(
            infer_expr_abi(then_branch, &HashMap::new(), &self.function_abis),
            infer_expr_abi(else_branch, &HashMap::new(), &self.function_abis),
        );
        if result_abi == AbiType::Unknown {
            return self
                .static_eval(
                    &RirExpr::If {
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
        scrutinee: &RirExpr,
        arms: &[RirMatchArm],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if arms.is_empty() {
            bail!("cannot lower empty match expression");
        }
        let Some(parent) = self.current_function() else {
            bail!("match expression emitted without an active function");
        };
        let result_abi = arms
            .iter()
            .map(|arm| infer_expr_abi(&arm.body, &HashMap::new(), &self.function_abis))
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
            if pattern_is_irrefutable(&arm.pattern) {
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
            self.bind_pattern_env(&mut arm_env, &arm.pattern, &scrutinee)?;
            let value = self.emit_expr(&arm.body, &mut arm_env)?;
            let incoming_value = self.match_incoming_value(value, result_abi)?;
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
        pattern: &RirPattern,
    ) -> miette::Result<IntValue<'ctx>> {
        let bool_type = self.context.bool_type();
        match pattern {
            RirPattern::Wildcard | RirPattern::Bind { .. } => Ok(bool_type.const_int(1, false)),
            RirPattern::Unit => {
                let scrutinee = self.value_as_runtime(scrutinee.clone())?;
                let unit = self.call_runtime_value("riot_rt_value_unit", &[])?;
                Ok(self
                    .builder
                    .build_int_compare(IntPredicate::EQ, scrutinee, unit, "match_unit")
                    .map_err(|error| miette::miette!("failed to emit unit pattern: {error}"))?)
            }
            RirPattern::Bool(expected) => {
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
            RirPattern::Int(expected) => {
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
            RirPattern::String(expected) => {
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
            RirPattern::Constructor {
                type_name,
                constructor,
                payload,
            } => {
                let scrutinee = self.value_as_runtime(scrutinee.clone())?;
                let (type_ptr, type_len) = self.string_literal(type_name.as_str())?;
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
                Ok(tag_matches)
            }
            RirPattern::Tuple(items) => {
                let scrutinee = self.value_as_runtime(scrutinee.clone())?;
                self.call_runtime_value(
                    "riot_rt_value_tuple_arity_is",
                    &[
                        scrutinee.into(),
                        self.context
                            .i64_type()
                            .const_int(items.len() as u64, false)
                            .into(),
                    ],
                )
            }
            RirPattern::Record { type_name, .. } => {
                let scrutinee = self.value_as_runtime(scrutinee.clone())?;
                let (path_ptr, path_len) = self.string_literal(type_name.as_str())?;
                self.call_runtime_value(
                    "riot_rt_value_record_is",
                    &[scrutinee.into(), path_ptr.into(), path_len.into()],
                )
            }
        }
    }

    fn bind_pattern_env(
        &mut self,
        env: &mut Env<'ctx>,
        pattern: &RirPattern,
        scrutinee: &CgValue<'ctx>,
    ) -> miette::Result<()> {
        match pattern {
            RirPattern::Bind { binding, type_ } => {
                let value = match scrutinee {
                    CgValue::Value(value) => self.runtime_value_as_type(*value, type_)?,
                    _ => scrutinee.clone(),
                };
                env.values.insert(binding.as_str().to_owned(), value);
            }
            RirPattern::Constructor { payload, .. } if !payload.is_empty() => {
                let scrutinee = self.value_as_runtime(scrutinee.clone())?;
                let payload_value = self
                    .call_runtime_value("riot_rt_value_variant_get_payload", &[scrutinee.into()])?;
                self.bind_payload_patterns(env, payload, payload_value)?;
            }
            RirPattern::Tuple(items) => {
                let tuple = self.value_as_runtime(scrutinee.clone())?;
                self.bind_payload_patterns(env, items, tuple)?;
            }
            RirPattern::Record { fields, .. } => {
                let record = self.value_as_runtime(scrutinee.clone())?;
                self.bind_record_patterns(env, fields, record)?;
            }
            RirPattern::Wildcard
            | RirPattern::Constructor { .. }
            | RirPattern::Unit
            | RirPattern::Bool(_)
            | RirPattern::Int(_)
            | RirPattern::String(_) => {}
        }
        Ok(())
    }

    fn bind_record_patterns(
        &mut self,
        env: &mut Env<'ctx>,
        fields: &[(String, RirPattern)],
        record: IntValue<'ctx>,
    ) -> miette::Result<()> {
        for (field, pattern) in fields {
            let (field_ptr, field_len) = self.string_literal(field)?;
            let value = self.call_runtime_value(
                "riot_rt_value_record_get",
                &[record.into(), field_ptr.into(), field_len.into()],
            )?;
            self.bind_pattern_env(env, pattern, &CgValue::Value(value))?;
        }
        Ok(())
    }

    fn bind_payload_patterns(
        &mut self,
        env: &mut Env<'ctx>,
        patterns: &[RirPattern],
        payload: IntValue<'ctx>,
    ) -> miette::Result<()> {
        match patterns {
            [] => Ok(()),
            [pattern] => self.bind_pattern_env(env, pattern, &CgValue::Value(payload)),
            patterns => {
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
                    self.bind_pattern_env(env, pattern, &CgValue::Value(item))?;
                }
                Ok(())
            }
        }
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
        let (type_ptr, type_len) = self.string_literal(type_name.as_str())?;
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
        items: &[RirExpr],
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
        body: &RirBlock,
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
        callee: &RirExpr,
        args: &[RirExpr],
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
        body: &RirBlock,
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
                let nested = RirExpr::Lambda {
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
        fields: &[(String, RirExpr)],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let path = path.join(".");
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
        if let Some(value) = resolve_static_path(path, &env.statics) {
            return Ok(CgValue::Static(value));
        }
        bail!("unknown value `{}`", path.join("."))
    }

    fn emit_call(
        &mut self,
        callee: &[String],
        args: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        match callee {
            [name] if name == "dbg" || name == "println" => self.emit_output(name, args, env),
            [name] if name == "send" => self.emit_send(args, env),
            [name] if name == "monitor" => {
                self.emit_actor_id_runtime_call("riot_rt_monitor", args, env)
            }
            [name] if name == "link" => self.emit_actor_id_runtime_call("riot_rt_link", args, env),
            [name] if self.externals.contains_key(name.as_str()) => {
                let external = *self.externals.get(name.as_str()).unwrap();
                self.emit_external_call(
                    &external.abi,
                    &external.params,
                    &external.result,
                    args,
                    env,
                )
            }
            [name] if name == "list_len" => self.emit_list_len(args, env),
            [name] if name == "list_get" => self.emit_list_get(args, env),
            [name] if name == "string_len" => self.emit_string_len(args, env),
            [name] if name == "string_concat" => self.emit_string_concat(args, env),
            [name] if self.functions.contains_key(name) => self.emit_local_call(name, args, env),
            [name] => {
                if let Some(static_value) = self.static_eval_call(name, args, &env.statics) {
                    Ok(CgValue::Static(static_value))
                } else {
                    bail!("cannot lower call to `{name}` with the currently inferred ABI")
                }
            }
            [module, name] => self.emit_imported_call(module, name, args, env),
            _ => bail!("unsupported call path `{}`", callee.join(".")),
        }
    }

    fn emit_list_len(
        &mut self,
        args: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if args.len() != 1 {
            bail!("list_len expects one argument");
        }
        let list = self.emit_expr(&args[0], env)?;
        let list = self.value_as_runtime(list)?;
        self.push_runtime_root(list)?;
        let len = self.call_runtime_value("riot_rt_value_list_len", &[list.into()])?;
        self.pop_runtime_roots(1)?;
        Ok(CgValue::I64(len))
    }

    fn emit_list_get(
        &mut self,
        args: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if args.len() != 2 {
            bail!("list_get expects two arguments");
        }
        let list = self.emit_expr(&args[0], env)?;
        let list = self.value_as_runtime(list)?;
        self.push_runtime_root(list)?;
        let index = self.emit_i64(&args[1], env)?;
        let value =
            self.call_runtime_value("riot_rt_value_list_get", &[list.into(), index.into()])?;
        self.pop_runtime_roots(1)?;
        Ok(CgValue::Value(value))
    }

    fn emit_string_len(
        &mut self,
        args: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if args.len() != 1 {
            bail!("string_len expects one argument");
        }
        let string = self.emit_expr(&args[0], env)?;
        let string = self.value_as_runtime(string)?;
        self.push_runtime_root(string)?;
        let len = self.call_runtime_value("riot_rt_value_string_len", &[string.into()])?;
        self.pop_runtime_roots(1)?;
        Ok(CgValue::I64(len))
    }

    fn emit_string_concat(
        &mut self,
        args: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if args.len() != 2 {
            bail!("string_concat expects two arguments");
        }
        let lhs = self.emit_expr(&args[0], env)?;
        let lhs = self.value_as_runtime(lhs)?;
        self.push_runtime_root(lhs)?;
        let rhs = self.emit_expr(&args[1], env)?;
        let rhs = self.value_as_runtime(rhs)?;
        self.push_runtime_root(rhs)?;
        let value =
            self.call_runtime_value("riot_rt_value_string_concat", &[lhs.into(), rhs.into()])?;
        self.pop_runtime_roots(2)?;
        Ok(CgValue::Value(value))
    }

    fn emit_local_call(
        &mut self,
        name: &str,
        args: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let function = self.functions[name];
        let abi = self.function_abis[name].clone();
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
        args: &[RirExpr],
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
            RsigExport::External(external) => self.emit_external_call(
                &external.abi,
                &external.params,
                &external.result,
                args,
                env,
            ),
        }
    }

    fn emit_declared_function_call(
        &mut self,
        symbol: &str,
        params: &[RsigType],
        result: &RsigType,
        args: &[RirExpr],
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
        args: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if params.len() != args.len() {
            bail!("external `{symbol}` called with wrong arity");
        }
        let function =
            self.get_or_add_function(symbol, self.external_function_type(params, result)?);
        let mut call_args = Vec::new();
        let mut rooted_values = 0;
        for (arg, param) in args.iter().zip(params) {
            rooted_values += self.push_external_arg(&mut call_args, arg, param, env)?;
        }
        let call = self
            .builder
            .build_call(function, &call_args, &format!("call_{symbol}"))
            .map_err(|error| miette::miette!("failed to emit external call `{symbol}`: {error}"))?;
        let result = self.call_result(
            call.try_as_basic_value().basic(),
            external_result_abi(result),
        );
        self.pop_runtime_roots(rooted_values)?;
        result
    }

    fn emit_output(
        &mut self,
        name: &str,
        args: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        let Some(arg) = args.first() else {
            bail!("{name} expects one argument");
        };
        let value = self.emit_expr(arg, env)?;
        match value {
            CgValue::I64(value) => {
                let symbol = if name == "println" {
                    "riot_rt_println_i64"
                } else {
                    "riot_rt_dbg_i64"
                };
                self.call_void_runtime(symbol, &[value.into()])?;
            }
            CgValue::Bool(value) => self.emit_bool_output(name, value)?,
            CgValue::Value(value) => {
                self.push_runtime_root(value)?;
                let external_string_abi = self
                    .externals
                    .get(name)
                    .filter(|external| matches!(external.params.as_slice(), [RsigType::String]))
                    .map(|external| external.abi.clone());
                if let Some(abi) = external_string_abi {
                    let (ptr, len) = self.value_as_bytes(CgValue::Value(value))?;
                    self.call_void_runtime(&abi, &[ptr.into(), len.into()])?;
                } else {
                    let symbol = if name == "println" {
                        "riot_rt_println_value"
                    } else {
                        "riot_rt_dbg_value"
                    };
                    self.call_void_runtime(symbol, &[value.into()])?;
                }
                self.pop_runtime_roots(1)?;
            }
            CgValue::Static(value) => {
                let rendered = value.to_print_string();
                let (ptr, len) = self.string_literal(&rendered)?;
                let symbol = self.output_string_symbol(name);
                self.call_void_runtime(symbol, &[ptr.into(), len.into()])?;
            }
            CgValue::Unit => {
                let (ptr, len) = self.string_literal("()")?;
                let symbol = self.output_string_symbol(name);
                self.call_void_runtime(symbol, &[ptr.into(), len.into()])?;
            }
            CgValue::ActorId(actor_id) => {
                let symbol = if name == "println" {
                    "riot_rt_println_i64"
                } else {
                    "riot_rt_dbg_i64"
                };
                self.call_void_runtime(symbol, &[actor_id.into()])?;
            }
        }
        Ok(CgValue::Unit)
    }

    fn emit_bool_output(&mut self, name: &str, value: IntValue<'ctx>) -> miette::Result<()> {
        let Some(parent) = self.current_function() else {
            bail!("bool output emitted without an active function");
        };
        let true_block = self.context.append_basic_block(parent, "bool.true");
        let false_block = self.context.append_basic_block(parent, "bool.false");
        let cont_block = self.context.append_basic_block(parent, "bool.cont");
        self.builder
            .build_conditional_branch(value, true_block, false_block)
            .map_err(|error| miette::miette!("failed to emit bool output branch: {error}"))?;

        self.builder.position_at_end(true_block);
        let (ptr, len) = self.string_literal("true")?;
        let symbol = self.output_string_symbol(name);
        self.call_void_runtime(symbol, &[ptr.into(), len.into()])?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|error| miette::miette!("failed to emit bool output branch: {error}"))?;

        self.builder.position_at_end(false_block);
        let (ptr, len) = self.string_literal("false")?;
        let symbol = self.output_string_symbol(name);
        self.call_void_runtime(symbol, &[ptr.into(), len.into()])?;
        self.builder
            .build_unconditional_branch(cont_block)
            .map_err(|error| miette::miette!("failed to emit bool output branch: {error}"))?;

        self.builder.position_at_end(cont_block);
        Ok(())
    }

    fn emit_send(
        &mut self,
        args: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if args.len() != 2 {
            bail!("send expects two arguments");
        }
        let actor_id = self.emit_expr(&args[0], env)?;
        let actor_id = self.value_as_actor_id(actor_id)?;
        let message = self.emit_expr(&args[1], env)?;
        match message {
            CgValue::Value(value) => {
                self.push_runtime_root(value)?;
                self.call_void_runtime("riot_rt_send_value", &[actor_id.into(), value.into()])?;
                self.pop_runtime_roots(1)?;
            }
            CgValue::I64(value) => {
                self.call_void_runtime("riot_rt_send_i64", &[actor_id.into(), value.into()])?;
            }
            CgValue::ActorId(value) => {
                self.call_void_runtime("riot_rt_send_actor_id", &[actor_id.into(), value.into()])?;
            }
            CgValue::Static(value) => {
                let rendered = value.to_print_string();
                let (ptr, len) = self.string_literal(&rendered)?;
                self.call_void_runtime("riot_rt_send", &[actor_id.into(), ptr.into(), len.into()])?;
            }
            CgValue::Bool(value) => {
                self.call_void_runtime("riot_rt_send_bool", &[actor_id.into(), value.into()])?;
            }
            CgValue::Unit => {
                self.call_void_runtime("riot_rt_send_unit", &[actor_id.into()])?;
            }
        }
        Ok(CgValue::Unit)
    }

    fn emit_actor_id_runtime_call(
        &mut self,
        symbol: &str,
        args: &[RirExpr],
        env: &mut Env<'ctx>,
    ) -> miette::Result<CgValue<'ctx>> {
        if args.len() != 1 {
            bail!("{symbol} expects one argument");
        }
        let actor_id = self.emit_expr(&args[0], env)?;
        let actor_id = self.value_as_actor_id(actor_id)?;
        self.call_void_runtime(symbol, &[actor_id.into()])?;
        Ok(CgValue::Unit)
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
                crate::ir::ActorStateNext::State(index) => index,
                crate::ir::ActorStateNext::Done => shape.states.len(),
            };
            match &state.op {
                ActorFrameOp::Let { name, value } => {
                    let value = self.emit_expr(value, &mut env)?;
                    let slot = shape
                        .slots
                        .iter()
                        .find(|slot| slot.name == *name)
                        .ok_or_else(|| miette::miette!("missing actor slot `{name}`"))?;
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
                        self.bind_pattern_env(
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

    fn emit_i64(&mut self, expr: &RirExpr, env: &mut Env<'ctx>) -> miette::Result<IntValue<'ctx>> {
        let value = self.emit_expr(expr, env)?;
        self.value_as_i64(value)
    }

    fn emit_bool(&mut self, expr: &RirExpr, env: &mut Env<'ctx>) -> miette::Result<IntValue<'ctx>> {
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
        arg: &RirExpr,
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
        output: &mut Vec<BasicMetadataValueEnum<'ctx>>,
        arg: &RirExpr,
        type_: &RsigType,
        env: &mut Env<'ctx>,
    ) -> miette::Result<usize> {
        match type_ {
            RsigType::String => {
                let value = self.emit_expr(arg, env)?;
                let roots = self.push_optional_runtime_root(&value)?;
                let (ptr, len) = self.value_as_bytes(value)?;
                output.push(ptr.into());
                output.push(len.into());
                Ok(roots)
            }
            RsigType::I64 => {
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
            type_ if external_param_is_boxed(type_) => {
                let value = self.emit_expr(arg, env)?;
                let value = self.value_as_runtime(value)?;
                self.push_runtime_root(value)?;
                output.push(value.into());
                Ok(1)
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
            "riot_rt_value_tuple_get" | "riot_rt_value_list_get" => {
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
        params: &[RsigType],
        result: &RsigType,
    ) -> miette::Result<FunctionType<'ctx>> {
        let mut llvm_params = Vec::new();
        for param in params {
            match param {
                RsigType::String => {
                    llvm_params.push(self.ptr_type().into());
                    llvm_params.push(self.context.i64_type().into());
                }
                RsigType::I64 | RsigType::ActorId(_) => {
                    llvm_params.push(self.context.i64_type().into())
                }
                RsigType::Bool => llvm_params.push(self.context.bool_type().into()),
                type_ if external_param_is_boxed(type_) => {
                    llvm_params.push(self.context.i64_type().into())
                }
                _ => bail!(
                    "unsupported external parameter type `{}`",
                    param.canonical()
                ),
            }
        }
        match external_result_abi(result) {
            AbiType::Unit => Ok(self.context.void_type().fn_type(&llvm_params, false)),
            AbiType::I64 | AbiType::ActorId | AbiType::Bool | AbiType::Value => Ok(self
                .basic_type_for_abi(external_result_abi(result))?
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

    fn output_string_symbol(&self, name: &str) -> &str {
        if let Some(external) = self.externals.get(name) {
            return &external.abi;
        }
        match name {
            "println" => "riot_rt_println",
            _ => "riot_rt_dbg",
        }
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
        expr: &RirExpr,
        bindings: &HashMap<String, StaticValue>,
    ) -> Option<StaticValue> {
        static_eval_expr(expr, bindings, &self.function_map, 0)
    }

    fn static_eval_call(
        &self,
        name: &str,
        args: &[RirExpr],
        bindings: &HashMap<String, StaticValue>,
    ) -> Option<StaticValue> {
        static_eval_call(name, args, bindings, &self.function_map, 0)
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

fn frame_slot_from_actor_ir(slot: &crate::ir::ActorFrameSlot) -> FrameSlot {
    FrameSlot {
        name: slot.name.clone(),
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

fn pattern_is_irrefutable(pattern: &RirPattern) -> bool {
    matches!(pattern, RirPattern::Wildcard | RirPattern::Bind { .. })
}

fn external_param_is_boxed(type_: &RsigType) -> bool {
    matches!(
        type_,
        RsigType::Arrow { .. }
            | RsigType::List(_)
            | RsigType::Record(_)
            | RsigType::Tuple(_)
            | RsigType::Unknown
            | RsigType::Var(_)
            | RsigType::Variant(_)
    )
}

fn external_result_abi(type_: &RsigType) -> AbiType {
    if external_result_is_boxed(type_) {
        AbiType::Value
    } else {
        AbiType::from_rsig(type_)
    }
}

fn external_result_is_boxed(type_: &RsigType) -> bool {
    matches!(
        type_,
        RsigType::Arrow { .. }
            | RsigType::List(_)
            | RsigType::Record(_)
            | RsigType::String
            | RsigType::Tuple(_)
            | RsigType::Unknown
            | RsigType::Var(_)
            | RsigType::Variant(_)
    )
}

fn infer_function_abis(program: &RirProgram) -> HashMap<String, FunctionAbi> {
    let mut abis = program
        .functions
        .iter()
        .map(|function| {
            (
                function.name.clone(),
                FunctionAbi {
                    params: function
                        .param_types
                        .iter()
                        .map(AbiType::from_rsig)
                        .collect(),
                    result: AbiType::from_rsig(&function.result),
                },
            )
        })
        .collect::<HashMap<_, _>>();

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
    block: &RirBlock,
    params: &[Param],
    locals: &mut HashMap<String, AbiType>,
    param_types: &mut [AbiType],
    functions: &HashMap<String, FunctionAbi>,
) -> AbiType {
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => {
                let abi = infer_expr_abi(value, locals, functions);
                locals.insert(name.as_str().to_owned(), abi);
            }
            RirStmt::Expr(expr) => {
                mark_expr_constraints(expr, params, locals, param_types, functions);
            }
        }
    }
    if let Some(tail) = &block.tail {
        mark_expr_constraints(tail, params, locals, param_types, functions);
        infer_expr_abi(tail, locals, functions)
    } else {
        AbiType::Unit
    }
}

fn infer_expr_abi(
    expr: &RirExpr,
    locals: &HashMap<String, AbiType>,
    functions: &HashMap<String, FunctionAbi>,
) -> AbiType {
    match expr {
        RirExpr::Add(_, _)
        | RirExpr::Sub(_, _)
        | RirExpr::Mul(_, _)
        | RirExpr::Div(_, _)
        | RirExpr::Mod(_, _)
        | RirExpr::Neg(_)
        | RirExpr::Int(_) => AbiType::I64,
        RirExpr::Eq(_, _)
        | RirExpr::Lt(_, _)
        | RirExpr::And(_, _)
        | RirExpr::Or(_, _)
        | RirExpr::Not(_)
        | RirExpr::Bool(_) => AbiType::Bool,
        RirExpr::If {
            then_branch,
            else_branch,
            ..
        } => unify_abi(
            infer_expr_abi(then_branch, locals, functions),
            infer_expr_abi(else_branch, locals, functions),
        ),
        RirExpr::Match { arms, .. } => arms
            .iter()
            .map(|arm| infer_expr_abi(&arm.body, locals, functions))
            .fold(AbiType::Unknown, unify_abi),
        RirExpr::Block(block) => infer_block_expr_abi(block, locals, functions),
        RirExpr::Call { callee, .. } => match callee.as_slice() {
            [name] if name == "dbg" || name == "println" => AbiType::Unit,
            [name] if name == "send" || name == "monitor" || name == "link" => AbiType::Unit,
            [name] if name == "list_len" || name == "string_len" => AbiType::I64,
            [name] if name == "list_get" || name == "string_concat" => AbiType::Value,
            [name] => functions
                .get(name)
                .map(|abi| abi.result)
                .unwrap_or(AbiType::Unknown),
            _ => AbiType::Unknown,
        },
        RirExpr::Unit => AbiType::Unit,
        RirExpr::Apply { result, .. } => AbiType::from_rsig(result),
        RirExpr::Lambda { .. } => AbiType::Value,
        RirExpr::Path(path) => path
            .first()
            .and_then(|name| locals.get(name))
            .copied()
            .unwrap_or(AbiType::Unknown),
        RirExpr::Tuple(_)
        | RirExpr::List(_)
        | RirExpr::Record { .. }
        | RirExpr::Variant { .. }
        | RirExpr::Field { .. }
        | RirExpr::TupleIndex { .. }
        | RirExpr::String(_) => AbiType::Value,
        RirExpr::Spawn { .. } => AbiType::ActorId,
        RirExpr::Receive { .. } | RirExpr::Char(_) | RirExpr::Float(_) => AbiType::Unknown,
    }
}

fn infer_block_expr_abi(
    block: &RirBlock,
    outer_locals: &HashMap<String, AbiType>,
    functions: &HashMap<String, FunctionAbi>,
) -> AbiType {
    let mut locals = outer_locals.clone();
    let mut param_types = Vec::new();
    infer_block_abi(block, &[], &mut locals, &mut param_types, functions)
}

fn mark_expr_constraints(
    expr: &RirExpr,
    params: &[Param],
    locals: &mut HashMap<String, AbiType>,
    param_types: &mut [AbiType],
    functions: &HashMap<String, FunctionAbi>,
) {
    match expr {
        RirExpr::Add(lhs, rhs)
        | RirExpr::Sub(lhs, rhs)
        | RirExpr::Mul(lhs, rhs)
        | RirExpr::Div(lhs, rhs)
        | RirExpr::Mod(lhs, rhs)
        | RirExpr::Lt(lhs, rhs) => {
            mark_expr_as(params, locals, param_types, lhs, AbiType::I64);
            mark_expr_as(params, locals, param_types, rhs, AbiType::I64);
        }
        RirExpr::Neg(value) => mark_expr_as(params, locals, param_types, value, AbiType::I64),
        RirExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            mark_expr_constraints(condition, params, locals, param_types, functions);
            mark_expr_constraints(then_branch, params, locals, param_types, functions);
            mark_expr_constraints(else_branch, params, locals, param_types, functions);
        }
        RirExpr::Match { scrutinee, arms } => {
            mark_expr_constraints(scrutinee, params, locals, param_types, functions);
            for arm in arms {
                mark_expr_constraints(&arm.body, params, locals, param_types, functions);
            }
        }
        RirExpr::Block(block) => {
            let mut nested_locals = locals.clone();
            let mut nested_params = Vec::new();
            infer_block_abi(
                block,
                &[],
                &mut nested_locals,
                &mut nested_params,
                functions,
            );
        }
        RirExpr::Call { args, .. } => {
            for arg in args {
                mark_expr_constraints(arg, params, locals, param_types, functions);
            }
        }
        RirExpr::Apply { callee, args, .. } => {
            mark_expr_constraints(callee, params, locals, param_types, functions);
            for arg in args {
                mark_expr_constraints(arg, params, locals, param_types, functions);
            }
        }
        RirExpr::Lambda {
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
            );
        }
        RirExpr::Tuple(items) | RirExpr::List(items) => {
            for item in items {
                mark_expr_constraints(item, params, locals, param_types, functions);
            }
        }
        RirExpr::Record { fields, .. } => {
            for (_, value) in fields {
                mark_expr_constraints(value, params, locals, param_types, functions);
            }
        }
        RirExpr::Field { base, .. } | RirExpr::TupleIndex { base, .. } => {
            mark_expr_constraints(base, params, locals, param_types, functions);
        }
        RirExpr::And(lhs, rhs) | RirExpr::Or(lhs, rhs) | RirExpr::Eq(lhs, rhs) => {
            mark_expr_constraints(lhs, params, locals, param_types, functions);
            mark_expr_constraints(rhs, params, locals, param_types, functions);
        }
        RirExpr::Not(value) => {
            mark_expr_constraints(value, params, locals, param_types, functions);
        }
        RirExpr::Receive { arms } => {
            for arm in arms {
                mark_expr_constraints(&arm.body, params, locals, param_types, functions);
            }
        }
        RirExpr::Spawn { body, .. } => {
            let mut nested_locals = HashMap::new();
            let mut nested_params = Vec::new();
            infer_block_abi(body, &[], &mut nested_locals, &mut nested_params, functions);
        }
        RirExpr::Variant { payload, .. } => {
            for value in payload {
                mark_expr_constraints(value, params, locals, param_types, functions);
            }
        }
        RirExpr::Bool(_)
        | RirExpr::Unit
        | RirExpr::Char(_)
        | RirExpr::Float(_)
        | RirExpr::Int(_)
        | RirExpr::Path(_)
        | RirExpr::String(_) => {}
    }
}

fn mark_expr_as(
    params: &[Param],
    locals: &mut HashMap<String, AbiType>,
    param_types: &mut [AbiType],
    expr: &RirExpr,
    abi: AbiType,
) {
    if let RirExpr::Block(block) = expr {
        if let Some(tail) = &block.tail {
            mark_expr_as(params, locals, param_types, tail, abi);
        }
        return;
    }

    if let RirExpr::Path(path) = expr
        && let [name] = path.as_slice()
    {
        locals.insert(name.clone(), abi);
        if let Some(index) = params.iter().position(|param| param.as_str() == name) {
            param_types[index] = unify_abi(param_types[index], abi);
        }
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

#[cfg(test)]
mod tests {
    use crate::ir::{
        BindingKey, Capture, Param, RirBlock, RirExpr, RirFunction, RirProgram, RirStmt,
    };
    use crate::signature::{ImportedSignatures, ModuleName, RsigType};

    use super::{CodegenMode, emit_llvm_text};

    #[test]
    fn lambda_values_lower_to_runtime_closure_calls() {
        let program = RirProgram {
            module_name: ModuleName::new("ClosureTest"),
            uses: Vec::new(),
            externals: Vec::new(),
            functions: vec![RirFunction {
                name: "main".to_owned(),
                params: Vec::new(),
                param_types: Vec::new(),
                result: RsigType::Unknown,
                body: RirBlock {
                    statements: vec![RirStmt::Let {
                        name: BindingKey::new("n"),
                        value: RirExpr::Int(1),
                    }],
                    tail: Some(RirExpr::Lambda {
                        params: vec![Param::new("x")],
                        captures: vec![Capture::new("n")],
                        body: Box::new(RirBlock {
                            statements: Vec::new(),
                            tail: Some(RirExpr::Path(vec!["n".to_owned()])),
                        }),
                    }),
                },
                symbol: "riot_mod_ClosureTest_main".to_owned(),
            }],
        };

        let llvm = emit_llvm_text(
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
        let program = RirProgram {
            module_name: ModuleName::new("ClosureApplyTest"),
            uses: Vec::new(),
            externals: Vec::new(),
            functions: vec![RirFunction {
                name: "main".to_owned(),
                params: Vec::new(),
                param_types: Vec::new(),
                result: RsigType::Unknown,
                body: RirBlock {
                    statements: Vec::new(),
                    tail: Some(RirExpr::Apply {
                        callee: Box::new(RirExpr::Lambda {
                            params: vec![Param::new("x")],
                            captures: Vec::new(),
                            body: Box::new(RirBlock {
                                statements: Vec::new(),
                                tail: Some(RirExpr::Add(
                                    Box::new(RirExpr::Path(vec!["x".to_owned()])),
                                    Box::new(RirExpr::Int(1)),
                                )),
                            }),
                        }),
                        args: vec![RirExpr::Int(41)],
                        result: RsigType::I64,
                    }),
                },
                symbol: "riot_mod_ClosureApplyTest_main".to_owned(),
            }],
        };

        let llvm = emit_llvm_text(
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
