use std::collections::{BTreeMap, BTreeSet};

use crate::ast::{
    AstBlock, AstDecl, AstExpr, AstPattern, AstProgram, AstStmt, AstTypeAnnotation, AstTypeBody,
    TextSpan,
};
use crate::signature::{
    ConstructorName, FieldName, ImportedSignatures, ModuleName, Rsig, RsigDependency, RsigExport,
    RsigExternal, RsigFunction, RsigRecordField, RsigType, RsigTypeDecl, RsigTypeDeclKind,
    RsigTypeScheme, RsigVariantConstructor, TypeName, TypeParamName,
};
use crate::type_lowerer::RsigTypeLowerer;

use crate::actor::air::{
    ActorFrameLayout, ActorFrameOp, ActorFrameSlot, ActorFrameSlotName, ActorFrameState,
    ActorIrActor, ActorIrProgram, ActorSlotType, ActorStateNext,
};
use crate::actor::frame::{
    ActorSlotTypeContext, bind_pattern_actor_slot_types, infer_actor_slot_type,
};
use crate::checker::tyir::{
    BindingId, EntityId, TypedBlock, TypedExpr, TypedExprKind, TypedExternal, TypedFunction,
    TypedLiteral, TypedMatchArm, TypedParam, TypedPattern, TypedProgram, TypedReceiveArm,
    TypedRecordField, TypedStmt, TypedTypeBody, TypedTypeDecl, TypedUse, TypedVariantConstructor,
};
use crate::lambda::closure::{bind_pattern_names, collect_free_expr};
use crate::lambda::ir::{
    BindingKey, Param, RirBlock, RirExpr, RirExternal, RirFunction, RirMatchArm, RirPattern,
    RirProgram, RirReceiveArm, RirStmt,
};

pub(crate) fn typed_program_from_ast(
    module_name: ModuleName,
    ast: AstProgram,
    imports: &ImportedSignatures,
    function_types: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
    expression_types: Option<&BTreeMap<TextSpan, RsigType>>,
) -> TypedProgram {
    let mut uses = Vec::new();
    let mut raw_externals = Vec::new();
    let mut types = Vec::new();
    let mut ast_functions = Vec::new();
    let predeclared_variants = declared_variant_names_from_ast(&ast, imports);

    for decl in ast.decls {
        match decl {
            AstDecl::Use(use_) => {
                let fingerprint = imports
                    .get(use_.name.as_str())
                    .map(|rsig| rsig.module_fingerprint)
                    .unwrap_or(0);
                uses.push(TypedUse {
                    name: ModuleName::new(use_.name),
                    fingerprint,
                });
            }
            AstDecl::External(external) => {
                raw_externals.push(external);
            }
            AstDecl::Module(_) | AstDecl::Include(_) => {}
            AstDecl::Type(type_) => {
                types.push(TypedTypeDecl {
                    name: TypeName::new(type_.name),
                    params: type_
                        .params
                        .into_iter()
                        .map(|param| TypeParamName::new(param.name))
                        .collect(),
                    body: match type_.body {
                        AstTypeBody::Abstract => TypedTypeBody::Abstract,
                        AstTypeBody::Variant { constructors } => TypedTypeBody::Variant {
                            constructors: constructors
                                .into_iter()
                                .map(|constructor| TypedVariantConstructor {
                                    name: ConstructorName::new(constructor.name),
                                    payload: constructor
                                        .payload
                                        .into_iter()
                                        .map(|payload| {
                                            lower_type_annotation(&payload, &predeclared_variants)
                                        })
                                        .collect(),
                                })
                                .collect(),
                        },
                        AstTypeBody::Record { fields } => TypedTypeBody::Record {
                            fields: fields
                                .into_iter()
                                .map(|field| TypedRecordField {
                                    name: FieldName::new(field.name),
                                    type_: lower_type_annotation(
                                        &field.type_annotation,
                                        &predeclared_variants,
                                    ),
                                })
                                .collect(),
                        },
                    },
                });
            }
            AstDecl::Function(function) => ast_functions.push(function),
        }
    }
    let declared_variants = declared_variant_names(&types, &uses, imports);
    let externals = raw_externals
        .into_iter()
        .map(|external| {
            let (params, result) =
                lower_signature_annotation(&external.type_annotation, &declared_variants);
            TypedExternal {
                name: external.name,
                params,
                result,
                abi: external.abi,
            }
        })
        .collect::<Vec<_>>();
    let constructors = constructor_type_map(&types);
    let records = record_type_map(&types, &uses, imports);
    let record_fields = record_field_type_map(&types, &uses, imports);

    let external_types = externals
        .iter()
        .map(|external| {
            (
                external.name.clone(),
                (external.params.clone(), external.result.clone()),
            )
        })
        .collect::<BTreeMap<_, _>>();
    let mut functions = Vec::new();
    for function in ast_functions {
        let symbol = module_function_symbol(&module_name, &function.name);
        let (param_types, inferred_result) = function_types
            .get(&function.name)
            .cloned()
            .unwrap_or_else(|| {
                (
                    vec![RsigType::Unknown; function.params.len()],
                    RsigType::Unknown,
                )
            });
        let annotated_result = function
            .return_type
            .as_ref()
            .map(|annotation| lower_type_annotation(annotation, &declared_variants));
        let mut context = TypeContext::new(TypeContextInputs {
            function_types,
            externals: &external_types,
            imports,
            constructors: &constructors,
            records: &records,
            record_fields: &record_fields,
            declared_variants: &declared_variants,
            expression_types,
        });
        let params = function
            .params
            .iter()
            .enumerate()
            .map(|(index, name)| {
                let type_ = param_types.get(index).cloned().unwrap_or(RsigType::Unknown);
                TypedParam {
                    binding: context.bind(name, type_.clone()),
                    type_,
                }
            })
            .collect::<Vec<_>>();
        let body = type_block(function.body, &mut context);
        let result =
            annotated_result.unwrap_or_else(|| merge_types(inferred_result, body.type_.clone()));
        functions.push(TypedFunction {
            name: function.name,
            params,
            body,
            result,
            symbol,
        });
    }

    TypedProgram {
        module_name,
        uses,
        externals,
        types,
        functions,
    }
}

fn lower_type_annotation(
    annotation: &AstTypeAnnotation,
    declared_variants: &BTreeSet<TypeName>,
) -> RsigType {
    RsigTypeLowerer::new().lower(&annotation.syntax, declared_variants)
}

fn lower_signature_annotation(
    annotation: &AstTypeAnnotation,
    declared_variants: &BTreeSet<TypeName>,
) -> (Vec<RsigType>, RsigType) {
    RsigTypeLowerer::new().lower_signature(&annotation.syntax, declared_variants)
}

pub(crate) fn signature_for(program: &TypedProgram) -> Rsig {
    let types = program
        .types
        .iter()
        .map(|type_| RsigTypeDecl {
            name: type_.name.clone(),
            params: type_.params.clone(),
            body: match &type_.body {
                TypedTypeBody::Abstract => RsigTypeDeclKind::Abstract,
                TypedTypeBody::Variant { constructors } => RsigTypeDeclKind::Variant {
                    constructors: constructors
                        .iter()
                        .map(|constructor| RsigVariantConstructor {
                            name: constructor.name.clone(),
                            payload: constructor.payload.clone(),
                        })
                        .collect(),
                },
                TypedTypeBody::Record { fields } => RsigTypeDeclKind::Record {
                    fields: fields
                        .iter()
                        .map(|field| RsigRecordField {
                            name: field.name.clone(),
                            type_: field.type_.clone(),
                        })
                        .collect(),
                },
            },
            fingerprint: 0,
        })
        .collect::<Vec<_>>();
    let mut exports = Vec::new();
    for function in &program.functions {
        if function.name == "main" {
            continue;
        }
        exports.push(RsigExport::Function(RsigFunction {
            name: function.name.clone(),
            params: function
                .params
                .iter()
                .map(|param| param.type_.clone())
                .collect(),
            result: function.result.clone(),
            scheme: RsigTypeScheme::from_signature(
                &function
                    .params
                    .iter()
                    .map(|param| param.type_.clone())
                    .collect::<Vec<_>>(),
                &function.result,
            ),
            symbol: function.symbol.clone(),
            fingerprint: 0,
        }));
    }
    for external in &program.externals {
        exports.push(RsigExport::External(RsigExternal {
            name: external.name.clone(),
            params: external.params.clone(),
            result: external.result.clone(),
            scheme: RsigTypeScheme::from_signature(&external.params, &external.result),
            abi: external.abi.clone(),
            fingerprint: 0,
        }));
    }
    let dependencies = program
        .uses
        .iter()
        .map(|use_| RsigDependency {
            module: use_.name.clone(),
            fingerprint: use_.fingerprint,
        })
        .collect();
    Rsig::with_dependencies(program.module_name.clone(), dependencies, types, exports)
}

pub(crate) fn lower_typed_to_rir(program: TypedProgram) -> RirProgram {
    let mut context = LowerContext::default();
    let lowered = RirProgram {
        module_name: program.module_name,
        uses: program.uses.into_iter().map(|use_| use_.name).collect(),
        externals: program
            .externals
            .into_iter()
            .map(|external| RirExternal {
                name: external.name,
                params: external.params,
                result: external.result,
                abi: external.abi,
            })
            .collect(),
        functions: program
            .functions
            .into_iter()
            .map(|function| {
                context.push_scope();
                let mut params = Vec::new();
                let mut param_types = Vec::new();
                for param in function.params {
                    let key = context.bind_existing(&param.binding);
                    params.push(Param::from_key(key));
                    param_types.push(param.type_);
                }
                let body = lower_block(function.body, &mut context);
                context.pop_scope();
                RirFunction {
                    name: function.name,
                    params,
                    param_types,
                    result: function.result,
                    body,
                    symbol: function.symbol,
                }
            })
            .collect(),
    };
    crate::lambda::closure::closure_convert_program(lowered)
}

pub(crate) fn lower_rir_to_actor_ir(
    program: &RirProgram,
    imports: &ImportedSignatures,
) -> ActorIrProgram {
    let mut actors = Vec::new();
    let context = ActorSlotTypeContext::from_program(program, imports);
    for function in &program.functions {
        let mut locals = function
            .params
            .iter()
            .zip(&function.param_types)
            .map(|(name, type_)| (name.as_str().to_owned(), ActorSlotType::from_rsig(type_)))
            .collect::<BTreeMap<_, _>>();
        collect_actors_from_block(&function.body, &mut locals, &context, &mut actors);
    }
    ActorIrProgram { actors }
}

fn module_function_symbol(module: &ModuleName, name: &str) -> String {
    format!("riot_mod_{module}_{name}")
}

#[derive(Debug, Clone)]
struct TypedBindingInfo {
    binding: BindingId,
    type_: RsigType,
}

#[derive(Debug, Clone)]
struct ConstructorSignature {
    type_name: TypeName,
    payload: Vec<RsigType>,
}

struct TypeContext<'a> {
    next_binding_id: usize,
    scopes: Vec<BTreeMap<String, TypedBindingInfo>>,
    functions: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    externals: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    imports: &'a ImportedSignatures,
    constructors: &'a BTreeMap<String, ConstructorSignature>,
    records: &'a BTreeMap<String, TypeName>,
    record_fields: &'a BTreeMap<TypeName, BTreeMap<String, RsigType>>,
    declared_variants: &'a BTreeSet<TypeName>,
    expression_types: Option<&'a BTreeMap<TextSpan, RsigType>>,
}

struct TypeContextInputs<'a> {
    function_types: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    externals: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    imports: &'a ImportedSignatures,
    constructors: &'a BTreeMap<String, ConstructorSignature>,
    records: &'a BTreeMap<String, TypeName>,
    record_fields: &'a BTreeMap<TypeName, BTreeMap<String, RsigType>>,
    declared_variants: &'a BTreeSet<TypeName>,
    expression_types: Option<&'a BTreeMap<TextSpan, RsigType>>,
}

impl<'a> TypeContext<'a> {
    fn new(inputs: TypeContextInputs<'a>) -> Self {
        Self {
            next_binding_id: 0,
            scopes: vec![BTreeMap::new()],
            functions: inputs.function_types,
            externals: inputs.externals,
            imports: inputs.imports,
            constructors: inputs.constructors,
            records: inputs.records,
            record_fields: inputs.record_fields,
            declared_variants: inputs.declared_variants,
            expression_types: inputs.expression_types,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn bind(&mut self, name: &str, type_: RsigType) -> BindingId {
        if self.scopes.is_empty() {
            self.push_scope();
        }
        let binding = BindingId::new(name, self.next_binding_id);
        self.next_binding_id += 1;
        self.scopes
            .last_mut()
            .expect("typed HIR construction always has a lexical scope")
            .insert(
                name.to_owned(),
                TypedBindingInfo {
                    binding: binding.clone(),
                    type_,
                },
            );
        binding
    }

    fn resolve(&self, name: &str) -> Option<&TypedBindingInfo> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    fn expression_type(&self, span: TextSpan) -> Option<RsigType> {
        self.expression_types
            .and_then(|types| types.get(&span))
            .cloned()
    }
}

fn constructor_type_map(types: &[TypedTypeDecl]) -> BTreeMap<String, ConstructorSignature> {
    let mut constructors = crate::stdlib::Stdlib::new()
        .prelude_signature()
        .ok()
        .map(|rsig| constructor_type_map_from_rsig(&rsig, None))
        .unwrap_or_default();
    constructors.extend(types.iter().flat_map(|type_| {
        let TypedTypeBody::Variant { constructors } = &type_.body else {
            return Vec::new();
        };
        constructors
            .iter()
            .map(|constructor| {
                (
                    constructor.name.as_str().to_owned(),
                    ConstructorSignature {
                        type_name: type_.name.clone(),
                        payload: constructor.payload.clone(),
                    },
                )
            })
            .collect::<Vec<_>>()
    }));
    constructors
}

fn constructor_type_map_from_rsig(
    rsig: &Rsig,
    module: Option<&str>,
) -> BTreeMap<String, ConstructorSignature> {
    rsig.types
        .iter()
        .flat_map(|type_| {
            let RsigTypeDeclKind::Variant { constructors } = &type_.body else {
                return Vec::new();
            };
            let type_name = module
                .map(|module| imported_type_name(module, &type_.name))
                .unwrap_or_else(|| type_.name.clone());
            constructors
                .iter()
                .map(move |constructor| {
                    (
                        constructor.name.as_str().to_owned(),
                        ConstructorSignature {
                            type_name: type_name.clone(),
                            payload: constructor.payload.clone(),
                        },
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn record_type_map(
    types: &[TypedTypeDecl],
    uses: &[TypedUse],
    imports: &ImportedSignatures,
) -> BTreeMap<String, TypeName> {
    let mut records = BTreeMap::new();
    for type_ in types {
        if matches!(type_.body, TypedTypeBody::Record { .. }) {
            insert_record_type_aliases(&mut records, None, &type_.name, type_.name.clone());
        }
    }
    for use_ in uses {
        if let Some(rsig) = imports.get(use_.name.as_str()) {
            for type_ in &rsig.types {
                if matches!(type_.body, RsigTypeDeclKind::Record { .. }) {
                    insert_record_type_aliases(
                        &mut records,
                        Some(use_.name.as_str()),
                        &type_.name,
                        imported_type_name(use_.name.as_str(), &type_.name),
                    );
                }
            }
        }
    }
    records
}

fn record_field_type_map(
    types: &[TypedTypeDecl],
    uses: &[TypedUse],
    imports: &ImportedSignatures,
) -> BTreeMap<TypeName, BTreeMap<String, RsigType>> {
    let mut fields_by_type = BTreeMap::new();
    for type_ in types {
        let TypedTypeBody::Record { fields } = &type_.body else {
            continue;
        };
        fields_by_type.insert(
            type_.name.clone(),
            fields
                .iter()
                .map(|field| (field.name.as_str().to_owned(), field.type_.clone()))
                .collect(),
        );
    }
    for use_ in uses {
        let Some(rsig) = imports.get(use_.name.as_str()) else {
            continue;
        };
        for type_ in &rsig.types {
            let RsigTypeDeclKind::Record { fields } = &type_.body else {
                continue;
            };
            fields_by_type.insert(
                imported_type_name(use_.name.as_str(), &type_.name),
                fields
                    .iter()
                    .map(|field| (field.name.as_str().to_owned(), field.type_.clone()))
                    .collect(),
            );
        }
    }
    fields_by_type
}

fn insert_record_type_aliases(
    records: &mut BTreeMap<String, TypeName>,
    module: Option<&str>,
    source_name: &TypeName,
    resolved_name: TypeName,
) {
    let base = source_name.as_str();
    records.insert(base.to_owned(), resolved_name.clone());
    records.insert(type_constructor_name(base), resolved_name.clone());
    if let Some(module) = module {
        records.insert(format!("{module}.{base}"), resolved_name.clone());
        records.insert(
            format!("{module}.{}", type_constructor_name(base)),
            resolved_name,
        );
    }
}

fn type_constructor_name(type_name: &str) -> String {
    let mut output = String::new();
    let mut capitalize = true;
    for ch in type_name.chars() {
        if ch == '_' || ch == '-' || ch == '.' {
            capitalize = true;
        } else if capitalize && ch.is_ascii_lowercase() {
            output.push(ch.to_ascii_uppercase());
            capitalize = false;
        } else {
            output.push(ch);
            capitalize = false;
        }
    }
    output
}

fn declared_variant_names(
    types: &[TypedTypeDecl],
    uses: &[TypedUse],
    imports: &ImportedSignatures,
) -> BTreeSet<TypeName> {
    let mut names = prelude_variant_names();
    for type_ in types {
        if matches!(type_.body, TypedTypeBody::Variant { .. }) {
            names.insert(type_.name.clone());
        }
    }
    for use_ in uses {
        if let Some(rsig) = imports.get(use_.name.as_str()) {
            for type_ in &rsig.types {
                if matches!(type_.body, RsigTypeDeclKind::Variant { .. }) {
                    names.insert(imported_type_name(use_.name.as_str(), &type_.name));
                }
            }
        }
    }
    names
}

fn declared_variant_names_from_ast(
    program: &AstProgram,
    imports: &ImportedSignatures,
) -> BTreeSet<TypeName> {
    let mut names = prelude_variant_names();
    for decl in &program.decls {
        match decl {
            AstDecl::Type(type_) if matches!(type_.body, AstTypeBody::Variant { .. }) => {
                names.insert(TypeName::new(type_.name.clone()));
            }
            AstDecl::Use(use_) => {
                if let Some(rsig) = imports.get(use_.name.as_str()) {
                    for type_ in &rsig.types {
                        if matches!(type_.body, RsigTypeDeclKind::Variant { .. }) {
                            names.insert(imported_type_name(use_.name.as_str(), &type_.name));
                        }
                    }
                }
            }
            AstDecl::Module(_)
            | AstDecl::Include(_)
            | AstDecl::Type(_)
            | AstDecl::External(_)
            | AstDecl::Function(_) => {}
        }
    }
    names
}

fn prelude_variant_names() -> BTreeSet<TypeName> {
    crate::stdlib::Stdlib::new()
        .prelude_signature()
        .ok()
        .map(|rsig| {
            rsig.types
                .into_iter()
                .filter_map(|type_| {
                    matches!(type_.body, RsigTypeDeclKind::Variant { .. }).then_some(type_.name)
                })
                .collect()
        })
        .unwrap_or_default()
}

fn type_block(block: AstBlock, context: &mut TypeContext<'_>) -> TypedBlock {
    context.push_scope();
    let mut statements = Vec::new();
    for stmt in block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                let mut value = type_expr(value, context);
                if let Some(annotation) = type_annotation {
                    value.type_ = lower_type_annotation(&annotation, context.declared_variants);
                }
                let binding = context.bind(&name, value.type_.clone());
                statements.push(TypedStmt::Let { binding, value });
            }
            AstStmt::Expr(expr) => statements.push(TypedStmt::Expr(type_expr(expr, context))),
        }
    }
    let tail = block.tail.map(|tail| type_expr(tail, context));
    let type_ = tail
        .as_ref()
        .map(|tail| tail.type_.clone())
        .unwrap_or(RsigType::Unit);
    let typed = TypedBlock {
        statements,
        tail,
        type_,
    };
    context.pop_scope();
    typed
}

fn type_expr(expr: AstExpr, context: &mut TypeContext<'_>) -> TypedExpr {
    let span = ast_expr_span(&expr);
    let mut typed = type_expr_inner(expr, context);
    if let Some(type_) = context.expression_type(span) {
        typed.type_ = type_;
    }
    typed
}

fn type_expr_inner(expr: AstExpr, context: &mut TypeContext<'_>) -> TypedExpr {
    match expr {
        AstExpr::Add { lhs, rhs, .. } => {
            typed_operator_call("(+)", vec![*lhs, *rhs], RsigType::I64, context)
        }
        AstExpr::Sub { lhs, rhs, .. } => {
            typed_operator_call("(-)", vec![*lhs, *rhs], RsigType::I64, context)
        }
        AstExpr::Mul { lhs, rhs, .. } => {
            typed_operator_call("(*)", vec![*lhs, *rhs], RsigType::I64, context)
        }
        AstExpr::Div { lhs, rhs, .. } => {
            typed_operator_call("(/)", vec![*lhs, *rhs], RsigType::I64, context)
        }
        AstExpr::Mod { lhs, rhs, .. } => {
            typed_operator_call("(%)", vec![*lhs, *rhs], RsigType::I64, context)
        }
        AstExpr::Neg { expr, .. } => {
            typed_operator_call("(-)", vec![*expr], RsigType::I64, context)
        }
        AstExpr::Eq { lhs, rhs, .. } => {
            typed_operator_call("(==)", vec![*lhs, *rhs], RsigType::Bool, context)
        }
        AstExpr::Lt { lhs, rhs, .. } => {
            typed_operator_call("(<)", vec![*lhs, *rhs], RsigType::Bool, context)
        }
        AstExpr::And { lhs, rhs, .. } => {
            typed_operator_call("(&&)", vec![*lhs, *rhs], RsigType::Bool, context)
        }
        AstExpr::Or { lhs, rhs, .. } => {
            typed_operator_call("(||)", vec![*lhs, *rhs], RsigType::Bool, context)
        }
        AstExpr::Not { expr, .. } => {
            typed_operator_call("(!)", vec![*expr], RsigType::Bool, context)
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            let condition = type_expr(*condition, context);
            let then_branch = type_expr(*then_branch, context);
            let else_branch = type_expr(*else_branch, context);
            let type_ = merge_types(then_branch.type_.clone(), else_branch.type_.clone());
            TypedExpr {
                type_,
                kind: TypedExprKind::If {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
            }
        }
        AstExpr::Match {
            scrutinee, arms, ..
        } => {
            let scrutinee = type_expr(*scrutinee, context);
            let mut typed_arms = Vec::with_capacity(arms.len());
            let mut type_ = RsigType::Unknown;
            for arm in arms {
                context.push_scope();
                let pattern = type_pattern(arm.pattern, &scrutinee.type_, context);
                let body = type_expr(arm.body, context);
                type_ = merge_types(type_, body.type_.clone());
                context.pop_scope();
                typed_arms.push(TypedMatchArm { pattern, body });
            }
            TypedExpr {
                type_,
                kind: TypedExprKind::Match {
                    scrutinee: Box::new(scrutinee),
                    arms: typed_arms,
                },
            }
        }
        AstExpr::Block { block, .. } => {
            let block = type_block(*block, context);
            TypedExpr {
                type_: block.type_.clone(),
                kind: TypedExprKind::Block(Box::new(block)),
            }
        }
        AstExpr::Bool { value, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::Literal(TypedLiteral::Bool(value)),
        },
        AstExpr::Call { callee, args, .. } => {
            let callee_path = callee.segments;
            let args = args
                .into_iter()
                .map(|arg| type_expr(arg, context))
                .collect::<Vec<_>>();
            if let Some(signature) = constructor_signature(&callee_path, context)
                && args.len() == signature.payload.len()
            {
                let constructor = callee_path
                    .last()
                    .map(|name| ConstructorName::new(name.clone()))
                    .unwrap_or_else(|| ConstructorName::new("_"));
                return TypedExpr {
                    type_: RsigType::Variant(signature.type_name.clone()),
                    kind: TypedExprKind::Constructor {
                        type_name: Some(signature.type_name),
                        constructor,
                        payload: args,
                    },
                };
            }
            if is_named_call(&callee_path, context) {
                if let Some((params, result)) = call_signature(&callee_path, context)
                    && args.len() < params.len()
                {
                    return partial_call_lambda(callee_path, args, params, result, context);
                }
                let type_ = call_result_type(&callee_path, context);
                TypedExpr {
                    type_,
                    kind: TypedExprKind::Call {
                        callee: EntityId::from_segments(callee_path),
                        args,
                    },
                }
            } else {
                let callee = type_path_expr(callee_path, context);
                typed_apply_expr(callee, args)
            }
        }
        AstExpr::Apply { callee, args, .. } => {
            let callee = type_expr(*callee, context);
            let args = args
                .into_iter()
                .map(|arg| type_expr(arg, context))
                .collect::<Vec<_>>();
            typed_apply_expr(callee, args)
        }
        AstExpr::Lambda {
            params,
            param_types,
            body,
            ..
        } => {
            context.push_scope();
            let typed_params = params
                .iter()
                .enumerate()
                .map(|(index, name)| {
                    let type_ = param_types
                        .get(index)
                        .and_then(|annotation| annotation.as_ref())
                        .map(|annotation| {
                            lower_type_annotation(annotation, context.declared_variants)
                        })
                        .unwrap_or(RsigType::Unknown);
                    let binding = context.bind(name, type_.clone());
                    TypedParam { binding, type_ }
                })
                .collect::<Vec<_>>();
            let body = type_block(*body, context);
            context.pop_scope();
            let type_ = rsig_source_function_type(
                typed_params
                    .iter()
                    .map(|param| param.type_.clone())
                    .collect(),
                body.type_.clone(),
            );
            TypedExpr {
                type_,
                kind: TypedExprKind::Lambda {
                    params: typed_params,
                    body: Box::new(body),
                },
            }
        }
        AstExpr::Spawn { body, .. } => TypedExpr {
            type_: RsigType::ActorId(Box::new(RsigType::Unknown)),
            kind: {
                let body = type_block(*body, context);
                TypedExprKind::Spawn {
                    body: Box::new(body),
                }
            },
        },
        AstExpr::Receive { arms, .. } => TypedExpr {
            type_: RsigType::Unit,
            kind: TypedExprKind::Receive {
                arms: arms
                    .into_iter()
                    .map(|arm| {
                        context.push_scope();
                        let pattern_type = receive_pattern_type(&arm.pattern, context);
                        let pattern = type_pattern(arm.pattern, &pattern_type, context);
                        let body = type_expr(arm.body, context);
                        context.pop_scope();
                        TypedReceiveArm { pattern, body }
                    })
                    .collect(),
            },
        },
        AstExpr::Unit { .. } => TypedExpr {
            type_: RsigType::Unit,
            kind: TypedExprKind::Constructor {
                type_name: None,
                constructor: ConstructorName::new("()"),
                payload: Vec::new(),
            },
        },
        AstExpr::Tuple { items, .. } => {
            let items = items
                .into_iter()
                .map(|item| type_expr(item, context))
                .collect::<Vec<_>>();
            let type_ = RsigType::Tuple(items.iter().map(|item| item.type_.clone()).collect());
            TypedExpr {
                type_,
                kind: TypedExprKind::Tuple(items),
            }
        }
        AstExpr::List { items, .. } => {
            let items = items
                .into_iter()
                .map(|item| type_expr(item, context))
                .collect::<Vec<_>>();
            let type_ = RsigType::List(Box::new(
                items
                    .first()
                    .map(|item| item.type_.clone())
                    .unwrap_or(RsigType::Unknown),
            ));
            TypedExpr {
                type_,
                kind: TypedExprKind::List(items),
            }
        }
        AstExpr::Record { path, fields, .. } => {
            let fields = fields
                .into_iter()
                .map(|(name, value)| (name, type_expr(value, context)))
                .collect::<Vec<_>>();
            let type_name = record_expr_type(&path.segments, context);
            TypedExpr {
                type_: RsigType::Record(type_name),
                kind: TypedExprKind::Record {
                    path: EntityId::from_segments(path.segments),
                    fields,
                },
            }
        }
        AstExpr::Field { base, field, .. } => {
            let base = type_expr(*base, context);
            let type_ = typed_field_type(&base, &field);
            TypedExpr {
                type_,
                kind: TypedExprKind::Field {
                    base: Box::new(base),
                    field,
                },
            }
        }
        AstExpr::TupleIndex { base, index, .. } => {
            let base = type_expr(*base, context);
            let type_ = typed_tuple_index_type(&base, index);
            TypedExpr {
                type_,
                kind: TypedExprKind::TupleIndex {
                    base: Box::new(base),
                    index,
                },
            }
        }
        AstExpr::Char { value, .. } => TypedExpr {
            type_: RsigType::Char,
            kind: TypedExprKind::Literal(TypedLiteral::Char(value)),
        },
        AstExpr::Float { value, .. } => TypedExpr {
            type_: RsigType::F64,
            kind: TypedExprKind::Literal(TypedLiteral::Float(value)),
        },
        AstExpr::Int { value, .. } => TypedExpr {
            type_: RsigType::I64,
            kind: TypedExprKind::Literal(TypedLiteral::Int(value)),
        },
        AstExpr::Path { path, .. } => type_path_expr(path.segments, context),
        AstExpr::String { value, .. } => TypedExpr {
            type_: RsigType::String,
            kind: TypedExprKind::Literal(TypedLiteral::String(value)),
        },
    }
}

fn type_path_expr(path: Vec<String>, context: &mut TypeContext<'_>) -> TypedExpr {
    let type_ = path_type(&path, context);
    if let [name] = path.as_slice()
        && let Some(binding) = context.resolve(name)
    {
        return TypedExpr {
            type_,
            kind: TypedExprKind::Local(binding.binding.clone()),
        };
    }
    if let Some((params, result)) = call_signature(&path, context) {
        return partial_call_lambda(path, Vec::new(), params, result, context);
    }
    if let Some((type_name, constructor)) = nullary_constructor_type(&path, context) {
        return TypedExpr {
            type_: RsigType::Variant(type_name.clone()),
            kind: TypedExprKind::Constructor {
                type_name: Some(type_name),
                constructor,
                payload: Vec::new(),
            },
        };
    }
    TypedExpr {
        type_,
        kind: TypedExprKind::Entity(EntityId::from_segments(path)),
    }
}

fn nullary_constructor_type(
    path: &[String],
    context: &TypeContext<'_>,
) -> Option<(TypeName, ConstructorName)> {
    match path {
        [constructor] => {
            let signature = context.constructors.get(constructor)?;
            if !signature.payload.is_empty() {
                return None;
            }
            Some((
                signature.type_name.clone(),
                ConstructorName::new(constructor.clone()),
            ))
        }
        [_module, constructor] => {
            let signature = imported_constructor_signature(path, context)?;
            if !signature.payload.is_empty() {
                return None;
            }
            Some((
                signature.type_name,
                ConstructorName::new(constructor.clone()),
            ))
        }
        _ => None,
    }
}

fn typed_apply_expr(callee: TypedExpr, args: Vec<TypedExpr>) -> TypedExpr {
    args.into_iter().fold(callee, |callee, arg| {
        let type_ = apply_result_type(&callee.type_, 1);
        TypedExpr {
            type_,
            kind: TypedExprKind::Apply {
                callee: Box::new(callee),
                args: vec![arg],
            },
        }
    })
}

fn partial_call_lambda(
    callee: Vec<String>,
    mut supplied_args: Vec<TypedExpr>,
    params: Vec<RsigType>,
    result: RsigType,
    context: &mut TypeContext<'_>,
) -> TypedExpr {
    let supplied = supplied_args.len();
    let remaining = params.into_iter().skip(supplied).collect::<Vec<_>>();
    context.push_scope();
    let typed_params = remaining
        .iter()
        .enumerate()
        .map(|(index, type_)| {
            let binding = context.bind(&format!("arg{index}"), type_.clone());
            TypedParam {
                binding,
                type_: type_.clone(),
            }
        })
        .collect::<Vec<_>>();
    supplied_args.extend(typed_params.iter().map(|param| TypedExpr {
        type_: param.type_.clone(),
        kind: TypedExprKind::Local(param.binding.clone()),
    }));
    let call = TypedExpr {
        type_: result.clone(),
        kind: TypedExprKind::Call {
            callee: EntityId::from_segments(callee),
            args: supplied_args,
        },
    };
    context.pop_scope();
    let type_ = rsig_source_function_type(remaining, result.clone());
    TypedExpr {
        type_,
        kind: TypedExprKind::Lambda {
            params: typed_params,
            body: Box::new(TypedBlock {
                statements: Vec::new(),
                tail: Some(call),
                type_: result,
            }),
        },
    }
}

fn ast_expr_span(expr: &AstExpr) -> TextSpan {
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
        | AstExpr::Match { span, .. }
        | AstExpr::Block { span, .. }
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

fn rsig_source_function_type(params: Vec<RsigType>, result: RsigType) -> RsigType {
    if params.is_empty() {
        RsigType::Arrow {
            parameter: Box::new(RsigType::Unit),
            result: Box::new(result),
        }
    } else {
        params
            .into_iter()
            .rev()
            .fold(result, |result, parameter| RsigType::Arrow {
                parameter: Box::new(parameter),
                result: Box::new(result),
            })
    }
}

fn typed_field_type(base: &TypedExpr, field: &str) -> RsigType {
    if let TypedExprKind::Record { fields, .. } = &base.kind {
        return fields
            .iter()
            .find_map(|(name, value)| (name == field).then(|| value.type_.clone()))
            .unwrap_or(RsigType::Unknown);
    }
    RsigType::Unknown
}

fn typed_tuple_index_type(base: &TypedExpr, index: usize) -> RsigType {
    if let TypedExprKind::Tuple(items) = &base.kind {
        return items
            .get(index)
            .map(|item| item.type_.clone())
            .unwrap_or(RsigType::Unknown);
    }
    RsigType::Unknown
}

fn record_expr_type(path: &[String], context: &TypeContext<'_>) -> TypeName {
    let key = path.join(".");
    context
        .records
        .get(&key)
        .cloned()
        .unwrap_or_else(|| TypeName::new(key))
}

fn receive_pattern_type(pattern: &AstPattern, context: &TypeContext<'_>) -> RsigType {
    match pattern {
        AstPattern::Wildcard { .. } | AstPattern::Bind { .. } => RsigType::Unknown,
        AstPattern::Unit { .. } => RsigType::Unit,
        AstPattern::Bool { .. } => RsigType::Bool,
        AstPattern::Int { .. } => RsigType::I64,
        AstPattern::String { .. } => RsigType::String,
        AstPattern::Constructor { path, .. } => {
            pattern_constructor_signature(&path.segments, context)
                .map(|constructor| RsigType::Variant(constructor.type_name))
                .unwrap_or(RsigType::Unknown)
        }
        AstPattern::Tuple { items, .. } => RsigType::Tuple(
            items
                .iter()
                .map(|item| receive_pattern_type(item, context))
                .collect(),
        ),
        AstPattern::List { prefix, tail, .. } => {
            let item = prefix
                .iter()
                .map(|item| receive_pattern_type(item, context))
                .find(|type_| !matches!(type_, RsigType::Unknown))
                .or_else(|| {
                    tail.as_ref()
                        .and_then(|tail| match receive_pattern_type(tail, context) {
                            RsigType::List(item) => Some(*item),
                            _ => None,
                        })
                })
                .unwrap_or(RsigType::Unknown);
            RsigType::List(Box::new(item))
        }
        AstPattern::Record { path, .. } => {
            RsigType::Record(record_expr_type(&path.segments, context))
        }
    }
}

fn type_pattern(
    pattern: AstPattern,
    scrutinee_type: &RsigType,
    context: &mut TypeContext<'_>,
) -> TypedPattern {
    match pattern {
        AstPattern::Wildcard { .. } => TypedPattern::Wildcard,
        AstPattern::Bind { name, .. } => TypedPattern::Bind {
            binding: context.bind(&name, scrutinee_type.clone()),
            type_: scrutinee_type.clone(),
        },
        AstPattern::Constructor { path, payload, .. } => {
            let payload_types = pattern_constructor_signature(&path.segments, context)
                .map(|constructor| constructor.payload)
                .unwrap_or_default();
            let payload = payload
                .into_iter()
                .enumerate()
                .map(|(index, pattern)| {
                    let type_ = payload_types
                        .get(index)
                        .cloned()
                        .unwrap_or(RsigType::Unknown);
                    type_pattern(pattern, &type_, context)
                })
                .collect();
            let (type_name, constructor) = pattern_constructor_type(path.segments, context)
                .unwrap_or_else(|| (TypeName::new("_"), ConstructorName::new("_")));
            TypedPattern::Constructor {
                type_name,
                constructor,
                payload,
            }
        }
        AstPattern::Tuple { items, .. } => {
            let item_types = match scrutinee_type {
                RsigType::Tuple(items) => items.clone(),
                _ => Vec::new(),
            };
            TypedPattern::Tuple(
                items
                    .into_iter()
                    .enumerate()
                    .map(|(index, pattern)| {
                        let type_ = item_types.get(index).cloned().unwrap_or(RsigType::Unknown);
                        type_pattern(pattern, &type_, context)
                    })
                    .collect(),
            )
        }
        AstPattern::List { prefix, tail, .. } => {
            let item_type = match scrutinee_type {
                RsigType::List(item) => item.as_ref().clone(),
                _ => RsigType::Unknown,
            };
            TypedPattern::List {
                prefix: prefix
                    .into_iter()
                    .map(|pattern| type_pattern(pattern, &item_type, context))
                    .collect(),
                tail: tail.map(|tail| {
                    Box::new(type_pattern(
                        *tail,
                        &RsigType::List(Box::new(item_type.clone())),
                        context,
                    ))
                }),
            }
        }
        AstPattern::Record { path, fields, .. } => {
            let type_name = record_expr_type(&path.segments, context);
            let field_types = context
                .record_fields
                .get(&type_name)
                .cloned()
                .unwrap_or_default();
            TypedPattern::Record {
                type_name,
                fields: fields
                    .into_iter()
                    .map(|(name, pattern)| {
                        let type_ = field_types.get(&name).cloned().unwrap_or(RsigType::Unknown);
                        (name, type_pattern(pattern, &type_, context))
                    })
                    .collect(),
            }
        }
        AstPattern::Unit { .. } => TypedPattern::Unit,
        AstPattern::Bool { value, .. } => TypedPattern::Bool(value),
        AstPattern::Int { value, .. } => TypedPattern::Int(value),
        AstPattern::String { value, .. } => TypedPattern::String(value),
    }
}

fn pattern_constructor_type(
    path: Vec<String>,
    context: &TypeContext<'_>,
) -> Option<(TypeName, ConstructorName)> {
    let constructor = path
        .last()
        .map(|constructor| ConstructorName::new(constructor.clone()))?;
    pattern_constructor_signature(&path, context)
        .map(|signature| (signature.type_name, constructor))
}

fn pattern_constructor_signature(
    path: &[String],
    context: &TypeContext<'_>,
) -> Option<ConstructorSignature> {
    match path {
        [constructor] => context.constructors.get(constructor).cloned(),
        [_module, _constructor] => imported_constructor_signature(path, context),
        _ => None,
    }
}

fn typed_operator_call(
    operator: &str,
    args: Vec<AstExpr>,
    result: RsigType,
    context: &mut TypeContext<'_>,
) -> TypedExpr {
    TypedExpr {
        type_: result,
        kind: TypedExprKind::Call {
            callee: EntityId::from_segments(vec![
                "Std".to_owned(),
                "Prelude".to_owned(),
                operator.to_owned(),
            ]),
            args: args
                .into_iter()
                .map(|arg| type_expr(arg, context))
                .collect(),
        },
    }
}

fn is_named_call(callee: &[String], context: &TypeContext<'_>) -> bool {
    match callee {
        [name] if context.resolve(name).is_some() => false,
        [name]
            if matches!(
                name.as_str(),
                "dbg"
                    | "println"
                    | "send"
                    | "monitor"
                    | "link"
                    | "list_len"
                    | "list_get"
                    | "string_len"
                    | "string_concat"
            ) =>
        {
            true
        }
        [name] => context.externals.contains_key(name) || context.functions.contains_key(name),
        [module, name] => context
            .imports
            .get(module.as_str())
            .and_then(|rsig| rsig.find(name))
            .is_some(),
        _ => false,
    }
}

fn constructor_signature(
    path: &[String],
    context: &TypeContext<'_>,
) -> Option<ConstructorSignature> {
    match path {
        [constructor] => context.constructors.get(constructor).cloned(),
        [_module, _constructor] => imported_constructor_signature(path, context),
        _ => None,
    }
}

fn imported_constructor_signature(
    path: &[String],
    context: &TypeContext<'_>,
) -> Option<ConstructorSignature> {
    let [module, constructor] = path else {
        return None;
    };
    context
        .imports
        .get(module.as_str())
        .and_then(|rsig| rsig.find_constructor(constructor))
        .and_then(|type_| {
            let RsigTypeDeclKind::Variant { constructors } = &type_.body else {
                return None;
            };
            constructors
                .iter()
                .find(|candidate| candidate.name.as_str() == constructor)
                .map(|candidate| ConstructorSignature {
                    type_name: imported_type_name(module, &type_.name),
                    payload: candidate
                        .payload
                        .iter()
                        .map(|payload| qualify_imported_type(module, payload))
                        .collect(),
                })
        })
}

fn imported_type_name(module_name: &str, type_name: &TypeName) -> TypeName {
    TypeName::new(format!("{module_name}.{}", type_name.as_str()))
}

fn qualify_imported_type(module_name: &str, type_: &RsigType) -> RsigType {
    match type_ {
        RsigType::ActorId(message) => {
            RsigType::ActorId(Box::new(qualify_imported_type(module_name, message)))
        }
        RsigType::Arrow { parameter, result } => RsigType::Arrow {
            parameter: Box::new(qualify_imported_type(module_name, parameter)),
            result: Box::new(qualify_imported_type(module_name, result)),
        },
        RsigType::List(element) => {
            RsigType::List(Box::new(qualify_imported_type(module_name, element)))
        }
        RsigType::Tuple(items) => RsigType::Tuple(
            items
                .iter()
                .map(|item| qualify_imported_type(module_name, item))
                .collect(),
        ),
        RsigType::Record(name) => RsigType::Record(imported_type_name(module_name, name)),
        RsigType::Variant(name) if is_prelude_type_name(name) => RsigType::Variant(name.clone()),
        RsigType::Variant(name) => RsigType::Variant(imported_type_name(module_name, name)),
        RsigType::VariantApp { name, args } if is_prelude_type_name(name) => RsigType::VariantApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| qualify_imported_type(module_name, arg))
                .collect(),
        },
        RsigType::VariantApp { name, args } => RsigType::VariantApp {
            name: imported_type_name(module_name, name),
            args: args
                .iter()
                .map(|arg| qualify_imported_type(module_name, arg))
                .collect(),
        },
        other => other.clone(),
    }
}

fn is_prelude_type_name(type_name: &TypeName) -> bool {
    matches!(
        type_name.as_str(),
        "List" | "Option" | "Result" | "String" | "Never" | "int"
    )
}

fn call_result_type(callee: &[String], context: &TypeContext<'_>) -> RsigType {
    if let Some((_params, result)) = call_signature(callee, context) {
        return result;
    }
    RsigType::Unknown
}

fn call_signature(
    callee: &[String],
    context: &TypeContext<'_>,
) -> Option<(Vec<RsigType>, RsigType)> {
    match callee {
        [name] if name == "dbg" => Some((vec![RsigType::Unknown], RsigType::Unit)),
        [name] if name == "println" => Some((vec![RsigType::String], RsigType::Unit)),
        [name] if name == "send" => Some((
            vec![
                RsigType::ActorId(Box::new(RsigType::Unknown)),
                RsigType::Unknown,
            ],
            RsigType::Unit,
        )),
        [name] if name == "monitor" || name == "link" => Some((
            vec![RsigType::ActorId(Box::new(RsigType::Unknown))],
            RsigType::Unit,
        )),
        [name] => context
            .externals
            .get(name)
            .or_else(|| context.functions.get(name))
            .cloned()
            .or_else(|| match name.as_str() {
                "list_len" => Some((
                    vec![RsigType::List(Box::new(RsigType::Unknown))],
                    RsigType::I64,
                )),
                "list_get" => Some((
                    vec![RsigType::List(Box::new(RsigType::Unknown)), RsigType::I64],
                    RsigType::Unknown,
                )),
                "string_len" => Some((vec![RsigType::String], RsigType::I64)),
                "string_concat" => {
                    Some((vec![RsigType::String, RsigType::String], RsigType::String))
                }
                _ => None,
            }),
        [module, name] => context
            .imports
            .get(module.as_str())
            .and_then(|rsig| rsig.find(name))
            .map(|export| match export {
                RsigExport::Function(function) => {
                    (function.params.clone(), function.result.clone())
                }
                RsigExport::External(external) => {
                    (external.params.clone(), external.result.clone())
                }
            }),
        _ => None,
    }
}

fn apply_result_type(callee: &RsigType, arity: usize) -> RsigType {
    let mut current = callee;
    for _ in 0..arity {
        let RsigType::Arrow { result, .. } = current else {
            return RsigType::Unknown;
        };
        current = result;
    }
    current.clone()
}

fn path_type(path: &[String], context: &TypeContext<'_>) -> RsigType {
    let Some((head, tail)) = path.split_first() else {
        return RsigType::Unknown;
    };
    if tail.is_empty() {
        context
            .resolve(head)
            .map(|binding| binding.type_.clone())
            .or_else(|| context.constructors.get(head).map(constructor_value_type))
            .unwrap_or(RsigType::Unknown)
    } else if let Some(signature) = imported_constructor_signature(path, context) {
        constructor_value_type(&signature)
    } else {
        RsigType::Unknown
    }
}

fn constructor_value_type(signature: &ConstructorSignature) -> RsigType {
    if signature.payload.is_empty() {
        RsigType::Variant(signature.type_name.clone())
    } else {
        rsig_source_function_type(
            signature.payload.clone(),
            RsigType::Variant(signature.type_name.clone()),
        )
    }
}

fn merge_types(lhs: RsigType, rhs: RsigType) -> RsigType {
    if lhs == rhs {
        lhs
    } else if matches!(lhs, RsigType::Unknown) {
        rhs
    } else if matches!(rhs, RsigType::Unknown) {
        lhs
    } else {
        RsigType::Unknown
    }
}

#[derive(Default)]
struct LowerContext {
    next_actor_id: usize,
    scopes: Vec<BTreeMap<String, BindingKey>>,
}

impl LowerContext {
    fn next_actor_id(&mut self) -> usize {
        let id = self.next_actor_id;
        self.next_actor_id += 1;
        id
    }

    fn push_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn bind_existing(&mut self, binding: &BindingId) -> BindingKey {
        if self.scopes.is_empty() {
            self.push_scope();
        }
        let key = BindingKey::new(binding.key_name());
        self.scopes
            .last_mut()
            .expect("lowering always has a lexical scope")
            .insert(binding.name.clone(), key.clone());
        key
    }
}

fn collect_actors_from_block(
    block: &RirBlock,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext<'_>,
    actors: &mut Vec<ActorIrActor>,
) {
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => {
                collect_actors_from_expr(value, locals, context, actors);
                locals.insert(
                    name.as_str().to_owned(),
                    infer_actor_slot_type(value, locals, context),
                );
            }
            RirStmt::Expr(expr) => collect_actors_from_expr(expr, locals, context, actors),
        }
    }
    if let Some(tail) = &block.tail {
        collect_actors_from_expr(tail, locals, context, actors);
    }
}

fn collect_actors_from_expr(
    expr: &RirExpr,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext<'_>,
    actors: &mut Vec<ActorIrActor>,
) {
    match expr {
        RirExpr::Spawn { actor_id, body } => {
            let actor = actor_frame_from_block(*actor_id, body, locals, context);
            let mut actor_locals = actor
                .frame
                .slots
                .iter()
                .map(|slot| (slot.name.as_str().to_owned(), Some(slot.type_)))
                .collect::<BTreeMap<_, _>>();
            actors.push(actor);
            collect_actors_from_block(body, &mut actor_locals, context, actors);
        }
        RirExpr::Add(lhs, rhs)
        | RirExpr::Sub(lhs, rhs)
        | RirExpr::Mul(lhs, rhs)
        | RirExpr::Div(lhs, rhs)
        | RirExpr::Mod(lhs, rhs)
        | RirExpr::Eq(lhs, rhs)
        | RirExpr::Lt(lhs, rhs)
        | RirExpr::And(lhs, rhs)
        | RirExpr::Or(lhs, rhs) => {
            collect_actors_from_expr(lhs, locals, context, actors);
            collect_actors_from_expr(rhs, locals, context, actors);
        }
        RirExpr::Neg(value) | RirExpr::Not(value) => {
            collect_actors_from_expr(value, locals, context, actors);
        }
        RirExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_actors_from_expr(condition, locals, context, actors);
            collect_actors_from_expr(then_branch, locals, context, actors);
            collect_actors_from_expr(else_branch, locals, context, actors);
        }
        RirExpr::Match { scrutinee, arms } => {
            collect_actors_from_expr(scrutinee, locals, context, actors);
            let scrutinee_type = infer_actor_slot_type(scrutinee, locals, context);
            for arm in arms {
                let mut arm_locals = locals.clone();
                bind_pattern_actor_slot_types(&arm.pattern, scrutinee_type, &mut arm_locals);
                collect_actors_from_expr(&arm.body, &mut arm_locals, context, actors);
            }
        }
        RirExpr::Block(block) => {
            let mut block_locals = locals.clone();
            collect_actors_from_block(block, &mut block_locals, context, actors);
        }
        RirExpr::Call { args, .. } | RirExpr::Tuple(args) | RirExpr::List(args) => {
            for arg in args {
                collect_actors_from_expr(arg, locals, context, actors);
            }
        }
        RirExpr::Apply { callee, args, .. } => {
            collect_actors_from_expr(callee, locals, context, actors);
            for arg in args {
                collect_actors_from_expr(arg, locals, context, actors);
            }
        }
        RirExpr::Lambda { params, body, .. } => {
            let mut lambda_locals = locals.clone();
            for param in params {
                lambda_locals.insert(param.as_str().to_owned(), None);
            }
            collect_actors_from_block(body, &mut lambda_locals, context, actors);
        }
        RirExpr::Record { fields, .. } => {
            for (_, value) in fields {
                collect_actors_from_expr(value, locals, context, actors);
            }
        }
        RirExpr::Field { base, .. } | RirExpr::TupleIndex { base, .. } => {
            collect_actors_from_expr(base, locals, context, actors);
        }
        RirExpr::Receive { arms } => {
            for arm in arms {
                let mut arm_locals = locals.clone();
                bind_pattern_actor_slot_types(
                    &arm.pattern,
                    Some(ActorSlotType::Value),
                    &mut arm_locals,
                );
                collect_actors_from_expr(&arm.body, &mut arm_locals, context, actors);
            }
        }
        RirExpr::Variant { payload, .. } => {
            for value in payload {
                collect_actors_from_expr(value, locals, context, actors);
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

fn actor_frame_from_block(
    actor_id: usize,
    block: &RirBlock,
    outer_locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorSlotTypeContext<'_>,
) -> ActorIrActor {
    let ops = actor_ops(block);
    let mut bound = BTreeSet::new();
    let mut free = BTreeSet::new();
    for op in &ops {
        match &op {
            ActorFrameOp::Let { name, value } => {
                collect_free_expr(value, &bound, &mut free);
                bound.insert(name.as_str().to_owned());
            }
            ActorFrameOp::Expr(expr) => collect_free_expr(expr, &bound, &mut free),
            ActorFrameOp::Receive { arms } => {
                for arm in arms {
                    let mut receive_bound = bound.clone();
                    bind_pattern_names(&arm.pattern, &mut receive_bound);
                    collect_free_expr(&arm.body, &receive_bound, &mut free);
                }
            }
        }
    }

    let mut slots = Vec::new();
    for name in free {
        if let Some(Some(type_)) = outer_locals.get(&name) {
            slots.push(ActorFrameSlot {
                name: ActorFrameSlotName::new(name),
                type_: *type_,
                field_index: slots.len() as u32 + 1,
            });
        }
    }
    let captures = slots.clone();

    let mut local_types = slots
        .iter()
        .map(|slot| (slot.name.as_str().to_owned(), Some(slot.type_)))
        .collect::<BTreeMap<_, _>>();
    for op in &ops {
        if let ActorFrameOp::Let { name, value } = op {
            let type_ = infer_actor_slot_type(value, &local_types, context);
            if let Some(type_) = type_ {
                slots.push(ActorFrameSlot {
                    name: name.clone(),
                    type_,
                    field_index: slots.len() as u32 + 1,
                });
                local_types.insert(name.as_str().to_owned(), Some(type_));
            } else {
                local_types.insert(name.as_str().to_owned(), None);
            }
        }
    }

    let op_count = ops.len();
    let states = ops
        .into_iter()
        .enumerate()
        .map(|(index, op)| {
            let next = if index + 1 >= op_count {
                ActorStateNext::Done
            } else {
                ActorStateNext::State(index + 1)
            };
            ActorFrameState { op, next }
        })
        .collect::<Vec<_>>();

    ActorIrActor {
        id: actor_id,
        frame: ActorFrameLayout {
            size_bytes: (slots.len() + 1) * 8,
            align: 8,
            slots,
            captures,
        },
        states,
    }
}

fn actor_ops(block: &RirBlock) -> Vec<ActorFrameOp> {
    let mut ops = Vec::new();
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => ops.push(ActorFrameOp::Let {
                name: ActorFrameSlotName::new(name.as_str()),
                value: value.clone(),
            }),
            RirStmt::Expr(RirExpr::Receive { arms }) => {
                ops.push(ActorFrameOp::Receive { arms: arms.clone() });
            }
            RirStmt::Expr(expr) => ops.push(ActorFrameOp::Expr(expr.clone())),
        }
    }
    if let Some(expr) = &block.tail {
        match expr {
            RirExpr::Receive { arms } => {
                ops.push(ActorFrameOp::Receive { arms: arms.clone() });
            }
            expr => ops.push(ActorFrameOp::Expr(expr.clone())),
        }
    }
    ops
}

fn lower_block(block: TypedBlock, context: &mut LowerContext) -> RirBlock {
    context.push_scope();
    let lowered = RirBlock {
        statements: block
            .statements
            .into_iter()
            .map(|stmt| match stmt {
                TypedStmt::Let { binding, value, .. } => {
                    let value = lower_expr(value, context);
                    let name = context.bind_existing(&binding);
                    RirStmt::Let { name, value }
                }
                TypedStmt::Expr(expr) => RirStmt::Expr(lower_expr(expr, context)),
            })
            .collect(),
        tail: block.tail.map(|tail| lower_expr(tail, context)),
    };
    context.pop_scope();
    lowered
}

fn lower_expr(expr: TypedExpr, context: &mut LowerContext) -> RirExpr {
    let expr_type = expr.type_;
    match expr.kind {
        TypedExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => RirExpr::If {
            condition: Box::new(lower_expr(*condition, context)),
            then_branch: Box::new(lower_expr(*then_branch, context)),
            else_branch: Box::new(lower_expr(*else_branch, context)),
        },
        TypedExprKind::Match { scrutinee, arms } => {
            let scrutinee = lower_expr(*scrutinee, context);
            let arms = arms
                .into_iter()
                .map(|arm| {
                    context.push_scope();
                    let pattern = lower_pattern(arm.pattern, context);
                    let body = lower_expr(arm.body, context);
                    context.pop_scope();
                    RirMatchArm { pattern, body }
                })
                .collect();
            RirExpr::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            }
        }
        TypedExprKind::Block(block) => RirExpr::Block(Box::new(lower_block(*block, context))),
        TypedExprKind::Literal(literal) => lower_literal(literal),
        TypedExprKind::Call { callee, args } => lower_call(callee, args, context),
        TypedExprKind::Apply { callee, args } => RirExpr::Apply {
            callee: Box::new(lower_expr(*callee, context)),
            args: args
                .into_iter()
                .map(|arg| lower_expr(arg, context))
                .collect(),
            result: expr_type,
        },
        TypedExprKind::Lambda { params, body } => {
            context.push_scope();
            let params = params
                .into_iter()
                .map(|param| Param::from_key(context.bind_existing(&param.binding)))
                .collect::<Vec<_>>();
            let body = lower_block(*body, context);
            context.pop_scope();
            RirExpr::Lambda {
                params,
                captures: Vec::new(),
                body: Box::new(body),
            }
        }
        TypedExprKind::Spawn { body } => RirExpr::Spawn {
            actor_id: context.next_actor_id(),
            body: Box::new(lower_block(*body, context)),
        },
        TypedExprKind::Receive { arms } => RirExpr::Receive {
            arms: arms
                .into_iter()
                .map(|arm| {
                    context.push_scope();
                    let pattern = lower_pattern(arm.pattern, context);
                    let body = lower_expr(arm.body, context);
                    context.pop_scope();
                    RirReceiveArm { pattern, body }
                })
                .collect(),
        },
        TypedExprKind::Tuple(items) => RirExpr::Tuple(
            items
                .into_iter()
                .map(|item| lower_expr(item, context))
                .collect(),
        ),
        TypedExprKind::List(items) => RirExpr::List(
            items
                .into_iter()
                .map(|item| lower_expr(item, context))
                .collect(),
        ),
        TypedExprKind::Record { path, fields } => RirExpr::Record {
            path: path.as_strings(),
            fields: fields
                .into_iter()
                .map(|(name, value)| (name, lower_expr(value, context)))
                .collect(),
        },
        TypedExprKind::Field { base, field } => RirExpr::Field {
            base: Box::new(lower_expr(*base, context)),
            field,
        },
        TypedExprKind::TupleIndex { base, index } => RirExpr::TupleIndex {
            base: Box::new(lower_expr(*base, context)),
            index,
        },
        TypedExprKind::Constructor {
            type_name,
            constructor,
            payload,
        } => {
            if type_name.is_none() && constructor.as_str() == "()" && payload.is_empty() {
                RirExpr::Unit
            } else {
                RirExpr::Variant {
                    type_name: type_name.unwrap_or_else(|| TypeName::new("unit")),
                    constructor,
                    payload: payload
                        .into_iter()
                        .map(|payload| lower_expr(payload, context))
                        .collect(),
                }
            }
        }
        TypedExprKind::Entity(ident) => RirExpr::Path(lower_entity_path(ident, context)),
        TypedExprKind::Local(binding) => RirExpr::Path(vec![binding.key_name()]),
    }
}

fn lower_call(callee: EntityId, args: Vec<TypedExpr>, context: &mut LowerContext) -> RirExpr {
    let callee = lower_entity_path(callee, context);
    let args = args
        .into_iter()
        .map(|arg| lower_expr(arg, context))
        .collect::<Vec<_>>();
    lower_prelude_operator(callee, args)
}

fn lower_prelude_operator(callee: Vec<String>, args: Vec<RirExpr>) -> RirExpr {
    let operator = callee.last().map(String::as_str);
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
        _ => RirExpr::Call { callee, args },
    }
}

fn lower_entity_path(entity: EntityId, _context: &LowerContext) -> Vec<String> {
    entity.as_strings()
}

fn lower_literal(literal: TypedLiteral) -> RirExpr {
    match literal {
        TypedLiteral::Bool(value) => RirExpr::Bool(value),
        TypedLiteral::Char(value) => RirExpr::Char(value),
        TypedLiteral::Float(value) => RirExpr::Float(value),
        TypedLiteral::Int(value) => RirExpr::Int(value),
        TypedLiteral::String(value) => RirExpr::String(value),
    }
}

fn lower_pattern(pattern: TypedPattern, context: &mut LowerContext) -> RirPattern {
    match pattern {
        TypedPattern::Wildcard => RirPattern::Wildcard,
        TypedPattern::Bind { binding, type_ } => RirPattern::Bind {
            binding: context.bind_existing(&binding),
            type_,
        },
        TypedPattern::Constructor {
            type_name,
            constructor,
            payload,
        } => RirPattern::Constructor {
            type_name,
            constructor,
            payload: payload
                .into_iter()
                .map(|pattern| lower_pattern(pattern, context))
                .collect(),
        },
        TypedPattern::Tuple(items) => RirPattern::Tuple(
            items
                .into_iter()
                .map(|pattern| lower_pattern(pattern, context))
                .collect(),
        ),
        TypedPattern::List { prefix, tail } => RirPattern::List {
            prefix: prefix
                .into_iter()
                .map(|pattern| lower_pattern(pattern, context))
                .collect(),
            tail: tail.map(|tail| Box::new(lower_pattern(*tail, context))),
        },
        TypedPattern::Record { type_name, fields } => RirPattern::Record {
            type_name,
            fields: fields
                .into_iter()
                .map(|(name, pattern)| (name, lower_pattern(pattern, context)))
                .collect(),
        },
        TypedPattern::Unit => RirPattern::Unit,
        TypedPattern::Bool(value) => RirPattern::Bool(value),
        TypedPattern::Int(value) => RirPattern::Int(value),
        TypedPattern::String(value) => RirPattern::String(value),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        AstBlock, AstDecl, AstExpr, AstFnDecl, AstPath, AstProgram, AstStmt, TextSpan,
    };
    use crate::checker::tyir::TyIrBuilder;
    use crate::infer::module::ModuleInferencer;
    use crate::parser::SourceParser;
    use crate::signature::{ImportedSignatures, ModuleName, RsigType};

    use crate::lambda::lower::LambdaLowerer;

    use crate::lambda::ir::Capture;

    use super::{RirExpr, RirStmt, TypedExprKind, TypedStmt};

    fn span() -> TextSpan {
        TextSpan::new(0, 0)
    }

    fn path(name: &str) -> AstExpr {
        AstExpr::Path {
            path: AstPath {
                segments: vec![name.to_owned()],
            },
            span: span(),
        }
    }

    fn typed_program(module: &str, program: AstProgram) -> super::TypedProgram {
        let imports = ImportedSignatures::new();
        let inferred = ModuleInferencer::new(&program, &imports).infer().unwrap();
        let function_types = inferred.function_signatures(&program);
        TyIrBuilder::new(ModuleName::new(module), &imports, &function_types, None).build(program)
    }

    fn typed_program_with_inference_facts(module: &str, source: &str) -> super::TypedProgram {
        let path = camino::Utf8Path::new("test.ml");
        let program = SourceParser::new().parse(path, source).unwrap();
        let imports = ImportedSignatures::new();
        let inferred = ModuleInferencer::new(&program, &imports).infer().unwrap();
        let function_types = inferred.function_signatures(&program);
        let expression_types = inferred.expression_rsig_types();
        TyIrBuilder::new(
            ModuleName::new(module),
            &imports,
            &function_types,
            Some(&expression_types),
        )
        .build(program)
    }

    #[test]
    fn rir_lambdas_carry_capture_names() {
        let program = AstProgram {
            decls: vec![AstDecl::Function(AstFnDecl {
                name: "make_adder".to_owned(),
                name_span: span(),
                params: vec!["n".to_owned()],
                param_types: Vec::new(),
                return_type: None,
                body: AstBlock {
                    statements: Vec::new(),
                    tail: Some(AstExpr::Lambda {
                        params: vec!["x".to_owned()],
                        param_types: Vec::new(),
                        body: Box::new(AstBlock {
                            statements: Vec::new(),
                            tail: Some(AstExpr::Add {
                                lhs: Box::new(path("x")),
                                rhs: Box::new(path("n")),
                                span: span(),
                            }),
                            span: span(),
                        }),
                        span: span(),
                    }),
                    span: span(),
                },
                span: span(),
            })],
        };

        let typed = typed_program("LambdaTest", program);
        let rir = LambdaLowerer::new().lower(typed);
        let Some(RirExpr::Lambda { captures, .. }) = &rir.functions[0].body.tail else {
            panic!("expected lowered lambda");
        };

        assert_eq!(captures.as_slice(), &[Capture::new("n$0")]);
    }

    #[test]
    fn rir_lambda_captures_track_shadowed_binding_identity() {
        let program = AstProgram {
            decls: vec![AstDecl::Function(AstFnDecl {
                name: "main".to_owned(),
                name_span: span(),
                params: Vec::new(),
                param_types: Vec::new(),
                return_type: None,
                body: AstBlock {
                    statements: vec![
                        AstStmt::Let {
                            name: "a".to_owned(),
                            type_annotation: None,
                            value: AstExpr::Int {
                                value: 1,
                                span: span(),
                            },
                        },
                        AstStmt::Let {
                            name: "f".to_owned(),
                            type_annotation: None,
                            value: AstExpr::Lambda {
                                params: vec!["ignored".to_owned()],
                                param_types: Vec::new(),
                                body: Box::new(AstBlock {
                                    statements: Vec::new(),
                                    tail: Some(path("a")),
                                    span: span(),
                                }),
                                span: span(),
                            },
                        },
                        AstStmt::Let {
                            name: "a".to_owned(),
                            type_annotation: None,
                            value: AstExpr::Int {
                                value: 2,
                                span: span(),
                            },
                        },
                    ],
                    tail: Some(AstExpr::Call {
                        callee: AstPath {
                            segments: vec!["f".to_owned()],
                        },
                        args: vec![AstExpr::Unit { span: span() }],
                        span: span(),
                    }),
                    span: span(),
                },
                span: span(),
            })],
        };

        let typed = typed_program("ShadowTest", program);
        let rir = LambdaLowerer::new().lower(typed);
        let Some(RirStmt::Let {
            value: RirExpr::Lambda { captures, .. },
            ..
        }) = rir.functions[0].body.statements.get(1)
        else {
            panic!("expected second statement to bind a lambda");
        };

        assert_eq!(captures.as_slice(), &[Capture::new("a$0")]);
    }

    #[test]
    fn typed_hir_paths_resolve_to_shadowed_binding_identity() {
        let program = AstProgram {
            decls: vec![AstDecl::Function(AstFnDecl {
                name: "main".to_owned(),
                name_span: span(),
                params: Vec::new(),
                param_types: Vec::new(),
                return_type: None,
                body: AstBlock {
                    statements: vec![
                        AstStmt::Let {
                            name: "a".to_owned(),
                            type_annotation: None,
                            value: AstExpr::Int {
                                value: 1,
                                span: span(),
                            },
                        },
                        AstStmt::Let {
                            name: "f".to_owned(),
                            type_annotation: None,
                            value: AstExpr::Lambda {
                                params: vec!["ignored".to_owned()],
                                param_types: Vec::new(),
                                body: Box::new(AstBlock {
                                    statements: Vec::new(),
                                    tail: Some(path("a")),
                                    span: span(),
                                }),
                                span: span(),
                            },
                        },
                        AstStmt::Let {
                            name: "a".to_owned(),
                            type_annotation: None,
                            value: AstExpr::Int {
                                value: 2,
                                span: span(),
                            },
                        },
                    ],
                    tail: Some(path("f")),
                    span: span(),
                },
                span: span(),
            })],
        };

        let typed = typed_program("ShadowTest", program);
        let [
            TypedStmt::Let {
                binding: first_a, ..
            },
            TypedStmt::Let { value: f_value, .. },
            TypedStmt::Let {
                binding: second_a, ..
            },
        ] = typed.functions[0].body.statements.as_slice()
        else {
            panic!("expected three let statements");
        };
        let TypedExprKind::Lambda { body, .. } = &f_value.kind else {
            panic!("expected the second let to bind a lambda");
        };
        let Some(tail) = &body.tail else {
            panic!("expected lambda tail");
        };
        let TypedExprKind::Local(captured_a) = &tail.kind else {
            panic!("expected lambda tail to resolve a local");
        };

        assert_eq!(first_a.name, "a");
        assert_eq!(second_a.name, "a");
        assert_ne!(first_a, second_a);
        assert_eq!(captured_a, first_a);
    }

    #[test]
    fn rir_apply_carries_typed_arrow_result() {
        let program = AstProgram {
            decls: vec![AstDecl::Function(AstFnDecl {
                name: "apply_i64".to_owned(),
                name_span: span(),
                params: vec!["f".to_owned(), "x".to_owned()],
                param_types: Vec::new(),
                return_type: None,
                body: AstBlock {
                    statements: Vec::new(),
                    tail: Some(AstExpr::Add {
                        lhs: Box::new(AstExpr::Call {
                            callee: AstPath {
                                segments: vec!["f".to_owned()],
                            },
                            args: vec![AstExpr::Add {
                                lhs: Box::new(path("x")),
                                rhs: Box::new(AstExpr::Int {
                                    value: 0,
                                    span: span(),
                                }),
                                span: span(),
                            }],
                            span: span(),
                        }),
                        rhs: Box::new(AstExpr::Int {
                            value: 1,
                            span: span(),
                        }),
                        span: span(),
                    }),
                    span: span(),
                },
                span: span(),
            })],
        };

        let typed = typed_program("ApplyTest", program);
        let rir = LambdaLowerer::new().lower(typed);
        let Some(RirExpr::Add(lhs, _)) = &rir.functions[0].body.tail else {
            panic!("expected add tail");
        };
        let RirExpr::Apply { result, .. } = lhs.as_ref() else {
            panic!("expected typed apply");
        };

        assert_eq!(result, &RsigType::I64);
    }

    #[test]
    fn typed_hir_uses_inference_facts_for_lambda_apply_results() {
        let typed = typed_program_with_inference_facts(
            "ApplyFacts",
            "fn main() { let id = fn(x) { x }; id(1) }",
        );
        let Some(TypedStmt::Let { value, .. }) = typed.functions[0].body.statements.first() else {
            panic!("expected a let-bound lambda");
        };
        assert!(matches!(
            value.type_,
            RsigType::Arrow {
                parameter: _,
                result: _
            }
        ));
        let Some(tail) = &typed.functions[0].body.tail else {
            panic!("expected a tail expression");
        };
        let TypedExprKind::Apply { .. } = &tail.kind else {
            panic!("expected the local call to lower to HIR apply");
        };

        assert_eq!(tail.type_, RsigType::I64);
    }

    #[test]
    fn typed_entities_carry_surface_paths() {
        let typed = typed_program_with_inference_facts(
            "NamedCall",
            "fn helper() { 1 }\nfn main() { helper() }",
        );
        let Some(tail) = &typed.functions[1].body.tail else {
            panic!("expected a tail expression");
        };
        let TypedExprKind::Call { callee, .. } = &tail.kind else {
            panic!("expected a named call");
        };

        assert_eq!(callee.as_strings(), vec!["helper".to_owned()]);
        assert_eq!(callee.binding_id.name, "helper");
    }
}
