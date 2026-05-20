#![allow(dead_code)]

use std::collections::{BTreeMap, BTreeSet};

use crate::ast::{
    AstBlock, AstDecl, AstExpr, AstPath, AstPattern, AstProgram, AstStmt, AstTypeBody, TextSpan,
};
use crate::signature::{
    ConstructorName, FieldName, ImportedSignatures, ModuleName, Rsig, RsigDependency, RsigExport,
    RsigExternal, RsigFunction, RsigRecordField, RsigType, RsigTypeDecl, RsigTypeDeclKind,
    RsigTypeScheme, RsigVariantConstructor, TypeName, parse_type_signature,
    parse_type_signature_with_variants, parse_type_with_variants,
};

mod actor;
mod hir;
mod rir;

pub(crate) use actor::*;
pub(crate) use hir::*;
pub(crate) use rir::*;

pub(crate) fn typed_program_from_ast(
    module_name: ModuleName,
    ast: AstProgram,
    imports: &ImportedSignatures,
    function_types: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
    expression_types: Option<&BTreeMap<TextSpan, RsigType>>,
    binding_schemes: Option<&BTreeMap<TextSpan, RsigTypeScheme>>,
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
            AstDecl::Type(type_) => {
                types.push(TypedTypeDecl {
                    name: TypeName::new(type_.name),
                    body: match type_.body {
                        AstTypeBody::Variant { constructors } => TypedTypeBody::Variant {
                            constructors: constructors
                                .into_iter()
                                .map(|constructor| TypedVariantConstructor {
                                    name: ConstructorName::new(constructor.name),
                                })
                                .collect(),
                        },
                        AstTypeBody::Record { fields } => TypedTypeBody::Record {
                            fields: fields
                                .into_iter()
                                .map(|field| TypedRecordField {
                                    name: FieldName::new(field.name),
                                    type_: parse_type_with_variants(
                                        &field.type_annotation.text,
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
                parse_type_signature_with_variants(&external.type_text, &declared_variants);
            TypedExternal {
                name: external.name,
                type_text: external.type_text,
                params,
                result,
                abi: external.abi,
            }
        })
        .collect::<Vec<_>>();
    let constructors = constructor_type_map(&types);

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
            .map(|annotation| parse_type_with_variants(&annotation.text, &declared_variants));
        let mut context = TypeContext::new(
            function_types,
            &external_types,
            imports,
            &constructors,
            &declared_variants,
            expression_types,
            binding_schemes,
        );
        let params = function
            .params
            .iter()
            .enumerate()
            .map(|(index, name)| {
                let type_ = param_types.get(index).cloned().unwrap_or(RsigType::Unknown);
                TypedParam {
                    binding: context.bind(name, type_.clone()),
                    scheme: RsigTypeScheme::monomorphic(type_.clone()),
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

pub(crate) fn signature_for(program: &TypedProgram) -> Rsig {
    let types = program
        .types
        .iter()
        .map(|type_| RsigTypeDecl {
            name: type_.name.clone(),
            body: match &type_.body {
                TypedTypeBody::Variant { constructors } => RsigTypeDeclKind::Variant {
                    constructors: constructors
                        .iter()
                        .map(|constructor| RsigVariantConstructor {
                            name: constructor.name.clone(),
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
    closure_convert_rir(lowered)
}

pub(crate) fn closure_convert_rir(mut program: RirProgram) -> RirProgram {
    for function in &mut program.functions {
        closure_convert_block(&mut function.body);
    }
    program
}

pub(crate) fn lower_rir_to_actor_ir(
    program: &RirProgram,
    imports: &ImportedSignatures,
) -> ActorIrProgram {
    let mut actors = Vec::new();
    let context = ActorLowerContext {
        functions: function_type_map(program),
        externals: external_type_map(program),
        imports,
    };
    for function in &program.functions {
        let mut locals = function
            .params
            .iter()
            .zip(&function.param_types)
            .map(|(name, type_)| (name.as_str().to_owned(), ActorSlotType::from_rsig(type_)))
            .collect::<BTreeMap<_, _>>();
        collect_actors_from_block(
            &function.name,
            &function.body,
            &mut locals,
            &context,
            &mut actors,
        );
    }
    ActorIrProgram {
        module_name: program.module_name.clone(),
        actors,
    }
}

pub(crate) fn module_function_symbol(module: &ModuleName, name: &str) -> String {
    format!("riot_mod_{module}_{name}")
}

#[derive(Debug, Clone)]
struct TypedBindingInfo {
    binding: HirBinding,
    type_: RsigType,
}

struct TypeContext<'a> {
    next_binding_id: usize,
    scopes: Vec<BTreeMap<String, TypedBindingInfo>>,
    functions: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    externals: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
    imports: &'a ImportedSignatures,
    constructors: &'a BTreeMap<String, TypeName>,
    declared_variants: &'a BTreeSet<TypeName>,
    expression_types: Option<&'a BTreeMap<TextSpan, RsigType>>,
    binding_schemes: Option<&'a BTreeMap<TextSpan, RsigTypeScheme>>,
}

impl<'a> TypeContext<'a> {
    fn new(
        functions: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
        externals: &'a BTreeMap<String, (Vec<RsigType>, RsigType)>,
        imports: &'a ImportedSignatures,
        constructors: &'a BTreeMap<String, TypeName>,
        declared_variants: &'a BTreeSet<TypeName>,
        expression_types: Option<&'a BTreeMap<TextSpan, RsigType>>,
        binding_schemes: Option<&'a BTreeMap<TextSpan, RsigTypeScheme>>,
    ) -> Self {
        Self {
            next_binding_id: 0,
            scopes: vec![BTreeMap::new()],
            functions,
            externals,
            imports,
            constructors,
            declared_variants,
            expression_types,
            binding_schemes,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn bind(&mut self, name: &str, type_: RsigType) -> HirBinding {
        if self.scopes.is_empty() {
            self.push_scope();
        }
        let binding = HirBinding::new(name, self.next_binding_id);
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

    fn binding_scheme(&self, span: TextSpan, fallback: RsigType) -> RsigTypeScheme {
        self.binding_schemes
            .and_then(|schemes| schemes.get(&span))
            .cloned()
            .unwrap_or_else(|| RsigTypeScheme::monomorphic(fallback))
    }
}

fn constructor_type_map(types: &[TypedTypeDecl]) -> BTreeMap<String, TypeName> {
    types
        .iter()
        .flat_map(|type_| {
            let TypedTypeBody::Variant { constructors } = &type_.body else {
                return Vec::new();
            };
            constructors
                .iter()
                .map(|constructor| (constructor.name.as_str().to_owned(), type_.name.clone()))
                .collect::<Vec<_>>()
        })
        .collect()
}

fn declared_variant_names(
    types: &[TypedTypeDecl],
    uses: &[TypedUse],
    imports: &ImportedSignatures,
) -> BTreeSet<TypeName> {
    let mut names = BTreeSet::new();
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
    let mut names = BTreeSet::new();
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
            _ => {}
        }
    }
    names
}

fn infer_ast_function_types(
    functions: &[crate::ast::AstFnDecl],
) -> BTreeMap<String, (Vec<RsigType>, RsigType)> {
    let mut signatures = functions
        .iter()
        .map(|function| {
            let params = function
                .params
                .iter()
                .enumerate()
                .map(|(index, _)| {
                    function
                        .param_types
                        .get(index)
                        .and_then(|annotation| annotation.as_ref())
                        .map(|annotation| parse_type_signature(&annotation.text).1)
                        .unwrap_or(RsigType::Unknown)
                })
                .collect::<Vec<_>>();
            let result = function
                .return_type
                .as_ref()
                .map(|annotation| parse_type_signature(&annotation.text).1)
                .unwrap_or(RsigType::Unknown);
            (function.name.clone(), (params, result))
        })
        .collect::<BTreeMap<_, _>>();

    for _ in 0..4 {
        let previous = signatures.clone();
        for function in functions {
            let (mut param_types, previous_result) =
                previous.get(&function.name).cloned().unwrap_or_else(|| {
                    (
                        vec![RsigType::Unknown; function.params.len()],
                        RsigType::Unknown,
                    )
                });
            for (index, annotation) in function.param_types.iter().enumerate() {
                if let Some(annotation) = annotation
                    && let Some(param_type) = param_types.get_mut(index)
                {
                    *param_type = parse_type_signature(&annotation.text).1;
                }
            }
            let mut locals = function
                .params
                .iter()
                .zip(&param_types)
                .map(|(name, type_)| (name.clone(), type_.clone()))
                .collect::<BTreeMap<_, _>>();
            let result = infer_ast_block_type(
                &function.body,
                &function.params,
                &mut locals,
                &mut param_types,
                &previous,
            );
            let result = function
                .return_type
                .as_ref()
                .map(|annotation| parse_type_signature(&annotation.text).1)
                .unwrap_or(result);
            signatures.insert(
                function.name.clone(),
                (param_types, merge_types(previous_result, result)),
            );
        }
        if signatures == previous {
            break;
        }
    }

    signatures
}

fn infer_ast_block_type(
    block: &AstBlock,
    params: &[String],
    locals: &mut BTreeMap<String, RsigType>,
    param_types: &mut [RsigType],
    functions: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
) -> RsigType {
    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                mark_ast_constraints(value, params, locals, param_types, functions);
                let inferred = type_annotation
                    .as_ref()
                    .map(|annotation| parse_type_signature(&annotation.text).1)
                    .unwrap_or_else(|| infer_ast_expr_type(value, locals, functions));
                locals.insert(name.clone(), inferred);
            }
            AstStmt::Expr(expr) => {
                mark_ast_constraints(expr, params, locals, param_types, functions);
            }
        }
    }
    if let Some(tail) = &block.tail {
        mark_ast_constraints(tail, params, locals, param_types, functions);
        infer_ast_expr_type(tail, locals, functions)
    } else {
        RsigType::Unit
    }
}

fn infer_ast_expr_type(
    expr: &AstExpr,
    locals: &BTreeMap<String, RsigType>,
    functions: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
) -> RsigType {
    match expr {
        AstExpr::Add { lhs, rhs, .. }
        | AstExpr::Sub { lhs, rhs, .. }
        | AstExpr::Mul { lhs, rhs, .. }
        | AstExpr::Div { lhs, rhs, .. }
        | AstExpr::Mod { lhs, rhs, .. } => {
            let _ = lhs;
            let _ = rhs;
            RsigType::I64
        }
        AstExpr::Neg { .. } => RsigType::I64,
        AstExpr::Eq { lhs, rhs, .. } => {
            infer_ast_expr_type(lhs, locals, functions);
            infer_ast_expr_type(rhs, locals, functions);
            RsigType::Bool
        }
        AstExpr::Lt { lhs, rhs, .. } => {
            let _ = lhs;
            let _ = rhs;
            RsigType::Bool
        }
        AstExpr::And { lhs, rhs, .. } | AstExpr::Or { lhs, rhs, .. } => {
            infer_ast_expr_type(lhs, locals, functions);
            infer_ast_expr_type(rhs, locals, functions);
            RsigType::Bool
        }
        AstExpr::Not { expr, .. } => {
            infer_ast_expr_type(expr, locals, functions);
            RsigType::Bool
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            infer_ast_expr_type(condition, locals, functions);
            merge_types(
                infer_ast_expr_type(then_branch, locals, functions),
                infer_ast_expr_type(else_branch, locals, functions),
            )
        }
        AstExpr::Match {
            scrutinee, arms, ..
        } => {
            infer_ast_expr_type(scrutinee, locals, functions);
            arms.iter()
                .map(|arm| infer_ast_expr_type(&arm.body, locals, functions))
                .reduce(merge_types)
                .unwrap_or(RsigType::Unknown)
        }
        AstExpr::Block { block, .. } => {
            let mut nested_locals = locals.clone();
            let mut nested_params = Vec::new();
            infer_ast_block_type(
                block,
                &[],
                &mut nested_locals,
                &mut nested_params,
                functions,
            )
        }
        AstExpr::Bool { .. } => RsigType::Bool,
        AstExpr::Call { callee, args, .. } => {
            for arg in args {
                infer_ast_expr_type(arg, locals, functions);
            }
            match callee.segments.as_slice() {
                [name] if name == "dbg" || name == "println" => RsigType::Unit,
                [name] if name == "send" || name == "monitor" || name == "link" => RsigType::Unit,
                [name] if name == "list_len" || name == "string_len" => RsigType::I64,
                [name] if name == "string_concat" => RsigType::String,
                [name] if name == "list_get" => RsigType::Unknown,
                [name] => functions
                    .get(name)
                    .map(|(_, result)| result.clone())
                    .unwrap_or(RsigType::Unknown),
                _ => RsigType::Unknown,
            }
        }
        AstExpr::Apply { callee, args, .. } => {
            infer_ast_expr_type(callee, locals, functions);
            for arg in args {
                infer_ast_expr_type(arg, locals, functions);
            }
            RsigType::Unknown
        }
        AstExpr::Lambda { body, .. } => {
            let mut nested_locals = locals.clone();
            let mut nested_params = Vec::new();
            infer_ast_block_type(body, &[], &mut nested_locals, &mut nested_params, functions);
            RsigType::Unknown
        }
        AstExpr::Spawn { .. } => RsigType::ActorId(Box::new(RsigType::Unknown)),
        AstExpr::Receive { body, .. } => {
            infer_ast_expr_type(body, locals, functions);
            RsigType::Unit
        }
        AstExpr::Unit { .. } => RsigType::Unit,
        AstExpr::Tuple { items, .. } => RsigType::Tuple(
            items
                .iter()
                .map(|item| infer_ast_expr_type(item, locals, functions))
                .collect(),
        ),
        AstExpr::List { items, .. } => RsigType::List(Box::new(
            items
                .first()
                .map(|item| infer_ast_expr_type(item, locals, functions))
                .unwrap_or(RsigType::Unknown),
        )),
        AstExpr::Record { path, fields, .. } => {
            for (_, value) in fields {
                infer_ast_expr_type(value, locals, functions);
            }
            RsigType::Record(TypeName::new(path.segments.join(".")))
        }
        AstExpr::Field { base, field, .. } => infer_ast_field_type(base, field, locals, functions),
        AstExpr::TupleIndex { base, index, .. } => {
            infer_ast_tuple_index_type(base, *index, locals, functions)
        }
        AstExpr::Char { .. } => RsigType::Char,
        AstExpr::Float { .. } => RsigType::F64,
        AstExpr::Int { .. } => RsigType::I64,
        AstExpr::Path { path, .. } => path
            .segments
            .first()
            .and_then(|name| locals.get(name))
            .cloned()
            .unwrap_or(RsigType::Unknown),
        AstExpr::String { .. } => RsigType::String,
    }
}

fn infer_ast_field_type(
    base: &AstExpr,
    field: &str,
    locals: &BTreeMap<String, RsigType>,
    functions: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
) -> RsigType {
    if let AstExpr::Record { fields, .. } = base {
        return fields
            .iter()
            .find_map(|(name, value)| {
                (name == field).then(|| infer_ast_expr_type(value, locals, functions))
            })
            .unwrap_or(RsigType::Unknown);
    }
    RsigType::Unknown
}

fn infer_ast_tuple_index_type(
    base: &AstExpr,
    index: usize,
    locals: &BTreeMap<String, RsigType>,
    functions: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
) -> RsigType {
    if let AstExpr::Tuple { items, .. } = base {
        return items
            .get(index)
            .map(|item| infer_ast_expr_type(item, locals, functions))
            .unwrap_or(RsigType::Unknown);
    }
    RsigType::Unknown
}

fn mark_ast_constraints(
    expr: &AstExpr,
    params: &[String],
    locals: &mut BTreeMap<String, RsigType>,
    param_types: &mut [RsigType],
    functions: &BTreeMap<String, (Vec<RsigType>, RsigType)>,
) {
    match expr {
        AstExpr::Add { lhs, rhs, .. }
        | AstExpr::Sub { lhs, rhs, .. }
        | AstExpr::Mul { lhs, rhs, .. }
        | AstExpr::Div { lhs, rhs, .. }
        | AstExpr::Mod { lhs, rhs, .. }
        | AstExpr::Lt { lhs, rhs, .. } => {
            mark_ast_expr_as(params, locals, param_types, lhs, RsigType::I64);
            mark_ast_expr_as(params, locals, param_types, rhs, RsigType::I64);
        }
        AstExpr::Neg { expr, .. } => {
            mark_ast_expr_as(params, locals, param_types, expr, RsigType::I64)
        }
        AstExpr::And { lhs, rhs, .. } | AstExpr::Or { lhs, rhs, .. } => {
            mark_ast_expr_as(params, locals, param_types, lhs, RsigType::Bool);
            mark_ast_expr_as(params, locals, param_types, rhs, RsigType::Bool);
        }
        AstExpr::Not { expr, .. } => {
            mark_ast_expr_as(params, locals, param_types, expr, RsigType::Bool)
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            mark_ast_expr_as(params, locals, param_types, condition, RsigType::Bool);
            mark_ast_constraints(then_branch, params, locals, param_types, functions);
            mark_ast_constraints(else_branch, params, locals, param_types, functions);
        }
        AstExpr::Match {
            scrutinee, arms, ..
        } => {
            mark_ast_constraints(scrutinee, params, locals, param_types, functions);
            for arm in arms {
                mark_ast_constraints(&arm.body, params, locals, param_types, functions);
            }
        }
        AstExpr::Block { block, .. } => {
            let mut nested_locals = locals.clone();
            let mut nested_params = Vec::new();
            infer_ast_block_type(
                block,
                &[],
                &mut nested_locals,
                &mut nested_params,
                functions,
            );
        }
        AstExpr::Call { args, .. }
        | AstExpr::Tuple { items: args, .. }
        | AstExpr::List { items: args, .. } => {
            for arg in args {
                mark_ast_constraints(arg, params, locals, param_types, functions);
            }
        }
        AstExpr::Apply { callee, args, .. } => {
            mark_ast_constraints(callee, params, locals, param_types, functions);
            for arg in args {
                mark_ast_constraints(arg, params, locals, param_types, functions);
            }
        }
        AstExpr::Lambda {
            params: lambda_params,
            body,
            ..
        } => {
            let mut nested_locals = locals.clone();
            for param in lambda_params {
                nested_locals.insert(param.clone(), RsigType::Unknown);
            }
            let mut nested_params = vec![RsigType::Unknown; lambda_params.len()];
            infer_ast_block_type(
                body,
                lambda_params,
                &mut nested_locals,
                &mut nested_params,
                functions,
            );
        }
        AstExpr::Record { fields, .. } => {
            for (_, value) in fields {
                mark_ast_constraints(value, params, locals, param_types, functions);
            }
        }
        AstExpr::Field { base, .. } | AstExpr::TupleIndex { base, .. } => {
            mark_ast_constraints(base, params, locals, param_types, functions);
        }
        AstExpr::Receive { body, .. } => {
            mark_ast_constraints(body, params, locals, param_types, functions);
        }
        AstExpr::Spawn { body, .. } => {
            let mut nested_locals = BTreeMap::new();
            let mut nested_params = Vec::new();
            infer_ast_block_type(body, &[], &mut nested_locals, &mut nested_params, functions);
        }
        AstExpr::Eq { lhs, rhs, .. } => {
            mark_ast_constraints(lhs, params, locals, param_types, functions);
            mark_ast_constraints(rhs, params, locals, param_types, functions);
        }
        AstExpr::Bool { .. }
        | AstExpr::Unit { .. }
        | AstExpr::Char { .. }
        | AstExpr::Float { .. }
        | AstExpr::Int { .. }
        | AstExpr::Path { .. }
        | AstExpr::String { .. } => {}
    }
}

fn mark_ast_expr_as(
    params: &[String],
    locals: &mut BTreeMap<String, RsigType>,
    param_types: &mut [RsigType],
    expr: &AstExpr,
    type_: RsigType,
) {
    if let AstExpr::Path { path, .. } = expr
        && let [name] = path.segments.as_slice()
    {
        locals.insert(name.clone(), type_.clone());
        if let Some(index) = params.iter().position(|param| param == name) {
            param_types[index] = merge_types(param_types[index].clone(), type_);
        }
    }
}

fn type_block(block: AstBlock, context: &mut TypeContext<'_>) -> TypedBlock {
    context.push_scope();
    let mut statements = Vec::new();
    for stmt in block.statements {
        match stmt {
            AstStmt::Let {
                name,
                name_span,
                type_annotation,
                value,
                ..
            } => {
                let mut value = type_expr(value, context);
                if let Some(annotation) = type_annotation {
                    value.type_ =
                        parse_type_with_variants(&annotation.text, context.declared_variants);
                }
                let scheme = context.binding_scheme(name_span, value.type_.clone());
                let binding = context.bind(&name, value.type_.clone());
                statements.push(TypedStmt::Let {
                    binding,
                    scheme,
                    value,
                });
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
        AstExpr::Add { lhs, rhs, .. } => typed_binary_i64(TypedExprKind::Add, *lhs, *rhs, context),
        AstExpr::Sub { lhs, rhs, .. } => typed_binary_i64(TypedExprKind::Sub, *lhs, *rhs, context),
        AstExpr::Mul { lhs, rhs, .. } => typed_binary_i64(TypedExprKind::Mul, *lhs, *rhs, context),
        AstExpr::Div { lhs, rhs, .. } => typed_binary_i64(TypedExprKind::Div, *lhs, *rhs, context),
        AstExpr::Mod { lhs, rhs, .. } => typed_binary_i64(TypedExprKind::Mod, *lhs, *rhs, context),
        AstExpr::Neg { expr, .. } => TypedExpr {
            type_: RsigType::I64,
            kind: TypedExprKind::Neg(Box::new(type_expr(*expr, context))),
        },
        AstExpr::Eq { lhs, rhs, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::Eq(
                Box::new(type_expr(*lhs, context)),
                Box::new(type_expr(*rhs, context)),
            ),
        },
        AstExpr::Lt { lhs, rhs, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::Lt(
                Box::new(type_expr(*lhs, context)),
                Box::new(type_expr(*rhs, context)),
            ),
        },
        AstExpr::And { lhs, rhs, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::And(
                Box::new(type_expr(*lhs, context)),
                Box::new(type_expr(*rhs, context)),
            ),
        },
        AstExpr::Or { lhs, rhs, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::Or(
                Box::new(type_expr(*lhs, context)),
                Box::new(type_expr(*rhs, context)),
            ),
        },
        AstExpr::Not { expr, .. } => TypedExpr {
            type_: RsigType::Bool,
            kind: TypedExprKind::Not(Box::new(type_expr(*expr, context))),
        },
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
            kind: TypedExprKind::Bool(value),
        },
        AstExpr::Call { callee, args, .. } => {
            let callee_path = callee.segments;
            let args = args
                .into_iter()
                .map(|arg| type_expr(arg, context))
                .collect::<Vec<_>>();
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
                        callee: callee_path,
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
                            parse_type_with_variants(&annotation.text, context.declared_variants)
                        })
                        .unwrap_or(RsigType::Unknown);
                    let binding = context.bind(name, type_.clone());
                    TypedParam {
                        binding,
                        scheme: RsigTypeScheme::monomorphic(type_.clone()),
                        type_,
                    }
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
        AstExpr::Receive { binder, body, .. } => {
            context.push_scope();
            let binder = context.bind(&binder, RsigType::Unknown);
            let body = type_expr(*body, context);
            context.pop_scope();
            TypedExpr {
                type_: RsigType::Unit,
                kind: TypedExprKind::Receive {
                    binder,
                    body: Box::new(body),
                },
            }
        }
        AstExpr::Unit { .. } => TypedExpr {
            type_: RsigType::Unit,
            kind: TypedExprKind::Unit,
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
            TypedExpr {
                type_: RsigType::Record(TypeName::new(path.segments.join("."))),
                kind: TypedExprKind::Record {
                    path: path.segments,
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
            kind: TypedExprKind::Char(value),
        },
        AstExpr::Float { value, .. } => TypedExpr {
            type_: RsigType::F64,
            kind: TypedExprKind::Float(value),
        },
        AstExpr::Int { value, .. } => TypedExpr {
            type_: RsigType::I64,
            kind: TypedExprKind::Int(value),
        },
        AstExpr::Path { path, .. } => type_path_expr(path.segments, context),
        AstExpr::String { value, .. } => TypedExpr {
            type_: RsigType::String,
            kind: TypedExprKind::String(value),
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
    if let Some((type_name, constructor)) = value_constructor_type(&path, context) {
        return TypedExpr {
            type_: RsigType::Variant(type_name.clone()),
            kind: TypedExprKind::Variant {
                type_name,
                constructor,
            },
        };
    }
    TypedExpr {
        type_,
        kind: TypedExprKind::Path(path),
    }
}

fn value_constructor_type(
    path: &[String],
    context: &TypeContext<'_>,
) -> Option<(TypeName, ConstructorName)> {
    match path {
        [constructor] => {
            let type_name = context.constructors.get(constructor)?.clone();
            Some((type_name, ConstructorName::new(constructor.clone())))
        }
        [_module, constructor] => imported_constructor_type(path, context)
            .map(|type_name| (type_name, ConstructorName::new(constructor.clone()))),
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
                scheme: RsigTypeScheme::monomorphic(type_.clone()),
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
            callee,
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
        AstPattern::Constructor { path, .. } => {
            let (type_name, constructor) = pattern_constructor_type(path.segments, context)
                .unwrap_or_else(|| (TypeName::new("_"), ConstructorName::new("_")));
            TypedPattern::Constructor {
                type_name,
                constructor,
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
    match path.as_slice() {
        [constructor] => {
            let type_name = context.constructors.get(constructor)?.clone();
            Some((type_name, ConstructorName::new(constructor.clone())))
        }
        [_module, constructor] => imported_constructor_type(&path, context)
            .map(|type_name| (type_name, ConstructorName::new(constructor.clone()))),
        _ => None,
    }
}

fn typed_binary_i64(
    build: fn(Box<TypedExpr>, Box<TypedExpr>) -> TypedExprKind,
    lhs: AstExpr,
    rhs: AstExpr,
    context: &mut TypeContext<'_>,
) -> TypedExpr {
    TypedExpr {
        type_: RsigType::I64,
        kind: build(
            Box::new(type_expr(lhs, context)),
            Box::new(type_expr(rhs, context)),
        ),
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

fn imported_constructor_type(path: &[String], context: &TypeContext<'_>) -> Option<TypeName> {
    let [module, constructor] = path else {
        return None;
    };
    context
        .imports
        .get(module.as_str())
        .and_then(|rsig| rsig.find_constructor(constructor))
        .map(|type_| imported_type_name(module, &type_.name))
}

fn imported_type_name(module_name: &str, type_name: &TypeName) -> TypeName {
    TypeName::new(format!("{module_name}.{}", type_name.as_str()))
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
            .or_else(|| {
                context
                    .constructors
                    .get(head)
                    .map(|name| RsigType::Variant(name.clone()))
            })
            .unwrap_or(RsigType::Unknown)
    } else if let Some(type_name) = imported_constructor_type(path, context) {
        RsigType::Variant(type_name)
    } else {
        RsigType::Unknown
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
    next_binding_id: usize,
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

    fn bind_fresh(&mut self, source_name: &str) -> BindingKey {
        if self.scopes.is_empty() {
            self.push_scope();
        }
        let key = BindingKey::new(format!("{source_name}${}", self.next_binding_id));
        self.next_binding_id += 1;
        self.scopes
            .last_mut()
            .expect("lowering always has a lexical scope")
            .insert(source_name.to_owned(), key.clone());
        key
    }

    fn bind_existing(&mut self, binding: &HirBinding) -> BindingKey {
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

    fn resolve(&self, source_name: &str) -> Option<&BindingKey> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(source_name))
    }
}

struct ActorLowerContext<'a> {
    functions: BTreeMap<String, (Vec<RsigType>, RsigType)>,
    externals: BTreeMap<String, (Vec<RsigType>, RsigType)>,
    imports: &'a ImportedSignatures,
}

fn function_type_map(program: &RirProgram) -> BTreeMap<String, (Vec<RsigType>, RsigType)> {
    program
        .functions
        .iter()
        .map(|function| {
            (
                function.name.clone(),
                (function.param_types.clone(), function.result.clone()),
            )
        })
        .collect()
}

fn external_type_map(program: &RirProgram) -> BTreeMap<String, (Vec<RsigType>, RsigType)> {
    program
        .externals
        .iter()
        .map(|external| {
            (
                external.name.clone(),
                (external.params.clone(), external.result.clone()),
            )
        })
        .collect()
}

fn collect_actors_from_block(
    owner: &str,
    block: &RirBlock,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorLowerContext<'_>,
    actors: &mut Vec<ActorIrActor>,
) {
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => {
                collect_actors_from_expr(owner, value, locals, context, actors);
                locals.insert(
                    name.as_str().to_owned(),
                    infer_actor_slot_type(value, locals, context),
                );
            }
            RirStmt::Expr(expr) => collect_actors_from_expr(owner, expr, locals, context, actors),
        }
    }
    if let Some(tail) = &block.tail {
        collect_actors_from_expr(owner, tail, locals, context, actors);
    }
}

fn collect_actors_from_expr(
    owner: &str,
    expr: &RirExpr,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorLowerContext<'_>,
    actors: &mut Vec<ActorIrActor>,
) {
    match expr {
        RirExpr::Spawn { actor_id, body } => {
            let actor = actor_frame_from_block(*actor_id, owner, body, locals, context);
            let mut actor_locals = actor
                .frame
                .slots
                .iter()
                .map(|slot| (slot.name.clone(), Some(slot.type_)))
                .collect::<BTreeMap<_, _>>();
            actors.push(actor);
            collect_actors_from_block(owner, body, &mut actor_locals, context, actors);
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
            collect_actors_from_expr(owner, lhs, locals, context, actors);
            collect_actors_from_expr(owner, rhs, locals, context, actors);
        }
        RirExpr::Neg(value) | RirExpr::Not(value) => {
            collect_actors_from_expr(owner, value, locals, context, actors);
        }
        RirExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_actors_from_expr(owner, condition, locals, context, actors);
            collect_actors_from_expr(owner, then_branch, locals, context, actors);
            collect_actors_from_expr(owner, else_branch, locals, context, actors);
        }
        RirExpr::Match { scrutinee, arms } => {
            collect_actors_from_expr(owner, scrutinee, locals, context, actors);
            let scrutinee_type = infer_actor_slot_type(scrutinee, locals, context);
            for arm in arms {
                let mut arm_locals = locals.clone();
                if let RirPattern::Bind(binding) = &arm.pattern {
                    arm_locals.insert(binding.as_str().to_owned(), scrutinee_type);
                }
                collect_actors_from_expr(owner, &arm.body, &mut arm_locals, context, actors);
            }
        }
        RirExpr::Block(block) => {
            let mut block_locals = locals.clone();
            collect_actors_from_block(owner, block, &mut block_locals, context, actors);
        }
        RirExpr::Call { args, .. } | RirExpr::Tuple(args) | RirExpr::List(args) => {
            for arg in args {
                collect_actors_from_expr(owner, arg, locals, context, actors);
            }
        }
        RirExpr::Apply { callee, args, .. } => {
            collect_actors_from_expr(owner, callee, locals, context, actors);
            for arg in args {
                collect_actors_from_expr(owner, arg, locals, context, actors);
            }
        }
        RirExpr::Lambda { params, body, .. } => {
            let mut lambda_locals = locals.clone();
            for param in params {
                lambda_locals.insert(param.as_str().to_owned(), None);
            }
            collect_actors_from_block(owner, body, &mut lambda_locals, context, actors);
        }
        RirExpr::Record { fields, .. } => {
            for (_, value) in fields {
                collect_actors_from_expr(owner, value, locals, context, actors);
            }
        }
        RirExpr::Field { base, .. } | RirExpr::TupleIndex { base, .. } => {
            collect_actors_from_expr(owner, base, locals, context, actors);
        }
        RirExpr::Receive { binder, body } => {
            let previous = locals.insert(binder.as_str().to_owned(), None);
            collect_actors_from_expr(owner, body, locals, context, actors);
            if let Some(previous) = previous {
                locals.insert(binder.as_str().to_owned(), previous);
            } else {
                locals.remove(binder.as_str());
            }
        }
        RirExpr::Bool(_)
        | RirExpr::Unit
        | RirExpr::Char(_)
        | RirExpr::Float(_)
        | RirExpr::Int(_)
        | RirExpr::Variant { .. }
        | RirExpr::Path(_)
        | RirExpr::String(_) => {}
    }
}

fn actor_frame_from_block(
    actor_id: usize,
    owner: &str,
    block: &RirBlock,
    outer_locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorLowerContext<'_>,
) -> ActorIrActor {
    let ops = actor_ops(block);
    let mut bound = BTreeSet::new();
    let mut free = BTreeSet::new();
    for op in &ops {
        match &op {
            ActorFrameOp::Let { name, value } => {
                collect_free_expr(value, &bound, &mut free);
                bound.insert(name.clone());
            }
            ActorFrameOp::Expr(expr) => collect_free_expr(expr, &bound, &mut free),
            ActorFrameOp::Receive { binder, body } => {
                let mut receive_bound = bound.clone();
                receive_bound.insert(binder.clone());
                collect_free_expr(body, &receive_bound, &mut free);
            }
        }
    }

    let mut slots = Vec::new();
    for name in free {
        if let Some(Some(type_)) = outer_locals.get(&name) {
            slots.push(ActorFrameSlot {
                name,
                type_: *type_,
                field_index: slots.len() as u32 + 1,
            });
        }
    }
    let captures = slots.clone();

    let mut local_types = slots
        .iter()
        .map(|slot| (slot.name.clone(), Some(slot.type_)))
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
                local_types.insert(name.clone(), Some(type_));
            } else {
                local_types.insert(name.clone(), None);
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
            ActorFrameState { index, op, next }
        })
        .collect::<Vec<_>>();

    ActorIrActor {
        id: actor_id,
        owner: owner.to_owned(),
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
                name: name.as_str().to_owned(),
                value: value.clone(),
            }),
            RirStmt::Expr(RirExpr::Receive { binder, body }) => {
                ops.push(ActorFrameOp::Receive {
                    binder: binder.as_str().to_owned(),
                    body: *body.clone(),
                });
            }
            RirStmt::Expr(expr) => ops.push(ActorFrameOp::Expr(expr.clone())),
        }
    }
    if let Some(expr) = &block.tail {
        match expr {
            RirExpr::Receive { binder, body } => {
                ops.push(ActorFrameOp::Receive {
                    binder: binder.as_str().to_owned(),
                    body: *body.clone(),
                });
            }
            expr => ops.push(ActorFrameOp::Expr(expr.clone())),
        }
    }
    ops
}

fn collect_free_expr(expr: &RirExpr, bound: &BTreeSet<String>, free: &mut BTreeSet<String>) {
    match expr {
        RirExpr::Path(path) => {
            if let [name] = path.as_slice()
                && !bound.contains(name)
            {
                free.insert(name.clone());
            }
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
            collect_free_expr(lhs, bound, free);
            collect_free_expr(rhs, bound, free);
        }
        RirExpr::Neg(value) | RirExpr::Not(value) => collect_free_expr(value, bound, free),
        RirExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_free_expr(condition, bound, free);
            collect_free_expr(then_branch, bound, free);
            collect_free_expr(else_branch, bound, free);
        }
        RirExpr::Match { scrutinee, arms } => {
            collect_free_expr(scrutinee, bound, free);
            for arm in arms {
                let mut nested = bound.clone();
                bind_pattern_names(&arm.pattern, &mut nested);
                collect_free_expr(&arm.body, &nested, free);
            }
        }
        RirExpr::Block(block) => collect_free_block(block, bound, free),
        RirExpr::Call { args, .. } | RirExpr::Tuple(args) | RirExpr::List(args) => {
            for arg in args {
                collect_free_expr(arg, bound, free);
            }
        }
        RirExpr::Apply { callee, args, .. } => {
            collect_free_expr(callee, bound, free);
            for arg in args {
                collect_free_expr(arg, bound, free);
            }
        }
        RirExpr::Lambda { params, body, .. } => {
            let mut nested = bound.clone();
            for param in params {
                nested.insert(param.as_str().to_owned());
            }
            collect_free_block(body, &nested, free);
        }
        RirExpr::Record { fields, .. } => {
            for (_, value) in fields {
                collect_free_expr(value, bound, free);
            }
        }
        RirExpr::Field { base, .. } | RirExpr::TupleIndex { base, .. } => {
            collect_free_expr(base, bound, free)
        }
        RirExpr::Spawn { body, .. } => collect_free_block(body, bound, free),
        RirExpr::Receive { binder, body } => {
            let mut nested = bound.clone();
            nested.insert(binder.as_str().to_owned());
            collect_free_expr(body, &nested, free);
        }
        RirExpr::Bool(_)
        | RirExpr::Unit
        | RirExpr::Char(_)
        | RirExpr::Float(_)
        | RirExpr::Int(_)
        | RirExpr::Variant { .. }
        | RirExpr::String(_) => {}
    }
}

fn collect_free_block(
    block: &RirBlock,
    outer_bound: &BTreeSet<String>,
    free: &mut BTreeSet<String>,
) {
    let mut bound = outer_bound.clone();
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => {
                collect_free_expr(value, &bound, free);
                bound.insert(name.as_str().to_owned());
            }
            RirStmt::Expr(expr) => collect_free_expr(expr, &bound, free),
        }
    }
    if let Some(tail) = &block.tail {
        collect_free_expr(tail, &bound, free);
    }
}

fn bind_pattern_names(pattern: &RirPattern, bound: &mut BTreeSet<String>) {
    if let RirPattern::Bind(binding) = pattern {
        bound.insert(binding.as_str().to_owned());
    }
}

fn closure_convert_block(block: &mut RirBlock) {
    for stmt in &mut block.statements {
        match stmt {
            RirStmt::Let { value, .. } | RirStmt::Expr(value) => closure_convert_expr(value),
        }
    }
    if let Some(tail) = &mut block.tail {
        closure_convert_expr(tail);
    }
}

fn closure_convert_expr(expr: &mut RirExpr) {
    match expr {
        RirExpr::Add(lhs, rhs)
        | RirExpr::Sub(lhs, rhs)
        | RirExpr::Mul(lhs, rhs)
        | RirExpr::Div(lhs, rhs)
        | RirExpr::Mod(lhs, rhs)
        | RirExpr::Eq(lhs, rhs)
        | RirExpr::Lt(lhs, rhs)
        | RirExpr::And(lhs, rhs)
        | RirExpr::Or(lhs, rhs) => {
            closure_convert_expr(lhs);
            closure_convert_expr(rhs);
        }
        RirExpr::Neg(value) | RirExpr::Not(value) => closure_convert_expr(value),
        RirExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            closure_convert_expr(condition);
            closure_convert_expr(then_branch);
            closure_convert_expr(else_branch);
        }
        RirExpr::Match { scrutinee, arms } => {
            closure_convert_expr(scrutinee);
            for arm in arms {
                closure_convert_expr(&mut arm.body);
            }
        }
        RirExpr::Block(block) => closure_convert_block(block),
        RirExpr::Call { args, .. } | RirExpr::Tuple(args) | RirExpr::List(args) => {
            for arg in args {
                closure_convert_expr(arg);
            }
        }
        RirExpr::Apply { callee, args, .. } => {
            closure_convert_expr(callee);
            for arg in args {
                closure_convert_expr(arg);
            }
        }
        RirExpr::Lambda {
            params,
            captures,
            body,
        } => {
            closure_convert_block(body);
            let bound = params
                .iter()
                .map(|param| param.as_str().to_owned())
                .collect::<BTreeSet<_>>();
            let mut free = BTreeSet::new();
            collect_free_block(body, &bound, &mut free);
            *captures = free.into_iter().map(Capture::new).collect();
        }
        RirExpr::Record { fields, .. } => {
            for (_, value) in fields {
                closure_convert_expr(value);
            }
        }
        RirExpr::Field { base, .. } | RirExpr::TupleIndex { base, .. } => {
            closure_convert_expr(base);
        }
        RirExpr::Spawn { body, .. } => closure_convert_block(body),
        RirExpr::Receive { body, .. } => closure_convert_expr(body),
        RirExpr::Bool(_)
        | RirExpr::Unit
        | RirExpr::Char(_)
        | RirExpr::Float(_)
        | RirExpr::Int(_)
        | RirExpr::Variant { .. }
        | RirExpr::Path(_)
        | RirExpr::String(_) => {}
    }
}

fn infer_actor_slot_type(
    expr: &RirExpr,
    locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorLowerContext<'_>,
) -> Option<ActorSlotType> {
    match expr {
        RirExpr::Add(_, _)
        | RirExpr::Sub(_, _)
        | RirExpr::Mul(_, _)
        | RirExpr::Div(_, _)
        | RirExpr::Mod(_, _)
        | RirExpr::Neg(_)
        | RirExpr::Int(_) => Some(ActorSlotType::I64),
        RirExpr::Eq(_, _)
        | RirExpr::Lt(_, _)
        | RirExpr::And(_, _)
        | RirExpr::Or(_, _)
        | RirExpr::Not(_)
        | RirExpr::Bool(_) => Some(ActorSlotType::Bool),
        RirExpr::If {
            then_branch,
            else_branch,
            ..
        } => unify_actor_slot_type(
            infer_actor_slot_type(then_branch, locals, context),
            infer_actor_slot_type(else_branch, locals, context),
        ),
        RirExpr::Match { arms, .. } => arms
            .iter()
            .map(|arm| infer_actor_slot_type(&arm.body, locals, context))
            .fold(None, unify_actor_slot_type),
        RirExpr::Block(block) => infer_actor_block_slot_type(block, locals, context),
        RirExpr::Call { callee, .. } => match callee.as_slice() {
            [name] if name == "dbg" || name == "println" => None,
            [name] if name == "send" || name == "monitor" || name == "link" => None,
            [name] if name == "list_len" || name == "string_len" => Some(ActorSlotType::I64),
            [name] if name == "list_get" || name == "string_concat" => Some(ActorSlotType::Value),
            [name] => context
                .externals
                .get(name)
                .or_else(|| context.functions.get(name))
                .and_then(|(_, result)| ActorSlotType::from_rsig(result)),
            [module, name] => context
                .imports
                .get(module.as_str())
                .and_then(|rsig| rsig.find(name))
                .and_then(|export| match export {
                    RsigExport::Function(function) => ActorSlotType::from_rsig(&function.result),
                    RsigExport::External(external) => ActorSlotType::from_rsig(&external.result),
                }),
            _ => None,
        },
        RirExpr::Path(path) => path
            .first()
            .and_then(|name| locals.get(name))
            .copied()
            .flatten(),
        RirExpr::Spawn { .. } => Some(ActorSlotType::ActorId),
        RirExpr::Tuple(_)
        | RirExpr::List(_)
        | RirExpr::Lambda { .. }
        | RirExpr::Record { .. }
        | RirExpr::Variant { .. }
        | RirExpr::Field { .. }
        | RirExpr::TupleIndex { .. }
        | RirExpr::String(_) => Some(ActorSlotType::Value),
        RirExpr::Apply { result, .. } => ActorSlotType::from_rsig(result),
        RirExpr::Unit | RirExpr::Receive { .. } | RirExpr::Char(_) | RirExpr::Float(_) => None,
    }
}

fn infer_actor_block_slot_type(
    block: &RirBlock,
    outer_locals: &BTreeMap<String, Option<ActorSlotType>>,
    context: &ActorLowerContext<'_>,
) -> Option<ActorSlotType> {
    let mut locals = outer_locals.clone();
    for stmt in &block.statements {
        match stmt {
            RirStmt::Let { name, value } => {
                let type_ = infer_actor_slot_type(value, &locals, context);
                locals.insert(name.as_str().to_owned(), type_);
            }
            RirStmt::Expr(expr) => {
                infer_actor_slot_type(expr, &locals, context);
            }
        }
    }
    block
        .tail
        .as_ref()
        .and_then(|tail| infer_actor_slot_type(tail, &locals, context))
}

fn unify_actor_slot_type(
    lhs: Option<ActorSlotType>,
    rhs: Option<ActorSlotType>,
) -> Option<ActorSlotType> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
        (Some(value), None) | (None, Some(value)) => Some(value),
        _ => None,
    }
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
        TypedExprKind::Add(lhs, rhs) => RirExpr::Add(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Sub(lhs, rhs) => RirExpr::Sub(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Mul(lhs, rhs) => RirExpr::Mul(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Div(lhs, rhs) => RirExpr::Div(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Mod(lhs, rhs) => RirExpr::Mod(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Neg(expr) => RirExpr::Neg(Box::new(lower_expr(*expr, context))),
        TypedExprKind::Eq(lhs, rhs) => RirExpr::Eq(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Lt(lhs, rhs) => RirExpr::Lt(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::And(lhs, rhs) => RirExpr::And(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Or(lhs, rhs) => RirExpr::Or(
            Box::new(lower_expr(*lhs, context)),
            Box::new(lower_expr(*rhs, context)),
        ),
        TypedExprKind::Not(expr) => RirExpr::Not(Box::new(lower_expr(*expr, context))),
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
        TypedExprKind::Bool(value) => RirExpr::Bool(value),
        TypedExprKind::Call { callee, args } => RirExpr::Call {
            callee,
            args: args
                .into_iter()
                .map(|arg| lower_expr(arg, context))
                .collect(),
        },
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
        TypedExprKind::Receive { binder, body } => {
            context.push_scope();
            let binder = context.bind_existing(&binder);
            let body = lower_expr(*body, context);
            context.pop_scope();
            RirExpr::Receive {
                binder,
                body: Box::new(body),
            }
        }
        TypedExprKind::Unit => RirExpr::Unit,
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
            path,
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
        TypedExprKind::Variant {
            type_name,
            constructor,
        } => RirExpr::Variant {
            type_name,
            constructor,
        },
        TypedExprKind::Char(value) => RirExpr::Char(value),
        TypedExprKind::Float(value) => RirExpr::Float(value),
        TypedExprKind::Int(value) => RirExpr::Int(value),
        TypedExprKind::Local(binding) => RirExpr::Path(vec![binding.key_name()]),
        TypedExprKind::Path(path) => {
            if let [name] = path.as_slice()
                && let Some(binding) = context.resolve(name)
            {
                return RirExpr::Path(vec![binding.as_str().to_owned()]);
            }
            RirExpr::Path(path)
        }
        TypedExprKind::String(value) => RirExpr::String(value),
    }
}

fn lower_pattern(pattern: TypedPattern, context: &mut LowerContext) -> RirPattern {
    match pattern {
        TypedPattern::Wildcard => RirPattern::Wildcard,
        TypedPattern::Bind { binding, .. } => RirPattern::Bind(context.bind_existing(&binding)),
        TypedPattern::Constructor {
            type_name,
            constructor,
        } => RirPattern::Constructor {
            type_name,
            constructor,
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
    use crate::infer::module::infer_function_signatures;
    use crate::parser::parse_source;
    use crate::signature::{ImportedSignatures, ModuleName, RsigType};

    use super::{
        Capture, RirExpr, RirStmt, TypedExprKind, TypedStmt, lower_typed_to_rir,
        typed_program_from_ast,
    };

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
        let function_types = infer_function_signatures(&program, &imports).unwrap();
        typed_program_from_ast(
            ModuleName::new(module),
            program,
            &imports,
            &function_types,
            None,
            None,
        )
    }

    fn typed_program_with_inference_facts(module: &str, source: &str) -> super::TypedProgram {
        let path = camino::Utf8Path::new("test.ml");
        let program = parse_source(path, source).unwrap();
        let imports = ImportedSignatures::new();
        let inferred = crate::infer::module::infer_program(&program, &imports).unwrap();
        let function_types = inferred.function_signatures(&program);
        let expression_types = inferred.expression_rsig_types();
        let binding_schemes = inferred.binding_rsig_schemes();
        typed_program_from_ast(
            ModuleName::new(module),
            program,
            &imports,
            &function_types,
            Some(&expression_types),
            Some(&binding_schemes),
        )
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
        let rir = lower_typed_to_rir(typed);
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
                            name_span: span(),
                            type_annotation: None,
                            value: AstExpr::Int {
                                value: 1,
                                span: span(),
                            },
                            span: span(),
                        },
                        AstStmt::Let {
                            name: "f".to_owned(),
                            name_span: span(),
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
                            span: span(),
                        },
                        AstStmt::Let {
                            name: "a".to_owned(),
                            name_span: span(),
                            type_annotation: None,
                            value: AstExpr::Int {
                                value: 2,
                                span: span(),
                            },
                            span: span(),
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
        let rir = lower_typed_to_rir(typed);
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
                            name_span: span(),
                            type_annotation: None,
                            value: AstExpr::Int {
                                value: 1,
                                span: span(),
                            },
                            span: span(),
                        },
                        AstStmt::Let {
                            name: "f".to_owned(),
                            name_span: span(),
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
                            span: span(),
                        },
                        AstStmt::Let {
                            name: "a".to_owned(),
                            name_span: span(),
                            type_annotation: None,
                            value: AstExpr::Int {
                                value: 2,
                                span: span(),
                            },
                            span: span(),
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
        let rir = lower_typed_to_rir(typed);
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
        let Some(TypedStmt::Let { value, scheme, .. }) = typed.functions[0].body.statements.first()
        else {
            panic!("expected a let-bound lambda");
        };
        assert_eq!(scheme.quantifiers.len(), 1);
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
}

#[allow(dead_code)]
fn _keep_ast_path_used(_: &AstPath) {}
