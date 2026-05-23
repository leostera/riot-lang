use std::collections::{BTreeMap, BTreeSet};

use crate::ast::{
    AstBlock, AstDecl, AstExpr, AstPattern, AstProgram, AstStmt, AstTypeAnnotation, AstTypeBody,
    TextSpan,
};
use crate::imported_types::{imported_type_name, qualify_imported_type};
use crate::infer::module::ExpressionTypeTable;
use crate::signature::{
    ConstructorName, FieldName, FunctionSignature, FunctionTable, ImportedSignatures, ModuleName,
    Rsig, RsigType, RsigTypeDeclKind, TypeName, TypeParamName,
};
use crate::stdlib::Stdlib;
use crate::type_lowerer::RsigTypeLowerer;

use crate::checker::tyir::{
    BindingId, EntityId, TypedBlock, TypedExpr, TypedExprKind, TypedExternal, TypedFunction,
    TypedLiteral, TypedMatchArm, TypedParam, TypedPattern, TypedProgram, TypedReceiveArm,
    TypedRecordField, TypedStmt, TypedTypeBody, TypedTypeDecl, TypedUse, TypedVariantConstructor,
};

use super::callable::{
    CallableResolver, ExternalSignature, ExternalTable, prelude_external_signatures,
};

pub(crate) fn typed_program_from_ast(
    module_name: ModuleName,
    ast: AstProgram,
    imports: &ImportedSignatures,
    function_types: &FunctionTable,
    expression_types: Option<&ExpressionTypeTable>,
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
                abi: external.abi.into(),
            }
        })
        .collect::<Vec<_>>();
    let constructors = constructor_type_map(&types);
    let records = record_type_map(&types, &uses, imports);
    let record_fields = record_field_type_map(&types, &uses, imports);
    let record_type_params = record_type_param_map(&types, &uses, imports);

    let mut external_types = prelude_external_signatures();
    for external in &externals {
        external_types.insert(
            external.name.clone(),
            ExternalSignature::new(
                external.params.clone(),
                external.result.clone(),
                external.abi.clone(),
            ),
        );
    }
    let mut functions = Vec::new();
    for function in ast_functions {
        let symbol = module_function_symbol(&module_name, &function.name);
        let function_type = function_types
            .get(&function.name)
            .cloned()
            .unwrap_or_else(|| FunctionSignature::unknown_arity(function.params.len()));
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
            record_type_params: &record_type_params,
            declared_variants: &declared_variants,
            expression_types,
        });
        let params = function
            .params
            .iter()
            .enumerate()
            .map(|(index, name)| {
                let type_ = function_type
                    .params
                    .get(index)
                    .cloned()
                    .unwrap_or(RsigType::Unknown);
                TypedParam {
                    binding: context.bind(name, type_.clone()),
                    type_,
                }
            })
            .collect::<Vec<_>>();
        let body = type_block(function.body, &mut context);
        let result = annotated_result
            .unwrap_or_else(|| merge_types(function_type.result.clone(), body.type_.clone()));
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
    type_params: Vec<TypeParamName>,
    payload: Vec<RsigType>,
}

struct TypeContext<'a> {
    next_binding_id: usize,
    scopes: Vec<BTreeMap<String, TypedBindingInfo>>,
    functions: &'a FunctionTable,
    externals: &'a ExternalTable,
    imports: &'a ImportedSignatures,
    constructors: &'a BTreeMap<String, ConstructorSignature>,
    records: &'a BTreeMap<String, TypeName>,
    record_fields: &'a BTreeMap<TypeName, BTreeMap<String, RsigType>>,
    record_type_params: &'a BTreeMap<TypeName, Vec<TypeParamName>>,
    declared_variants: &'a BTreeSet<TypeName>,
    expression_types: Option<&'a ExpressionTypeTable>,
}

struct TypeContextInputs<'a> {
    function_types: &'a FunctionTable,
    externals: &'a ExternalTable,
    imports: &'a ImportedSignatures,
    constructors: &'a BTreeMap<String, ConstructorSignature>,
    records: &'a BTreeMap<String, TypeName>,
    record_fields: &'a BTreeMap<TypeName, BTreeMap<String, RsigType>>,
    record_type_params: &'a BTreeMap<TypeName, Vec<TypeParamName>>,
    declared_variants: &'a BTreeSet<TypeName>,
    expression_types: Option<&'a ExpressionTypeTable>,
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
            record_type_params: inputs.record_type_params,
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

    fn binding_type(&self, binding: &BindingId) -> Option<RsigType> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.get(&binding.name))
            .find(|info| info.binding.id == binding.id)
            .map(|info| info.type_.clone())
    }

    fn update_binding_type(&mut self, binding: &BindingId, type_: RsigType) {
        if matches!(type_, RsigType::Unknown | RsigType::Var(_)) {
            return;
        }
        for scope in self.scopes.iter_mut().rev() {
            let Some(info) = scope.get_mut(&binding.name) else {
                continue;
            };
            if info.binding.id == binding.id {
                info.type_ = type_;
                return;
            }
        }
    }

    fn expression_type(&self, span: TextSpan) -> Option<RsigType> {
        self.expression_types
            .and_then(|types| types.get(span))
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
                        type_params: type_.params.clone(),
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
                            type_params: type_.params.clone(),
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

fn record_type_param_map(
    types: &[TypedTypeDecl],
    uses: &[TypedUse],
    imports: &ImportedSignatures,
) -> BTreeMap<TypeName, Vec<TypeParamName>> {
    let mut params_by_type = BTreeMap::new();
    for type_ in types {
        if matches!(type_.body, TypedTypeBody::Record { .. }) {
            params_by_type.insert(type_.name.clone(), type_.params.clone());
        }
    }
    for use_ in uses {
        let Some(rsig) = imports.get(use_.name.as_str()) else {
            continue;
        };
        for type_ in &rsig.types {
            if matches!(type_.body, RsigTypeDeclKind::Record { .. }) {
                params_by_type.insert(
                    imported_type_name(use_.name.as_str(), &type_.name),
                    type_.params.clone(),
                );
            }
        }
    }
    params_by_type
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
                    .map(|field| {
                        (
                            field.name.as_str().to_owned(),
                            qualify_imported_type(use_.name.as_str(), &field.type_),
                        )
                    })
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
        typed.type_ = type_.clone();
        if let TypedExprKind::Local(binding) = &typed.kind {
            context.update_binding_type(binding, type_);
        }
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
            typed_operator_call("neg", vec![*expr], RsigType::I64, context)
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
                let mut pattern = type_pattern(arm.pattern, &scrutinee.type_, context);
                let body = type_expr(arm.body, context);
                refine_pattern_binding_types(&mut pattern, context);
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
                if let Some(signature) = call_signature(&callee_path, context) {
                    if args.len() < signature.params.len() {
                        return partial_call_lambda(
                            callee_path,
                            args,
                            signature.params,
                            signature.result,
                            context,
                        );
                    }
                    for (arg, expected) in args.iter().zip(signature.params.iter().cloned()) {
                        refine_expr_as_type(arg, expected, context);
                    }
                    TypedExpr {
                        type_: signature.result,
                        kind: TypedExprKind::Call {
                            callee: EntityId::from_segments(callee_path),
                            args,
                        },
                    }
                } else {
                    TypedExpr {
                        type_: RsigType::Unknown,
                        kind: TypedExprKind::Call {
                            callee: EntityId::from_segments(callee_path),
                            args,
                        },
                    }
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
            span,
        } => {
            let inferred_param_types = context
                .expression_type(span)
                .map(|type_| arrow_parameter_types(&type_, params.len()))
                .unwrap_or_default();
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
                        .or_else(|| inferred_param_types.get(index).cloned())
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
        AstExpr::Spawn { body, span } => TypedExpr {
            type_: context
                .expression_type(span)
                .filter(concrete_actor_id_type)
                .unwrap_or_else(|| RsigType::ActorId(Box::new(RsigType::Unknown))),
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
                        let mut pattern = type_pattern(arm.pattern, &pattern_type, context);
                        let body = type_expr(arm.body, context);
                        refine_pattern_binding_types(&mut pattern, context);
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
    if let Some(signature) = call_signature(&path, context) {
        return partial_call_lambda(
            path,
            Vec::new(),
            signature.params,
            signature.result,
            context,
        );
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

fn concrete_actor_id_type(type_: &RsigType) -> bool {
    matches!(
        type_,
        RsigType::ActorId(message)
            if !matches!(message.as_ref(), RsigType::Unknown | RsigType::Var(_))
    )
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

fn arrow_parameter_types(type_: &RsigType, arity: usize) -> Vec<RsigType> {
    let mut params = Vec::new();
    let mut current = type_;
    while params.len() < arity {
        let RsigType::Arrow { parameter, result } = current else {
            break;
        };
        params.push(parameter.as_ref().clone());
        current = result;
    }
    params
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
                .map(|constructor| instantiate_constructor_payload(&constructor, scrutinee_type))
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
            let field_types = instantiate_record_fields(&type_name, scrutinee_type, context);
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

fn refine_pattern_binding_types(pattern: &mut TypedPattern, context: &TypeContext<'_>) {
    match pattern {
        TypedPattern::Bind { binding, type_ } => {
            if let Some(refined) = context.binding_type(binding) {
                *type_ = refined;
            }
        }
        TypedPattern::Constructor { payload, .. } | TypedPattern::Tuple(payload) => {
            for pattern in payload {
                refine_pattern_binding_types(pattern, context);
            }
        }
        TypedPattern::List { prefix, tail } => {
            for pattern in prefix {
                refine_pattern_binding_types(pattern, context);
            }
            if let Some(tail) = tail {
                refine_pattern_binding_types(tail, context);
            }
        }
        TypedPattern::Record { fields, .. } => {
            for (_, pattern) in fields {
                refine_pattern_binding_types(pattern, context);
            }
        }
        TypedPattern::Wildcard
        | TypedPattern::Unit
        | TypedPattern::Bool(_)
        | TypedPattern::Int(_)
        | TypedPattern::String(_) => {}
    }
}

fn instantiate_constructor_payload(
    constructor: &ConstructorSignature,
    scrutinee_type: &RsigType,
) -> Vec<RsigType> {
    let substitutions = generic_type_substitutions(
        &constructor.type_name,
        &constructor.type_params,
        scrutinee_type,
    );
    constructor
        .payload
        .iter()
        .map(|type_| substitute_generic_type_vars(type_, &substitutions))
        .collect()
}

fn instantiate_record_fields(
    type_name: &TypeName,
    scrutinee_type: &RsigType,
    context: &TypeContext<'_>,
) -> BTreeMap<String, RsigType> {
    let field_types = context
        .record_fields
        .get(type_name)
        .cloned()
        .unwrap_or_default();
    let params = context
        .record_type_params
        .get(type_name)
        .cloned()
        .unwrap_or_default();
    let substitutions = generic_type_substitutions(type_name, &params, scrutinee_type);
    field_types
        .into_iter()
        .map(|(field, type_)| (field, substitute_generic_type_vars(&type_, &substitutions)))
        .collect()
}

fn generic_type_substitutions(
    type_name: &TypeName,
    params: &[TypeParamName],
    scrutinee_type: &RsigType,
) -> BTreeMap<String, RsigType> {
    let args = match scrutinee_type {
        RsigType::RecordApp { name, args } | RsigType::VariantApp { name, args }
            if name == type_name =>
        {
            args
        }
        _ => return BTreeMap::new(),
    };
    params
        .iter()
        .zip(args)
        .map(|(param, arg)| (param.as_str().to_owned(), arg.clone()))
        .collect()
}

fn substitute_generic_type_vars(
    type_: &RsigType,
    substitutions: &BTreeMap<String, RsigType>,
) -> RsigType {
    match type_ {
        RsigType::Var(name) => substitutions
            .get(name.as_str())
            .cloned()
            .unwrap_or_else(|| type_.clone()),
        RsigType::ActorId(message) => RsigType::ActorId(Box::new(substitute_generic_type_vars(
            message,
            substitutions,
        ))),
        RsigType::Arrow { parameter, result } => RsigType::Arrow {
            parameter: Box::new(substitute_generic_type_vars(parameter, substitutions)),
            result: Box::new(substitute_generic_type_vars(result, substitutions)),
        },
        RsigType::Tuple(items) => RsigType::Tuple(
            items
                .iter()
                .map(|item| substitute_generic_type_vars(item, substitutions))
                .collect(),
        ),
        RsigType::List(item) => {
            RsigType::List(Box::new(substitute_generic_type_vars(item, substitutions)))
        }
        RsigType::RecordApp { name, args } => RsigType::RecordApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| substitute_generic_type_vars(arg, substitutions))
                .collect(),
        },
        RsigType::VariantApp { name, args } => RsigType::VariantApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| substitute_generic_type_vars(arg, substitutions))
                .collect(),
        },
        _ => type_.clone(),
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
    let typed_args = args
        .into_iter()
        .map(|arg| type_expr(arg, context))
        .collect::<Vec<_>>();
    let expected_args = operator_argument_types(operator, typed_args.len());
    for (arg, expected) in typed_args.iter().zip(expected_args) {
        refine_expr_as_type(arg, expected, context);
    }
    TypedExpr {
        type_: result,
        kind: TypedExprKind::Call {
            callee: EntityId::from_segments(Stdlib::prelude_path(operator)),
            args: typed_args,
        },
    }
}

fn operator_argument_types(operator: &str, arity: usize) -> Vec<RsigType> {
    let type_ = match operator {
        "(+)" | "(-)" | "(*)" | "(/)" | "(%)" | "neg" => Some(RsigType::I64),
        "(&&)" | "(||)" | "(!)" => Some(RsigType::Bool),
        _ => None,
    };
    type_.into_iter().cycle().take(arity).collect()
}

fn refine_expr_as_type(expr: &TypedExpr, expected: RsigType, context: &mut TypeContext<'_>) {
    if let TypedExprKind::Local(binding) = &expr.kind {
        context.update_binding_type(binding, expected);
    }
}

fn is_named_call(callee: &[String], context: &TypeContext<'_>) -> bool {
    match callee {
        [name] if context.resolve(name).is_some() => false,
        _ => call_signature(callee, context).is_some(),
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
                    type_params: type_.params.clone(),
                    payload: candidate
                        .payload
                        .iter()
                        .map(|payload| qualify_imported_type(module, payload))
                        .collect(),
                })
        })
}

fn call_signature(callee: &[String], context: &TypeContext<'_>) -> Option<FunctionSignature> {
    CallableResolver::new(context.functions, context.externals, context.imports)
        .resolve(callee)
        .map(|signature| signature.function)
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

#[cfg(test)]
mod tests {
    use crate::ast::{
        AstBlock, AstDecl, AstExpr, AstFnDecl, AstPath, AstProgram, AstStmt, TextSpan,
    };
    use crate::checker::tyir::{TyIrBuilder, TypedPattern};
    use crate::infer::module::ModuleInferencer;
    use crate::parser::SourceParser;
    use crate::signature::{
        ConstructorName, FieldName, FunctionTable, ImportedSignatures, ModuleName, Rsig,
        RsigRecordField, RsigType, RsigTypeDecl, RsigTypeDeclKind, RsigVariantConstructor, TypeName,
    };

    use crate::actor::air::ActorSlotType;
    use crate::actor::lower::ActorIrLowerer;
    use crate::lambda::lower::LambdaLowerer;

    use crate::lambda::ir::{BindingKey, Capture, LambdaExpr, LambdaPattern, LambdaStmt};

    use super::{TypedExprKind, TypedStmt, TypedUse, record_field_type_map};

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
        let imports = ImportedSignatures::new();
        typed_program_with_imports_and_inference_facts(module, source, imports)
    }

    fn typed_program_with_imports_and_inference_facts(
        module: &str,
        source: &str,
        imports: ImportedSignatures,
    ) -> super::TypedProgram {
        let path = camino::Utf8Path::new("test.ml");
        let program = SourceParser::new().parse(path, source).unwrap();
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

    fn imported_boxes_signature() -> ImportedSignatures {
        let mut imports = ImportedSignatures::new();
        imports.insert(
            ModuleName::new("Boxes"),
            Rsig {
                module: ModuleName::new("Boxes"),
                dependencies: Vec::new(),
                types: vec![RsigTypeDecl {
                    name: TypeName::new("box"),
                    params: vec![crate::signature::TypeParamName::new("'a")],
                    body: RsigTypeDeclKind::Record {
                        fields: vec![RsigRecordField {
                            name: FieldName::new("value"),
                            type_: RsigType::Var(crate::signature::TypeVarName::new("'a")),
                        }],
                    },
                    fingerprint: 0,
                }],
                exports: Vec::new(),
                module_fingerprint: 0,
            },
        );
        imports
    }

    fn imported_options_signature() -> ImportedSignatures {
        let mut imports = ImportedSignatures::new();
        insert_options_signature(&mut imports);
        imports
    }

    fn imported_boxes_and_options_signature() -> ImportedSignatures {
        let mut imports = imported_boxes_signature();
        insert_options_signature(&mut imports);
        imports
    }

    fn insert_options_signature(imports: &mut ImportedSignatures) {
        imports.insert(
            ModuleName::new("Options"),
            Rsig {
                module: ModuleName::new("Options"),
                dependencies: Vec::new(),
                types: vec![RsigTypeDecl {
                    name: TypeName::new("option"),
                    params: vec![crate::signature::TypeParamName::new("'a")],
                    body: RsigTypeDeclKind::Variant {
                        constructors: vec![
                            RsigVariantConstructor {
                                name: ConstructorName::new("Some"),
                                payload: vec![RsigType::Var(crate::signature::TypeVarName::new(
                                    "'a",
                                ))],
                            },
                            RsigVariantConstructor {
                                name: ConstructorName::new("None"),
                                payload: Vec::new(),
                            },
                        ],
                    },
                    fingerprint: 0,
                }],
                exports: Vec::new(),
                module_fingerprint: 0,
            },
        );
    }

    #[test]
    fn lambda_ir_lambdas_carry_capture_names() {
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
        let lambda_ir = LambdaLowerer::new().lower(typed);
        let Some(LambdaExpr::Lambda { captures, .. }) = &lambda_ir.functions[0].body.tail else {
            panic!("expected lowered lambda");
        };

        assert_eq!(
            captures.as_slice(),
            &[Capture::from_key(BindingKey::resolved("n", 0))]
        );
    }

    #[test]
    fn lambda_ir_captures_track_shadowed_binding_identity() {
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
        let lambda_ir = LambdaLowerer::new().lower(typed);
        let Some(LambdaStmt::Let {
            value: LambdaExpr::Lambda { captures, .. },
            ..
        }) = lambda_ir.functions[0].body.statements.get(1)
        else {
            panic!("expected second statement to bind a lambda");
        };

        assert_eq!(
            captures.as_slice(),
            &[Capture::from_key(BindingKey::resolved("a", 0))]
        );
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
    fn typed_hir_uses_inference_facts_for_lambda_parameter_types() {
        let typed = typed_program_with_inference_facts(
            "LambdaParamFacts",
            "fn main() { let inc = fn(x) { x + 1 }; inc }",
        );
        let Some(TypedStmt::Let { value, .. }) = typed.functions[0].body.statements.first() else {
            panic!("expected a let-bound lambda");
        };
        let TypedExprKind::Lambda { params, .. } = &value.kind else {
            panic!("expected lambda value");
        };

        assert_eq!(params[0].type_, RsigType::I64);
    }

    #[test]
    fn lambda_ir_apply_carries_typed_arrow_result() {
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
        let lambda_ir = LambdaLowerer::new().lower(typed);
        let Some(LambdaExpr::Call { callee, args, .. }) = &lambda_ir.functions[0].body.tail else {
            panic!("expected operator call tail");
        };
        assert_eq!(
            callee.as_slice(),
            &["Std".to_owned(), "Prelude".to_owned(), "(+)".to_owned()]
        );
        let LambdaExpr::Apply { result, .. } = &args[0] else {
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
    fn typed_hir_uses_inference_facts_for_spawn_message_types() {
        let typed = typed_program_with_inference_facts(
            "SpawnMessageFacts",
            "fn main() { spawn { receive { 1 -> (), 2 -> () } } }",
        );
        let Some(tail) = &typed.functions[0].body.tail else {
            panic!("expected a tail expression");
        };
        let TypedExprKind::Spawn { .. } = &tail.kind else {
            panic!("expected spawn tail");
        };

        assert_eq!(tail.type_, RsigType::ActorId(Box::new(RsigType::I64)));
    }

    #[test]
    fn typed_hir_keeps_lambda_and_spawn_metadata_unknown_without_inference_facts() {
        let path = camino::Utf8Path::new("test.ml");
        let program = SourceParser::new()
            .parse(
                path,
                "fn main() {\n  let id = fn(x) { x };\n  let applied = id(1);\n  let worker = spawn { receive { 1 -> () } };\n  (id, applied, worker)\n}",
            )
            .unwrap();
        let typed = typed_program("ConservativeUnknownFacts", program);
        let [
            TypedStmt::Let { value: id, .. },
            TypedStmt::Let { value: applied, .. },
            TypedStmt::Let { value: worker, .. },
        ] = typed.functions[0].body.statements.as_slice()
        else {
            panic!("expected lambda, apply, and spawn let statements");
        };
        let TypedExprKind::Lambda { params, .. } = &id.kind else {
            panic!("expected lambda value");
        };
        let TypedExprKind::Apply { .. } = &applied.kind else {
            panic!("expected apply value");
        };
        let TypedExprKind::Spawn { .. } = &worker.kind else {
            panic!("expected spawn value");
        };

        assert_eq!(
            id.type_,
            RsigType::Arrow {
                parameter: Box::new(RsigType::Unknown),
                result: Box::new(RsigType::Unknown),
            }
        );
        assert_eq!(params[0].type_, RsigType::Unknown);
        assert_eq!(applied.type_, RsigType::Unknown);
        assert_eq!(worker.type_, RsigType::ActorId(Box::new(RsigType::Unknown)));
    }

    #[test]
    fn typed_hir_keeps_match_pattern_metadata_unknown_without_scrutinee_facts() {
        let path = camino::Utf8Path::new("test.ml");
        let program = SourceParser::new()
            .parse(
                path,
                "type option<'a> = Some('a) | None\n\
                 type box<'a> = { value: 'a }\n\
                 fn main() {\n\
                   let variant = match message { Some(value) -> value, None -> 0 };\n\
                   let tupled = match message { (left, right) -> left };\n\
                   let listed = match message { [head, ..tail] -> head };\n\
                   let recorded = match message { box { value } -> value };\n\
                   (variant, tupled, listed, recorded)\n\
                 }",
            )
            .unwrap();
        let imports = ImportedSignatures::new();
        let function_types = FunctionTable::new();
        let typed = TyIrBuilder::new(
            ModuleName::new("ConservativePatternFacts"),
            &imports,
            &function_types,
            None,
        )
        .build(program);
        let [
            TypedStmt::Let { value: variant, .. },
            TypedStmt::Let { value: tupled, .. },
            TypedStmt::Let { value: listed, .. },
            TypedStmt::Let { value: recorded, .. },
        ] = typed.functions[0].body.statements.as_slice()
        else {
            panic!("expected pattern fallback let statements");
        };

        let TypedExprKind::Match { arms, .. } = &variant.kind else {
            panic!("expected variant match");
        };
        let TypedPattern::Constructor { payload, .. } = &arms[0].pattern else {
            panic!("expected constructor pattern");
        };
        let TypedPattern::Bind { type_, .. } = &payload[0] else {
            panic!("expected constructor payload binder");
        };
        assert!(matches!(type_, RsigType::Unknown | RsigType::Var(_)));

        let TypedExprKind::Match { arms, .. } = &tupled.kind else {
            panic!("expected tuple match");
        };
        let TypedPattern::Tuple(items) = &arms[0].pattern else {
            panic!("expected tuple pattern");
        };
        let TypedPattern::Bind { type_, .. } = &items[0] else {
            panic!("expected tuple item binder");
        };
        assert_eq!(type_, &RsigType::Unknown);

        let TypedExprKind::Match { arms, .. } = &listed.kind else {
            panic!("expected list match");
        };
        let TypedPattern::List { prefix, tail } = &arms[0].pattern else {
            panic!("expected list pattern");
        };
        let TypedPattern::Bind { type_, .. } = &prefix[0] else {
            panic!("expected list head binder");
        };
        assert_eq!(type_, &RsigType::Unknown);
        let Some(tail) = tail else {
            panic!("expected list tail binder");
        };
        let TypedPattern::Bind { type_, .. } = tail.as_ref() else {
            panic!("expected list tail binder");
        };
        assert_eq!(type_, &RsigType::List(Box::new(RsigType::Unknown)));

        let TypedExprKind::Match { arms, .. } = &recorded.kind else {
            panic!("expected record match");
        };
        let TypedPattern::Record { fields, .. } = &arms[0].pattern else {
            panic!("expected record pattern");
        };
        let TypedPattern::Bind { type_, .. } = &fields[0].1 else {
            panic!("expected record field binder");
        };
        assert!(matches!(type_, RsigType::Unknown | RsigType::Var(_)));
    }

    #[test]
    fn typed_hir_keeps_raw_receive_binder_metadata_unknown_without_message_facts() {
        let path = camino::Utf8Path::new("test.ml");
        let program = SourceParser::new()
            .parse(
                path,
                "fn main() {\n\
                   receive { value -> () }\n\
                 }",
            )
            .unwrap();
        let imports = ImportedSignatures::new();
        let function_types = FunctionTable::new();
        let typed = TyIrBuilder::new(
            ModuleName::new("ConservativeReceiveFacts"),
            &imports,
            &function_types,
            None,
        )
        .build(program);
        let Some(receive) = &typed.functions[0].body.tail else {
            panic!("expected receive tail");
        };
        let TypedExprKind::Receive { arms } = &receive.kind else {
            panic!("expected receive expression");
        };
        let TypedPattern::Bind { type_, .. } = &arms[0].pattern else {
            panic!("expected receive binder");
        };

        assert_eq!(receive.type_, RsigType::Unit);
        assert_eq!(type_, &RsigType::Unknown);
    }

    #[test]
    fn typed_hir_keeps_unknown_path_and_apply_callee_metadata_unknown_without_inference_facts() {
        let path = camino::Utf8Path::new("test.ml");
        let program = SourceParser::new()
            .parse(
                path,
                "fn main() {\n\
                   let missing_value = missing;\n\
                   let missing_call = missing(1);\n\
                   (missing_value, missing_call)\n\
                 }",
            )
            .unwrap();
        let imports = ImportedSignatures::new();
        let function_types = FunctionTable::new();
        let typed = TyIrBuilder::new(
            ModuleName::new("ConservativePathFacts"),
            &imports,
            &function_types,
            None,
        )
        .build(program);
        let [
            TypedStmt::Let {
                value: missing_value,
                ..
            },
            TypedStmt::Let {
                value: missing_call,
                ..
            },
        ] = typed.functions[0].body.statements.as_slice()
        else {
            panic!("expected unknown path and call let statements");
        };
        let TypedExprKind::Entity(_) = &missing_value.kind else {
            panic!("expected unresolved path entity");
        };
        let TypedExprKind::Apply { .. } = &missing_call.kind else {
            panic!("expected unresolved path apply");
        };

        assert_eq!(missing_value.type_, RsigType::Unknown);
        assert_eq!(missing_call.type_, RsigType::Unknown);
    }

    #[test]
    fn typed_hir_keeps_incompatible_branch_result_metadata_unknown_without_inference_facts() {
        let path = camino::Utf8Path::new("test.ml");
        let program = SourceParser::new()
            .parse(
                path,
                "fn main() {\n\
                   let branched = if true { 1 } else { \"one\" };\n\
                   let matched = match true { true -> 1, false -> \"zero\" };\n\
                   (branched, matched)\n\
                 }",
            )
            .unwrap();
        let imports = ImportedSignatures::new();
        let function_types = FunctionTable::new();
        let typed = TyIrBuilder::new(
            ModuleName::new("ConservativeBranchFacts"),
            &imports,
            &function_types,
            None,
        )
        .build(program);
        let [
            TypedStmt::Let { value: branched, .. },
            TypedStmt::Let { value: matched, .. },
        ] = typed.functions[0].body.statements.as_slice()
        else {
            panic!("expected incompatible branch result let statements");
        };
        let TypedExprKind::If { .. } = &branched.kind else {
            panic!("expected if expression");
        };
        let TypedExprKind::Match { .. } = &matched.kind else {
            panic!("expected match expression");
        };

        assert_eq!(branched.type_, RsigType::Unknown);
        assert_eq!(matched.type_, RsigType::Unknown);
    }

    #[test]
    fn typed_hir_keeps_projection_and_empty_list_metadata_unknown_without_inference_facts() {
        let path = camino::Utf8Path::new("test.ml");
        let program = SourceParser::new()
            .parse(
                path,
                "type point = { x: i64, y: i64 }\n\
                 fn main() {\n\
                   let empty = [];\n\
                   let missing_item = (1, 2).2;\n\
                   let p = point { x: 1, y: 2 };\n\
                   let missing_field = p.z;\n\
                   (empty, missing_item, missing_field)\n\
                 }",
            )
            .unwrap();
        let imports = ImportedSignatures::new();
        let function_types = FunctionTable::new();
        let typed = TyIrBuilder::new(
            ModuleName::new("ConservativeProjectionFacts"),
            &imports,
            &function_types,
            None,
        )
        .build(program);
        let [
            TypedStmt::Let { value: empty, .. },
            TypedStmt::Let {
                value: missing_item,
                ..
            },
            TypedStmt::Let { .. },
            TypedStmt::Let {
                value: missing_field,
                ..
            },
        ] = typed.functions[0].body.statements.as_slice()
        else {
            panic!("expected empty-list and projection let statements");
        };

        assert_eq!(empty.type_, RsigType::List(Box::new(RsigType::Unknown)));
        assert_eq!(missing_item.type_, RsigType::Unknown);
        assert_eq!(missing_field.type_, RsigType::Unknown);
    }

    #[test]
    fn typed_hir_uses_inference_facts_for_partial_receive_message_wrappers() {
        let typed = typed_program_with_inference_facts(
            "PartialReceiveMessageWrappers",
            "type option<'a> = Some('a) | None\n\
             type box<'a> = { value: 'a }\n\
             fn variant_worker() { spawn { receive { Some(value) -> value + 1, None -> 0 } } }\n\
             fn record_worker() { spawn { receive { box { value } -> value + 1, box { value: _ } -> 0 } } }\n\
             fn list_worker() { spawn { receive { [head, .._] -> head + 1, [.._] -> 0 } } }",
        );

        let spawn_tail = |index: usize| {
            typed.functions[index]
                .body
                .tail
                .as_ref()
                .expect("expected spawn tail")
        };

        assert_eq!(
            spawn_tail(0).type_,
            RsigType::ActorId(Box::new(RsigType::VariantApp {
                name: TypeName::new("option"),
                args: vec![RsigType::I64],
            }))
        );
        assert_eq!(
            spawn_tail(1).type_,
            RsigType::ActorId(Box::new(RsigType::RecordApp {
                name: TypeName::new("box"),
                args: vec![RsigType::I64],
            }))
        );
        assert_eq!(
            spawn_tail(2).type_,
            RsigType::ActorId(Box::new(RsigType::List(Box::new(RsigType::I64))))
        );
    }

    #[test]
    fn typed_hir_uses_inference_facts_for_generic_variant_constructor_results() {
        let typed = typed_program_with_inference_facts(
            "GenericVariantResult",
            "type option<'a> = Some('a) | None\nfn main() { Some(1) }",
        );
        let Some(tail) = &typed.functions[0].body.tail else {
            panic!("expected constructor call tail");
        };
        let TypedExprKind::Constructor { constructor, .. } = &tail.kind else {
            panic!("expected generic constructor expression");
        };

        assert_eq!(constructor, &ConstructorName::new("Some"));
        assert_eq!(
            tail.type_,
            RsigType::VariantApp {
                name: TypeName::new("option"),
                args: vec![RsigType::I64],
            }
        );
    }

    #[test]
    fn typed_hir_uses_inference_facts_for_imported_generic_variant_constructor_results() {
        let typed = typed_program_with_imports_and_inference_facts(
            "ImportedGenericVariantResult",
            "use Options\nfn main() { Options.Some(1) }",
            imported_options_signature(),
        );
        let Some(tail) = &typed.functions[0].body.tail else {
            panic!("expected imported constructor call tail");
        };
        let TypedExprKind::Constructor { constructor, .. } = &tail.kind else {
            panic!("expected imported generic constructor expression");
        };

        assert_eq!(constructor, &ConstructorName::new("Some"));
        assert_eq!(
            tail.type_,
            RsigType::VariantApp {
                name: TypeName::new("Options.option"),
                args: vec![RsigType::I64],
            }
        );
    }

    #[test]
    fn typed_hir_uses_inference_facts_for_generic_record_field_access() {
        let typed = typed_program_with_inference_facts(
            "GenericRecordField",
            "type box<'a> = { value: 'a }\nfn main() { let item = box { value: 1 }; item.value }",
        );
        let Some(TypedStmt::Let { value, .. }) = typed.functions[0].body.statements.first() else {
            panic!("expected generic record let binding");
        };
        assert_eq!(
            value.type_,
            RsigType::RecordApp {
                name: TypeName::new("box"),
                args: vec![RsigType::I64],
            }
        );
        let Some(tail) = &typed.functions[0].body.tail else {
            panic!("expected field projection tail");
        };
        let TypedExprKind::Field { base, field } = &tail.kind else {
            panic!("expected generic record field projection");
        };

        assert_eq!(field, "value");
        assert_eq!(
            base.type_,
            RsigType::RecordApp {
                name: TypeName::new("box"),
                args: vec![RsigType::I64],
            }
        );
        assert_eq!(tail.type_, RsigType::I64);
    }

    #[test]
    fn typed_hir_uses_inference_facts_for_imported_generic_record_field_access() {
        let typed = typed_program_with_imports_and_inference_facts(
            "ImportedGenericRecordField",
            "use Boxes\nfn main() { let item = Boxes.box { value: 1 }; item.value }",
            imported_boxes_signature(),
        );
        let Some(TypedStmt::Let { value, .. }) = typed.functions[0].body.statements.first() else {
            panic!("expected imported generic record let binding");
        };
        assert_eq!(
            value.type_,
            RsigType::RecordApp {
                name: TypeName::new("Boxes.box"),
                args: vec![RsigType::I64],
            }
        );
        let Some(tail) = &typed.functions[0].body.tail else {
            panic!("expected field projection tail");
        };
        let TypedExprKind::Field { base, field } = &tail.kind else {
            panic!("expected imported generic record field projection");
        };

        assert_eq!(field, "value");
        assert_eq!(
            base.type_,
            RsigType::RecordApp {
                name: TypeName::new("Boxes.box"),
                args: vec![RsigType::I64],
            }
        );
        assert_eq!(tail.type_, RsigType::I64);
    }

    #[test]
    fn lambda_ir_preserves_generic_record_pattern_binding_types() {
        let typed = typed_program_with_inference_facts(
            "GenericRecordPatternLambda",
            "type box<'a> = { value: 'a }\nfn main() { let item = box { value: 1 }; match item { box { value } -> value } }",
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let Some(LambdaExpr::Match { arms, .. }) = &lambda.functions[0].body.tail else {
            panic!("expected match tail");
        };
        let LambdaPattern::Record { fields, .. } = &arms[0].pattern else {
            panic!("expected record pattern");
        };
        let LambdaPattern::Bind { type_, .. } = &fields[0].1 else {
            panic!("expected record field binding");
        };

        assert_eq!(type_, &RsigType::I64);
    }

    #[test]
    fn lambda_ir_preserves_generic_variant_pattern_binding_types() {
        let typed = typed_program_with_inference_facts(
            "GenericVariantPatternLambda",
            "type option<'a> = Some('a) | None\nfn main() { let item = Some(1); match item { Some(value) -> value } }",
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let Some(LambdaExpr::Match { arms, .. }) = &lambda.functions[0].body.tail else {
            panic!("expected match tail");
        };
        let LambdaPattern::Constructor { payload, .. } = &arms[0].pattern else {
            panic!("expected constructor pattern");
        };
        let LambdaPattern::Bind { type_, .. } = &payload[0] else {
            panic!("expected constructor payload binding");
        };

        assert_eq!(type_, &RsigType::I64);
    }

    #[test]
    fn lambda_ir_preserves_imported_nested_generic_pattern_binding_types() {
        let typed = typed_program_with_imports_and_inference_facts(
            "ImportedNestedGenericPatternLambda",
            "use Boxes\nuse Options\nfn main() { match Options.Some(Boxes.box { value: 1 }) {\nOptions.Some(Boxes.box { value }) -> value\n} }",
            imported_boxes_and_options_signature(),
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let Some(LambdaExpr::Match { arms, .. }) = &lambda.functions[0].body.tail else {
            panic!("expected match tail");
        };
        let LambdaPattern::Constructor { payload, .. } = &arms[0].pattern else {
            panic!("expected imported constructor pattern");
        };
        let LambdaPattern::Record { fields, .. } = &payload[0] else {
            panic!("expected nested imported record pattern");
        };
        let LambdaPattern::Bind { type_, .. } = &fields[0].1 else {
            panic!("expected nested record field binding");
        };

        assert_eq!(type_, &RsigType::I64);
    }

    #[test]
    fn lambda_ir_preserves_imported_generic_variant_pattern_binding_types() {
        let typed = typed_program_with_imports_and_inference_facts(
            "ImportedGenericVariantPatternLambda",
            "use Options\nfn main() { let item = Options.Some(1); match item { Options.Some(value) -> value } }",
            imported_options_signature(),
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let Some(LambdaExpr::Match { arms, .. }) = &lambda.functions[0].body.tail else {
            panic!("expected match tail");
        };
        let LambdaPattern::Constructor { payload, .. } = &arms[0].pattern else {
            panic!("expected imported constructor pattern");
        };
        let LambdaPattern::Bind { type_, .. } = &payload[0] else {
            panic!("expected imported constructor payload binding");
        };

        assert_eq!(type_, &RsigType::I64);
    }

    #[test]
    fn actor_ir_classifies_generic_application_captures_as_values() {
        let typed = typed_program_with_inference_facts(
            "GenericActorCapture",
            "type box<'a> = { value: 'a }\ntype option<'a> = Some('a) | None\nfn main() { let item = box { value: 1 }; let maybe = Some(item); spawn { let kept = maybe; () } }",
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let actor_ir = ActorIrLowerer::new(&ImportedSignatures::new()).lower(&lambda);
        let actor = actor_ir.actors.first().expect("expected spawned actor");

        assert_eq!(actor.frame.captures.len(), 1);
        assert_eq!(actor.frame.captures[0].name.as_str(), "maybe$1");
        assert_eq!(actor.frame.captures[0].type_, ActorSlotType::Value);
    }

    #[test]
    fn actor_ir_classifies_inferred_lambda_param_captures_as_i64() {
        let typed = typed_program_with_inference_facts(
            "InferredLambdaParamActorCapture",
            "fn main() { let f = fn(x) { let actor = spawn { let kept = x; () }; x + 1 }; f }",
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let actor_ir = ActorIrLowerer::new(&ImportedSignatures::new()).lower(&lambda);
        let actor = actor_ir.actors.first().expect("expected spawned actor");

        assert_eq!(actor.frame.captures.len(), 1);
        assert_eq!(actor.frame.captures[0].name.as_str(), "x$0");
        assert_eq!(actor.frame.captures[0].type_, ActorSlotType::I64);
    }

    #[test]
    fn actor_ir_classifies_inferred_apply_result_captures_as_i64() {
        let typed = typed_program_with_inference_facts(
            "InferredApplyResultActorCapture",
            "fn main() { let inc = fn(x) { x + 1 }; let y = inc(41); spawn { let kept = y; () } }",
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let actor_ir = ActorIrLowerer::new(&ImportedSignatures::new()).lower(&lambda);
        let actor = actor_ir.actors.first().expect("expected spawned actor");
        let capture = actor
            .frame
            .captures
            .iter()
            .find(|slot| slot.name.as_str().starts_with("y$"))
            .expect("expected spawned actor to capture apply result");

        assert_eq!(capture.type_, ActorSlotType::I64);
    }

    #[test]
    fn lambda_ir_receive_patterns_use_inferred_payload_types() {
        let typed = typed_program_with_inference_facts(
            "InferredReceivePayload",
            "type option<'a> = Some('a) | None\nfn main() { spawn { receive { Some(value) -> value + 1, None -> 0 } } }",
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let LambdaExpr::Spawn { body, .. } = &lambda.functions[0]
            .body
            .tail
            .as_ref()
            .expect("expected spawned actor")
        else {
            panic!("expected spawn tail");
        };
        let Some(LambdaExpr::Receive { arms }) = &body.tail else {
            panic!("expected receive tail");
        };
        let LambdaPattern::Constructor { payload, .. } = &arms[0].pattern else {
            panic!("expected constructor receive pattern");
        };
        let LambdaPattern::Bind { type_, .. } = &payload[0] else {
            panic!("expected constructor payload binding");
        };

        assert_eq!(type_, &RsigType::I64);
    }

    #[test]
    fn actor_ir_classifies_inferred_receive_payload_captures_as_i64() {
        let typed = typed_program_with_inference_facts(
            "InferredReceivePayloadCapture",
            "type option<'a> = Some('a) | None\nfn main() { spawn { receive { Some(value) -> { let y = value + 1; spawn { let kept = value; () } }, None -> spawn { () } } } }",
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let actor_ir = ActorIrLowerer::new(&ImportedSignatures::new()).lower(&lambda);
        let inner_actor = actor_ir
            .actors
            .iter()
            .find(|actor| {
                actor
                    .frame
                    .captures
                    .iter()
                    .any(|slot| slot.name.as_str() == "value$0")
            })
            .expect("expected nested actor to capture receive payload");
        let capture = inner_actor
            .frame
            .captures
            .iter()
            .find(|slot| slot.name.as_str() == "value$0")
            .expect("expected receive payload capture");

        assert_eq!(capture.type_, ActorSlotType::I64);
    }

    #[test]
    fn lambda_ir_receive_record_patterns_use_inferred_field_types() {
        let typed = typed_program_with_inference_facts(
            "InferredReceiveRecordPayload",
            "type box<'a> = { value: 'a }\nfn main() { spawn { receive { box { value } -> value + 1 } } }",
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let LambdaExpr::Spawn { body, .. } = &lambda.functions[0]
            .body
            .tail
            .as_ref()
            .expect("expected spawned actor")
        else {
            panic!("expected spawn tail");
        };
        let Some(LambdaExpr::Receive { arms }) = &body.tail else {
            panic!("expected receive tail");
        };
        let LambdaPattern::Record { fields, .. } = &arms[0].pattern else {
            panic!("expected record receive pattern");
        };
        let LambdaPattern::Bind { type_, .. } = &fields[0].1 else {
            panic!("expected record field binding");
        };

        assert_eq!(type_, &RsigType::I64);
    }

    #[test]
    fn actor_ir_classifies_inferred_receive_record_field_captures_as_i64() {
        let typed = typed_program_with_inference_facts(
            "InferredReceiveRecordFieldCapture",
            "type box<'a> = { value: 'a }\nfn main() { spawn { receive { box { value } -> { let y = value + 1; spawn { let kept = value; () } } } } }",
        );
        let lambda = LambdaLowerer::new().lower(typed);
        let actor_ir = ActorIrLowerer::new(&ImportedSignatures::new()).lower(&lambda);
        let inner_actor = actor_ir
            .actors
            .iter()
            .find(|actor| {
                actor
                    .frame
                    .captures
                    .iter()
                    .any(|slot| slot.name.as_str() == "value$0")
            })
            .expect("expected nested actor to capture receive record field");
        let capture = inner_actor
            .frame
            .captures
            .iter()
            .find(|slot| slot.name.as_str() == "value$0")
            .expect("expected receive record field capture");

        assert_eq!(capture.type_, ActorSlotType::I64);
    }

    #[test]
    fn imported_record_field_maps_qualify_field_types() {
        let mut imports = ImportedSignatures::new();
        imports.insert(
            ModuleName::new("Syntax"),
            Rsig {
                module: ModuleName::new("Syntax"),
                dependencies: Vec::new(),
                types: vec![RsigTypeDecl {
                    name: TypeName::new("entry"),
                    params: Vec::new(),
                    body: RsigTypeDeclKind::Record {
                        fields: vec![
                            RsigRecordField {
                                name: FieldName::new("head"),
                                type_: RsigType::Variant(TypeName::new("token")),
                            },
                            RsigRecordField {
                                name: FieldName::new("trail"),
                                type_: RsigType::RecordApp {
                                    name: TypeName::new("box"),
                                    args: vec![
                                        RsigType::Variant(TypeName::new("token")),
                                        RsigType::VariantApp {
                                            name: TypeName::new("Option"),
                                            args: vec![RsigType::String],
                                        },
                                    ],
                                },
                            },
                            RsigRecordField {
                                name: FieldName::new("pair"),
                                type_: RsigType::Tuple(vec![
                                    RsigType::Variant(TypeName::new("token")),
                                    RsigType::List(Box::new(RsigType::Record(TypeName::new(
                                        "span",
                                    )))),
                                ]),
                            },
                        ],
                    },
                    fingerprint: 0,
                }],
                exports: Vec::new(),
                module_fingerprint: 0,
            },
        );
        let uses = vec![TypedUse {
            name: ModuleName::new("Syntax"),
            fingerprint: 0,
        }];

        let fields = record_field_type_map(&[], &uses, &imports);

        let entry_fields = fields
            .get(&TypeName::new("Syntax.entry"))
            .expect("expected imported entry fields");

        assert_eq!(
            entry_fields.get("head"),
            Some(&RsigType::Variant(TypeName::new("Syntax.token")))
        );
        assert_eq!(
            entry_fields.get("trail"),
            Some(&RsigType::RecordApp {
                name: TypeName::new("Syntax.box"),
                args: vec![
                    RsigType::Variant(TypeName::new("Syntax.token")),
                    RsigType::VariantApp {
                        name: TypeName::new("Option"),
                        args: vec![RsigType::String],
                    },
                ],
            })
        );
        assert_eq!(
            entry_fields.get("pair"),
            Some(&RsigType::Tuple(vec![
                RsigType::Variant(TypeName::new("Syntax.token")),
                RsigType::List(Box::new(RsigType::Record(TypeName::new("Syntax.span",)))),
            ]))
        );
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
