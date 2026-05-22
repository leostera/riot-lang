use std::collections::{BTreeMap, BTreeSet, HashMap};

use thiserror::Error;

use super::env::Env;
use super::scheme::TypeScheme;
use super::state::State;
use super::types::Type;
use super::unifier::UnifyError;
use crate::ast::{
    AstBlock, AstDecl, AstExpr, AstFnDecl, AstPattern, AstProgram, AstStmt, AstTypeAnnotation,
    AstTypeBody, TextSpan,
};
use crate::imported_types::{imported_type_name, qualify_imported_type};
use crate::signature::{
    FunctionSignature, FunctionTable, ImportedSignatures, Rsig, RsigExport, RsigType,
    RsigTypeDeclKind, RsigTypeScheme, TypeName, TypeVarName,
};
use crate::type_lowerer::RsigTypeLowerer;

#[derive(Debug, Error, PartialEq, Eq)]
pub(crate) enum InferError {
    #[error("cannot infer expression type: {0}")]
    Unsupported(&'static str),
    #[error("unknown value `{0}`")]
    UnknownValue(String),
    #[error("{context} expected Bool")]
    ExpectedBool {
        context: &'static str,
        actual: RsigType,
    },
    #[error("{context} expected i64")]
    ExpectedI64 {
        context: &'static str,
        actual: RsigType,
    },
    #[error("{context} expected a numeric value")]
    ExpectedNumeric {
        context: &'static str,
        actual: RsigType,
    },
    #[error("{context} expected a comparable value")]
    ExpectedComparable {
        context: &'static str,
        actual: RsigType,
    },
    #[error("{0}")]
    Unify(#[from] UnifyError),
    #[error("{error}")]
    At {
        span: TextSpan,
        error: Box<InferError>,
    },
}

impl InferError {
    pub(crate) fn span(&self) -> Option<TextSpan> {
        match self {
            InferError::At { span, .. } => Some(*span),
            InferError::Unsupported(_)
            | InferError::UnknownValue(_)
            | InferError::ExpectedBool { .. }
            | InferError::ExpectedI64 { .. }
            | InferError::ExpectedNumeric { .. }
            | InferError::ExpectedComparable { .. }
            | InferError::Unify(_) => None,
        }
    }

    pub(crate) fn unknown_value_name(&self) -> Option<&str> {
        match self {
            InferError::UnknownValue(name) => Some(name.as_str()),
            InferError::At { error, .. } => error.unknown_value_name(),
            InferError::Unsupported(_)
            | InferError::ExpectedBool { .. }
            | InferError::ExpectedI64 { .. }
            | InferError::ExpectedNumeric { .. }
            | InferError::ExpectedComparable { .. }
            | InferError::Unify(_) => None,
        }
    }

    pub(crate) fn unsupported_reason(&self) -> Option<&'static str> {
        match self {
            InferError::Unsupported(reason) => Some(reason),
            InferError::At { error, .. } => error.unsupported_reason(),
            InferError::UnknownValue(_)
            | InferError::ExpectedBool { .. }
            | InferError::ExpectedI64 { .. }
            | InferError::ExpectedNumeric { .. }
            | InferError::ExpectedComparable { .. }
            | InferError::Unify(_) => None,
        }
    }

    pub(crate) fn is_occurs_check(&self) -> bool {
        match self {
            InferError::Unify(UnifyError::OccursCheck { .. }) => true,
            InferError::At { error, .. } => error.is_occurs_check(),
            InferError::Unsupported(_)
            | InferError::UnknownValue(_)
            | InferError::ExpectedBool { .. }
            | InferError::ExpectedI64 { .. }
            | InferError::ExpectedNumeric { .. }
            | InferError::ExpectedComparable { .. }
            | InferError::Unify(_) => false,
        }
    }

    pub(crate) fn type_mismatch_types(&self) -> Option<(RsigType, RsigType)> {
        match self {
            InferError::Unify(UnifyError::TypeMismatch { lhs, rhs }) => {
                Some((infer_type_to_rsig_type(lhs), infer_type_to_rsig_type(rhs)))
            }
            InferError::At { error, .. } => error.type_mismatch_types(),
            InferError::Unsupported(_)
            | InferError::UnknownValue(_)
            | InferError::ExpectedBool { .. }
            | InferError::ExpectedI64 { .. }
            | InferError::ExpectedNumeric { .. }
            | InferError::ExpectedComparable { .. }
            | InferError::Unify(_) => None,
        }
    }

    pub(crate) fn tuple_arity_mismatch(&self) -> Option<(usize, usize)> {
        match self {
            InferError::Unify(UnifyError::TupleArityMismatch { lhs, rhs }) => Some((*lhs, *rhs)),
            InferError::At { error, .. } => error.tuple_arity_mismatch(),
            InferError::Unsupported(_)
            | InferError::UnknownValue(_)
            | InferError::ExpectedBool { .. }
            | InferError::ExpectedI64 { .. }
            | InferError::ExpectedNumeric { .. }
            | InferError::ExpectedComparable { .. }
            | InferError::Unify(_) => None,
        }
    }

    pub(crate) fn expected_bool_context(&self) -> Option<(&'static str, &RsigType)> {
        match self {
            InferError::ExpectedBool { context, actual } => Some((*context, actual)),
            InferError::At { error, .. } => error.expected_bool_context(),
            InferError::Unsupported(_)
            | InferError::UnknownValue(_)
            | InferError::ExpectedI64 { .. }
            | InferError::ExpectedNumeric { .. }
            | InferError::ExpectedComparable { .. }
            | InferError::Unify(_) => None,
        }
    }

    pub(crate) fn expected_i64_context(&self) -> Option<(&'static str, &RsigType)> {
        match self {
            InferError::ExpectedI64 { context, actual } => Some((*context, actual)),
            InferError::At { error, .. } => error.expected_i64_context(),
            InferError::Unsupported(_)
            | InferError::UnknownValue(_)
            | InferError::ExpectedBool { .. }
            | InferError::ExpectedNumeric { .. }
            | InferError::ExpectedComparable { .. }
            | InferError::Unify(_) => None,
        }
    }

    pub(crate) fn expected_numeric_context(&self) -> Option<(&'static str, &RsigType)> {
        match self {
            InferError::ExpectedNumeric { context, actual } => Some((*context, actual)),
            InferError::At { error, .. } => error.expected_numeric_context(),
            InferError::Unsupported(_)
            | InferError::UnknownValue(_)
            | InferError::ExpectedBool { .. }
            | InferError::ExpectedI64 { .. }
            | InferError::ExpectedComparable { .. }
            | InferError::Unify(_) => None,
        }
    }

    pub(crate) fn expected_comparable_context(&self) -> Option<(&'static str, &RsigType)> {
        match self {
            InferError::ExpectedComparable { context, actual } => Some((*context, actual)),
            InferError::At { error, .. } => error.expected_comparable_context(),
            InferError::Unsupported(_)
            | InferError::UnknownValue(_)
            | InferError::ExpectedBool { .. }
            | InferError::ExpectedI64 { .. }
            | InferError::ExpectedNumeric { .. }
            | InferError::Unify(_) => None,
        }
    }

    fn at(self, span: TextSpan) -> Self {
        match self {
            InferError::At { .. } => self,
            error => InferError::At {
                span,
                error: Box::new(error),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct InferredModule {
    pub(crate) env: Env,
    pub(crate) expression_types: BTreeMap<TextSpan, Type>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct ExpressionTypeTable {
    types: BTreeMap<TextSpan, RsigType>,
}

impl ExpressionTypeTable {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn insert(&mut self, span: TextSpan, type_: RsigType) {
        self.types.insert(span, type_);
    }

    pub(crate) fn get(&self, span: TextSpan) -> Option<&RsigType> {
        self.types.get(&span)
    }
}

#[derive(Debug, Clone)]
struct InferRecordShape {
    type_name: TypeName,
    type_params: Vec<TypeVarName>,
    fields: Vec<(String, RsigType)>,
}

#[derive(Debug)]
pub(crate) struct ModuleInferencer<'a> {
    program: &'a AstProgram,
    imports: &'a ImportedSignatures,
}

impl<'a> ModuleInferencer<'a> {
    pub(crate) fn new(program: &'a AstProgram, imports: &'a ImportedSignatures) -> Self {
        Self { program, imports }
    }

    pub(crate) fn infer(&self) -> Result<InferredModule, InferError> {
        let mut state = State::default();
        let mut expression_types = BTreeMap::new();
        let declared_variants = declared_variant_names(self.program, self.imports);
        let record_shapes = record_shapes(self.program, self.imports, &declared_variants);
        install_prelude(&mut state);
        install_imports(&mut state, self.program, self.imports);
        install_annotated_functions(&mut state, self.program, &declared_variants);
        for decl in &self.program.decls {
            infer_decl(
                &mut state,
                decl,
                &declared_variants,
                &record_shapes,
                &mut expression_types,
            )?;
        }
        Ok(InferredModule {
            env: state.into_env(),
            expression_types,
        })
    }
}

impl InferredModule {
    pub(crate) fn function_signatures(&self, program: &AstProgram) -> FunctionTable {
        let function_arities = program
            .decls
            .iter()
            .filter_map(|decl| match decl {
                AstDecl::Function(function) => Some((function.name.clone(), function.params.len())),
                _ => None,
            })
            .collect::<BTreeMap<_, _>>();
        let mut signatures = FunctionTable::new();
        for (name, arity) in function_arities {
            let Some(scheme) = self.env.get_value(&name) else {
                continue;
            };
            if let Some((params, result)) = peel_source_function_type(&scheme.body, arity) {
                signatures.insert(
                    name,
                    FunctionSignature::new(
                        params.iter().map(infer_type_to_rsig_type).collect(),
                        infer_type_to_rsig_type(&result),
                    ),
                );
            }
        }
        signatures
    }

    pub(crate) fn expression_rsig_types(&self) -> ExpressionTypeTable {
        let mut types = ExpressionTypeTable::new();
        for (span, type_) in &self.expression_types {
            types.insert(*span, infer_type_to_rsig_type(type_));
        }
        types
    }
}

fn declared_variant_names(
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
                            names.insert(imported_type_name(&use_.name, &type_.name));
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

fn record_shapes(
    program: &AstProgram,
    imports: &ImportedSignatures,
    declared_variants: &BTreeSet<TypeName>,
) -> HashMap<String, InferRecordShape> {
    let mut shapes = HashMap::new();
    for decl in &program.decls {
        match decl {
            AstDecl::Type(type_) => {
                let AstTypeBody::Record { fields } = &type_.body else {
                    continue;
                };
                let type_name = TypeName::new(type_.name.clone());
                let shape = InferRecordShape {
                    type_name: type_name.clone(),
                    type_params: type_
                        .params
                        .iter()
                        .map(|param| TypeVarName::new(param.name.clone()))
                        .collect(),
                    fields: fields
                        .iter()
                        .map(|field| {
                            (
                                field.name.clone(),
                                lower_type_annotation(&field.type_annotation, declared_variants),
                            )
                        })
                        .collect(),
                };
                insert_record_shape_aliases(&mut shapes, None, &type_name, shape);
            }
            AstDecl::Use(use_) => {
                let Some(rsig) = imports.get(use_.name.as_str()) else {
                    continue;
                };
                for type_ in &rsig.types {
                    let RsigTypeDeclKind::Record { fields } = &type_.body else {
                        continue;
                    };
                    let type_name = imported_type_name(use_.name.as_str(), &type_.name);
                    let shape = InferRecordShape {
                        type_name,
                        type_params: type_
                            .params
                            .iter()
                            .map(|param| TypeVarName::new(param.as_str()))
                            .collect(),
                        fields: fields
                            .iter()
                            .map(|field| {
                                (
                                    field.name.as_str().to_owned(),
                                    qualify_imported_type(use_.name.as_str(), &field.type_),
                                )
                            })
                            .collect(),
                    };
                    insert_record_shape_aliases(
                        &mut shapes,
                        Some(use_.name.as_str()),
                        &type_.name,
                        shape,
                    );
                }
            }
            _ => {}
        }
    }
    shapes
}

fn insert_record_shape_aliases(
    shapes: &mut HashMap<String, InferRecordShape>,
    module: Option<&str>,
    source_name: &TypeName,
    shape: InferRecordShape,
) {
    let base = source_name.as_str();
    shapes.insert(base.to_owned(), shape.clone());
    shapes.insert(type_constructor_name(base), shape.clone());
    if let Some(module) = module {
        shapes.insert(format!("{module}.{base}"), shape.clone());
        shapes.insert(format!("{module}.{}", type_constructor_name(base)), shape);
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

fn install_prelude(state: &mut State) {
    let rsig = crate::stdlib::Stdlib::new()
        .prelude_signature()
        .expect("compiler/std/prelude.ml must parse");
    install_unqualified_signature(state, &rsig);
}

fn install_imports(state: &mut State, program: &AstProgram, imports: &ImportedSignatures) {
    for decl in &program.decls {
        let AstDecl::Use(use_) = decl else {
            continue;
        };
        let Some(rsig) = imports.get(use_.name.as_str()) else {
            continue;
        };
        install_import_signature(state, &use_.name, rsig);
    }
}

fn install_annotated_functions(
    state: &mut State,
    program: &AstProgram,
    declared_variants: &BTreeSet<TypeName>,
) {
    for decl in &program.decls {
        let AstDecl::Function(function) = decl else {
            continue;
        };
        let Some(type_) = annotated_function_type(state, function, declared_variants) else {
            continue;
        };
        state.add_value(function.name.clone(), state.generalize(type_));
    }
}

fn annotated_function_type(
    state: &mut State,
    function: &AstFnDecl,
    declared_variants: &BTreeSet<TypeName>,
) -> Option<Type> {
    let result = function.return_type.as_ref().map(|annotation| {
        rsig_type_to_infer_type(&lower_type_annotation(annotation, declared_variants), state)
    })?;
    let params = function
        .param_types
        .iter()
        .map(|annotation| {
            annotation.as_ref().map(|annotation| {
                rsig_type_to_infer_type(
                    &lower_type_annotation(annotation, declared_variants),
                    state,
                )
            })
        })
        .collect::<Option<Vec<_>>>()?;
    Some(source_function_type(params, result))
}

fn install_import_signature(state: &mut State, module_name: &str, rsig: &Rsig) {
    for export in &rsig.exports {
        let (name, scheme) = match export {
            RsigExport::Function(function) => (&function.name, &function.scheme),
            RsigExport::External(external) => (&external.name, &external.scheme),
        };
        let scheme = qualify_imported_scheme(module_name, scheme);
        let scheme = rsig_scheme_to_infer_scheme(&scheme, state);
        state.add_prelude_value(format!("{module_name}.{name}"), scheme);
    }
    for type_ in &rsig.types {
        let type_name = imported_type_name(module_name, &type_.name);
        let RsigTypeDeclKind::Variant { constructors } = &type_.body else {
            continue;
        };
        for constructor in constructors {
            let payload = constructor
                .payload
                .iter()
                .map(|type_| qualify_imported_type(module_name, type_))
                .collect::<Vec<_>>();
            let result = generic_variant_rsig_type(
                type_name.clone(),
                type_.params.iter().map(|param| param.as_str()),
            );
            let type_ = constructor_type(&payload, &result, state);
            state.add_prelude_value(
                format!("{module_name}.{}", constructor.name.as_str()),
                state.generalize(type_),
            );
        }
    }
}

fn install_unqualified_signature(state: &mut State, rsig: &Rsig) {
    for export in &rsig.exports {
        let scheme = match export {
            RsigExport::Function(function) => &function.scheme,
            RsigExport::External(external) => &external.scheme,
        };
        let scheme = rsig_scheme_to_infer_scheme(scheme, state);
        state.add_prelude_value(export.name(), scheme);
    }
    for type_ in &rsig.types {
        let RsigTypeDeclKind::Variant { constructors } = &type_.body else {
            continue;
        };
        for constructor in constructors {
            let result = generic_variant_rsig_type(
                type_.name.clone(),
                type_.params.iter().map(|param| param.as_str()),
            );
            let type_ = constructor_type(&constructor.payload, &result, state);
            state.add_prelude_value(
                constructor.name.as_str().to_owned(),
                state.generalize(type_),
            );
        }
    }
}

fn source_function_type(params: Vec<Type>, result: Type) -> Type {
    if params.is_empty() {
        Type::arrow(Type::Unit, result)
    } else {
        Type::arrows(params, result)
    }
}

fn generic_variant_rsig_type<'a>(
    name: TypeName,
    params: impl Iterator<Item = &'a str>,
) -> RsigType {
    let args = params
        .map(|param| RsigType::Var(TypeVarName::new(param)))
        .collect::<Vec<_>>();
    if args.is_empty() {
        RsigType::Variant(name)
    } else {
        RsigType::VariantApp { name, args }
    }
}

fn constructor_type(payload: &[RsigType], result: &RsigType, state: &mut State) -> Type {
    let mut vars = BTreeMap::new();
    let result = rsig_type_to_infer_type_with_vars(result, state, &mut vars);
    if payload.is_empty() {
        result
    } else {
        Type::arrows(
            payload
                .iter()
                .map(|type_| rsig_type_to_infer_type_with_vars(type_, state, &mut vars))
                .collect(),
            result,
        )
    }
}

fn peel_source_function_type(type_: &Type, arity: usize) -> Option<(Vec<Type>, Type)> {
    let mut params = Vec::new();
    let mut current = type_;

    if arity == 0 {
        let Type::Arrow { parameter, result } = current else {
            return None;
        };
        if parameter.as_ref() != &Type::Unit {
            return None;
        }
        return Some((params, result.as_ref().clone()));
    }

    for _ in 0..arity {
        let Type::Arrow { parameter, result } = current else {
            return None;
        };
        params.push(parameter.as_ref().clone());
        current = result;
    }

    Some((params, current.clone()))
}

fn infer_decl(
    state: &mut State,
    decl: &AstDecl,
    declared_variants: &BTreeSet<TypeName>,
    record_shapes: &HashMap<String, InferRecordShape>,
    expression_types: &mut BTreeMap<TextSpan, Type>,
) -> Result<(), InferError> {
    match decl {
        AstDecl::Use(_) | AstDecl::Module(_) | AstDecl::Include(_) => Ok(()),
        AstDecl::Type(type_) => {
            let type_name = TypeName::new(type_.name.clone());
            if let AstTypeBody::Variant { constructors } = &type_.body {
                for constructor in constructors {
                    let payload = constructor
                        .payload
                        .iter()
                        .map(|payload| lower_type_annotation(payload, declared_variants))
                        .collect::<Vec<_>>();
                    let result = generic_variant_rsig_type(
                        type_name.clone(),
                        type_.params.iter().map(|param| param.name.as_str()),
                    );
                    let type_ = constructor_type(&payload, &result, state);
                    state.add_value(constructor.name.clone(), state.generalize(type_));
                }
            }
            Ok(())
        }
        AstDecl::External(external) => {
            let (params, result) =
                lower_signature_annotation(&external.type_annotation, declared_variants);
            let type_ = rsig_signature_to_infer_type(&params, &result, state);
            state.add_value(external.name.clone(), state.generalize(type_));
            Ok(())
        }
        AstDecl::Function(function) => {
            let type_ = infer_function(
                state,
                function,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            state.add_value(function.name.clone(), state.generalize(type_));
            Ok(())
        }
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

fn infer_function(
    state: &mut State,
    function: &AstFnDecl,
    declared_variants: &BTreeSet<TypeName>,
    record_shapes: &HashMap<String, InferRecordShape>,
    expression_types: &mut BTreeMap<TextSpan, Type>,
) -> Result<Type, InferError> {
    let params = function
        .params
        .iter()
        .enumerate()
        .map(|(index, _param)| {
            function
                .param_types
                .get(index)
                .and_then(|annotation| annotation.as_ref())
                .map(|annotation| {
                    rsig_type_to_infer_type(
                        &lower_type_annotation(annotation, declared_variants),
                        state,
                    )
                })
                .unwrap_or_else(|| state.fresh_var())
        })
        .collect::<Vec<_>>();
    let result_constraint = function
        .return_type
        .as_ref()
        .map(|annotation| {
            rsig_type_to_infer_type(&lower_type_annotation(annotation, declared_variants), state)
        })
        .unwrap_or_else(|| state.fresh_var());
    let self_type = source_function_type(params.clone(), result_constraint.clone());

    state.push_scope();
    state.add_value(function.name.clone(), state.monomorphic(self_type));
    for (param, type_) in function.params.iter().zip(&params) {
        state.add_value(param.clone(), state.monomorphic(type_.clone()));
    }
    let result = infer_block(
        state,
        &function.body,
        declared_variants,
        record_shapes,
        expression_types,
    )?;
    state.unify(&result_constraint, &result)?;
    let result = state.resolve(&result_constraint);
    state.pop_scope();
    Ok(source_function_type(params, state.resolve(&result)))
}

fn infer_block(
    state: &mut State,
    block: &AstBlock,
    declared_variants: &BTreeSet<TypeName>,
    record_shapes: &HashMap<String, InferRecordShape>,
    expression_types: &mut BTreeMap<TextSpan, Type>,
) -> Result<Type, InferError> {
    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                let inferred = infer_expr(
                    state,
                    value,
                    declared_variants,
                    record_shapes,
                    expression_types,
                )?;
                let type_ = if let Some(annotation) = type_annotation {
                    let expected = rsig_type_to_infer_type(
                        &lower_type_annotation(annotation, declared_variants),
                        state,
                    );
                    state.unify(&expected, &inferred)?;
                    expected
                } else {
                    inferred
                };
                let scheme = state.generalize(type_);
                state.add_value(name.clone(), scheme);
            }
            AstStmt::Expr(expr) => {
                infer_expr(
                    state,
                    expr,
                    declared_variants,
                    record_shapes,
                    expression_types,
                )?;
            }
        }
    }

    if let Some(tail) = &block.tail {
        infer_expr(
            state,
            tail,
            declared_variants,
            record_shapes,
            expression_types,
        )
    } else {
        Ok(Type::Unit)
    }
}

fn infer_expr(
    state: &mut State,
    expr: &AstExpr,
    declared_variants: &BTreeSet<TypeName>,
    record_shapes: &HashMap<String, InferRecordShape>,
    expression_types: &mut BTreeMap<TextSpan, Type>,
) -> Result<Type, InferError> {
    let span = expr_span(expr);
    let inferred = infer_expr_kind(
        state,
        expr,
        declared_variants,
        record_shapes,
        expression_types,
    )
    .map_err(|error| error.at(span))?;
    let resolved = state.resolve(&inferred);
    expression_types.insert(span, resolved.clone());
    Ok(resolved)
}

fn infer_expr_kind(
    state: &mut State,
    expr: &AstExpr,
    declared_variants: &BTreeSet<TypeName>,
    record_shapes: &HashMap<String, InferRecordShape>,
    expression_types: &mut BTreeMap<TextSpan, Type>,
) -> Result<Type, InferError> {
    match expr {
        AstExpr::Bool { .. } => Ok(Type::Bool),
        AstExpr::Char { .. } => Ok(Type::Char),
        AstExpr::Float { .. } => Ok(Type::F64),
        AstExpr::Int { .. } => Ok(Type::I64),
        AstExpr::String { .. } => Ok(Type::String),
        AstExpr::Unit { .. } => Ok(Type::Unit),
        AstExpr::Lambda {
            params: names,
            param_types,
            body,
            ..
        } => {
            let parameter_types = names
                .iter()
                .enumerate()
                .map(|(index, _param)| {
                    param_types
                        .get(index)
                        .and_then(|annotation| annotation.as_ref())
                        .map(|annotation| {
                            rsig_type_to_infer_type(
                                &lower_type_annotation(annotation, declared_variants),
                                state,
                            )
                        })
                        .unwrap_or_else(|| state.fresh_var())
                })
                .collect::<Vec<_>>();

            state.push_scope();
            for (name, type_) in names.iter().zip(&parameter_types) {
                state.add_value(name.clone(), state.monomorphic(type_.clone()));
            }
            let result = infer_block(
                state,
                body,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            state.pop_scope();
            Ok(source_function_type(
                parameter_types,
                state.resolve(&result),
            ))
        }
        AstExpr::Path { path, .. } if path.segments.len() == 1 => {
            let name = &path.segments[0];
            let scheme = state
                .get_value(name)
                .cloned()
                .ok_or_else(|| InferError::UnknownValue(name.clone()))?;
            Ok(state.instantiate(&scheme))
        }
        AstExpr::Add { lhs, rhs, .. }
        | AstExpr::Sub { lhs, rhs, .. }
        | AstExpr::Mul { lhs, rhs, .. }
        | AstExpr::Div { lhs, rhs, .. }
        | AstExpr::Mod { lhs, rhs, .. } => {
            let lhs = infer_expr(
                state,
                lhs,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            let rhs = infer_expr(
                state,
                rhs,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            if state.unify(&Type::I64, &lhs).is_err() {
                return Err(InferError::ExpectedI64 {
                    context: "arithmetic expression",
                    actual: infer_type_to_rsig_type(&state.resolve(&lhs)),
                });
            }
            if state.unify(&Type::I64, &rhs).is_err() {
                return Err(InferError::ExpectedI64 {
                    context: "arithmetic expression",
                    actual: infer_type_to_rsig_type(&state.resolve(&rhs)),
                });
            }
            Ok(Type::I64)
        }
        AstExpr::Neg { expr, .. } => {
            let actual = infer_expr(
                state,
                expr,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            match state.resolve(&actual) {
                Type::I64 => Ok(Type::I64),
                Type::F64 => Ok(Type::F64),
                Type::Var(_) => {
                    state.unify(&Type::I64, &actual)?;
                    Ok(Type::I64)
                }
                actual => Err(InferError::ExpectedNumeric {
                    context: "negation expression",
                    actual: infer_type_to_rsig_type(&actual),
                }),
            }
        }
        AstExpr::Eq { lhs, rhs, .. } => {
            let lhs = infer_expr(
                state,
                lhs,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            let rhs = infer_expr(
                state,
                rhs,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            state.unify(&lhs, &rhs)?;
            Ok(Type::Bool)
        }
        AstExpr::Lt { lhs, rhs, .. } => {
            let lhs = infer_expr(
                state,
                lhs,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            let rhs = infer_expr(
                state,
                rhs,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            state.unify(&lhs, &rhs)?;
            match state.resolve(&lhs) {
                Type::I64 | Type::String | Type::Var(_) => Ok(Type::Bool),
                actual => Err(InferError::ExpectedComparable {
                    context: "ordering expression",
                    actual: infer_type_to_rsig_type(&actual),
                }),
            }
        }
        AstExpr::And { lhs, rhs, .. } | AstExpr::Or { lhs, rhs, .. } => {
            let lhs = infer_expr(
                state,
                lhs,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            let rhs = infer_expr(
                state,
                rhs,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            if state.unify(&Type::Bool, &lhs).is_err() {
                return Err(InferError::ExpectedBool {
                    context: "logical expression",
                    actual: infer_type_to_rsig_type(&state.resolve(&lhs)),
                });
            }
            if state.unify(&Type::Bool, &rhs).is_err() {
                return Err(InferError::ExpectedBool {
                    context: "logical expression",
                    actual: infer_type_to_rsig_type(&state.resolve(&rhs)),
                });
            }
            Ok(Type::Bool)
        }
        AstExpr::Not { expr, .. } => {
            let actual = infer_expr(
                state,
                expr,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            if state.unify(&Type::Bool, &actual).is_err() {
                return Err(InferError::ExpectedBool {
                    context: "not expression",
                    actual: infer_type_to_rsig_type(&state.resolve(&actual)),
                });
            }
            Ok(Type::Bool)
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            let condition = infer_expr(
                state,
                condition,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            if state.unify(&Type::Bool, &condition).is_err() {
                return Err(InferError::ExpectedBool {
                    context: "if condition",
                    actual: infer_type_to_rsig_type(&state.resolve(&condition)),
                });
            }
            let then_type = infer_expr(
                state,
                then_branch,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            let else_type = infer_expr(
                state,
                else_branch,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            state.unify(&then_type, &else_type)?;
            Ok(state.resolve(&then_type))
        }
        AstExpr::Match {
            scrutinee, arms, ..
        } => {
            if arms.is_empty() {
                return Err(InferError::Unsupported("empty match"));
            }
            let scrutinee_type = infer_expr(
                state,
                scrutinee,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            let result = state.fresh_var();
            for arm in arms {
                state.push_scope();
                infer_pattern(state, &arm.pattern, &scrutinee_type)?;
                let body_type = infer_expr(
                    state,
                    &arm.body,
                    declared_variants,
                    record_shapes,
                    expression_types,
                )?;
                state.unify(&result, &body_type)?;
                state.pop_scope();
            }
            expression_types.insert(expr_span(scrutinee), state.resolve(&scrutinee_type));
            Ok(state.resolve(&result))
        }
        AstExpr::Block { block, .. } => {
            state.push_scope();
            let result = infer_block(
                state,
                block,
                declared_variants,
                record_shapes,
                expression_types,
            );
            state.pop_scope();
            result
        }
        AstExpr::Tuple { items, .. } => Ok(Type::Tuple(
            items
                .iter()
                .map(|item| {
                    infer_expr(
                        state,
                        item,
                        declared_variants,
                        record_shapes,
                        expression_types,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?,
        )),
        AstExpr::List { items, .. } => {
            let element = state.fresh_var();
            for item in items {
                let actual = infer_expr(
                    state,
                    item,
                    declared_variants,
                    record_shapes,
                    expression_types,
                )?;
                state.unify(&element, &actual)?;
            }
            Ok(Type::List(Box::new(state.resolve(&element))))
        }
        AstExpr::Call { callee, args, .. } if callee.segments.len() == 1 => {
            let name = &callee.segments[0];
            let scheme = state
                .get_value(name)
                .cloned()
                .ok_or_else(|| InferError::UnknownValue(name.clone()))?;
            let callee_type = state.instantiate(&scheme);
            let arg_types = args
                .iter()
                .map(|arg| {
                    infer_expr(
                        state,
                        arg,
                        declared_variants,
                        record_shapes,
                        expression_types,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            apply_call(state, callee_type, arg_types)
        }
        AstExpr::Apply { callee, args, .. } => {
            let callee_type = infer_expr(
                state,
                callee,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            let arg_types = args
                .iter()
                .map(|arg| {
                    infer_expr(
                        state,
                        arg,
                        declared_variants,
                        record_shapes,
                        expression_types,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            apply_call(state, callee_type, arg_types)
        }
        AstExpr::Spawn { body, .. } => {
            state.push_scope();
            infer_block(
                state,
                body,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            state.pop_scope();
            let message_type = actor_block_message_type(state, body)
                .map(|type_| rsig_type_to_infer_type(&type_, state))
                .unwrap_or_else(|| state.fresh_var());
            Ok(Type::ActorId(Box::new(message_type)))
        }
        AstExpr::Record { path, fields, .. } => infer_record_literal(
            state,
            path,
            fields,
            record_shapes,
            declared_variants,
            expression_types,
        ),
        AstExpr::TupleIndex { base, index, .. } => {
            let inferred_base = infer_expr(
                state,
                base,
                declared_variants,
                record_shapes,
                expression_types,
            )?;
            let base_type = state.resolve(&inferred_base);
            match base_type {
                Type::Tuple(items) => items
                    .get(*index)
                    .cloned()
                    .ok_or(InferError::Unsupported("tuple projection index")),
                Type::Var(_) => Ok(state.fresh_var()),
                _ => Err(InferError::Unsupported("tuple projection")),
            }
        }
        AstExpr::Field { base, field, .. } => {
            if let AstExpr::Record { fields, .. } = base.as_ref() {
                fields
                    .iter()
                    .find_map(|(name, value)| {
                        (name == field).then(|| {
                            infer_expr(
                                state,
                                value,
                                declared_variants,
                                record_shapes,
                                expression_types,
                            )
                        })
                    })
                    .unwrap_or(Err(InferError::Unsupported("record field")))
            } else {
                let inferred_base = infer_expr(
                    state,
                    base,
                    declared_variants,
                    record_shapes,
                    expression_types,
                )?;
                let base_type = state.resolve(&inferred_base);
                if let Some(type_) =
                    infer_record_field_type(state, &base_type, field, record_shapes)
                {
                    Ok(type_)
                } else if matches!(base_type, Type::Var(_)) {
                    Ok(state.fresh_var())
                } else {
                    Ok(state.fresh_var())
                }
            }
        }
        AstExpr::Receive { arms, .. } => {
            let message = state.fresh_var();
            for arm in arms {
                state.push_scope();
                infer_pattern(state, &arm.pattern, &message)?;
                infer_expr(
                    state,
                    &arm.body,
                    declared_variants,
                    record_shapes,
                    expression_types,
                )?;
                state.pop_scope();
            }
            Ok(Type::Unit)
        }
        AstExpr::Call { callee, args, .. } if callee.segments.len() == 2 => {
            let name = callee.segments.join(".");
            let scheme = state
                .get_value(&name)
                .cloned()
                .ok_or_else(|| InferError::UnknownValue(name.clone()))?;
            let callee_type = state.instantiate(&scheme);
            let arg_types = args
                .iter()
                .map(|arg| {
                    infer_expr(
                        state,
                        arg,
                        declared_variants,
                        record_shapes,
                        expression_types,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            apply_call(state, callee_type, arg_types)
        }
        AstExpr::Path { path, .. } if path.segments.len() == 2 => {
            let name = path.segments.join(".");
            let scheme = state
                .get_value(&name)
                .cloned()
                .ok_or_else(|| InferError::UnknownValue(name.clone()))?;
            Ok(state.instantiate(&scheme))
        }
        AstExpr::Call { callee, .. } if callee.segments.len() > 2 => {
            Err(InferError::Unsupported("nested path call"))
        }
        AstExpr::Call { .. } => Err(InferError::Unsupported("path call")),
        AstExpr::Path { path, .. } if path.segments.len() > 2 => {
            Err(InferError::Unsupported("nested path"))
        }
        AstExpr::Path { .. } => Err(InferError::Unsupported("path")),
    }
}

fn actor_block_message_type(state: &mut State, block: &AstBlock) -> Option<RsigType> {
    let mut message_type = None;
    for stmt in &block.statements {
        if let AstStmt::Expr(AstExpr::Receive { arms, .. }) = stmt {
            merge_receive_message_type(state, arms, &mut message_type)?;
        }
    }
    if let Some(AstExpr::Receive { arms, .. }) = &block.tail {
        merge_receive_message_type(state, arms, &mut message_type)?;
    }
    message_type
}

fn merge_receive_message_type(
    state: &mut State,
    arms: &[crate::ast::AstReceiveArm],
    message_type: &mut Option<RsigType>,
) -> Option<()> {
    for arm in arms {
        let arm_type = pattern_message_type(state, &arm.pattern)?;
        *message_type = Some(match message_type.take() {
            Some(current) => merge_message_type(current, arm_type)?,
            None => arm_type,
        });
    }
    Some(())
}

fn pattern_message_type(state: &mut State, pattern: &AstPattern) -> Option<RsigType> {
    match pattern {
        AstPattern::Wildcard { .. } | AstPattern::Bind { .. } => None,
        AstPattern::Unit { .. } => Some(RsigType::Unit),
        AstPattern::Bool { .. } => Some(RsigType::Bool),
        AstPattern::Int { .. } => Some(RsigType::I64),
        AstPattern::String { .. } => Some(RsigType::String),
        AstPattern::Tuple { items, .. } => Some(RsigType::Tuple(
            items
                .iter()
                .map(|item| pattern_message_type(state, item).unwrap_or(RsigType::Unknown))
                .collect(),
        )),
        AstPattern::List { prefix, tail, .. } => {
            let item = prefix
                .iter()
                .filter_map(|item| pattern_message_type(state, item))
                .next()
                .or_else(|| {
                    tail.as_ref()
                        .and_then(|tail| match pattern_message_type(state, tail) {
                            Some(RsigType::List(item)) => Some(*item),
                            _ => None,
                        })
                })
                .unwrap_or(RsigType::Unknown);
            Some(RsigType::List(Box::new(item)))
        }
        AstPattern::Record { path, .. } => {
            Some(RsigType::Record(TypeName::new(path.segments.join("."))))
        }
        AstPattern::Constructor { path, .. } => {
            let name = path.segments.join(".");
            let scheme = state.get_value(&name)?.clone();
            let mut type_ = state.instantiate(&scheme);
            loop {
                match state.resolve(&type_) {
                    Type::Arrow { result, .. } => type_ = *result,
                    result => return Some(infer_type_to_rsig_type(&result)),
                }
            }
        }
    }
}

fn merge_message_type(lhs: RsigType, rhs: RsigType) -> Option<RsigType> {
    match (lhs, rhs) {
        (RsigType::Unknown, type_) | (type_, RsigType::Unknown) => Some(type_),
        (RsigType::Tuple(lhs), RsigType::Tuple(rhs)) if lhs.len() == rhs.len() => {
            let items = lhs
                .into_iter()
                .zip(rhs)
                .map(|(lhs, rhs)| merge_message_type(lhs, rhs))
                .collect::<Option<Vec<_>>>()?;
            Some(RsigType::Tuple(items))
        }
        (RsigType::ActorId(lhs), RsigType::ActorId(rhs)) => {
            Some(RsigType::ActorId(Box::new(merge_message_type(*lhs, *rhs)?)))
        }
        (lhs, rhs) if lhs == rhs => Some(lhs),
        _ => None,
    }
}

fn apply_call(
    state: &mut State,
    callee_type: Type,
    arg_types: Vec<Type>,
) -> Result<Type, InferError> {
    let mut callee_type = callee_type;
    if arg_types.is_empty() {
        let result = state.fresh_var();
        state.unify(&callee_type, &Type::arrow(Type::Unit, result.clone()))?;
        return Ok(state.resolve(&result));
    }

    for arg_type in arg_types {
        let result = state.fresh_var();
        state.unify(&callee_type, &Type::arrow(arg_type, result.clone()))?;
        callee_type = state.resolve(&result);
    }

    Ok(callee_type)
}

fn infer_pattern(
    state: &mut State,
    pattern: &AstPattern,
    scrutinee_type: &Type,
) -> Result<(), InferError> {
    match pattern {
        AstPattern::Wildcard { .. } => Ok(()),
        AstPattern::Bind { name, .. } => {
            state.add_value(
                name.clone(),
                state.monomorphic(state.resolve(scrutinee_type)),
            );
            Ok(())
        }
        AstPattern::Constructor { path, payload, .. } => {
            let name = path.segments.join(".");
            let scheme = state
                .get_value(&name)
                .cloned()
                .ok_or_else(|| InferError::UnknownValue(name.clone()))?;
            let mut constructor_type = state.instantiate(&scheme);
            for payload_pattern in payload {
                let Type::Arrow { parameter, result } = state.resolve(&constructor_type) else {
                    return Err(InferError::Unsupported("constructor payload arity"));
                };
                let parameter = *parameter;
                infer_pattern(state, payload_pattern, &parameter)?;
                constructor_type = *result;
            }
            state.unify(&constructor_type, scrutinee_type)?;
            Ok(())
        }
        AstPattern::Tuple { items, .. } => {
            let item_types = items.iter().map(|_| state.fresh_var()).collect::<Vec<_>>();
            state.unify(scrutinee_type, &Type::Tuple(item_types.clone()))?;
            for (item, item_type) in items.iter().zip(&item_types) {
                infer_pattern(state, item, item_type)?;
            }
            Ok(())
        }
        AstPattern::List { prefix, tail, .. } => {
            let item_type = state.fresh_var();
            state.unify(scrutinee_type, &Type::List(Box::new(item_type.clone())))?;
            for item in prefix {
                infer_pattern(state, item, &item_type)?;
            }
            if let Some(tail) = tail {
                infer_pattern(state, tail, &Type::List(Box::new(item_type)))?;
            }
            Ok(())
        }
        AstPattern::Record { path, fields, .. } => {
            state.unify(
                scrutinee_type,
                &Type::Record(TypeName::new(path.segments.join("."))),
            )?;
            for (_, field_pattern) in fields {
                let field_type = state.fresh_var();
                infer_pattern(state, field_pattern, &field_type)?;
            }
            Ok(())
        }
        AstPattern::Unit { .. } => state.unify(&Type::Unit, scrutinee_type).map_err(Into::into),
        AstPattern::Bool { .. } => state.unify(&Type::Bool, scrutinee_type).map_err(Into::into),
        AstPattern::Int { .. } => state.unify(&Type::I64, scrutinee_type).map_err(Into::into),
        AstPattern::String { .. } => state
            .unify(&Type::String, scrutinee_type)
            .map_err(Into::into),
    }
}

fn expr_span(expr: &AstExpr) -> TextSpan {
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

fn infer_record_literal(
    state: &mut State,
    path: &crate::ast::AstPath,
    fields: &[(String, AstExpr)],
    record_shapes: &HashMap<String, InferRecordShape>,
    declared_variants: &BTreeSet<TypeName>,
    expression_types: &mut BTreeMap<TextSpan, Type>,
) -> Result<Type, InferError> {
    let key = path.segments.join(".");
    let Some(shape) = record_shapes.get(&key) else {
        return Ok(Type::Record(TypeName::new(key)));
    };
    let result = generic_record_rsig_type(
        shape.type_name.clone(),
        shape.type_params.iter().map(|param| param.as_str()),
    );
    let mut vars = BTreeMap::new();
    let result = rsig_type_to_infer_type_with_vars(&result, state, &mut vars);
    for (name, value) in fields {
        let Some((_, expected)) = shape
            .fields
            .iter()
            .find(|(field_name, _)| field_name == name)
        else {
            continue;
        };
        let expected = rsig_type_to_infer_type_with_vars(expected, state, &mut vars);
        let actual = infer_expr(
            state,
            value,
            declared_variants,
            record_shapes,
            expression_types,
        )?;
        state.unify(&expected, &actual)?;
    }
    Ok(state.resolve(&result))
}

fn infer_record_field_type(
    state: &mut State,
    base_type: &Type,
    field: &str,
    record_shapes: &HashMap<String, InferRecordShape>,
) -> Option<Type> {
    let (name, args) = match base_type {
        Type::Record(name) => (name, Vec::new()),
        Type::RecordApp { name, args } => (name, args.clone()),
        _ => return None,
    };
    let shape = record_shapes.get(name.as_str())?;
    let mut vars = shape
        .type_params
        .iter()
        .cloned()
        .zip(args)
        .collect::<BTreeMap<_, _>>();
    shape
        .fields
        .iter()
        .find_map(|(name, type_)| (name == field).then_some(type_))
        .map(|type_| rsig_type_to_infer_type_with_vars(type_, state, &mut vars))
}

fn generic_record_rsig_type<'a>(name: TypeName, params: impl Iterator<Item = &'a str>) -> RsigType {
    let args = params
        .map(|param| RsigType::Var(TypeVarName::new(param)))
        .collect::<Vec<_>>();
    if args.is_empty() {
        RsigType::Record(name)
    } else {
        RsigType::RecordApp { name, args }
    }
}

fn rsig_signature_to_infer_type(params: &[RsigType], result: &RsigType, state: &mut State) -> Type {
    source_function_type(
        params
            .iter()
            .map(|param| rsig_type_to_infer_type(param, state))
            .collect(),
        rsig_type_to_infer_type(result, state),
    )
}

fn rsig_type_to_infer_type(type_: &RsigType, state: &mut State) -> Type {
    match type_ {
        RsigType::ActorId(message) => {
            Type::ActorId(Box::new(rsig_type_to_infer_type(message, state)))
        }
        RsigType::Arrow { parameter, result } => Type::arrow(
            rsig_type_to_infer_type(parameter, state),
            rsig_type_to_infer_type(result, state),
        ),
        RsigType::Bool => Type::Bool,
        RsigType::Char => Type::Char,
        RsigType::F64 => Type::F64,
        RsigType::I32 => Type::I32,
        RsigType::I64 => Type::I64,
        RsigType::List(element) => Type::List(Box::new(rsig_type_to_infer_type(element, state))),
        RsigType::Record(name) => Type::Record(name.clone()),
        RsigType::RecordApp { name, args } => Type::RecordApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| rsig_type_to_infer_type(arg, state))
                .collect(),
        },
        RsigType::Variant(name) => Type::Variant(name.clone()),
        RsigType::VariantApp { name, args } => Type::VariantApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| rsig_type_to_infer_type(arg, state))
                .collect(),
        },
        RsigType::String => Type::String,
        RsigType::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|item| rsig_type_to_infer_type(item, state))
                .collect(),
        ),
        RsigType::Unit => Type::Unit,
        RsigType::Unknown | RsigType::Var(_) => state.fresh_var(),
    }
}

fn rsig_scheme_to_infer_scheme(scheme: &RsigTypeScheme, state: &mut State) -> TypeScheme {
    let mut vars = BTreeMap::new();
    let mut quantifiers = Vec::new();
    for name in &scheme.quantifiers {
        let fresh = state.fresh_var();
        if let Type::Var(var) = fresh {
            quantifiers.push(var);
            vars.insert(name.clone(), Type::Var(var));
        }
    }
    TypeScheme {
        quantifiers,
        body: rsig_type_to_infer_type_with_vars(&scheme.body, state, &mut vars),
    }
}

fn rsig_type_to_infer_type_with_vars(
    type_: &RsigType,
    state: &mut State,
    vars: &mut BTreeMap<TypeVarName, Type>,
) -> Type {
    match type_ {
        RsigType::ActorId(message) => Type::ActorId(Box::new(rsig_type_to_infer_type_with_vars(
            message, state, vars,
        ))),
        RsigType::Arrow { parameter, result } => Type::arrow(
            rsig_type_to_infer_type_with_vars(parameter, state, vars),
            rsig_type_to_infer_type_with_vars(result, state, vars),
        ),
        RsigType::Bool => Type::Bool,
        RsigType::Char => Type::Char,
        RsigType::F64 => Type::F64,
        RsigType::I32 => Type::I32,
        RsigType::I64 => Type::I64,
        RsigType::List(element) => Type::List(Box::new(rsig_type_to_infer_type_with_vars(
            element, state, vars,
        ))),
        RsigType::Record(name) => Type::Record(name.clone()),
        RsigType::RecordApp { name, args } => Type::RecordApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| rsig_type_to_infer_type_with_vars(arg, state, vars))
                .collect(),
        },
        RsigType::Variant(name) => Type::Variant(name.clone()),
        RsigType::VariantApp { name, args } => Type::VariantApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| rsig_type_to_infer_type_with_vars(arg, state, vars))
                .collect(),
        },
        RsigType::String => Type::String,
        RsigType::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|item| rsig_type_to_infer_type_with_vars(item, state, vars))
                .collect(),
        ),
        RsigType::Unit => Type::Unit,
        RsigType::Var(name) => vars
            .entry(name.clone())
            .or_insert_with(|| state.fresh_var())
            .clone(),
        RsigType::Unknown => state.fresh_var(),
    }
}

fn infer_type_to_rsig_type(type_: &Type) -> RsigType {
    match type_ {
        Type::ActorId(message) => RsigType::ActorId(Box::new(infer_type_to_rsig_type(message))),
        Type::Bool => RsigType::Bool,
        Type::Char => RsigType::Char,
        Type::F64 => RsigType::F64,
        Type::I32 => RsigType::I32,
        Type::Arrow { parameter, result } => RsigType::Arrow {
            parameter: Box::new(infer_type_to_rsig_type(parameter)),
            result: Box::new(infer_type_to_rsig_type(result)),
        },
        Type::I64 => RsigType::I64,
        Type::List(element) => RsigType::List(Box::new(infer_type_to_rsig_type(element))),
        Type::Record(name) => RsigType::Record(name.clone()),
        Type::RecordApp { name, args } => RsigType::RecordApp {
            name: name.clone(),
            args: args.iter().map(infer_type_to_rsig_type).collect(),
        },
        Type::Variant(name) => RsigType::Variant(name.clone()),
        Type::VariantApp { name, args } => RsigType::VariantApp {
            name: name.clone(),
            args: args.iter().map(infer_type_to_rsig_type).collect(),
        },
        Type::String => RsigType::String,
        Type::Tuple(items) => RsigType::Tuple(items.iter().map(infer_type_to_rsig_type).collect()),
        Type::Unit => RsigType::Unit,
        Type::Var(var) => RsigType::Var(TypeVarName::new(format!("'t{}", var.index()))),
    }
}

fn qualify_imported_scheme(module_name: &str, scheme: &RsigTypeScheme) -> RsigTypeScheme {
    RsigTypeScheme {
        quantifiers: scheme.quantifiers.clone(),
        body: qualify_imported_type(module_name, &scheme.body),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        AstBlock, AstDecl, AstExpr, AstFnDecl, AstMatchArm, AstPath, AstPattern, AstProgram,
        AstReceiveArm, AstRecordTypeField, AstStmt, AstTypeAnnotation, AstTypeBody, AstTypeDecl,
        AstTypeExpr, AstTypeParam, AstUseDecl, TextSpan,
    };
    use crate::infer::module::{InferError, InferredModule, ModuleInferencer};
    use crate::infer::scheme::TypeScheme;
    use crate::infer::types::Type;
    use crate::signature::{
        FieldName, FunctionSignature, FunctionTable, ImportedSignatures, ModuleName, Rsig,
        RsigRecordField, RsigType, RsigTypeDecl, RsigTypeDeclKind, TypeName, TypeParamName,
        TypeVarName,
    };

    fn span() -> TextSpan {
        TextSpan::new(0, 0)
    }

    fn int(value: i64) -> AstExpr {
        AstExpr::Int {
            value,
            span: span(),
        }
    }

    fn bool_(value: bool) -> AstExpr {
        AstExpr::Bool {
            value,
            span: span(),
        }
    }

    fn path(name: &str) -> AstExpr {
        path_segments(&[name])
    }

    fn path_segments(segments: &[&str]) -> AstExpr {
        AstExpr::Path {
            path: AstPath {
                segments: segments
                    .iter()
                    .map(|segment| (*segment).to_owned())
                    .collect(),
            },
            span: span(),
        }
    }

    fn add(lhs: AstExpr, rhs: AstExpr) -> AstExpr {
        AstExpr::Add {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: span(),
        }
    }

    fn call(name: &str, args: Vec<AstExpr>) -> AstExpr {
        call_path(&[name], args)
    }

    fn call_path(segments: &[&str], args: Vec<AstExpr>) -> AstExpr {
        AstExpr::Call {
            callee: AstPath {
                segments: segments
                    .iter()
                    .map(|segment| (*segment).to_owned())
                    .collect(),
            },
            args,
            span: span(),
        }
    }

    fn lambda(params: Vec<&str>, tail: AstExpr) -> AstExpr {
        AstExpr::Lambda {
            params: params.into_iter().map(str::to_owned).collect(),
            param_types: Vec::new(),
            body: Box::new(AstBlock {
                statements: Vec::new(),
                tail: Some(tail),
                span: span(),
            }),
            span: span(),
        }
    }

    fn apply(callee: AstExpr, args: Vec<AstExpr>) -> AstExpr {
        AstExpr::Apply {
            callee: Box::new(callee),
            args,
            span: span(),
        }
    }

    fn tuple(items: Vec<AstExpr>) -> AstExpr {
        AstExpr::Tuple {
            items,
            span: span(),
        }
    }

    fn tuple_index(base: AstExpr, index: usize) -> AstExpr {
        AstExpr::TupleIndex {
            base: Box::new(base),
            index,
            span: span(),
        }
    }

    fn record(type_name: &str, fields: Vec<(&str, AstExpr)>) -> AstExpr {
        record_path(&[type_name], fields)
    }

    fn record_path(segments: &[&str], fields: Vec<(&str, AstExpr)>) -> AstExpr {
        AstExpr::Record {
            path: AstPath {
                segments: segments
                    .iter()
                    .map(|segment| (*segment).to_owned())
                    .collect(),
            },
            fields: fields
                .into_iter()
                .map(|(name, value)| (name.to_owned(), value))
                .collect(),
            span: span(),
        }
    }

    fn field(base: AstExpr, name: &str) -> AstExpr {
        AstExpr::Field {
            base: Box::new(base),
            field: name.to_owned(),
            span: span(),
        }
    }

    fn type_var(name: &str) -> AstTypeAnnotation {
        AstTypeAnnotation {
            text: name.to_owned(),
            syntax: AstTypeExpr::Var {
                name: name.to_owned(),
                span: span(),
            },
            span: span(),
        }
    }

    fn generic_record_type(name: &str, param: &str, field: &str) -> AstDecl {
        AstDecl::Type(AstTypeDecl {
            name: name.to_owned(),
            name_span: span(),
            params: vec![AstTypeParam {
                name: param.to_owned(),
                span: span(),
            }],
            body: AstTypeBody::Record {
                fields: vec![AstRecordTypeField {
                    name: field.to_owned(),
                    type_annotation: type_var(param),
                }],
            },
            span: span(),
        })
    }

    fn receive(pattern: AstPattern) -> AstExpr {
        AstExpr::Receive {
            arms: vec![AstReceiveArm {
                pattern,
                body: AstExpr::Unit { span: span() },
            }],
            span: span(),
        }
    }

    fn constructor_pattern(name: &str, payload: Vec<AstPattern>) -> AstPattern {
        AstPattern::Constructor {
            path: AstPath {
                segments: vec![name.to_owned()],
            },
            payload,
            span: span(),
        }
    }

    fn bind_pattern(name: &str) -> AstPattern {
        AstPattern::Bind {
            name: name.to_owned(),
            span: span(),
        }
    }

    fn match_(scrutinee: AstExpr, pattern: AstPattern, body: AstExpr) -> AstExpr {
        AstExpr::Match {
            scrutinee: Box::new(scrutinee),
            arms: vec![AstMatchArm { pattern, body }],
            span: span(),
        }
    }

    fn spawn_with_receive(pattern: AstPattern) -> AstExpr {
        AstExpr::Spawn {
            body: Box::new(AstBlock {
                statements: vec![AstStmt::Expr(receive(pattern))],
                tail: None,
                span: span(),
            }),
            span: span(),
        }
    }

    fn function(name: &str, params: Vec<&str>, statements: Vec<AstStmt>, tail: AstExpr) -> AstDecl {
        AstDecl::Function(AstFnDecl {
            name: name.to_owned(),
            name_span: span(),
            params: params.into_iter().map(str::to_owned).collect(),
            param_types: Vec::new(),
            return_type: None,
            body: AstBlock {
                statements,
                tail: Some(tail),
                span: span(),
            },
            span: span(),
        })
    }

    fn infer(program: &AstProgram) -> Result<InferredModule, InferError> {
        ModuleInferencer::new(program, &ImportedSignatures::new()).infer()
    }

    fn infer_with_imports(
        program: &AstProgram,
        imports: &ImportedSignatures,
    ) -> Result<InferredModule, InferError> {
        ModuleInferencer::new(program, imports).infer()
    }

    fn signatures(program: &AstProgram) -> Result<FunctionTable, InferError> {
        infer(program).map(|inferred| inferred.function_signatures(program))
    }

    fn root_error(error: &InferError) -> &InferError {
        match error {
            InferError::At { error, .. } => root_error(error),
            error => error,
        }
    }

    #[test]
    fn scans_module_declarations_top_to_bottom() {
        let program = AstProgram {
            decls: vec![
                function("one", vec![], Vec::new(), int(1)),
                function(
                    "two",
                    vec![],
                    Vec::new(),
                    add(call("one", Vec::new()), int(1)),
                ),
            ],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(exports.len(), 2);
        assert_eq!(exports[0].0, "one");
        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::I64))
        );
        assert_eq!(exports[1].0, "two");
        assert_eq!(
            exports[1].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::I64))
        );
    }

    #[test]
    fn later_declarations_cannot_see_future_values() {
        let program = AstProgram {
            decls: vec![function("two", vec![], Vec::new(), call("one", Vec::new()))],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(
            &crate::infer::module::InferError::UnknownValue("one".to_owned()),
            root_error(&err)
        );
    }

    #[test]
    fn unsupported_empty_value_paths_are_classified() {
        let program = AstProgram {
            decls: vec![function("main", vec![], Vec::new(), path_segments(&[]))],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(Some("path"), err.unsupported_reason());
    }

    #[test]
    fn unsupported_empty_call_paths_are_classified() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                call_path(&[], Vec::new()),
            )],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(Some("path call"), err.unsupported_reason());
    }

    #[test]
    fn unsupported_nested_value_paths_are_classified() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                path_segments(&["Module", "value", "field"]),
            )],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(Some("nested path"), err.unsupported_reason());
    }

    #[test]
    fn unsupported_nested_call_paths_are_classified() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                call_path(&["Module", "value", "call"], Vec::new()),
            )],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(Some("nested path call"), err.unsupported_reason());
    }

    #[test]
    fn empty_matches_are_classified() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                AstExpr::Match {
                    scrutinee: Box::new(int(1)),
                    arms: Vec::new(),
                    span: span(),
                },
            )],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(Some("empty match"), err.unsupported_reason());
    }

    #[test]
    fn tuple_projection_index_failures_are_classified() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                tuple_index(tuple(vec![int(1)]), 1),
            )],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(Some("tuple projection index"), err.unsupported_reason());
    }

    #[test]
    fn tuple_projection_shape_failures_are_classified() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                tuple_index(bool_(true), 0),
            )],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(Some("tuple projection"), err.unsupported_reason());
    }

    #[test]
    fn inline_record_field_failures_are_classified() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                field(record("point", vec![("x", int(1))]), "y"),
            )],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(Some("record field"), err.unsupported_reason());
    }

    #[test]
    fn inline_record_field_successes_infer_the_field_type() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                field(record("point", vec![("x", int(1))]), "x"),
            )],
        };

        let signatures = signatures(&program).unwrap();

        assert_eq!(
            signatures.get("main"),
            Some(&FunctionSignature::new(Vec::new(), RsigType::I64))
        );
    }

    #[test]
    fn record_literals_infer_nominal_record_types() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                record("point", vec![("x", int(1))]),
            )],
        };

        let signatures = signatures(&program).unwrap();

        assert_eq!(
            signatures.get("main"),
            Some(&FunctionSignature::new(
                Vec::new(),
                RsigType::Record(TypeName::new("point"))
            ))
        );
    }

    #[test]
    fn constructor_payload_pattern_arity_failures_are_classified() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                match_(
                    call("Some", vec![int(1)]),
                    constructor_pattern(
                        "Some",
                        vec![bind_pattern("first"), bind_pattern("second")],
                    ),
                    AstExpr::Unit { span: span() },
                ),
            )],
        };

        let err = infer(&program).unwrap_err();

        assert_eq!(Some(span()), err.span());
        assert_eq!(Some("constructor payload arity"), err.unsupported_reason());
    }

    #[test]
    fn lexical_let_rebinding_uses_the_latest_binding() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                vec![
                    AstStmt::Let {
                        name: "a".to_owned(),
                        type_annotation: None,
                        value: int(0),
                    },
                    AstStmt::Let {
                        name: "a".to_owned(),
                        type_annotation: None,
                        value: bool_(true),
                    },
                ],
                path("a"),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::Bool))
        );
    }

    #[test]
    fn function_parameters_are_generalized_at_the_top_level() {
        let program = AstProgram {
            decls: vec![function("id", vec!["x"], Vec::new(), path("x"))],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();
        let var = exports[0].1.quantifiers[0];

        assert_eq!(exports[0].0, "id");
        assert_eq!(exports[0].1.quantifiers.len(), 1);
        assert_eq!(
            exports[0].1,
            TypeScheme {
                quantifiers: vec![var],
                body: Type::arrow(Type::Var(var), Type::Var(var)),
            }
        );
    }

    #[test]
    fn lambda_expressions_infer_unary_arrow_chains() {
        let program = AstProgram {
            decls: vec![function(
                "make_adder",
                vec!["n"],
                Vec::new(),
                lambda(vec!["x"], add(path("x"), path("n"))),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::I64, Type::arrow(Type::I64, Type::I64)))
        );
    }

    #[test]
    fn generic_constructor_types_link_payload_vars_to_result_args() {
        let mut state = crate::infer::state::State::default();
        let payload = vec![RsigType::Var(crate::signature::TypeVarName::new("'value"))];
        let result =
            super::generic_variant_rsig_type(TypeName::new("option"), ["'value"].into_iter());
        let type_ = super::constructor_type(&payload, &result, &mut state);

        let Type::Arrow { parameter, result } = type_ else {
            panic!("expected constructor to infer as a function");
        };
        let Type::VariantApp { name, args } = *result else {
            panic!("expected generic constructor result to preserve type args");
        };

        assert_eq!(TypeName::new("option"), name);
        assert_eq!(vec![*parameter], args);
    }

    #[test]
    fn generic_record_field_access_infers_substituted_field_type() {
        let program = AstProgram {
            decls: vec![
                generic_record_type("box", "'a", "value"),
                function(
                    "main",
                    vec![],
                    vec![AstStmt::Let {
                        name: "item".to_owned(),
                        type_annotation: None,
                        value: record("box", vec![("value", int(1))]),
                    }],
                    field(path("item"), "value"),
                ),
            ],
        };

        let signatures = signatures(&program).unwrap();

        assert_eq!(
            signatures.get("main"),
            Some(&FunctionSignature::new(vec![], RsigType::I64))
        );
    }

    fn boxes_imports() -> ImportedSignatures {
        let mut imports = ImportedSignatures::new();
        imports.insert(
            ModuleName::new("Boxes"),
            Rsig {
                module: ModuleName::new("Boxes"),
                dependencies: Vec::new(),
                types: vec![RsigTypeDecl {
                    name: TypeName::new("box"),
                    params: vec![TypeParamName::new("'a")],
                    body: RsigTypeDeclKind::Record {
                        fields: vec![RsigRecordField {
                            name: FieldName::new("value"),
                            type_: RsigType::Var(TypeVarName::new("'a")),
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

    #[test]
    fn imported_generic_record_literals_infer_result_args_from_fields() {
        let imports = boxes_imports();
        let program = AstProgram {
            decls: vec![
                AstDecl::Use(AstUseDecl {
                    name: "Boxes".to_owned(),
                    name_span: span(),
                    span: span(),
                }),
                function(
                    "make",
                    vec![],
                    Vec::new(),
                    record_path(&["Boxes", "box"], vec![("value", int(1))]),
                ),
            ],
        };

        let inferred = infer_with_imports(&program, &imports).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(
                Type::Unit,
                Type::RecordApp {
                    name: TypeName::new("Boxes.box"),
                    args: vec![Type::I64],
                }
            ))
        );
    }

    #[test]
    fn imported_generic_record_field_access_infers_substituted_field_type() {
        let imports = boxes_imports();
        let program = AstProgram {
            decls: vec![
                AstDecl::Use(AstUseDecl {
                    name: "Boxes".to_owned(),
                    name_span: span(),
                    span: span(),
                }),
                function(
                    "main",
                    vec![],
                    vec![AstStmt::Let {
                        name: "item".to_owned(),
                        type_annotation: None,
                        value: record_path(&["Boxes", "box"], vec![("value", int(1))]),
                    }],
                    field(path("item"), "value"),
                ),
            ],
        };

        let inferred = infer_with_imports(&program, &imports).unwrap();
        let signatures = inferred.function_signatures(&program);

        assert_eq!(
            signatures.get("main"),
            Some(&FunctionSignature::new(vec![], RsigType::I64))
        );
    }

    #[test]
    fn apply_expressions_infer_from_the_callee_arrow() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                apply(lambda(vec!["x"], path("x")), vec![int(1)]),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::I64))
        );
    }

    #[test]
    fn let_bound_lambdas_can_be_called_by_name() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                vec![AstStmt::Let {
                    name: "id".to_owned(),
                    type_annotation: None,
                    value: lambda(vec!["x"], path("x")),
                }],
                call("id", vec![int(1)]),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::I64))
        );
    }

    #[test]
    fn let_bound_lambdas_are_instantiated_at_each_use() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                vec![
                    AstStmt::Let {
                        name: "id".to_owned(),
                        type_annotation: None,
                        value: lambda(vec!["x"], path("x")),
                    },
                    AstStmt::Expr(call("id", vec![int(1)])),
                ],
                call("id", vec![bool_(true)]),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::Bool))
        );
    }

    #[test]
    fn prelude_values_are_visible_but_not_exported() {
        let program = AstProgram {
            decls: vec![function(
                "main",
                vec![],
                Vec::new(),
                call("dbg", vec![int(1)]),
            )],
        };

        let inferred = infer(&program).unwrap();
        let exports = inferred.env.exported_values();

        assert_eq!(exports.len(), 1);
        assert_eq!(exports[0].0, "main");
        assert_eq!(
            exports[0].1,
            TypeScheme::monomorphic(Type::arrow(Type::Unit, Type::Unit))
        );
    }

    #[test]
    fn function_signatures_project_concrete_parameter_types() {
        let program = AstProgram {
            decls: vec![function(
                "add",
                vec!["n", "x"],
                Vec::new(),
                add(path("x"), path("n")),
            )],
        };

        let signatures = signatures(&program).unwrap();

        assert_eq!(
            signatures.get("add"),
            Some(&FunctionSignature::new(
                vec![RsigType::I64, RsigType::I64],
                RsigType::I64
            ))
        );
    }

    #[test]
    fn function_signatures_project_arrow_parameter_types() {
        let program = AstProgram {
            decls: vec![function(
                "apply_i64",
                vec!["f", "x"],
                Vec::new(),
                add(call("f", vec![add(path("x"), int(0))]), int(1)),
            )],
        };

        let signatures = signatures(&program).unwrap();

        assert_eq!(
            signatures.get("apply_i64"),
            Some(&FunctionSignature::new(
                vec![
                    RsigType::Arrow {
                        parameter: Box::new(RsigType::I64),
                        result: Box::new(RsigType::I64),
                    },
                    RsigType::I64,
                ],
                RsigType::I64,
            ))
        );
    }

    #[test]
    fn function_signatures_project_actor_message_types_from_receive() {
        let program = AstProgram {
            decls: vec![function(
                "make_worker",
                vec![],
                Vec::new(),
                spawn_with_receive(AstPattern::String {
                    value: "go".to_owned(),
                    span: span(),
                }),
            )],
        };

        let signatures = signatures(&program).unwrap();

        assert_eq!(
            signatures.get("make_worker"),
            Some(&FunctionSignature::new(
                vec![],
                RsigType::ActorId(Box::new(RsigType::String))
            ))
        );
    }
}
