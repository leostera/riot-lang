use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use camino::Utf8Path;

mod blocks;
mod callable;
mod const_eval;
mod entrypoint;
mod imports;
mod lower;
mod patterns;
mod shapes;
mod signature;
mod span;
pub(crate) mod tyir;
mod type_annotation;
mod types;

use blocks::BlockValidator;
use const_eval::{ConstFunction, ConstFunctionTable, ConstValue};
use entrypoint::EntrypointValidator;
use imports::ImportValidator;
use patterns::PatternValidator;
use shapes::TypeShapeCollector;
use span::expr_span;
use type_annotation::TypeAnnotationChecker;

use callable::{
    CallableKind, CallableResolver, CallableSignature, ExternalSignature, ExternalTable,
    prelude_external_signatures,
};

use crate::ast::{
    AstBlock, AstDecl, AstExpr, AstPath, AstPattern, AstProgram, AstStmt, AstTypeAnnotation,
    AstTypeBody, TextSpan,
};
use crate::checker::tyir::{CheckedProgram, RsigBuilder, TyIrBuilder};
use crate::diagnostic::{SourceDiagnostic, SourceDiagnostics};
use crate::imported_types::{imported_type_name, qualify_imported_type};
use crate::infer::module::{ExpressionTypeTable, InferError, ModuleInferencer};
use crate::signature::{
    FunctionSignature, FunctionTable, ImportedSignatures, ModuleName, Rsig, RsigExport, RsigType,
    RsigTypeDeclKind, TypeName, TypeVarName,
};
use crate::type_lowerer::RsigTypeLowerer;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CheckMode {
    Executable,
    Interface,
}

pub(crate) struct Checker<'a> {
    source_path: &'a Utf8Path,
    source: &'a str,
    module_name: ModuleName,
    imports: &'a ImportedSignatures,
    mode: CheckMode,
}

impl<'a> Checker<'a> {
    pub(crate) fn new(
        source_path: &'a Utf8Path,
        source: &'a str,
        module_name: ModuleName,
        imports: &'a ImportedSignatures,
        mode: CheckMode,
    ) -> Self {
        Self {
            source_path,
            source,
            module_name,
            imports,
            mode,
        }
    }

    pub(crate) fn check(&self, program: AstProgram) -> miette::Result<CheckedProgram> {
        ProgramValidator::new(self.source_path, self.source, self.imports, self.mode, None)
            .validate(&program)?;
        let inferred = ModuleInferencer::new(&program, self.imports)
            .infer()
            .map_err(|error| self.infer_diagnostic(&program, error))?;
        let expression_types = inferred.expression_rsig_types();
        ProgramValidator::new(
            self.source_path,
            self.source,
            self.imports,
            self.mode,
            Some(&expression_types),
        )
        .validate(&program)?;
        let function_types = inferred.function_signatures(&program);
        let typed_tree = TyIrBuilder::new(
            self.module_name.clone(),
            self.imports,
            &function_types,
            Some(&expression_types),
        )
        .build(program);
        let signature = RsigBuilder::new().build(&typed_tree);
        Ok(CheckedProgram {
            typed_tree,
            signature,
        })
    }

    fn infer_diagnostic(&self, program: &AstProgram, error: InferError) -> SourceDiagnostic {
        let span = error
            .span()
            .or_else(|| first_decl_span(program))
            .unwrap_or_else(|| TextSpan::new(0, self.source.len().min(1)));
        let diagnostics = SourceDiagnostics::new(self.source_path, self.source);
        if error.is_occurs_check() {
            return diagnostics.at(
                span,
                "recursive type inferred",
                "this expression would require a value to have a type that contains itself",
                Some("avoid applying a value to itself or add an explicit non-recursive function type boundary"),
            );
        }
        if let Some((lhs, rhs)) = error.type_mismatch_types() {
            if span_is_list_expression(program, span) {
                return diagnostics.at(
                    span,
                    "list items have different types",
                    format!(
                        "this list contains both `{}` and `{}` values",
                        lhs.canonical(),
                        rhs.canonical()
                    ),
                    Some("make every item in the list have the same type"),
                );
            }
            if span_is_if_expression(program, span) {
                return diagnostics.at(
                    span,
                    "if branches have different types",
                    format!(
                        "one branch has type `{}`, but another branch has type `{}`",
                        lhs.canonical(),
                        rhs.canonical()
                    ),
                    Some("make both branches produce the same type"),
                );
            }
            if span_is_match_expression(program, span) {
                return diagnostics.at(
                    span,
                    "match arms have different types",
                    format!(
                        "one arm has type `{}`, but another arm has type `{}`",
                        lhs.canonical(),
                        rhs.canonical()
                    ),
                    Some("make every match arm produce the same type"),
                );
            }
            let lhs_is_arrow = matches!(lhs, RsigType::Arrow { .. });
            let rhs_is_arrow = matches!(rhs, RsigType::Arrow { .. });
            if lhs_is_arrow != rhs_is_arrow {
                let actual = if lhs_is_arrow { &rhs } else { &lhs };
                return diagnostics.at(
                    span,
                    "called value is not a function",
                    format!(
                        "this value has type `{}`, so it cannot be called",
                        actual.canonical()
                    ),
                    Some("call a function value or remove the function arguments"),
                );
            }
            if span_is_call_expression(program, span) {
                return diagnostics.at(
                    span,
                    "function argument type does not match",
                    format!(
                        "this call requires `{}`, but the argument provides `{}`",
                        lhs.canonical(),
                        rhs.canonical()
                    ),
                    Some("pass an argument with the parameter type expected by the function"),
                );
            }
            return diagnostics.at(
                span,
                "inferred types do not match",
                format!(
                    "this expression requires `{}`, but another branch or constraint requires `{}`",
                    lhs.canonical(),
                    rhs.canonical()
                ),
                Some("make both sides of the expression produce the same type or add an annotation at the intended boundary"),
            );
        }
        if let Some((lhs, rhs)) = error.tuple_arity_mismatch() {
            return diagnostics.at(
                span,
                "tuple shapes do not match",
                format!(
                    "this expression requires a tuple with {lhs} fields, but another branch or constraint requires {rhs} fields"
                ),
                Some("make both tuple expressions have the same number of fields"),
            );
        }
        if let Some((context, actual)) = error.expected_bool_context() {
            let message = match context {
                "if condition" => "if condition must be Bool",
                "not expression" => "not expression requires Bool",
                "logical expression" => "logical operands must be Bool",
                _ => "expression must be Bool",
            };
            return diagnostics.at(
                span,
                message,
                format!("this {context} has type `{}`", actual.canonical()),
                Some("use a Bool expression here"),
            );
        }
        if let Some((context, actual)) = error.expected_i64_context() {
            return diagnostics.at(
                span,
                "arithmetic operands must be i64",
                format!("this {context} has type `{}`", actual.canonical()),
                Some("use i64 operands on both sides of this arithmetic expression"),
            );
        }
        if let Some((context, actual)) = error.expected_numeric_context() {
            return diagnostics.at(
                span,
                "numeric operand required",
                format!("this {context} has type `{}`", actual.canonical()),
                Some("use an i64 or f64 value here"),
            );
        }
        if let Some((context, actual)) = error.expected_comparable_context() {
            return diagnostics.at(
                span,
                "ordering operands are not comparable",
                format!("this {context} has type `{}`", actual.canonical()),
                Some("compare i64 values with i64 values or string values with string values"),
            );
        }
        if matches!(error.unsupported_reason(), Some("tuple projection index")) {
            return diagnostics.at(
                span,
                "tuple projection is out of bounds",
                "this tuple does not have a field at the requested index",
                Some("use a tuple projection index that exists"),
            );
        }
        if matches!(error.unsupported_reason(), Some("tuple projection")) {
            return diagnostics.at(
                span,
                "tuple projection on non-tuple value",
                "only tuple values can be projected with `.0`, `.1`, and similar indexes",
                Some("project a tuple value or remove the tuple index"),
            );
        }
        if matches!(error.unsupported_reason(), Some("record field")) {
            return diagnostics.at(
                span,
                "record literal has no such field",
                "this inline record literal does not contain the projected field",
                Some("project a field that exists on the record literal or bind the record before projecting"),
            );
        }
        if let Some(name) = error.unknown_value_name()
            && let Some(function) = program.decls.iter().find_map(|decl| match decl {
                AstDecl::Function(function) if function.name == name => Some(function),
                _ => None,
            })
        {
            let hint = if function.return_type.is_some()
                && function
                    .param_types
                    .iter()
                    .all(|annotation| annotation.is_some())
            {
                "move the callee above this function or check that every function in the recursion cycle is fully annotated"
            } else {
                "move the callee above this function or add parameter and return type annotations to every function in the recursion cycle"
            };
            return diagnostics.at(
                span,
                "top-level function used before definition",
                format!("`{name}` is declared later in this module, but stage0 resolves functions top-to-bottom unless a recursion cycle is fully annotated"),
                Some(hint),
            );
        }
        diagnostics.at(
            span,
            "type inference failed",
            error.to_string(),
            Some("fix the type error before lowering"),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BindingKind {
    Actor,
    Value(Option<RsigType>),
}

#[derive(Debug)]
struct ValidationContext<'a> {
    source_path: &'a Utf8Path,
    source: &'a str,
    functions: ConstFunctionTable,
    function_signatures: FunctionTable,
    function_names: FunctionNameSet,
    function_results: HashMap<String, RsigType>,
    declared_external_names: ExternalNameSet,
    external_signatures: ExternalTable,
    external_names: ExternalNameSet,
    constructor_types: HashMap<String, ConstructorShape>,
    declared_variants: BTreeSet<TypeName>,
    record_shapes: HashMap<String, RecordShape>,
    record_shapes_by_type: BTreeMap<TypeName, RecordShape>,
    imports: &'a ImportedSignatures,
    expression_types: Option<&'a ExpressionTypeTable>,
}

#[derive(Debug, Clone, Default)]
struct FunctionNameSet {
    names: HashSet<String>,
}

impl FunctionNameSet {
    fn new() -> Self {
        Self::default()
    }

    fn insert(&mut self, name: impl Into<String>) {
        self.names.insert(name.into());
    }

    fn contains(&self, name: &str) -> bool {
        self.names.contains(name)
    }
}

#[derive(Debug, Clone, Default)]
struct ExternalNameSet {
    names: HashSet<String>,
}

impl ExternalNameSet {
    fn new() -> Self {
        Self::default()
    }

    fn insert(&mut self, name: impl Into<String>) {
        self.names.insert(name.into());
    }

    fn contains(&self, name: &str) -> bool {
        self.names.contains(name)
    }
}

impl<'a> ValidationContext<'a> {
    fn lower_type_annotation(&self, annotation: &AstTypeAnnotation) -> RsigType {
        lower_type_annotation(annotation, &self.declared_variants)
    }

    fn callable_signature(&self, callee: &[String]) -> Option<CallableSignature> {
        CallableResolver::new(
            &self.function_signatures,
            &self.external_signatures,
            self.imports,
        )
        .resolve(callee)
    }

    fn diagnostic(
        &self,
        span: TextSpan,
        message: impl Into<String>,
        label: impl Into<String>,
        help: Option<&'static str>,
    ) -> SourceDiagnostic {
        SourceDiagnostics::new(self.source_path, self.source).at(span, message, label, help)
    }
}

struct ProgramValidator<'a> {
    source_path: &'a Utf8Path,
    source: &'a str,
    imports: &'a ImportedSignatures,
    mode: CheckMode,
    expression_types: Option<&'a ExpressionTypeTable>,
}

impl<'a> ProgramValidator<'a> {
    fn new(
        source_path: &'a Utf8Path,
        source: &'a str,
        imports: &'a ImportedSignatures,
        mode: CheckMode,
        expression_types: Option<&'a ExpressionTypeTable>,
    ) -> Self {
        Self {
            source_path,
            source,
            imports,
            mode,
            expression_types,
        }
    }

    fn validate(&self, program: &AstProgram) -> miette::Result<()> {
        let functions = const_functions(program);
        let mut function_names = FunctionNameSet::new();
        for name in functions.names() {
            function_names.insert(name.clone());
        }
        let type_shapes = TypeShapeCollector::new(self.imports).collect(program);
        let function_signatures = function_signatures(program, &type_shapes.declared_variants);
        let function_results = function_results(program, &type_shapes.declared_variants);
        let declared_external_names = declared_external_names(program);
        let external_signatures = external_signatures(program, &type_shapes.declared_variants);
        let external_names = external_names(program);
        let declared_variants = type_shapes.declared_variants;
        let ctx = ValidationContext {
            source_path: self.source_path,
            source: self.source,
            functions,
            function_signatures,
            function_names,
            function_results,
            declared_external_names,
            external_signatures,
            external_names,
            constructor_types: type_shapes.constructor_types,
            declared_variants,
            record_shapes: type_shapes.record_shapes,
            record_shapes_by_type: type_shapes.record_shapes_by_type,
            imports: self.imports,
            expression_types: self.expression_types,
        };

        let main_decls = program
            .decls
            .iter()
            .filter_map(|decl| match decl {
                AstDecl::Function(function) if function.name == "main" => Some(function),
                _ => None,
            })
            .collect::<Vec<_>>();

        let entrypoint_validator = EntrypointValidator::new(&ctx, self.mode);
        let import_validator = ImportValidator::new(&ctx);
        entrypoint_validator.validate_program(program, &main_decls)?;
        validate_unique_top_level_names(
            &SourceDiagnostics::new(self.source_path, self.source),
            program,
        )?;

        for decl in &program.decls {
            match decl {
                AstDecl::Use(use_) => {
                    import_validator.validate_use(use_)?;
                }
                AstDecl::Module(_) | AstDecl::Include(_) => {}
                AstDecl::External(external) => {
                    if external.type_annotation.text.trim().is_empty() {
                        return Err(ctx
                            .diagnostic(
                                external.type_annotation.span,
                                "empty external type",
                                "external declarations need a source-level type",
                                Some(
                                    "try: external println : String -> unit = \"riot_prim_println\"",
                                ),
                            )
                            .into());
                    }
                    TypeAnnotationChecker::new(&ctx)
                        .validate_source_spelling(&external.type_annotation)?;
                }
                AstDecl::Type(type_) => {
                    if matches!(&type_.body, AstTypeBody::Variant { constructors } if constructors.is_empty())
                    {
                        return Err(ctx
                            .diagnostic(
                                type_.name_span,
                                "empty variant type",
                                "variant types need at least one constructor",
                                Some("try: type color = Red | Green | Blue"),
                            )
                            .into());
                    }
                    validate_type_decl_spelling(&ctx, type_)?;
                }
                AstDecl::Function(function) => {
                    validate_unique_params(
                        &ctx,
                        &function.params,
                        function.name_span,
                        "duplicate function parameter",
                        "this parameter name is already used by this function",
                        "choose a unique name for each function parameter",
                    )?;
                    TypeAnnotationChecker::new(&ctx).validate_function(function)?;
                    entrypoint_validator.validate_function(function)?;
                    BlockValidator::new(&ctx).validate_function_body(
                        &function.body,
                        entrypoint_validator.requires_output_action(function),
                        &function.params,
                        &function.param_types,
                    )?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
struct RecordShape {
    type_name: TypeName,
    type_params: Vec<TypeVarName>,
    fields: Vec<(String, RsigType)>,
}

#[derive(Debug, Clone)]
struct ConstructorShape {
    type_name: TypeName,
    type_params: Vec<TypeVarName>,
    payload: Vec<RsigType>,
}

fn validate_unique_top_level_names(
    diagnostics: &SourceDiagnostics<'_>,
    program: &AstProgram,
) -> miette::Result<()> {
    let mut types = HashMap::<String, TextSpan>::new();
    let mut constructors = HashMap::<String, TextSpan>::new();
    let mut externals = HashMap::<String, TextSpan>::new();
    let mut functions = HashMap::<String, TextSpan>::new();

    for decl in &program.decls {
        match decl {
            AstDecl::Type(type_) => {
                reject_duplicate_name(
                    diagnostics,
                    &mut types,
                    type_.name.clone(),
                    type_.name_span,
                    "duplicate type declaration",
                    "this type name is already declared in this module",
                    "choose a unique type name for each type declaration",
                )?;
                let mut type_params = HashMap::<String, TextSpan>::new();
                for param in &type_.params {
                    reject_duplicate_name(
                        diagnostics,
                        &mut type_params,
                        param.name.clone(),
                        param.span,
                        "duplicate type parameter",
                        "this type parameter name is already used by this type",
                        "choose a unique name for each type parameter",
                    )?;
                }
                match &type_.body {
                    AstTypeBody::Variant {
                        constructors: variant_constructors,
                    } => {
                        for constructor in variant_constructors {
                            reject_duplicate_name(
                                diagnostics,
                                &mut constructors,
                                constructor.name.clone(),
                                constructor.name_span,
                                "duplicate variant constructor",
                                "this constructor name is already declared in this module",
                                "choose a unique constructor name; constructors share a module namespace",
                            )?;
                        }
                    }
                    AstTypeBody::Record { fields } => {
                        let mut field_names = HashMap::<String, TextSpan>::new();
                        for field in fields {
                            reject_duplicate_name(
                                diagnostics,
                                &mut field_names,
                                field.name.clone(),
                                field.type_annotation.span,
                                "duplicate record type field",
                                "this field is already declared on the record type",
                                "remove the duplicate field or choose a unique field name",
                            )?;
                        }
                    }
                    AstTypeBody::Abstract => {}
                }
            }
            AstDecl::External(external) => {
                reject_duplicate_name(
                    diagnostics,
                    &mut externals,
                    external.name.clone(),
                    external.span,
                    "duplicate external declaration",
                    "this external name is already declared in this module",
                    "choose a unique external name",
                )?;
            }
            AstDecl::Function(function) => {
                reject_duplicate_name(
                    diagnostics,
                    &mut functions,
                    function.name.clone(),
                    function.name_span,
                    "duplicate function declaration",
                    "this function name is already declared in this module",
                    "choose a unique function name",
                )?;
            }
            AstDecl::Use(_) | AstDecl::Module(_) | AstDecl::Include(_) => {}
        }
    }

    Ok(())
}

fn reject_duplicate_name(
    diagnostics: &SourceDiagnostics<'_>,
    seen: &mut HashMap<String, TextSpan>,
    name: String,
    span: TextSpan,
    message: &'static str,
    label: &'static str,
    help: &'static str,
) -> miette::Result<()> {
    if seen.insert(name, span).is_some() {
        return Err(diagnostics.at(span, message, label, Some(help)).into());
    }
    Ok(())
}

fn validate_unique_params(
    ctx: &ValidationContext<'_>,
    params: &[String],
    span: TextSpan,
    message: &'static str,
    label: &'static str,
    help: &'static str,
) -> miette::Result<()> {
    let mut seen = HashSet::new();
    for param in params {
        if !seen.insert(param) {
            return Err(ctx.diagnostic(span, message, label, Some(help)).into());
        }
    }
    Ok(())
}

fn validate_type_decl_spelling(
    ctx: &ValidationContext<'_>,
    type_: &crate::ast::AstTypeDecl,
) -> miette::Result<()> {
    match &type_.body {
        AstTypeBody::Abstract => Ok(()),
        AstTypeBody::Variant { constructors } => {
            for constructor in constructors {
                for payload in &constructor.payload {
                    TypeAnnotationChecker::new(ctx).validate_source_spelling(payload)?;
                }
            }
            Ok(())
        }
        AstTypeBody::Record { fields } => {
            for field in fields {
                TypeAnnotationChecker::new(ctx).validate_source_spelling(&field.type_annotation)?;
            }
            Ok(())
        }
    }
}

fn main_requires_output_action(
    ctx: &ValidationContext<'_>,
    function: &crate::ast::AstFnDecl,
) -> bool {
    if function.name != "main" {
        return false;
    }
    function
        .return_type
        .as_ref()
        .map(|annotation| ctx.lower_type_annotation(annotation))
        .is_none_or(|type_| matches!(type_, RsigType::Unit))
}

fn validate_main_function_signature(
    ctx: &ValidationContext<'_>,
    function: &crate::ast::AstFnDecl,
) -> miette::Result<()> {
    if function.name != "main" {
        return Ok(());
    }

    match function.params.as_slice() {
        [] => {}
        [_] => {
            let Some(Some(annotation)) = function.param_types.first() else {
                return Err(ctx
                    .diagnostic(
                        function.name_span,
                        "main argument type is missing",
                        "main arguments must be annotated as `List<String>`",
                        Some("try: fn main(args: List<String>) -> i32 { 0 }"),
                    )
                    .into());
            };
            let actual = ctx.lower_type_annotation(annotation);
            let expected = RsigType::List(Box::new(RsigType::String));
            if actual != expected {
                return Err(ctx
                    .diagnostic(
                        annotation.span,
                        "invalid main argument type",
                        format!(
                            "expected `List<String>`, but found `{}`",
                            actual.canonical()
                        ),
                        Some("try: fn main(args: List<String>) -> i32 { 0 }"),
                    )
                    .into());
            }
        }
        _ => {
            return Err(ctx
                .diagnostic(
                    function.name_span,
                    "main function has too many parameters",
                    "stage0 supports at most one main argument list",
                    Some("try: fn main(args: List<String>) -> i32 { 0 }"),
                )
                .into());
        }
    }

    let Some(annotation) = &function.return_type else {
        return Ok(());
    };
    let result = ctx.lower_type_annotation(annotation);
    if matches!(result, RsigType::Unit | RsigType::I32) || is_main_result_exit_type(&result) {
        return Ok(());
    }

    Err(ctx
        .diagnostic(
            annotation.span,
            "invalid main return type",
            format!(
                "main can return `unit`, `i32`, or `Result<(), i32>`, but this annotation is `{}`",
                result.canonical()
            ),
            Some("use `-> i32` when main should control the process exit code"),
        )
        .into())
}

fn is_main_result_exit_type(type_: &RsigType) -> bool {
    match type_ {
        RsigType::VariantApp { name, args } if name.as_str() == "Result" => {
            matches!(args.as_slice(), [RsigType::Unit, RsigType::I32])
        }
        _ => false,
    }
}

fn function_results(
    program: &AstProgram,
    declared_variants: &BTreeSet<TypeName>,
) -> HashMap<String, RsigType> {
    program
        .decls
        .iter()
        .filter_map(|decl| match decl {
            AstDecl::Function(function) => function.return_type.as_ref().map(|annotation| {
                (
                    function.name.clone(),
                    lower_type_annotation(annotation, declared_variants),
                )
            }),
            _ => None,
        })
        .collect()
}

fn function_signatures(
    program: &AstProgram,
    declared_variants: &BTreeSet<TypeName>,
) -> FunctionTable {
    let mut signatures = FunctionTable::new();
    for decl in &program.decls {
        if let AstDecl::Function(function) = decl {
            let params = function
                .params
                .iter()
                .enumerate()
                .map(|(index, _)| {
                    function
                        .param_types
                        .get(index)
                        .and_then(|annotation| annotation.as_ref())
                        .map(|annotation| lower_type_annotation(annotation, declared_variants))
                        .unwrap_or(RsigType::Unknown)
                })
                .collect();
            let result = function
                .return_type
                .as_ref()
                .map(|annotation| lower_type_annotation(annotation, declared_variants))
                .unwrap_or(RsigType::Unknown);
            signatures.insert(
                function.name.clone(),
                FunctionSignature::new(params, result),
            );
        }
    }
    signatures
}

fn lower_type_annotation(
    annotation: &AstTypeAnnotation,
    declared_variants: &BTreeSet<TypeName>,
) -> RsigType {
    RsigTypeLowerer::new().lower(&annotation.syntax, declared_variants)
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

fn const_functions(program: &AstProgram) -> ConstFunctionTable {
    let mut functions = ConstFunctionTable::new();
    for decl in &program.decls {
        if let AstDecl::Function(function) = decl
            && function.name != "main"
        {
            functions.insert(
                function.name.clone(),
                ConstFunction {
                    params: function.params.clone(),
                    body: function.body.clone(),
                },
            );
        }
    }
    functions
}

fn external_names(program: &AstProgram) -> ExternalNameSet {
    let mut names = ExternalNameSet::new();
    for name in crate::stdlib::Stdlib::new()
        .prelude_signature()
        .expect("compiler/std/prelude.ml must parse")
        .exports
        .iter()
        .map(|export| export.name().to_owned())
    {
        names.insert(name);
    }
    for decl in &program.decls {
        if let AstDecl::External(external) = decl {
            names.insert(external.name.clone());
        }
    }
    names
}

fn external_signatures(
    program: &AstProgram,
    declared_variants: &BTreeSet<TypeName>,
) -> ExternalTable {
    let mut signatures = prelude_external_signatures();
    for decl in &program.decls {
        if let AstDecl::External(external) = decl {
            let (params, result) = RsigTypeLowerer::new()
                .lower_signature(&external.type_annotation.syntax, declared_variants);
            signatures.insert(
                external.name.clone(),
                ExternalSignature::new(params, result, external.abi.clone()),
            );
        }
    }
    signatures
}

fn declared_external_names(program: &AstProgram) -> ExternalNameSet {
    let mut names = ExternalNameSet::new();
    for decl in &program.decls {
        if let AstDecl::External(external) = decl {
            names.insert(external.name.clone());
        }
    }
    names
}

fn first_decl_span(program: &AstProgram) -> Option<TextSpan> {
    program.decls.first().map(|decl| match decl {
        AstDecl::Use(use_) => use_.span,
        AstDecl::Module(module) => module.span,
        AstDecl::Include(include) => include.span,
        AstDecl::External(external) => external.span,
        AstDecl::Type(type_) => type_.span,
        AstDecl::Function(function) => function.span,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SpanExprKind {
    Call,
    If,
    List,
    Match,
}

fn span_is_call_expression(program: &AstProgram, span: TextSpan) -> bool {
    span_is_expr_kind(program, span, SpanExprKind::Call)
}

fn span_is_if_expression(program: &AstProgram, span: TextSpan) -> bool {
    span_is_expr_kind(program, span, SpanExprKind::If)
}

fn span_is_list_expression(program: &AstProgram, span: TextSpan) -> bool {
    span_is_expr_kind(program, span, SpanExprKind::List)
}

fn span_is_match_expression(program: &AstProgram, span: TextSpan) -> bool {
    span_is_expr_kind(program, span, SpanExprKind::Match)
}

fn span_is_expr_kind(program: &AstProgram, span: TextSpan, kind: SpanExprKind) -> bool {
    program.decls.iter().any(|decl| match decl {
        AstDecl::Function(function) => block_contains_expr_kind(&function.body, span, kind),
        AstDecl::Use(_)
        | AstDecl::Module(_)
        | AstDecl::Include(_)
        | AstDecl::External(_)
        | AstDecl::Type(_) => false,
    })
}

fn block_contains_expr_kind(block: &AstBlock, span: TextSpan, kind: SpanExprKind) -> bool {
    block.statements.iter().any(|stmt| match stmt {
        AstStmt::Let { value, .. } | AstStmt::Expr(value) => {
            expr_contains_expr_kind(value, span, kind)
        }
    }) || block
        .tail
        .as_ref()
        .is_some_and(|tail| expr_contains_expr_kind(tail, span, kind))
}

fn expr_contains_expr_kind(expr: &AstExpr, span: TextSpan, kind: SpanExprKind) -> bool {
    if expr_span(expr) == span {
        let matches_kind = match kind {
            SpanExprKind::Call => matches!(expr, AstExpr::Call { .. } | AstExpr::Apply { .. }),
            SpanExprKind::If => matches!(expr, AstExpr::If { .. }),
            SpanExprKind::List => matches!(expr, AstExpr::List { .. }),
            SpanExprKind::Match => matches!(expr, AstExpr::Match { .. }),
        };
        if matches_kind {
            return true;
        }
    }

    match expr {
        AstExpr::Add { lhs, rhs, .. }
        | AstExpr::Sub { lhs, rhs, .. }
        | AstExpr::Mul { lhs, rhs, .. }
        | AstExpr::Div { lhs, rhs, .. }
        | AstExpr::Mod { lhs, rhs, .. }
        | AstExpr::Eq { lhs, rhs, .. }
        | AstExpr::Lt { lhs, rhs, .. }
        | AstExpr::And { lhs, rhs, .. }
        | AstExpr::Or { lhs, rhs, .. } => {
            expr_contains_expr_kind(lhs, span, kind) || expr_contains_expr_kind(rhs, span, kind)
        }
        AstExpr::Neg { expr, .. } | AstExpr::Not { expr, .. } => {
            expr_contains_expr_kind(expr, span, kind)
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            expr_contains_expr_kind(condition, span, kind)
                || expr_contains_expr_kind(then_branch, span, kind)
                || expr_contains_expr_kind(else_branch, span, kind)
        }
        AstExpr::Match {
            scrutinee, arms, ..
        } => {
            expr_contains_expr_kind(scrutinee, span, kind)
                || arms
                    .iter()
                    .any(|arm| expr_contains_expr_kind(&arm.body, span, kind))
        }
        AstExpr::Block { block, .. }
        | AstExpr::Lambda { body: block, .. }
        | AstExpr::Spawn { body: block, .. } => block_contains_expr_kind(block, span, kind),
        AstExpr::Receive { arms, .. } => arms
            .iter()
            .any(|arm| expr_contains_expr_kind(&arm.body, span, kind)),
        AstExpr::Call { args, .. }
        | AstExpr::Tuple { items: args, .. }
        | AstExpr::List { items: args, .. } => args
            .iter()
            .any(|arg| expr_contains_expr_kind(arg, span, kind)),
        AstExpr::Apply { callee, args, .. } => {
            expr_contains_expr_kind(callee, span, kind)
                || args
                    .iter()
                    .any(|arg| expr_contains_expr_kind(arg, span, kind))
        }
        AstExpr::Record { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_contains_expr_kind(value, span, kind)),
        AstExpr::Field { base, .. } | AstExpr::TupleIndex { base, .. } => {
            expr_contains_expr_kind(base, span, kind)
        }
        AstExpr::Bool { .. }
        | AstExpr::Unit { .. }
        | AstExpr::Char { .. }
        | AstExpr::Float { .. }
        | AstExpr::Int { .. }
        | AstExpr::Path { .. }
        | AstExpr::String { .. } => false,
    }
}

fn constructor_types(
    program: &AstProgram,
    declared_variants: &BTreeSet<TypeName>,
) -> HashMap<String, ConstructorShape> {
    let mut constructors = crate::stdlib::Stdlib::new()
        .prelude_signature()
        .ok()
        .map(|rsig| constructor_types_from_rsig(&rsig, None))
        .unwrap_or_default();
    constructors.extend(
        program
            .decls
            .iter()
            .filter_map(|decl| match decl {
                AstDecl::Type(type_) => Some(type_),
                _ => None,
            })
            .flat_map(|type_| {
                let type_name = TypeName::new(type_.name.clone());
                let AstTypeBody::Variant { constructors } = &type_.body else {
                    return Vec::new();
                };
                constructors
                    .iter()
                    .map(move |constructor| {
                        (
                            constructor.name.clone(),
                            ConstructorShape {
                                type_name: type_name.clone(),
                                type_params: type_
                                    .params
                                    .iter()
                                    .map(|param| TypeVarName::new(param.name.clone()))
                                    .collect(),
                                payload: constructor
                                    .payload
                                    .iter()
                                    .map(|payload| {
                                        lower_type_annotation(payload, declared_variants)
                                    })
                                    .collect(),
                            },
                        )
                    })
                    .collect::<Vec<_>>()
            }),
    );
    constructors
}

fn constructor_types_from_rsig(
    rsig: &Rsig,
    module: Option<&str>,
) -> HashMap<String, ConstructorShape> {
    rsig.types
        .iter()
        .flat_map(|type_| {
            let RsigTypeDeclKind::Variant { constructors } = &type_.body else {
                return Vec::new();
            };
            let type_name = module
                .map(|module| TypeName::new(format!("{module}.{}", type_.name.as_str())))
                .unwrap_or_else(|| type_.name.clone());
            constructors
                .iter()
                .map(move |constructor| {
                    (
                        constructor.name.as_str().to_owned(),
                        ConstructorShape {
                            type_name: type_name.clone(),
                            type_params: type_
                                .params
                                .iter()
                                .map(|param| TypeVarName::new(param.as_str()))
                                .collect(),
                            payload: constructor.payload.clone(),
                        },
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn record_shapes(
    program: &AstProgram,
    imports: &ImportedSignatures,
    declared_variants: &BTreeSet<TypeName>,
) -> (
    HashMap<String, RecordShape>,
    BTreeMap<TypeName, RecordShape>,
) {
    let mut shapes = HashMap::new();
    let mut shapes_by_type = BTreeMap::new();
    for decl in &program.decls {
        match decl {
            AstDecl::Type(type_) => {
                let AstTypeBody::Record { fields } = &type_.body else {
                    continue;
                };
                let type_name = TypeName::new(type_.name.clone());
                let shape = RecordShape {
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
                insert_record_shape_aliases(&mut shapes, None, &type_name, shape.clone());
                shapes_by_type.insert(type_name, shape);
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
                    let shape = RecordShape {
                        type_name: type_name.clone(),
                        type_params: type_
                            .params
                            .iter()
                            .map(|param| TypeVarName::new(param.as_str()))
                            .collect(),
                        fields: fields
                            .iter()
                            .map(|field| (field.name.as_str().to_owned(), field.type_.clone()))
                            .collect(),
                    };
                    insert_record_shape_aliases(
                        &mut shapes,
                        Some(use_.name.as_str()),
                        &type_.name,
                        shape.clone(),
                    );
                    shapes_by_type.insert(type_name, shape);
                }
            }
            _ => {}
        }
    }
    (shapes, shapes_by_type)
}

fn insert_record_shape_aliases(
    shapes: &mut HashMap<String, RecordShape>,
    module: Option<&str>,
    source_name: &TypeName,
    shape: RecordShape,
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

fn validate_block(
    ctx: &ValidationContext<'_>,
    block: &AstBlock,
    is_main: bool,
    params: &[String],
    param_types: &[Option<AstTypeAnnotation>],
) -> miette::Result<()> {
    let mut bindings = params
        .iter()
        .enumerate()
        .map(|(index, param)| {
            let type_ = param_types
                .get(index)
                .and_then(|annotation| annotation.as_ref())
                .map(|annotation| ctx.lower_type_annotation(annotation));
            (param.clone(), BindingKind::Value(type_))
        })
        .collect::<HashMap<_, _>>();
    let mut output_actions = 0_usize;
    let mut actor_actions = 0_usize;

    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
            } => {
                if let Some(annotation) = type_annotation {
                    TypeAnnotationChecker::new(ctx).validate_binding(annotation, value)?;
                }
                let kind = match value {
                    AstExpr::Spawn { body, span: _ } => {
                        validate_actor_block(ctx, body, &bindings)?;
                        actor_actions += 1;
                        BindingKind::Actor
                    }
                    _ => {
                        validate_expr(ctx, value, &bindings, false)?;
                        let type_ = type_annotation
                            .as_ref()
                            .map(|annotation| ctx.lower_type_annotation(annotation))
                            .or_else(|| simple_expr_type(ctx, value, &bindings));
                        BindingKind::Value(type_)
                    }
                };
                bindings.insert(name.clone(), kind);
            }
            AstStmt::Expr(expr) => {
                let category = validate_expr(ctx, expr, &bindings, false)?;
                match category {
                    ExprCategory::Output => output_actions += 1,
                    ExprCategory::Actor => actor_actions += 1,
                    ExprCategory::Other => {}
                }
            }
        }
    }

    if let Some(tail) = &block.tail {
        let category = validate_expr(ctx, tail, &bindings, false)?;
        match category {
            ExprCategory::Output => output_actions += 1,
            ExprCategory::Actor => actor_actions += 1,
            ExprCategory::Other => {}
        }
    }

    if is_main && output_actions == 0 && actor_actions == 0 {
        return Err(ctx
            .diagnostic(
                block.span,
                "unsupported main body",
                "stage0 expects main to produce output or actor actions",
                Some("try: fn main() { dbg(\"hello world\") }"),
            )
            .into());
    }

    Ok(())
}

fn validate_scoped_block(
    ctx: &ValidationContext<'_>,
    block: &AstBlock,
    outer_bindings: &HashMap<String, BindingKind>,
    params: &[String],
    param_types: &[Option<AstTypeAnnotation>],
    in_actor: bool,
) -> miette::Result<()> {
    let mut bindings = outer_bindings.clone();
    for (index, param) in params.iter().enumerate() {
        let type_ = param_types
            .get(index)
            .and_then(|annotation| annotation.as_ref())
            .map(|annotation| ctx.lower_type_annotation(annotation));
        bindings.insert(param.clone(), BindingKind::Value(type_));
    }

    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                if let Some(annotation) = type_annotation {
                    TypeAnnotationChecker::new(ctx).validate_binding(annotation, value)?;
                }
                let kind = match value {
                    AstExpr::Spawn { body, .. } => {
                        validate_actor_block(ctx, body, &bindings)?;
                        BindingKind::Actor
                    }
                    _ => {
                        validate_expr(ctx, value, &bindings, in_actor)?;
                        let type_ = type_annotation
                            .as_ref()
                            .map(|annotation| ctx.lower_type_annotation(annotation))
                            .or_else(|| simple_expr_type(ctx, value, &bindings));
                        BindingKind::Value(type_)
                    }
                };
                bindings.insert(name.clone(), kind);
            }
            AstStmt::Expr(expr) => {
                validate_expr(ctx, expr, &bindings, in_actor)?;
            }
        }
    }

    if let Some(tail) = &block.tail {
        validate_expr(ctx, tail, &bindings, in_actor)?;
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprCategory {
    Output,
    Actor,
    Other,
}

fn validate_expr(
    ctx: &ValidationContext<'_>,
    expr: &AstExpr,
    bindings: &HashMap<String, BindingKind>,
    in_actor: bool,
) -> miette::Result<ExprCategory> {
    match expr {
        AstExpr::Call { callee, args, span } => {
            if let [module, name] = callee.segments.as_slice() {
                if let Some(shape) = imported_constructor_shape(ctx, module, name) {
                    validate_constructor_call(ctx, *span, name, &shape, args, bindings, in_actor)?;
                    return Ok(ExprCategory::Other);
                }
            }

            if let Some(signature) = ctx.callable_signature(&callee.segments) {
                return validate_callable_call(
                    ctx,
                    *span,
                    &callee.segments,
                    &signature,
                    args,
                    bindings,
                    in_actor,
                );
            }

            if let [module, name] = callee.segments.as_slice() {
                let Some(rsig) = ctx.imports.get(module.as_str()) else {
                    return Err(ctx
                        .diagnostic(
                            *span,
                            "unknown imported module",
                            format!(
                                "`{module}` has not been brought into scope with `use {module}`"
                            ),
                            Some("add a top-level `use` declaration and pass --sig-dir"),
                        )
                        .into());
                };
                if rsig.find(name).is_none() {
                    return Err(ctx
                        .diagnostic(
                            *span,
                            "unknown imported value",
                            format!("module `{module}` does not export `{name}`"),
                            Some("check the producer .rsig"),
                        )
                        .into());
                }
            }

            let [name] = callee.segments.as_slice() else {
                if let [module, value, ..] = callee.segments.as_slice() {
                    return Err(ctx
                        .diagnostic(
                            *span,
                            "unsupported nested path call",
                            format!(
                                "stage0 can call `{module}.{value}(...)`, but this callee has more path segments"
                            ),
                            Some("call a two-segment imported function or bind the intermediate value first"),
                        )
                        .into());
                }
                return Err(ctx
                    .diagnostic(
                        *span,
                        "unsupported path call",
                        "stage0 only supports local calls and qualified module calls",
                        Some("try: Module.value(...)"),
                    )
                    .into());
            };

            match name.as_str() {
                _ if ctx.constructor_types.contains_key(name) => {
                    let shape = ctx
                        .constructor_types
                        .get(name)
                        .expect("constructor existence checked");
                    validate_constructor_call(ctx, *span, name, shape, args, bindings, in_actor)?;
                    Ok(ExprCategory::Other)
                }
                _ if matches!(bindings.get(name), Some(BindingKind::Value(_))) => {
                    for arg in args {
                        validate_expr(ctx, arg, bindings, in_actor)?;
                    }
                    Ok(ExprCategory::Other)
                }
                _ => Err(ctx
                    .diagnostic(
                        *span,
                        "unsupported function call",
                        format!("stage0 does not know `{name}`"),
                        Some("try: dbg(\"hello world\")"),
                    )
                    .into()),
            }
        }
        AstExpr::Apply { callee, args, .. } => {
            validate_expr(ctx, callee, bindings, in_actor)?;
            for arg in args {
                validate_expr(ctx, arg, bindings, in_actor)?;
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Lambda {
            params,
            param_types,
            body,
            span,
        } => {
            validate_unique_params(
                ctx,
                params,
                *span,
                "duplicate lambda parameter",
                "this parameter name is already used by this lambda",
                "choose a unique name for each lambda parameter",
            )?;
            BlockValidator::new(ctx).validate_scoped(
                body,
                bindings,
                params,
                param_types,
                in_actor,
            )?;
            Ok(ExprCategory::Other)
        }
        AstExpr::Block { block, .. } => validate_block_expr(ctx, block, bindings, in_actor),
        AstExpr::Spawn { body, span: _ } => {
            validate_actor_block(ctx, body, bindings)?;
            Ok(ExprCategory::Actor)
        }
        AstExpr::Receive { span, .. } if !in_actor => Err(ctx
            .diagnostic(
                *span,
                "receive outside spawn",
                "`receive` is only valid inside an actor body",
                Some("wrap the receive in `spawn { ... }`"),
            )
            .into()),
        AstExpr::Receive { arms, .. } => {
            validate_receive_arms(ctx, arms, bindings)?;
            Ok(ExprCategory::Actor)
        }
        AstExpr::Eq { lhs, rhs, span } => {
            validate_expr(ctx, lhs, bindings, in_actor)?;
            validate_expr(ctx, rhs, bindings, in_actor)?;
            let lhs_type = simple_expr_type(ctx, lhs, bindings);
            let rhs_type = simple_expr_type(ctx, rhs, bindings);
            if let (Some(lhs_type), Some(rhs_type)) = (lhs_type, rhs_type)
                && !type_matches(&lhs_type, &rhs_type)
            {
                return Err(ctx
                    .diagnostic(
                        *span,
                        "equality operands have different types",
                        format!(
                            "left side has type `{}`, but right side has type `{}`",
                            lhs_type.canonical(),
                            rhs_type.canonical()
                        ),
                        Some("compare values with the same type"),
                    )
                    .into());
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Lt { lhs, rhs, span } => {
            validate_expr(ctx, lhs, bindings, in_actor)?;
            validate_expr(ctx, rhs, bindings, in_actor)?;
            let lhs_type = simple_expr_type(ctx, lhs, bindings);
            let rhs_type = simple_expr_type(ctx, rhs, bindings);
            if let (Some(lhs_type), Some(rhs_type)) = (lhs_type, rhs_type) {
                let comparable = matches!(lhs_type, RsigType::I64 | RsigType::String)
                    && matches!(rhs_type, RsigType::I64 | RsigType::String)
                    && type_matches(&lhs_type, &rhs_type);
                if !comparable {
                    return Err(ctx.diagnostic(*span,
                        "ordering operands are not comparable",
                        format!(
                            "left side has type `{}`, but right side has type `{}`",
                            lhs_type.canonical(),
                            rhs_type.canonical()
                        ),
                        Some("compare i64 values with i64 values or string values with string values"),
                    )
                    .into());
                }
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Add { lhs, rhs, .. }
        | AstExpr::Sub { lhs, rhs, .. }
        | AstExpr::Mul { lhs, rhs, .. }
        | AstExpr::Div { lhs, rhs, .. }
        | AstExpr::Mod { lhs, rhs, .. }
        | AstExpr::And { lhs, rhs, .. }
        | AstExpr::Or { lhs, rhs, .. } => {
            validate_expr(ctx, lhs, bindings, in_actor)?;
            validate_expr(ctx, rhs, bindings, in_actor)?;
            Ok(ExprCategory::Other)
        }
        AstExpr::Neg { expr, .. } | AstExpr::Not { expr, .. } => {
            validate_expr(ctx, expr, bindings, in_actor)?;
            Ok(ExprCategory::Other)
        }
        AstExpr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            validate_expr(ctx, condition, bindings, in_actor)?;
            validate_expr(ctx, then_branch, bindings, in_actor)?;
            validate_expr(ctx, else_branch, bindings, in_actor)?;
            Ok(ExprCategory::Other)
        }
        AstExpr::Match {
            scrutinee,
            arms,
            span,
        } => {
            validate_expr(ctx, scrutinee, bindings, in_actor)?;
            if arms.is_empty() {
                return Err(ctx
                    .diagnostic(
                        *span,
                        "empty match expression",
                        "match expressions need at least one arm",
                        Some("add a wildcard arm like `_ -> value`"),
                    )
                    .into());
            };
            let scrutinee_type = simple_expr_type(ctx, scrutinee, bindings);
            let pattern_validator = PatternValidator::new(ctx);
            if !pattern_validator.is_match_exhaustive(arms, scrutinee_type.as_ref()) {
                return Err(ctx
                    .diagnostic(
                        *span,
                        "non-exhaustive match expression",
                        non_exhaustive_match_message(ctx, arms, scrutinee_type.as_ref()),
                        Some("add the missing constructors, list cases, or a final `_ -> ...` arm"),
                    )
                    .into());
            }
            for arm in arms {
                pattern_validator.validate(&arm.pattern, scrutinee_type.as_ref())?;
                let mut arm_bindings = bindings.clone();
                pattern_validator.bind(&arm.pattern, scrutinee_type.clone(), &mut arm_bindings);
                validate_expr(ctx, &arm.body, &arm_bindings, in_actor)?;
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Tuple { items, .. } | AstExpr::List { items, .. } => {
            for item in items {
                validate_expr(ctx, item, bindings, in_actor)?;
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Record { path, fields, span } => {
            for (_, value) in fields {
                validate_expr(ctx, value, bindings, in_actor)?;
            }
            validate_record_literal_shape(ctx, path, fields, *span, bindings)?;
            Ok(ExprCategory::Other)
        }
        AstExpr::Field { base, field, span } => {
            validate_expr(ctx, base, bindings, in_actor)?;
            validate_declared_record_field(ctx, base, field, *span, bindings)?;
            Ok(ExprCategory::Other)
        }
        AstExpr::TupleIndex { base, index, span } => {
            validate_expr(ctx, base, bindings, in_actor)?;
            if let AstExpr::Tuple { items, .. } = base.as_ref()
                && *index >= items.len()
            {
                return Err(ctx
                    .diagnostic(
                        *span,
                        "tuple projection is out of bounds",
                        format!(
                            "tuple has {} item(s), but projection requested index {index}",
                            items.len()
                        ),
                        Some("use a tuple projection index that exists"),
                    )
                    .into());
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Path { path, span } => {
            match path.segments.as_slice() {
                [name]
                    if bindings.contains_key(name)
                        || ctx.function_names.contains(name)
                        || ctx.external_names.contains(name)
                        || ctx
                            .constructor_types
                            .get(name)
                            .is_some_and(|constructor| constructor.payload.is_empty()) => {}
                [module, name] => {
                    let Some(rsig) = ctx.imports.get(module.as_str()) else {
                        return Err(ctx
                            .diagnostic(
                                *span,
                                "unknown imported module",
                                format!(
                                    "`{module}` has not been brought into scope with `use {module}`"
                                ),
                                Some("add a top-level `use` declaration and pass --sig-dir"),
                            )
                            .into());
                    };
                    let Some(export) = rsig.find(name) else {
                        if imported_constructor_shape(ctx, module, name)
                            .is_some_and(|constructor| constructor.payload.is_empty())
                        {
                            return Ok(ExprCategory::Other);
                        }
                        return Err(ctx
                            .diagnostic(
                                *span,
                                "unknown imported value",
                                format!("module `{module}` does not export `{name}`"),
                                Some("check the producer .rsig"),
                            )
                            .into());
                    };
                    if export_has_unknown_abi(export) {
                        return Err(ctx.diagnostic(*span,
                            "imported value has unknown ABI",
                            format!(
                                "module `{module}` exports `{name}`, but its .rsig type is not concrete enough to use as a value"
                            ),
                            Some("add enough type information for the exported function"),
                        )
                        .into());
                    }
                }
                [module, value, ..] => {
                    return Err(ctx
                        .diagnostic(
                            *span,
                            "unsupported nested path",
                            format!(
                                "stage0 can resolve `{module}.{value}`, but this path has more segments"
                            ),
                            Some("use a two-segment imported path or bind the intermediate value first"),
                        )
                        .into());
                }
                [head, ..] => {
                    return Err(ctx
                        .diagnostic(
                            *span,
                            "unknown value",
                            format!("`{head}` is not bound in this scope"),
                            Some("bind the value with `let` before using it"),
                        )
                        .into());
                }
                [] => {}
            }
            Ok(ExprCategory::Other)
        }
        AstExpr::Bool { .. }
        | AstExpr::Char { .. }
        | AstExpr::Unit { .. }
        | AstExpr::Float { .. }
        | AstExpr::Int { .. }
        | AstExpr::String { .. } => Ok(ExprCategory::Other),
    }
}

fn validate_block_expr(
    ctx: &ValidationContext<'_>,
    block: &AstBlock,
    outer_bindings: &HashMap<String, BindingKind>,
    in_actor: bool,
) -> miette::Result<ExprCategory> {
    let mut bindings = outer_bindings.clone();
    let mut output_actions = 0_usize;
    let mut actor_actions = 0_usize;

    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                if let Some(annotation) = type_annotation {
                    TypeAnnotationChecker::new(ctx).validate_binding(annotation, value)?;
                }
                let kind = match value {
                    AstExpr::Spawn { body, .. } => {
                        validate_actor_block(ctx, body, &bindings)?;
                        actor_actions += 1;
                        BindingKind::Actor
                    }
                    _ => {
                        let category = validate_expr(ctx, value, &bindings, in_actor)?;
                        match category {
                            ExprCategory::Output => output_actions += 1,
                            ExprCategory::Actor => actor_actions += 1,
                            ExprCategory::Other => {}
                        }
                        let type_ = type_annotation
                            .as_ref()
                            .map(|annotation| ctx.lower_type_annotation(annotation))
                            .or_else(|| simple_expr_type(ctx, value, &bindings));
                        BindingKind::Value(type_)
                    }
                };
                bindings.insert(name.clone(), kind);
            }
            AstStmt::Expr(expr) => {
                let category = validate_expr(ctx, expr, &bindings, in_actor)?;
                match category {
                    ExprCategory::Output => output_actions += 1,
                    ExprCategory::Actor => actor_actions += 1,
                    ExprCategory::Other => {}
                }
            }
        }
    }

    if let Some(tail) = &block.tail {
        let category = validate_expr(ctx, tail, &bindings, in_actor)?;
        match category {
            ExprCategory::Output => output_actions += 1,
            ExprCategory::Actor => actor_actions += 1,
            ExprCategory::Other => {}
        }
    }

    if output_actions > 0 {
        Ok(ExprCategory::Output)
    } else if actor_actions > 0 {
        Ok(ExprCategory::Actor)
    } else {
        Ok(ExprCategory::Other)
    }
}

fn pattern_is_irrefutable(pattern: &AstPattern) -> bool {
    match pattern {
        AstPattern::Wildcard { .. } | AstPattern::Bind { .. } => true,
        AstPattern::Tuple { items, .. } => items.iter().all(pattern_is_irrefutable),
        AstPattern::Record { fields, .. } => fields
            .iter()
            .all(|(_, field_pattern)| pattern_is_irrefutable(field_pattern)),
        AstPattern::List { prefix, tail, .. } => {
            prefix.is_empty() && tail.as_deref().is_some_and(pattern_is_irrefutable)
        }
        AstPattern::Unit { .. }
        | AstPattern::Bool { .. }
        | AstPattern::Int { .. }
        | AstPattern::String { .. }
        | AstPattern::Constructor { .. } => false,
    }
}

fn non_exhaustive_match_message(
    ctx: &ValidationContext<'_>,
    arms: &[crate::ast::AstMatchArm],
    scrutinee_type: Option<&RsigType>,
) -> String {
    if let Some(RsigType::List(_)) = scrutinee_type {
        let missing = missing_list_cases(arms);
        if !missing.is_empty() {
            return format!("the match arms are missing list case(s): {}", missing.join(", "));
        }
    }
    if let Some(type_) = scrutinee_type {
        let missing = missing_variant_constructors(ctx, arms, type_);
        if !missing.is_empty() {
            return format!(
                "the match arms are missing constructor(s): {}",
                missing.join(", ")
            );
        }
    }
    "the match arms do not cover every value of the scrutinee type".to_owned()
}

fn missing_list_cases(arms: &[crate::ast::AstMatchArm]) -> Vec<String> {
    let mut covers_empty = false;
    let mut covers_non_empty = false;
    for arm in arms {
        let AstPattern::List { prefix, tail, .. } = &arm.pattern else {
            continue;
        };
        if prefix.is_empty() && tail.is_none() {
            covers_empty = true;
        }
        if prefix.is_empty() && tail.as_deref().is_some_and(pattern_is_irrefutable) {
            covers_empty = true;
            covers_non_empty = true;
        }
        if prefix.len() == 1 && tail.as_deref().is_some_and(pattern_is_irrefutable) {
            covers_non_empty = true;
        }
    }
    let mut missing = Vec::new();
    if !covers_empty {
        missing.push("[]".to_owned());
    }
    if !covers_non_empty {
        missing.push("[head, ..tail]".to_owned());
    }
    missing
}

fn missing_variant_constructors(
    ctx: &ValidationContext<'_>,
    arms: &[crate::ast::AstMatchArm],
    scrutinee_type: &RsigType,
) -> Vec<String> {
    let Some(type_name) = variant_type_name(scrutinee_type) else {
        return Vec::new();
    };
    let expected = expected_constructor_names(ctx, type_name);
    let covered = arms
        .iter()
        .filter_map(|arm| covered_constructor_name(ctx, &arm.pattern, type_name))
        .collect::<BTreeSet<_>>();
    expected
        .difference(&covered)
        .map(|name| qualify_constructor_for_message(type_name, name))
        .collect()
}

fn qualify_constructor_for_message(type_name: &TypeName, constructor: &str) -> String {
    type_name
        .as_str()
        .split_once('.')
        .map(|(module, _)| format!("{module}.{constructor}"))
        .unwrap_or_else(|| constructor.to_owned())
}

fn match_is_exhaustive(
    ctx: &ValidationContext<'_>,
    arms: &[crate::ast::AstMatchArm],
    scrutinee_type: Option<&RsigType>,
) -> bool {
    if arms.iter().any(|arm| pattern_is_irrefutable(&arm.pattern)) {
        return true;
    }
    match scrutinee_type {
        Some(RsigType::List(_)) => list_patterns_are_exhaustive(arms),
        Some(type_) => variant_patterns_are_exhaustive(ctx, arms, type_),
        None => ctx.expression_types.is_none(),
    }
}

fn list_patterns_are_exhaustive(arms: &[crate::ast::AstMatchArm]) -> bool {
    let mut covers_empty = false;
    let mut covers_non_empty = false;
    for arm in arms {
        let AstPattern::List { prefix, tail, .. } = &arm.pattern else {
            continue;
        };
        if prefix.is_empty() && tail.is_none() {
            covers_empty = true;
        }
        if prefix.is_empty() && tail.as_deref().is_some_and(pattern_is_irrefutable) {
            return true;
        }
        if prefix.len() == 1 && tail.as_deref().is_some_and(pattern_is_irrefutable) {
            covers_non_empty = true;
        }
    }
    covers_empty && covers_non_empty
}

fn variant_patterns_are_exhaustive(
    ctx: &ValidationContext<'_>,
    arms: &[crate::ast::AstMatchArm],
    scrutinee_type: &RsigType,
) -> bool {
    let Some(type_name) = variant_type_name(scrutinee_type) else {
        return false;
    };
    let expected = expected_constructor_names(ctx, type_name);
    if expected.is_empty() {
        return false;
    }
    let covered = arms
        .iter()
        .filter_map(|arm| covered_constructor_name(ctx, &arm.pattern, type_name))
        .collect::<BTreeSet<_>>();
    expected.is_subset(&covered)
}

fn expected_constructor_names(
    ctx: &ValidationContext<'_>,
    type_name: &TypeName,
) -> BTreeSet<String> {
    if let Some((module, imported_type)) = type_name.as_str().split_once('.')
        && let Some(rsig) = ctx.imports.get(module)
        && let Some(type_) = rsig
            .types
            .iter()
            .find(|type_| type_.name.as_str() == imported_type)
        && let RsigTypeDeclKind::Variant { constructors } = &type_.body
    {
        return constructors
            .iter()
            .map(|constructor| constructor.name.as_str().to_owned())
            .collect();
    }

    ctx.constructor_types
        .iter()
        .filter_map(|(name, shape)| {
            (shape.type_name == *type_name).then_some(name.as_str().to_owned())
        })
        .collect()
}

fn variant_type_name(type_: &RsigType) -> Option<&TypeName> {
    match type_ {
        RsigType::Variant(name) | RsigType::VariantApp { name, .. } => Some(name),
        _ => None,
    }
}

fn covered_constructor_name(
    ctx: &ValidationContext<'_>,
    pattern: &AstPattern,
    type_name: &TypeName,
) -> Option<String> {
    let AstPattern::Constructor { path, payload, .. } = pattern else {
        return None;
    };
    if !payload.iter().all(pattern_is_irrefutable) {
        return None;
    }
    let shape = pattern_constructor_shape(ctx, path)?;
    if shape.type_name != *type_name {
        return None;
    }
    path.segments.last().cloned()
}

fn validate_pattern(
    ctx: &ValidationContext<'_>,
    pattern: &AstPattern,
    scrutinee_type: Option<&RsigType>,
) -> miette::Result<()> {
    validate_unique_pattern_bindings(ctx, pattern)?;
    if let AstPattern::Constructor {
        path,
        payload,
        span,
    } = pattern
    {
        let Some(constructor) = pattern_constructor_shape(ctx, path) else {
            if path.segments.len() > 2 {
                return Err(ctx
                    .diagnostic(
                        *span,
                        "unsupported nested constructor pattern",
                        format!(
                            "stage0 can resolve two-segment imported constructors, but `{}` has more segments",
                            path.segments.join(".")
                        ),
                        Some("use a local constructor or a two-segment imported constructor path"),
                    )
                    .into());
            }
            return Err(ctx
                .diagnostic(
                    *span,
                    "unknown variant constructor",
                    format!("`{}` is not a known constructor", path.segments.join(".")),
                    Some("declare the constructor with a top-level `type` declaration"),
                )
                .into());
        };
        if payload.len() != constructor.payload.len() {
            return Err(ctx
                .diagnostic(
                    *span,
                    "variant payload pattern arity mismatch",
                    format!(
                        "`{}` carries {} payload value(s), but this pattern has {}",
                        path.segments.join("."),
                        constructor.payload.len(),
                        payload.len()
                    ),
                    Some("match the constructor with the same number of payload patterns"),
                )
                .into());
        }
        let payload_types = instantiate_constructor_payload(&constructor, scrutinee_type);
        for (payload_pattern, payload_type) in payload.iter().zip(&payload_types) {
            validate_pattern(ctx, payload_pattern, Some(payload_type))?;
        }
    }
    if let AstPattern::Tuple { items, span } = pattern
        && let Some(RsigType::Tuple(types)) = scrutinee_type
    {
        if items.len() != types.len() {
            return Err(ctx
                .diagnostic(
                    *span,
                    "tuple pattern arity mismatch",
                    format!(
                        "matched tuple has {} item(s), but this pattern has {}",
                        types.len(),
                        items.len()
                    ),
                    Some("match the tuple with the same number of item patterns"),
                )
                .into());
        }
        for (item, type_) in items.iter().zip(types) {
            validate_pattern(ctx, item, Some(type_))?;
        }
    } else if let AstPattern::Tuple { items, .. } = pattern {
        for item in items {
            validate_simple_payload_pattern(
                ctx,
                item,
                &RsigType::Unknown,
                "tuple item",
                "use a binder or `_` for tuple items in this slice",
            )?;
        }
    }
    if let AstPattern::List { prefix, tail, span } = pattern {
        let item_type = match scrutinee_type {
            Some(RsigType::List(item)) => item.as_ref().clone(),
            Some(other) => {
                return Err(ctx
                    .diagnostic(
                        *span,
                        "match pattern has incompatible type",
                        format!(
                            "list pattern expects a list, but the matched value has type `{}`",
                            other.canonical()
                        ),
                        Some("match lists with `[]` or `[head, ..tail]`"),
                    )
                    .into());
            }
            None => RsigType::Unknown,
        };
        for item in prefix {
            validate_pattern(ctx, item, Some(&item_type))?;
        }
        if let Some(tail) = tail {
            validate_pattern(
                ctx,
                tail,
                Some(&RsigType::List(Box::new(item_type.clone()))),
            )?;
        }
    }
    if let AstPattern::Record { path, fields, span } = pattern {
        let Some(shape) = record_shape_for_pattern(ctx, path) else {
            if path.segments.len() > 2 {
                return Err(ctx
                    .diagnostic(
                        *span,
                        "unsupported nested record pattern",
                        format!(
                            "stage0 can resolve local records and two-segment imported records, but `{}` has more segments",
                            path.segments.join(".")
                        ),
                        Some("use a local record type or a two-segment imported record path"),
                    )
                    .into());
            }
            return Err(ctx
                .diagnostic(
                    *span,
                    "unknown record pattern",
                    format!("`{}` is not a known record type", path.segments.join(".")),
                    Some("declare the record with a top-level `type` declaration"),
                )
                .into());
        };
        let field_types = instantiate_record_fields(&shape, scrutinee_type)
            .into_iter()
            .collect::<HashMap<_, _>>();
        let mut seen = HashSet::new();
        for (field, field_pattern) in fields {
            if !seen.insert(field.as_str()) {
                return Err(ctx
                    .diagnostic(
                        pattern_span(field_pattern),
                        "duplicate record pattern field",
                        format!(
                            "field `{field}` appears more than once in `{}`",
                            shape.type_name
                        ),
                        Some("keep one pattern for each matched field"),
                    )
                    .into());
            }
            let Some(expected_type) = field_types.get(field.as_str()) else {
                return Err(ctx
                    .diagnostic(
                        pattern_span(field_pattern),
                        "unknown record pattern field",
                        format!("`{}` has no field named `{field}`", shape.type_name),
                        Some("use one of the fields declared on this record type"),
                    )
                    .into());
            };
            validate_pattern(ctx, field_pattern, Some(expected_type))?;
        }
    }
    let Some(expected) = pattern_type(ctx, pattern) else {
        return Ok(());
    };
    let Some(actual) = scrutinee_type else {
        return Ok(());
    };
    if type_matches(&expected, actual) {
        return Ok(());
    }
    Err(ctx
        .diagnostic(
            pattern_span(pattern),
            "match pattern has incompatible type",
            format!(
                "pattern has type `{}`, but the matched value has type `{}`",
                expected.canonical(),
                actual.canonical()
            ),
            Some("use patterns with the same type as the matched value"),
        )
        .into())
}

fn bind_pattern(
    ctx: &ValidationContext<'_>,
    pattern: &AstPattern,
    scrutinee_type: Option<RsigType>,
    bindings: &mut HashMap<String, BindingKind>,
) {
    match pattern {
        AstPattern::Bind { name, .. } => {
            bindings.insert(name.clone(), BindingKind::Value(scrutinee_type));
        }
        AstPattern::Constructor { path, payload, .. } => {
            let payload_types = pattern_constructor_shape(ctx, path)
                .map(|constructor| {
                    instantiate_constructor_payload(&constructor, scrutinee_type.as_ref())
                })
                .unwrap_or_default();
            for (index, nested) in payload.iter().enumerate() {
                bind_pattern(ctx, nested, payload_types.get(index).cloned(), bindings);
            }
        }
        AstPattern::Tuple { items, .. } => {
            let item_types = match scrutinee_type {
                Some(RsigType::Tuple(items)) => items,
                _ => Vec::new(),
            };
            for (index, nested) in items.iter().enumerate() {
                bind_pattern(ctx, nested, item_types.get(index).cloned(), bindings);
            }
        }
        AstPattern::List { prefix, tail, .. } => {
            let item_type = match scrutinee_type.as_ref() {
                Some(RsigType::List(item)) => item.as_ref().clone(),
                _ => RsigType::Unknown,
            };
            for nested in prefix {
                bind_pattern(ctx, nested, Some(item_type.clone()), bindings);
            }
            if let Some(tail) = tail {
                bind_pattern(
                    ctx,
                    tail,
                    Some(RsigType::List(Box::new(item_type))),
                    bindings,
                );
            }
        }
        AstPattern::Record { path, fields, .. } => {
            let field_types = record_shape_for_pattern(ctx, path)
                .map(|shape| instantiate_record_fields(&shape, scrutinee_type.as_ref()))
                .map(|fields| fields.into_iter().collect::<HashMap<_, _>>())
                .unwrap_or_default();
            for (field, nested) in fields {
                bind_pattern(ctx, nested, field_types.get(field).cloned(), bindings);
            }
        }
        AstPattern::Wildcard { .. }
        | AstPattern::Unit { .. }
        | AstPattern::Bool { .. }
        | AstPattern::Int { .. }
        | AstPattern::String { .. } => {}
    }
}

fn instantiate_constructor_payload(
    constructor: &ConstructorShape,
    scrutinee_type: Option<&RsigType>,
) -> Vec<RsigType> {
    let Some(RsigType::VariantApp { name, args }) = scrutinee_type else {
        return constructor.payload.clone();
    };
    if *name != constructor.type_name || args.len() != constructor.type_params.len() {
        return constructor.payload.clone();
    }
    let substitutions = constructor
        .type_params
        .iter()
        .cloned()
        .zip(args.iter().cloned())
        .collect::<BTreeMap<_, _>>();
    constructor
        .payload
        .iter()
        .map(|type_| substitute_type_vars(type_, &substitutions))
        .collect()
}

fn instantiate_record_fields(
    shape: &RecordShape,
    scrutinee_type: Option<&RsigType>,
) -> Vec<(String, RsigType)> {
    let Some(RsigType::RecordApp { name, args }) = scrutinee_type else {
        return shape.fields.clone();
    };
    if *name != shape.type_name || args.len() != shape.type_params.len() {
        return shape.fields.clone();
    }
    let substitutions = shape
        .type_params
        .iter()
        .cloned()
        .zip(args.iter().cloned())
        .collect::<BTreeMap<_, _>>();
    shape
        .fields
        .iter()
        .map(|(field, type_)| (field.clone(), substitute_type_vars(type_, &substitutions)))
        .collect()
}

fn substitute_type_vars(
    type_: &RsigType,
    substitutions: &BTreeMap<TypeVarName, RsigType>,
) -> RsigType {
    match type_ {
        RsigType::Var(name) => substitutions
            .get(name)
            .cloned()
            .unwrap_or_else(|| RsigType::Var(name.clone())),
        RsigType::ActorId(message) => {
            RsigType::ActorId(Box::new(substitute_type_vars(message, substitutions)))
        }
        RsigType::Arrow { parameter, result } => RsigType::Arrow {
            parameter: Box::new(substitute_type_vars(parameter, substitutions)),
            result: Box::new(substitute_type_vars(result, substitutions)),
        },
        RsigType::Tuple(items) => RsigType::Tuple(
            items
                .iter()
                .map(|item| substitute_type_vars(item, substitutions))
                .collect(),
        ),
        RsigType::List(item) => RsigType::List(Box::new(substitute_type_vars(item, substitutions))),
        RsigType::RecordApp { name, args } => RsigType::RecordApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| substitute_type_vars(arg, substitutions))
                .collect(),
        },
        RsigType::VariantApp { name, args } => RsigType::VariantApp {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| substitute_type_vars(arg, substitutions))
                .collect(),
        },
        other => other.clone(),
    }
}

fn pattern_type(ctx: &ValidationContext<'_>, pattern: &AstPattern) -> Option<RsigType> {
    match pattern {
        AstPattern::Unit { .. } => Some(RsigType::Unit),
        AstPattern::Bool { .. } => Some(RsigType::Bool),
        AstPattern::Int { .. } => Some(RsigType::I64),
        AstPattern::String { .. } => Some(RsigType::String),
        AstPattern::Constructor { path, .. } => pattern_constructor_type(ctx, path),
        AstPattern::Tuple { items, .. } => Some(RsigType::Tuple(
            items
                .iter()
                .map(|item| pattern_type(ctx, item).unwrap_or(RsigType::Unknown))
                .collect(),
        )),
        AstPattern::List { prefix, tail, .. } => {
            let item_type = prefix
                .iter()
                .filter_map(|item| pattern_type(ctx, item))
                .find(|type_| !matches!(type_, RsigType::Unknown))
                .or_else(|| {
                    tail.as_ref()
                        .and_then(|tail| match pattern_type(ctx, tail) {
                            Some(RsigType::List(item)) => Some(*item),
                            _ => None,
                        })
                })
                .unwrap_or(RsigType::Unknown);
            Some(RsigType::List(Box::new(item_type)))
        }
        AstPattern::Record { path, .. } => Some(RsigType::Record(
            record_shape_for_pattern(ctx, path)
                .map(|shape| shape.type_name)
                .unwrap_or_else(|| TypeName::new(path.segments.join("."))),
        )),
        AstPattern::Wildcard { .. } | AstPattern::Bind { .. } => None,
    }
}

fn validate_simple_payload_pattern(
    ctx: &ValidationContext<'_>,
    pattern: &AstPattern,
    expected: &RsigType,
    subject: &str,
    help: &'static str,
) -> miette::Result<()> {
    match pattern {
        AstPattern::Wildcard { .. } | AstPattern::Bind { .. } => Ok(()),
        _ => Err(ctx.diagnostic(pattern_span(pattern),
            format!("unsupported {subject} pattern"),
            format!(
                "stage0 can bind {subject}s of type `{}`, but cannot destructure nested {subject} patterns yet",
                expected.canonical()
            ),
            Some(help),
        )
        .into()),
    }
}

fn pattern_constructor_type(
    ctx: &ValidationContext<'_>,
    path: &crate::ast::AstPath,
) -> Option<RsigType> {
    pattern_constructor_shape(ctx, path).map(|constructor| RsigType::Variant(constructor.type_name))
}

fn pattern_constructor_shape(
    ctx: &ValidationContext<'_>,
    path: &crate::ast::AstPath,
) -> Option<ConstructorShape> {
    match path.segments.as_slice() {
        [name] => ctx.constructor_types.get(name).cloned(),
        [module, constructor] => imported_constructor_shape(ctx, module, constructor),
        _ => None,
    }
}

fn pattern_span(pattern: &AstPattern) -> TextSpan {
    match pattern {
        AstPattern::Wildcard { span }
        | AstPattern::Bind { span, .. }
        | AstPattern::Constructor { span, .. }
        | AstPattern::Unit { span }
        | AstPattern::Tuple { span, .. }
        | AstPattern::List { span, .. }
        | AstPattern::Record { span, .. }
        | AstPattern::Bool { span, .. }
        | AstPattern::Int { span, .. }
        | AstPattern::String { span, .. } => *span,
    }
}

fn validate_unique_pattern_bindings(
    ctx: &ValidationContext<'_>,
    pattern: &AstPattern,
) -> miette::Result<()> {
    let mut seen = HashSet::new();
    validate_unique_pattern_bindings_inner(ctx, pattern, &mut seen)
}

fn validate_unique_pattern_bindings_inner(
    ctx: &ValidationContext<'_>,
    pattern: &AstPattern,
    seen: &mut HashSet<String>,
) -> miette::Result<()> {
    match pattern {
        AstPattern::Bind { name, span } => {
            if !seen.insert(name.clone()) {
                return Err(ctx
                    .diagnostic(
                        *span,
                        "duplicate pattern binding",
                        format!("`{name}` is already bound by this pattern"),
                        Some("use a unique name for each binding in the pattern"),
                    )
                    .into());
            }
        }
        AstPattern::Constructor { payload, .. } | AstPattern::Tuple { items: payload, .. } => {
            for nested in payload {
                validate_unique_pattern_bindings_inner(ctx, nested, seen)?;
            }
        }
        AstPattern::List { prefix, tail, .. } => {
            for nested in prefix {
                validate_unique_pattern_bindings_inner(ctx, nested, seen)?;
            }
            if let Some(tail) = tail {
                validate_unique_pattern_bindings_inner(ctx, tail, seen)?;
            }
        }
        AstPattern::Record { fields, .. } => {
            for (_, nested) in fields {
                validate_unique_pattern_bindings_inner(ctx, nested, seen)?;
            }
        }
        AstPattern::Wildcard { .. }
        | AstPattern::Unit { .. }
        | AstPattern::Bool { .. }
        | AstPattern::Int { .. }
        | AstPattern::String { .. } => {}
    }
    Ok(())
}

fn record_shape_for_pattern(
    ctx: &ValidationContext<'_>,
    path: &crate::ast::AstPath,
) -> Option<RecordShape> {
    ctx.record_shapes.get(&path.segments.join(".")).cloned()
}

fn validate_constructor_call(
    ctx: &ValidationContext<'_>,
    span: TextSpan,
    constructor_name: &str,
    constructor: &ConstructorShape,
    args: &[AstExpr],
    bindings: &HashMap<String, BindingKind>,
    in_actor: bool,
) -> miette::Result<()> {
    if args.len() != constructor.payload.len() {
        return Err(call_arity_error(
            ctx,
            span,
            constructor_name,
            constructor.payload.len(),
            args.len(),
        ));
    }
    for (arg, expected) in args.iter().zip(&constructor.payload) {
        validate_expr(ctx, arg, bindings, in_actor)?;
        if let Some(actual) = simple_expr_type(ctx, arg, bindings)
            && !type_matches(expected, &actual)
        {
            return Err(ctx
                .diagnostic(
                    expr_span(arg),
                    "variant payload type mismatch",
                    format!(
                        "`{constructor_name}` expects `{}`, but this payload has type `{}`",
                        expected.canonical(),
                        actual.canonical()
                    ),
                    Some("construct the variant with a payload of the declared type"),
                )
                .into());
        }
    }
    Ok(())
}

fn constructor_value_type(constructor: &ConstructorShape) -> RsigType {
    if constructor.payload.is_empty() {
        RsigType::Variant(constructor.type_name.clone())
    } else {
        source_function_type(
            constructor.payload.clone(),
            RsigType::Variant(constructor.type_name.clone()),
        )
    }
}

fn source_function_type(params: Vec<RsigType>, result: RsigType) -> RsigType {
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

fn imported_constructor_shape(
    ctx: &ValidationContext<'_>,
    module: &str,
    constructor: &str,
) -> Option<ConstructorShape> {
    ctx.imports
        .get(module)
        .and_then(|rsig| rsig.find_constructor(constructor))
        .and_then(|type_| {
            let RsigTypeDeclKind::Variant { constructors } = &type_.body else {
                return None;
            };
            constructors
                .iter()
                .find(|candidate| candidate.name.as_str() == constructor)
                .map(|candidate| ConstructorShape {
                    type_name: TypeName::new(format!("{module}.{}", type_.name.as_str())),
                    type_params: type_
                        .params
                        .iter()
                        .map(|param| TypeVarName::new(param.as_str()))
                        .collect(),
                    payload: candidate
                        .payload
                        .iter()
                        .map(|payload| qualify_imported_type(module, payload))
                        .collect(),
                })
        })
}

fn validate_static_list_index(
    ctx: &ValidationContext<'_>,
    span: TextSpan,
    function_name: &str,
    list: &AstExpr,
    index: &AstExpr,
) -> miette::Result<()> {
    let Some(index_value) = static_i64_expr(index) else {
        return Ok(());
    };
    if index_value < 0 {
        return Err(ctx
            .diagnostic(
                expr_span(index),
                format!("{function_name} index is negative"),
                "`list_get` indexes start at 0",
                Some("use a non-negative index"),
            )
            .into());
    }
    if let AstExpr::List { items, .. } = list
        && index_value as usize >= items.len()
    {
        return Err(ctx
            .diagnostic(
                span,
                format!("{function_name} index out of bounds"),
                format!(
                    "`list_get` index {index_value} is outside this list of length {}",
                    items.len()
                ),
                Some("use an index that exists in the list"),
            )
            .into());
    }
    Ok(())
}

fn validate_record_literal_shape(
    ctx: &ValidationContext<'_>,
    path: &AstPath,
    fields: &[(String, AstExpr)],
    span: TextSpan,
    bindings: &HashMap<String, BindingKind>,
) -> miette::Result<()> {
    let key = path.segments.join(".");
    let Some(shape) = ctx.record_shapes.get(&key) else {
        return Ok(());
    };
    let expected = shape
        .fields
        .iter()
        .map(|(name, type_)| (name.as_str(), type_))
        .collect::<HashMap<_, _>>();
    let mut seen = HashSet::new();
    for (name, value) in fields {
        if !seen.insert(name.as_str()) {
            return Err(ctx
                .diagnostic(
                    expr_span(value),
                    "duplicate record field",
                    format!(
                        "field `{name}` appears more than once in `{}`",
                        shape.type_name
                    ),
                    Some("keep one value for each declared field"),
                )
                .into());
        }
        let Some(expected_type) = expected.get(name.as_str()) else {
            return Err(ctx
                .diagnostic(
                    expr_span(value),
                    "unknown record field",
                    format!("`{}` has no field named `{name}`", shape.type_name),
                    Some("use one of the fields declared on the record type"),
                )
                .into());
        };
        if let Some(actual_type) = simple_expr_type(ctx, value, bindings)
            && !type_matches(expected_type, &actual_type)
        {
            return Err(ctx
                .diagnostic(
                    expr_span(value),
                    "record field type mismatch",
                    format!(
                        "field `{name}` expects `{}`, but this value has type `{}`",
                        expected_type.canonical(),
                        actual_type.canonical()
                    ),
                    Some("provide a value with the declared field type"),
                )
                .into());
        }
    }
    for (name, _type_) in &shape.fields {
        if !seen.contains(name.as_str()) {
            return Err(ctx
                .diagnostic(
                    span,
                    "missing record field",
                    format!("`{}` requires field `{name}`", shape.type_name),
                    Some("initialize every field declared on the record type"),
                )
                .into());
        }
    }
    Ok(())
}

fn validate_declared_record_field(
    ctx: &ValidationContext<'_>,
    base: &AstExpr,
    field: &str,
    span: TextSpan,
    bindings: &HashMap<String, BindingKind>,
) -> miette::Result<()> {
    let Some(base_type) = simple_expr_type(ctx, base, bindings) else {
        return Ok(());
    };
    let RsigType::Record(type_name) = base_type else {
        if matches!(base_type, RsigType::Unknown | RsigType::Var(_)) {
            return Ok(());
        }
        return Err(ctx
            .diagnostic(
                span,
                "field access requires a record",
                format!(
                    "this value has type `{}`, so it has no field named `{field}`",
                    base_type.canonical()
                ),
                Some("access a field on a record value or remove the field projection"),
            )
            .into());
    };
    let Some(shape) = ctx.record_shapes_by_type.get(&type_name) else {
        return Ok(());
    };
    if shape
        .fields
        .iter()
        .any(|(declared_field, _)| declared_field == field)
    {
        return Ok(());
    }
    Err(ctx
        .diagnostic(
            span,
            "unknown record field",
            format!("`{}` has no field named `{field}`", shape.type_name),
            Some("use one of the fields declared on the record type"),
        )
        .into())
}

fn record_literal_type(ctx: &ValidationContext<'_>, path: &AstPath) -> RsigType {
    let key = path.segments.join(".");
    ctx.record_shapes
        .get(&key)
        .map(|shape| RsigType::Record(shape.type_name.clone()))
        .unwrap_or_else(|| RsigType::Record(TypeName::new(key)))
}

fn static_i64_expr(expr: &AstExpr) -> Option<i64> {
    match expr {
        AstExpr::Int { value, .. } => Some(*value),
        AstExpr::Neg { expr, .. } => static_i64_expr(expr).and_then(i64::checked_neg),
        _ => None,
    }
}

fn validate_actor_block(
    ctx: &ValidationContext<'_>,
    block: &AstBlock,
    inherited_bindings: &HashMap<String, BindingKind>,
) -> miette::Result<()> {
    let mut bindings = inherited_bindings.clone();
    let mut has_receive = false;

    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                value,
                type_annotation,
                ..
            } => {
                if type_annotation.is_some() {
                    return Err(ctx
                        .diagnostic(
                            expr_span(value),
                            "unsupported actor local annotation",
                            "stage0 actor locals do not support type annotations yet",
                            Some("omit the annotation"),
                        )
                        .into());
                }
                if let AstExpr::Spawn { body, .. } = value {
                    validate_actor_block(ctx, body, &bindings)?;
                    bindings.insert(name.clone(), BindingKind::Actor);
                } else {
                    validate_expr(ctx, value, &bindings, true)?;
                    let type_ = simple_expr_type(ctx, value, &bindings);
                    bindings.insert(name.clone(), BindingKind::Value(type_));
                }
            }
            AstStmt::Expr(expr) => {
                if let AstExpr::Receive { arms, .. } = expr {
                    has_receive = true;
                    validate_receive_arms(ctx, arms, &bindings)?;
                } else {
                    validate_expr(ctx, expr, &bindings, true)?;
                }
            }
        }
    }

    if let Some(tail) = &block.tail {
        if let AstExpr::Receive { arms, .. } = tail {
            has_receive = true;
            validate_receive_arms(ctx, arms, &bindings)?;
        } else {
            validate_expr(ctx, tail, &bindings, true)?;
        }
    }

    if !has_receive {
        return Err(ctx
            .diagnostic(
                block.span,
                "actor body never receives",
                "stage0 actors must contain at least one receive point",
                Some("try: spawn { receive { msg -> dbg(msg) } }"),
            )
            .into());
    }

    Ok(())
}

fn validate_receive_arms(
    ctx: &ValidationContext<'_>,
    arms: &[crate::ast::AstReceiveArm],
    bindings: &HashMap<String, BindingKind>,
) -> miette::Result<()> {
    let pattern_validator = PatternValidator::new(ctx);
    for arm in arms {
        let pattern_type = pattern_validator.pattern_type(&arm.pattern);
        pattern_validator.validate(&arm.pattern, pattern_type.as_ref())?;
        let mut receive_bindings = bindings.clone();
        pattern_validator.bind(&arm.pattern, pattern_type, &mut receive_bindings);
        validate_expr(ctx, &arm.body, &receive_bindings, true)?;
    }
    Ok(())
}

fn validate_callable_call(
    ctx: &ValidationContext<'_>,
    span: TextSpan,
    callee: &[String],
    signature: &CallableSignature,
    args: &[AstExpr],
    bindings: &HashMap<String, BindingKind>,
    in_actor: bool,
) -> miette::Result<ExprCategory> {
    let name = callee_display_name(callee);
    let params = &signature.function.params;
    if args.is_empty() && !params.is_empty() || args.len() > params.len() {
        return Err(call_arity_error(ctx, span, &name, params.len(), args.len()));
    }
    if callable_signature_has_unknown_import_abi(signature) {
        return Err(ctx
            .diagnostic(
                span,
                "imported value has unknown ABI",
                format!("`{name}` is imported, but its .rsig type is not concrete enough to call"),
                Some("add enough type information for the exported function"),
            )
            .into());
    }

    for (index, arg) in args.iter().enumerate() {
        if signature.is_actor_operation() && index == 0 {
            validate_actor_target(ctx, arg, bindings, span, &name)?;
        } else {
            validate_expr(ctx, arg, bindings, in_actor)?;
        }
        if let Some(expected) = params.get(index) {
            validate_signature_arg_type(ctx, &name, arg, expected, bindings)?;
        }
    }
    validate_static_callable_facts(ctx, span, signature, args)?;

    if args.len() < params.len() {
        return Ok(ExprCategory::Other);
    }
    if signature.is_output_operation() {
        Ok(ExprCategory::Output)
    } else if signature.is_actor_operation() {
        Ok(ExprCategory::Actor)
    } else {
        Ok(ExprCategory::Other)
    }
}

fn callable_signature_has_unknown_import_abi(signature: &CallableSignature) -> bool {
    match signature.kind {
        CallableKind::ImportedFunction { .. } => {
            rsig_type_has_unknown_abi(&signature.function.result)
                || signature
                    .function
                    .params
                    .iter()
                    .any(rsig_type_has_unknown_abi)
        }
        CallableKind::ImportedExternal { .. } => {
            rsig_external_type_has_unknown_abi(&signature.function.result)
                || signature
                    .function
                    .params
                    .iter()
                    .any(rsig_external_type_has_unknown_abi)
        }
        CallableKind::Function | CallableKind::External => false,
    }
}

fn validate_signature_arg_type(
    ctx: &ValidationContext<'_>,
    name: &str,
    arg: &AstExpr,
    expected: &RsigType,
    bindings: &HashMap<String, BindingKind>,
) -> miette::Result<()> {
    if !signature_arg_needs_static_check(expected) {
        return Ok(());
    }
    let Some(actual) = simple_expr_type(ctx, arg, bindings) else {
        return Ok(());
    };
    if type_matches(expected, &actual) {
        Ok(())
    } else {
        Err(ctx
            .diagnostic(
                expr_span(arg),
                format!("{name} argument has the wrong type"),
                format!(
                    "expected `{}`, found `{}`",
                    expected.canonical(),
                    actual.canonical()
                ),
                Some("check the call argument type"),
            )
            .into())
    }
}

fn signature_arg_needs_static_check(type_: &RsigType) -> bool {
    match type_ {
        RsigType::Unknown | RsigType::Var(_) => false,
        RsigType::ActorId(_) => false,
        RsigType::List(_) => true,
        RsigType::Tuple(items) => items.iter().any(signature_arg_needs_static_check),
        RsigType::RecordApp { args, .. } | RsigType::VariantApp { args, .. } => {
            args.iter().any(signature_arg_needs_static_check)
        }
        RsigType::Arrow { .. } => false,
        RsigType::Bool
        | RsigType::Char
        | RsigType::F64
        | RsigType::I32
        | RsigType::I64
        | RsigType::String
        | RsigType::Unit
        | RsigType::Record(_)
        | RsigType::Variant(_) => true,
    }
}

fn validate_static_callable_facts(
    ctx: &ValidationContext<'_>,
    span: TextSpan,
    signature: &CallableSignature,
    args: &[AstExpr],
) -> miette::Result<()> {
    if signature.is_static_list_get()
        && !ctx.declared_external_names.contains("list_get")
        && let [list, index] = args
    {
        validate_static_list_index(ctx, span, "list_get", list, index)?;
    }
    Ok(())
}

fn callee_display_name(callee: &[String]) -> String {
    callee.join(".")
}

fn validate_actor_target(
    ctx: &ValidationContext<'_>,
    expr: &AstExpr,
    bindings: &HashMap<String, BindingKind>,
    span: TextSpan,
    operation: &str,
) -> miette::Result<()> {
    if let AstExpr::Path { path, .. } = expr
        && let [name] = path.segments.as_slice()
    {
        match bindings.get(name) {
            Some(BindingKind::Actor) => return Ok(()),
            None => {
                return Err(ctx
                    .diagnostic(
                        span,
                        format!("{operation} target is unknown"),
                        format!("`{name}` is not bound in this scope"),
                        Some("send to the result of `spawn { ... }`"),
                    )
                    .into());
            }
            Some(BindingKind::Value(_)) => {
                return Err(ctx
                    .diagnostic(
                        span,
                        format!("{operation} target is not an actor"),
                        format!("`{name}` is a value, not an actor id"),
                        Some("send to the result of `spawn { ... }`"),
                    )
                    .into());
            }
        }
    }
    validate_expr(ctx, expr, bindings, false)?;
    Ok(())
}

fn infer_annotation_block_type(
    ctx: &ValidationContext<'_>,
    block: &AstBlock,
    bindings: &mut HashMap<String, RsigType>,
) -> Option<RsigType> {
    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                let type_ = type_annotation
                    .as_ref()
                    .map(|annotation| ctx.lower_type_annotation(annotation))
                    .or_else(|| infer_annotation_expr_type(ctx, value, bindings))
                    .unwrap_or(RsigType::Unknown);
                bindings.insert(name.clone(), type_);
            }
            AstStmt::Expr(expr) => {
                infer_annotation_expr_type(ctx, expr, bindings);
            }
        }
    }
    if let Some(tail) = &block.tail {
        infer_annotation_expr_type(ctx, tail, bindings)
    } else {
        Some(RsigType::Unit)
    }
}

fn infer_annotation_expr_type(
    ctx: &ValidationContext<'_>,
    expr: &AstExpr,
    bindings: &HashMap<String, RsigType>,
) -> Option<RsigType> {
    match expr {
        AstExpr::Add { .. }
        | AstExpr::Sub { .. }
        | AstExpr::Mul { .. }
        | AstExpr::Div { .. }
        | AstExpr::Mod { .. }
        | AstExpr::Neg { .. }
        | AstExpr::Int { .. } => Some(RsigType::I64),
        AstExpr::Eq { .. }
        | AstExpr::Lt { .. }
        | AstExpr::And { .. }
        | AstExpr::Or { .. }
        | AstExpr::Not { .. }
        | AstExpr::Bool { .. } => Some(RsigType::Bool),
        AstExpr::If {
            then_branch,
            else_branch,
            ..
        } => Some(merge_annotation_types(
            infer_annotation_expr_type(ctx, then_branch, bindings).unwrap_or(RsigType::Unknown),
            infer_annotation_expr_type(ctx, else_branch, bindings).unwrap_or(RsigType::Unknown),
        )),
        AstExpr::Match { arms, .. } => arms
            .iter()
            .map(|arm| {
                infer_annotation_expr_type(ctx, &arm.body, bindings).unwrap_or(RsigType::Unknown)
            })
            .reduce(merge_annotation_types)
            .or(Some(RsigType::Unknown)),
        AstExpr::Block { block, .. } => {
            infer_annotation_block_type(ctx, block, &mut bindings.clone())
        }
        AstExpr::Call { callee, args, .. } => match callee.segments.as_slice() {
            [name] if name == "list_get" => args.first().map(
                |list| match infer_annotation_expr_type(ctx, list, bindings) {
                    Some(RsigType::List(item)) => *item,
                    _ => external_result_type(ctx, name).unwrap_or(RsigType::Unknown),
                },
            ),
            [name] => ctx
                .constructor_types
                .get(name)
                .map(|constructor| RsigType::Variant(constructor.type_name.clone()))
                .or_else(|| ctx.function_results.get(name).cloned())
                .or_else(|| external_result_type(ctx, name)),
            [module, name] => ctx
                .imports
                .get(module.as_str())
                .and_then(|rsig| rsig.find(name))
                .map(|export| match export {
                    RsigExport::Function(function) => {
                        qualify_imported_type(module, &function.result)
                    }
                    RsigExport::External(external) => {
                        qualify_imported_type(module, &external.result)
                    }
                })
                .or_else(|| {
                    imported_constructor_shape(ctx, module, name)
                        .map(|constructor| RsigType::Variant(constructor.type_name))
                }),
            _ => None,
        },
        AstExpr::Apply { .. } => Some(RsigType::Unknown),
        AstExpr::Unit { .. } => Some(RsigType::Unit),
        AstExpr::Tuple { items, .. } => Some(RsigType::Tuple(
            items
                .iter()
                .map(|item| {
                    infer_annotation_expr_type(ctx, item, bindings).unwrap_or(RsigType::Unknown)
                })
                .collect(),
        )),
        AstExpr::List { items, .. } => Some(RsigType::List(Box::new(
            items
                .first()
                .and_then(|item| infer_annotation_expr_type(ctx, item, bindings))
                .unwrap_or(RsigType::Unknown),
        ))),
        AstExpr::Record { path, .. } => Some(record_literal_type(ctx, path)),
        AstExpr::Field { base, field, .. } => {
            infer_annotation_field_type(ctx, base, field, bindings)
        }
        AstExpr::TupleIndex { base, index, .. } => {
            infer_annotation_tuple_index_type(ctx, base, *index, bindings)
        }
        AstExpr::Char { .. } => Some(RsigType::Char),
        AstExpr::Float { .. } => Some(RsigType::F64),
        AstExpr::Path { path, .. } => path
            .segments
            .first()
            .and_then(|name| {
                bindings
                    .get(name)
                    .cloned()
                    .or_else(|| ctx.constructor_types.get(name).map(constructor_value_type))
            })
            .or_else(|| {
                let [module, constructor] = path.segments.as_slice() else {
                    return None;
                };
                imported_constructor_shape(ctx, module, constructor)
                    .map(|constructor| constructor_value_type(&constructor))
            }),
        AstExpr::Lambda { .. } => Some(RsigType::Unknown),
        AstExpr::String { .. } => Some(RsigType::String),
        AstExpr::Spawn { .. } => Some(RsigType::ActorId(Box::new(RsigType::Unknown))),
        AstExpr::Receive { .. } => Some(RsigType::Unit),
    }
}

fn infer_annotation_field_type(
    ctx: &ValidationContext<'_>,
    base: &AstExpr,
    field: &str,
    bindings: &HashMap<String, RsigType>,
) -> Option<RsigType> {
    if let AstExpr::Record { fields, .. } = base {
        return fields.iter().find_map(|(name, value)| {
            (name == field).then(|| {
                infer_annotation_expr_type(ctx, value, bindings).unwrap_or(RsigType::Unknown)
            })
        });
    }
    None
}

fn infer_annotation_tuple_index_type(
    ctx: &ValidationContext<'_>,
    base: &AstExpr,
    index: usize,
    bindings: &HashMap<String, RsigType>,
) -> Option<RsigType> {
    if let AstExpr::Tuple { items, .. } = base {
        return items
            .get(index)
            .and_then(|item| infer_annotation_expr_type(ctx, item, bindings));
    }
    None
}

fn external_result_type(ctx: &ValidationContext<'_>, name: &str) -> Option<RsigType> {
    ctx.external_signatures
        .get(name)
        .map(|external| external.function.result.clone())
}

fn merge_annotation_types(lhs: RsigType, rhs: RsigType) -> RsigType {
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

fn const_value_type(value: &ConstValue) -> RsigType {
    match value {
        ConstValue::Bool(_) => RsigType::Bool,
        ConstValue::Char(_) => RsigType::Char,
        ConstValue::Float(_) => RsigType::F64,
        ConstValue::Int(_) => RsigType::I64,
        ConstValue::String(_) => RsigType::String,
        ConstValue::Unit => RsigType::Unit,
        ConstValue::Tuple(items) => RsigType::Tuple(items.iter().map(const_value_type).collect()),
        ConstValue::List(items) => RsigType::List(Box::new(
            items
                .first()
                .map(const_value_type)
                .unwrap_or(RsigType::Unknown),
        )),
        ConstValue::Record { path, fields: _ } => RsigType::Record(TypeName::new(path.clone())),
    }
}

fn type_matches(expected: &RsigType, actual: &RsigType) -> bool {
    expected == actual
        || matches!(
            (expected, actual),
            (RsigType::I32, RsigType::I64) | (RsigType::I64, RsigType::I32)
        )
        || matches!(expected, RsigType::Unknown)
        || matches!(actual, RsigType::Unknown)
        || matches!(expected, RsigType::Var(_))
        || matches!(actual, RsigType::Var(_))
        || matches!(
            (expected, actual),
            (RsigType::List(expected), RsigType::List(actual))
                if type_matches(expected, actual)
        )
        || matches!(
            (expected, actual),
            (RsigType::Tuple(expected), RsigType::Tuple(actual))
                if expected.len() == actual.len()
                    && expected
                        .iter()
                        .zip(actual)
                        .all(|(expected, actual)| type_matches(expected, actual))
        )
        || matches!(
            (expected, actual),
            (
                RsigType::RecordApp {
                    name: expected_name,
                    args: expected_args
                },
                RsigType::RecordApp {
                    name: actual_name,
                    args: actual_args
                }
            ) if expected_name == actual_name
                && expected_args.len() == actual_args.len()
                && expected_args
                    .iter()
                    .zip(actual_args)
                    .all(|(expected, actual)| type_matches(expected, actual))
        )
        || matches!(
            (expected, actual),
            (RsigType::RecordApp { name, .. }, RsigType::Record(actual))
                | (RsigType::Record(actual), RsigType::RecordApp { name, .. })
                if name == actual
        )
        || matches!(
            (expected, actual),
            (
                RsigType::VariantApp {
                    name: expected_name,
                    args: expected_args
                },
                RsigType::VariantApp {
                    name: actual_name,
                    args: actual_args
                }
            ) if expected_name == actual_name
                && expected_args.len() == actual_args.len()
                && expected_args
                    .iter()
                    .zip(actual_args)
                    .all(|(expected, actual)| type_matches(expected, actual))
        )
        || matches!(
            (expected, actual),
            (RsigType::VariantApp { name, .. }, RsigType::Variant(actual))
                | (RsigType::Variant(actual), RsigType::VariantApp { name, .. })
                if name == actual
        )
}

fn export_has_unknown_abi(export: &RsigExport) -> bool {
    match export {
        RsigExport::Function(function) => {
            rsig_type_has_unknown_abi(&function.result)
                || function.params.iter().any(rsig_type_has_unknown_abi)
        }
        RsigExport::External(external) => {
            rsig_external_type_has_unknown_abi(&external.result)
                || external
                    .params
                    .iter()
                    .any(rsig_external_type_has_unknown_abi)
        }
    }
}

fn rsig_type_has_unknown_abi(type_: &RsigType) -> bool {
    match type_ {
        RsigType::Unknown | RsigType::Var(_) => true,
        RsigType::ActorId(message) | RsigType::List(message) => rsig_type_has_unknown_abi(message),
        RsigType::Arrow { parameter, result } => {
            rsig_type_has_unknown_abi(parameter) || rsig_type_has_unknown_abi(result)
        }
        RsigType::Tuple(items) => items.iter().any(rsig_type_has_unknown_abi),
        RsigType::RecordApp { .. } | RsigType::VariantApp { .. } => false,
        RsigType::Bool
        | RsigType::Char
        | RsigType::F64
        | RsigType::I32
        | RsigType::I64
        | RsigType::String
        | RsigType::Unit
        | RsigType::Record(_) => false,
        RsigType::Variant(_) => false,
    }
}

fn rsig_external_type_has_unknown_abi(type_: &RsigType) -> bool {
    match type_ {
        RsigType::Unknown => true,
        RsigType::Var(_) | RsigType::RecordApp { .. } | RsigType::VariantApp { .. } => false,
        other => rsig_type_has_unknown_abi(other),
    }
}

fn simple_expr_type(
    ctx: &ValidationContext<'_>,
    expr: &AstExpr,
    bindings: &HashMap<String, BindingKind>,
) -> Option<RsigType> {
    if let Some(type_) = ctx
        .expression_types
        .and_then(|expression_types| expression_types.get(expr_span(expr)))
        && !matches!(type_, RsigType::Unknown | RsigType::Var(_))
    {
        return Some(type_.clone());
    }

    match expr {
        AstExpr::Add { .. }
        | AstExpr::Sub { .. }
        | AstExpr::Mul { .. }
        | AstExpr::Div { .. }
        | AstExpr::Mod { .. }
        | AstExpr::Neg { .. }
        | AstExpr::Int { .. } => Some(RsigType::I64),
        AstExpr::Eq { .. }
        | AstExpr::Lt { .. }
        | AstExpr::And { .. }
        | AstExpr::Or { .. }
        | AstExpr::Not { .. }
        | AstExpr::Bool { .. } => Some(RsigType::Bool),
        AstExpr::Char { .. } => Some(RsigType::Char),
        AstExpr::Float { .. } => Some(RsigType::F64),
        AstExpr::String { .. } => Some(RsigType::String),
        AstExpr::Unit { .. } => Some(RsigType::Unit),
        AstExpr::Tuple { items, .. } => Some(RsigType::Tuple(
            items
                .iter()
                .map(|item| simple_expr_type(ctx, item, bindings).unwrap_or(RsigType::Unknown))
                .collect(),
        )),
        AstExpr::List { items, .. } => Some(RsigType::List(Box::new(
            items
                .first()
                .and_then(|item| simple_expr_type(ctx, item, bindings))
                .unwrap_or(RsigType::Unknown),
        ))),
        AstExpr::Path { path, .. } if path.segments.len() == 1 => {
            match bindings.get(&path.segments[0]) {
                Some(BindingKind::Actor) => Some(RsigType::ActorId(Box::new(RsigType::Unknown))),
                Some(BindingKind::Value(type_)) => type_.clone(),
                None => ctx
                    .constructor_types
                    .get(&path.segments[0])
                    .map(constructor_value_type),
            }
        }
        AstExpr::Path { path, .. } if path.segments.len() == 2 => {
            let [module, constructor] = path.segments.as_slice() else {
                unreachable!();
            };
            imported_constructor_shape(ctx, module, constructor)
                .map(|constructor| constructor_value_type(&constructor))
        }
        AstExpr::Call { callee, args, .. } => match callee.segments.as_slice() {
            [name] if name == "list_get" => {
                args.first()
                    .map(|list| match simple_expr_type(ctx, list, bindings) {
                        Some(RsigType::List(item)) => *item,
                        _ => external_result_type(ctx, name).unwrap_or(RsigType::Unknown),
                    })
            }
            [name] => ctx
                .constructor_types
                .get(name)
                .map(|constructor| RsigType::Variant(constructor.type_name.clone()))
                .or_else(|| ctx.function_results.get(name).cloned())
                .or_else(|| external_result_type(ctx, name)),
            [module, name] => ctx
                .imports
                .get(module.as_str())
                .and_then(|rsig| rsig.find(name))
                .map(|export| match export {
                    RsigExport::Function(function) => {
                        qualify_imported_type(module, &function.result)
                    }
                    RsigExport::External(external) => {
                        qualify_imported_type(module, &external.result)
                    }
                })
                .or_else(|| {
                    imported_constructor_shape(ctx, module, name)
                        .map(|constructor| RsigType::Variant(constructor.type_name))
                }),
            _ => None,
        },
        AstExpr::Apply { .. } => Some(RsigType::Unknown),
        AstExpr::If {
            then_branch,
            else_branch,
            ..
        } => {
            let then_type = simple_expr_type(ctx, then_branch, bindings)?;
            let else_type = simple_expr_type(ctx, else_branch, bindings)?;
            type_matches(&then_type, &else_type).then_some(then_type)
        }
        AstExpr::Match { arms, .. } => arms
            .iter()
            .map(|arm| simple_expr_type(ctx, &arm.body, bindings).unwrap_or(RsigType::Unknown))
            .reduce(merge_annotation_types),
        AstExpr::Block { block, .. } => simple_block_type(ctx, block, bindings),
        AstExpr::Record { path, .. } => Some(record_literal_type(ctx, path)),
        AstExpr::Field { base, field, .. } => simple_field_type(ctx, base, field, bindings),
        AstExpr::TupleIndex { base, index, .. } => {
            simple_tuple_index_type(ctx, base, *index, bindings)
        }
        AstExpr::Lambda { .. } => Some(RsigType::Unknown),
        AstExpr::Spawn { .. } => Some(RsigType::ActorId(Box::new(RsigType::Unknown))),
        AstExpr::Receive { .. } | AstExpr::Path { .. } => None,
    }
}

fn simple_block_type(
    ctx: &ValidationContext<'_>,
    block: &AstBlock,
    outer_bindings: &HashMap<String, BindingKind>,
) -> Option<RsigType> {
    let mut bindings = outer_bindings.clone();
    for stmt in &block.statements {
        match stmt {
            AstStmt::Let {
                name,
                type_annotation,
                value,
                ..
            } => {
                let type_ = type_annotation
                    .as_ref()
                    .map(|annotation| ctx.lower_type_annotation(annotation))
                    .or_else(|| simple_expr_type(ctx, value, &bindings));
                bindings.insert(name.clone(), BindingKind::Value(type_));
            }
            AstStmt::Expr(expr) => {
                simple_expr_type(ctx, expr, &bindings);
            }
        }
    }

    if let Some(tail) = &block.tail {
        simple_expr_type(ctx, tail, &bindings)
    } else {
        Some(RsigType::Unit)
    }
}

fn simple_field_type(
    ctx: &ValidationContext<'_>,
    base: &AstExpr,
    field: &str,
    bindings: &HashMap<String, BindingKind>,
) -> Option<RsigType> {
    if let AstExpr::Record { fields, .. } = base {
        return fields.iter().find_map(|(name, value)| {
            (name == field)
                .then(|| simple_expr_type(ctx, value, bindings).unwrap_or(RsigType::Unknown))
        });
    }
    None
}

fn simple_tuple_index_type(
    ctx: &ValidationContext<'_>,
    base: &AstExpr,
    index: usize,
    bindings: &HashMap<String, BindingKind>,
) -> Option<RsigType> {
    if let AstExpr::Tuple { items, .. } = base {
        return items
            .get(index)
            .and_then(|item| simple_expr_type(ctx, item, bindings));
    }
    None
}

fn call_arity_error(
    ctx: &ValidationContext<'_>,
    span: TextSpan,
    name: &str,
    expected: usize,
    actual: usize,
) -> miette::Report {
    ctx.diagnostic(
        span,
        format!("{name} expects {expected} argument(s)"),
        format!("found {actual} argument(s)"),
        Some("check the call arity"),
    )
    .into()
}
