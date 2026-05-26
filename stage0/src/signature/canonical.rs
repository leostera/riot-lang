use std::collections::BTreeMap;

use crate::fingerprint::SignatureFingerprinter;

use super::{
    ModuleName, Rsig, RsigDependency, RsigExport, RsigExternal, RsigFunction, RsigType,
    RsigTypeDecl, RsigTypeDeclKind, RsigTypeScheme, RsigVariantConstructor, TypeParamName,
    TypeVarName,
};

impl RsigTypeScheme {
    pub(crate) fn from_signature(params: &[RsigType], result: &RsigType) -> Self {
        let body = source_function_type(params.to_vec(), result.clone());
        let mut quantifiers = Vec::new();
        collect_type_vars(&body, &mut quantifiers);
        Self { quantifiers, body }
    }

    pub(crate) fn canonical(&self) -> String {
        if self.quantifiers.is_empty() {
            self.body.canonical()
        } else {
            format!(
                "forall {}. {}",
                self.quantifiers
                    .iter()
                    .map(TypeVarName::as_str)
                    .collect::<Vec<_>>()
                    .join(" "),
                self.body.canonical()
            )
        }
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

fn collect_type_vars(type_: &RsigType, vars: &mut Vec<TypeVarName>) {
    match type_ {
        RsigType::ActorId(message) | RsigType::List(message) => collect_type_vars(message, vars),
        RsigType::Arrow { parameter, result } => {
            collect_type_vars(parameter, vars);
            collect_type_vars(result, vars);
        }
        RsigType::Tuple(items) => {
            for item in items {
                collect_type_vars(item, vars);
            }
        }
        RsigType::RecordApp { args, .. } | RsigType::VariantApp { args, .. } => {
            for arg in args {
                collect_type_vars(arg, vars);
            }
        }
        RsigType::Var(name) if !vars.contains(name) => vars.push(name.clone()),
        RsigType::Var(_)
        | RsigType::Bool
        | RsigType::Char
        | RsigType::F64
        | RsigType::I32
        | RsigType::I64
        | RsigType::Record(_)
        | RsigType::String
        | RsigType::Unit
        | RsigType::Variant(_)
        | RsigType::Unknown => {}
    }
}

impl Rsig {
    pub(crate) fn with_dependencies(
        module: impl Into<ModuleName>,
        mut dependencies: Vec<RsigDependency>,
        mut types: Vec<RsigTypeDecl>,
        mut exports: Vec<RsigExport>,
    ) -> Self {
        let module = module.into();
        let fingerprinter = SignatureFingerprinter::new();
        dependencies.sort_by(|lhs, rhs| lhs.module.cmp(&rhs.module));
        types.sort_by(|lhs, rhs| lhs.name.as_str().cmp(rhs.name.as_str()));
        for type_ in &mut types {
            type_.fingerprint = fingerprinter.stable_text(&type_.canonical_without_fingerprint());
        }
        for export in &mut exports {
            export.canonicalize_type_vars();
        }
        exports.sort_by(|lhs, rhs| lhs.name().cmp(rhs.name()).then(lhs.kind().cmp(rhs.kind())));
        for export in &mut exports {
            export.set_fingerprint(
                fingerprinter.stable_text(&export.canonical_without_fingerprint()),
            );
        }
        let dependency_text = dependencies
            .iter()
            .map(RsigDependency::canonical)
            .collect::<Vec<_>>()
            .join("\n");
        let export_text = exports
            .iter()
            .map(RsigExport::canonical_with_fingerprint)
            .collect::<Vec<_>>()
            .join("\n");
        let type_text = types
            .iter()
            .map(RsigTypeDecl::canonical_with_fingerprint)
            .collect::<Vec<_>>()
            .join("\n");
        let module_fingerprint = fingerprinter.stable_text(
            &[
                dependency_text.as_str(),
                type_text.as_str(),
                export_text.as_str(),
            ]
            .into_iter()
            .filter(|part| !part.is_empty())
            .collect::<Vec<_>>()
            .join("\n"),
        );
        Self {
            module,
            dependencies,
            types,
            exports,
            module_fingerprint,
        }
    }

    pub(crate) fn canonical_text(&self) -> String {
        let mut text = String::new();
        text.push_str(&format!("module {}\n", self.module));
        text.push_str(&format!("fingerprint {:016x}\n", self.module_fingerprint));
        for dependency in &self.dependencies {
            text.push_str(&dependency.canonical());
            text.push('\n');
        }
        for type_ in &self.types {
            text.push_str(&type_.canonical_with_fingerprint());
            text.push('\n');
        }
        for export in &self.exports {
            text.push_str(&export.canonical_with_fingerprint());
            text.push('\n');
        }
        text
    }
}

impl RsigDependency {
    fn canonical(&self) -> String {
        format!("depends {} {:016x}", self.module, self.fingerprint)
    }
}

impl RsigTypeDecl {
    fn canonical_header(&self) -> String {
        if self.params.is_empty() {
            format!("type {}", self.name)
        } else {
            let params = self
                .params
                .iter()
                .map(TypeParamName::as_str)
                .collect::<Vec<_>>()
                .join(", ");
            format!("type {}<{}>", self.name, params)
        }
    }

    fn canonical_without_fingerprint(&self) -> String {
        match &self.body {
            RsigTypeDeclKind::Abstract => self.canonical_header(),
            RsigTypeDeclKind::Variant { constructors } => {
                let constructors = constructors
                    .iter()
                    .map(RsigVariantConstructor::canonical)
                    .collect::<Vec<_>>()
                    .join(" | ");
                format!("{} = {}", self.canonical_header(), constructors)
            }
            RsigTypeDeclKind::Record { fields } => {
                let fields = fields
                    .iter()
                    .map(|field| format!("{}: {}", field.name, field.type_.canonical()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} = {{ {} }}", self.canonical_header(), fields)
            }
        }
    }

    fn canonical_with_fingerprint(&self) -> String {
        format!(
            "{:016x} {}",
            self.fingerprint,
            self.canonical_without_fingerprint()
        )
    }
}

impl RsigExport {
    fn kind(&self) -> &'static str {
        match self {
            RsigExport::Function(_) => "fn",
            RsigExport::External(_) => "external",
        }
    }

    fn canonical_without_fingerprint(&self) -> String {
        match self {
            RsigExport::Function(function) => format!(
                "fn {}({}) -> {} : {} = {}",
                function.name,
                render_types(&function.params),
                function.result.canonical(),
                function.scheme.canonical(),
                function.symbol
            ),
            RsigExport::External(external) => format!(
                "external {}({}) -> {} : {} = {}",
                external.name,
                render_types(&external.params),
                external.result.canonical(),
                external.scheme.canonical(),
                external.abi
            ),
        }
    }

    fn canonical_with_fingerprint(&self) -> String {
        match self {
            RsigExport::Function(function) => {
                format!(
                    "{:016x} {}",
                    function.fingerprint,
                    self.canonical_without_fingerprint()
                )
            }
            RsigExport::External(external) => {
                format!(
                    "{:016x} {}",
                    external.fingerprint,
                    self.canonical_without_fingerprint()
                )
            }
        }
    }

    fn set_fingerprint(&mut self, fingerprint: u64) {
        match self {
            RsigExport::Function(function) => function.fingerprint = fingerprint,
            RsigExport::External(external) => external.fingerprint = fingerprint,
        }
    }

    fn canonicalize_type_vars(&mut self) {
        let mut vars = TypeVarCanonicalizer::default();
        match self {
            RsigExport::Function(function) => {
                canonicalize_function_type_vars(function, &mut vars);
            }
            RsigExport::External(external) => {
                canonicalize_external_type_vars(external, &mut vars);
            }
        }
    }
}

fn canonicalize_function_type_vars(function: &mut RsigFunction, vars: &mut TypeVarCanonicalizer) {
    for param in &mut function.params {
        vars.canonicalize(param);
    }
    vars.canonicalize(&mut function.result);
    vars.canonicalize_scheme(&mut function.scheme);
}

fn canonicalize_external_type_vars(external: &mut RsigExternal, vars: &mut TypeVarCanonicalizer) {
    for param in &mut external.params {
        vars.canonicalize(param);
    }
    vars.canonicalize(&mut external.result);
    vars.canonicalize_scheme(&mut external.scheme);
}

impl RsigVariantConstructor {
    fn canonical(&self) -> String {
        if self.payload.is_empty() {
            self.name.to_string()
        } else {
            format!("{}({})", self.name, render_types(&self.payload))
        }
    }
}

#[derive(Default)]
struct TypeVarCanonicalizer {
    names: BTreeMap<TypeVarName, TypeVarName>,
}

impl TypeVarCanonicalizer {
    fn canonicalize_scheme(&mut self, scheme: &mut RsigTypeScheme) {
        self.canonicalize(&mut scheme.body);
        let mut quantifiers = Vec::new();
        collect_type_vars(&scheme.body, &mut quantifiers);
        scheme.quantifiers = quantifiers;
    }

    fn canonicalize(&mut self, type_: &mut RsigType) {
        match type_ {
            RsigType::ActorId(message) | RsigType::List(message) => self.canonicalize(message),
            RsigType::Arrow { parameter, result } => {
                self.canonicalize(parameter);
                self.canonicalize(result);
            }
            RsigType::Tuple(items) => {
                for item in items {
                    self.canonicalize(item);
                }
            }
            RsigType::RecordApp { args, .. } | RsigType::VariantApp { args, .. } => {
                for arg in args {
                    self.canonicalize(arg);
                }
            }
            RsigType::Var(name) => {
                let next = canonical_type_var_name(self.names.len());
                let canonical = self.names.entry(name.clone()).or_insert(next);
                *name = canonical.clone();
            }
            RsigType::Bool
            | RsigType::Char
            | RsigType::F64
            | RsigType::I32
            | RsigType::I64
            | RsigType::Record(_)
            | RsigType::Variant(_)
            | RsigType::String
            | RsigType::Unit
            | RsigType::Unknown => {}
        }
    }
}

fn canonical_type_var_name(index: usize) -> TypeVarName {
    if index < 26 {
        let name = char::from(b'a' + index as u8);
        TypeVarName::new(format!("'{name}"))
    } else {
        TypeVarName::new(format!("'t{}", index - 26))
    }
}

impl RsigType {
    pub(crate) fn canonical(&self) -> String {
        match self {
            RsigType::Bool => "bool".to_owned(),
            RsigType::Char => "char".to_owned(),
            RsigType::F64 => "f64".to_owned(),
            RsigType::I32 => "i32".to_owned(),
            RsigType::I64 => "i64".to_owned(),
            RsigType::ActorId(message) => format!("actor_id<{}>", message.canonical()),
            RsigType::String => "String".to_owned(),
            RsigType::Unit => "unit".to_owned(),
            RsigType::Var(name) => name.to_string(),
            RsigType::Arrow { parameter, result } => {
                format!("({} -> {})", parameter.canonical(), result.canonical())
            }
            RsigType::Tuple(items) => format!("({})", render_types(items)),
            RsigType::List(item) => format!("List<{}>", item.canonical()),
            RsigType::Record(name) => name.to_string(),
            RsigType::RecordApp { name, args } => {
                format!("{}<{}>", name, render_types(args))
            }
            RsigType::Variant(name) => name.to_string(),
            RsigType::VariantApp { name, args } => {
                format!("{}<{}>", name, render_types(args))
            }
            RsigType::Unknown => "_".to_owned(),
        }
    }
}

fn render_types(types: &[RsigType]) -> String {
    types
        .iter()
        .map(RsigType::canonical)
        .collect::<Vec<_>>()
        .join(", ")
}
