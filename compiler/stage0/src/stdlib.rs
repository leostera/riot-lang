use camino::Utf8Path;
use miette::bail;

use crate::ast::{AstDecl, AstProgram};
use crate::parser::parse_source;
use crate::signature::{
    ModuleName, Rsig, RsigExport, RsigExternal, RsigTypeScheme, parse_type_signature,
};

const PRELUDE_SOURCE: &str = include_str!("../../std/prelude.ml");

pub(crate) fn prelude_signature() -> miette::Result<Rsig> {
    let path = Utf8Path::new("compiler/std/prelude.ml");
    let ast = parse_source(path, PRELUDE_SOURCE)?;
    extern_signature(ModuleName::new("Prelude"), ast)
}

fn extern_signature(module: ModuleName, ast: AstProgram) -> miette::Result<Rsig> {
    let mut exports = Vec::new();
    for decl in ast.decls {
        match decl {
            AstDecl::External(external) => {
                let (params, result) = parse_type_signature(&external.type_text);
                let scheme = RsigTypeScheme::from_signature(&params, &result);
                exports.push(RsigExport::External(RsigExternal {
                    name: external.name,
                    params,
                    result,
                    scheme,
                    abi: external.abi,
                    fingerprint: 0,
                }));
            }
            AstDecl::Use(_) | AstDecl::Type(_) | AstDecl::Function(_) => {
                bail!("compiler/std/prelude.ml currently supports external declarations only")
            }
        }
    }
    Ok(Rsig::with_dependencies(
        module,
        Vec::new(),
        Vec::new(),
        exports,
    ))
}

#[cfg(test)]
mod tests {
    use super::prelude_signature;

    #[test]
    fn prelude_source_loads_as_binary_signature_data() {
        let rsig = prelude_signature().unwrap();
        let names = rsig
            .exports
            .iter()
            .map(|export| export.name().to_owned())
            .collect::<Vec<_>>();

        assert_eq!(rsig.module.as_str(), "Prelude");
        assert!(names.contains(&"dbg".to_owned()));
        assert!(names.contains(&"send".to_owned()));
        assert!(names.contains(&"string_concat".to_owned()));
    }
}
