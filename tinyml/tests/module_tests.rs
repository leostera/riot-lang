use std::{fs, path::PathBuf, time::SystemTime};

use tinyml::{checker, modules::ModuleGraph};

fn temp_dir(name: &str) -> PathBuf {
    let nonce = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("clock is after epoch")
        .as_nanos();
    let path = std::env::temp_dir().join(format!(
        "tinyml-module-tests-{name}-{}-{nonce}",
        std::process::id()
    ));
    fs::create_dir_all(&path).expect("create temporary directory");
    path
}

fn check_graph(root: &std::path::Path) -> Result<(), String> {
    let graph = ModuleGraph::load(root, &[]).map_err(|error| error.to_string())?;
    let mut checked: Vec<checker::ModuleSummary> = Vec::with_capacity(graph.modules.len());
    for module in &graph.modules {
        let imports = checker::ModuleInterface {
            modules: module
                .dependencies
                .iter()
                .map(|index| checker::interface::ModuleDescription {
                    name: graph.modules[*index]
                        .name
                        .as_ref()
                        .expect("imported module has a name")
                        .to_ident(),
                    interface: Box::new(checked[*index].interface.clone()),
                })
                .collect(),
            ..checker::ModuleInterface::empty()
        };
        let summary = checker::Checker::new().check_module_with_interface(&module.ast, &imports);
        if !summary.diagnostics.is_empty() {
            return Err(format!(
                "{}: {:#?}",
                module.path.display(),
                summary.diagnostics
            ));
        }
        checked.push(summary);
    }
    Ok(())
}

#[test]
fn bundled_library_dependencies_typecheck_without_prebuilt_interfaces() {
    let dir = temp_dir("library");
    let root = dir.join("client.ml");
    fs::write(
        &root,
        r#"
        use Prelude.{List, Nil, Cons, Option, Some, Result, Ok}
        use List.{length}
        use Option.{map}
        use Result.{unwrap_or}
        use String.{concat}

        let size = length (Cons(1, Cons(2, Nil)))
        let mapped = map (fn value -> value + 1) (Some(41))
        let unwrapped = unwrap_or 0 (Ok(42))
        let message = concat "hello" " world"
        "#,
    )
    .unwrap();

    check_graph(&root).unwrap_or_else(|error| panic!("{error}"));
}
