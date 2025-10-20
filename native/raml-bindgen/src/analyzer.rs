use anyhow::Result;
use syn::{File, Item, ItemFn, ItemStruct, ItemEnum, Visibility, FnArg, ReturnType};
use crate::types::*;

pub struct CrateAnalyzer {
    bindings: Bindings,
}

impl CrateAnalyzer {
    pub fn new() -> Self {
        Self {
            bindings: Bindings::new(),
        }
    }
    
    pub fn analyze_file(&mut self, content: &str) -> Result<()> {
        let syntax = syn::parse_file(content)?;
        self.visit_file(&syntax);
        Ok(())
    }
    
    pub fn bindings(&self) -> &Bindings {
        &self.bindings
    }
    
    fn visit_file(&mut self, file: &File) {
        for item in &file.items {
            match item {
                Item::Fn(func) => self.visit_function(func),
                Item::Struct(s) => self.visit_struct(s),
                Item::Enum(e) => self.visit_enum(e),
                _ => {}
            }
        }
    }
    
    fn visit_function(&mut self, func: &ItemFn) {
        if !is_public(&func.vis) {
            return;
        }
        
        let mut params = Vec::new();
        for arg in &func.sig.inputs {
            if let FnArg::Typed(pat_type) = arg {
                if let syn::Pat::Ident(ident) = &*pat_type.pat {
                    let param = Param {
                        name: ident.ident.to_string(),
                        ty: rust_type_to_type(&pat_type.ty),
                    };
                    params.push(param);
                }
            }
        }
        
        let return_type = match &func.sig.output {
            ReturnType::Default => Type::Unit,
            ReturnType::Type(_, ty) => rust_type_to_type(ty),
        };
        
        let docs = extract_docs(&func.attrs);
        
        let function = Function {
            name: to_snake_case(&func.sig.ident.to_string()),
            rust_name: func.sig.ident.to_string(),
            params,
            return_type,
            docs,
            is_unsafe: func.sig.unsafety.is_some(),
        };
        
        self.bindings.functions.push(function);
    }
    
    fn visit_struct(&mut self, s: &ItemStruct) {
        if !is_public(&s.vis) {
            return;
        }
        
        if !has_derive_value(&s.attrs) {
            return;
        }
        
        let mut fields = Vec::new();
        if let syn::Fields::Named(named) = &s.fields {
            for field in &named.named {
                if let Some(ident) = &field.ident {
                    fields.push(Field {
                        name: ident.to_string(),
                        ty: rust_type_to_type(&field.ty),
                        docs: extract_docs(&field.attrs),
                    });
                }
            }
        }
        
        let type_def = TypeDef {
            name: to_snake_case(&s.ident.to_string()),
            rust_name: s.ident.to_string(),
            kind: TypeKind::Struct { fields },
            docs: extract_docs(&s.attrs),
        };
        
        self.bindings.types.push(type_def);
    }
    
    fn visit_enum(&mut self, e: &ItemEnum) {
        if !is_public(&e.vis) {
            return;
        }
        
        if !has_derive_value(&e.attrs) {
            return;
        }
        
        let mut variants = Vec::new();
        for variant in &e.variants {
            let fields = match &variant.fields {
                syn::Fields::Unit => Vec::new(),
                syn::Fields::Unnamed(fields) => {
                    fields.unnamed.iter()
                        .map(|f| rust_type_to_type(&f.ty))
                        .collect()
                }
                syn::Fields::Named(_) => continue,
            };
            
            variants.push(Variant {
                name: variant.ident.to_string(),
                fields,
                docs: extract_docs(&variant.attrs),
            });
        }
        
        let type_def = TypeDef {
            name: to_snake_case(&e.ident.to_string()),
            rust_name: e.ident.to_string(),
            kind: TypeKind::Enum { variants },
            docs: extract_docs(&e.attrs),
        };
        
        self.bindings.types.push(type_def);
    }
}

fn is_public(vis: &Visibility) -> bool {
    matches!(vis, Visibility::Public(_))
}

fn has_derive_value(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|attr| {
        if attr.path().is_ident("derive") {
            if let syn::Meta::List(meta) = &attr.meta {
                return meta.tokens.to_string().contains("Value");
            }
        }
        false
    })
}

fn extract_docs(attrs: &[syn::Attribute]) -> Option<String> {
    let mut docs = Vec::new();
    for attr in attrs {
        if attr.path().is_ident("doc") {
            if let syn::Meta::NameValue(meta) = &attr.meta {
                if let syn::Expr::Lit(expr_lit) = &meta.value {
                    if let syn::Lit::Str(lit) = &expr_lit.lit {
                        docs.push(lit.value().trim().to_string());
                    }
                }
            }
        }
    }
    
    if docs.is_empty() {
        None
    } else {
        Some(docs.join("\n"))
    }
}

fn rust_type_to_type(ty: &syn::Type) -> Type {
    match ty {
        syn::Type::Path(path) => {
            let segment = path.path.segments.last().unwrap();
            let ident = segment.ident.to_string();
            
            match ident.as_str() {
                "bool" => Type::Bool,
                "i8" => Type::I8,
                "i16" => Type::I16,
                "i32" => Type::I32,
                "i64" => Type::I64,
                "isize" => Type::Isize,
                "u8" => Type::U8,
                "u16" => Type::U16,
                "u32" => Type::U32,
                "u64" => Type::U64,
                "usize" => Type::Usize,
                "f32" => Type::F32,
                "f64" => Type::F64,
                "String" => Type::String,
                "Vec" => {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                            return Type::Vec(Box::new(rust_type_to_type(inner)));
                        }
                    }
                    Type::Named("Vec".to_string())
                }
                "Option" => {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                            return Type::Option(Box::new(rust_type_to_type(inner)));
                        }
                    }
                    Type::Named("Option".to_string())
                }
                "Result" => {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        let mut iter = args.args.iter();
                        if let (Some(syn::GenericArgument::Type(ok)), Some(syn::GenericArgument::Type(err))) 
                            = (iter.next(), iter.next()) {
                            return Type::Result {
                                ok: Box::new(rust_type_to_type(ok)),
                                err: Box::new(rust_type_to_type(err)),
                            };
                        }
                    }
                    Type::Named("Result".to_string())
                }
                _ => Type::Named(ident),
            }
        }
        syn::Type::Tuple(tuple) => {
            if tuple.elems.is_empty() {
                Type::Unit
            } else {
                Type::Tuple(tuple.elems.iter().map(rust_type_to_type).collect())
            }
        }
        syn::Type::Reference(ref_type) => {
            Type::Reference {
                mutable: ref_type.mutability.is_some(),
                inner: Box::new(rust_type_to_type(&ref_type.elem)),
            }
        }
        _ => Type::Named("Unknown".to_string()),
    }
}

fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, ch) in s.chars().enumerate() {
        if ch.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(ch.to_lowercase().next().unwrap());
        } else {
            result.push(ch);
        }
    }
    result
}
