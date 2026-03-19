use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(Value)]
pub fn derive_value(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    
    match &input.data {
        Data::Struct(data_struct) => derive_struct_value(&input, data_struct),
        Data::Enum(data_enum) => derive_enum_value(&input, data_enum),
        Data::Union(_) => panic!("Unions are not supported"),
    }
}

fn derive_struct_value(input: &DeriveInput, data_struct: &syn::DataStruct) -> TokenStream {
    let name = &input.ident;
    
    let fields = match &data_struct.fields {
        Fields::Named(fields) => &fields.named,
        Fields::Unnamed(_) => panic!("Tuple structs not yet supported"),
        Fields::Unit => panic!("Unit structs cannot be converted to OCaml values"),
    };
    
    let field_count = fields.len();
    let field_names: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    
    let into_impl = quote! {
        impl Into<riot_core::Value> for #name {
            fn into(self) -> riot_core::Value {
                unsafe {
                    let layout = std::alloc::Layout::from_size_align_unchecked(
                        std::mem::size_of::<riot_core::BlockHeader>() + 
                        #field_count * std::mem::size_of::<riot_core::Value>(),
                        std::mem::align_of::<riot_core::BlockHeader>()
                    );
                    
                    let ptr = std::alloc::alloc(layout) as *mut riot_core::Block;
                    
                    let header = riot_core::BlockHeader::new(
                        #field_count,
                        0,
                        riot_core::GcColor::White
                    );
                    
                    std::ptr::write(
                        ptr as *mut riot_core::BlockHeader,
                        header
                    );
                    
                    let block = &mut *ptr;
                    
                    let mut idx = 0;
                    #(
                        block.set_field(idx, self.#field_names.into());
                        idx += 1;
                    )*
                    
                    riot_core::Value::from_block_ptr(ptr)
                }
            }
        }
    };
    
    let try_from_impl = quote! {
        impl TryFrom<riot_core::Value> for #name {
            type Error = &'static str;
            
            fn try_from(value: riot_core::Value) -> Result<Self, Self::Error> {
                if !value.is_block() {
                    return Err("Expected a block");
                }
                
                let block_ptr = value.as_block().ok_or("Not a block")?;
                let block = unsafe { &*block_ptr };
                
                if block.tag() != 0 {
                    return Err("Expected record tag (0)");
                }
                
                if block.size() != #field_count {
                    return Err("Wrong number of fields");
                }
                
                unsafe {
                    let mut idx = 0;
                    Ok(#name {
                        #(
                            #field_names: {
                                let val = block.field(idx);
                                idx += 1;
                                val.try_into().map_err(|_| "Field conversion failed")?
                            }
                        ),*
                    })
                }
            }
        }
    };
    
    let expanded = quote! {
        #into_impl
        #try_from_impl
    };
    
    TokenStream::from(expanded)
}

fn derive_enum_value(input: &DeriveInput, data_enum: &syn::DataEnum) -> TokenStream {
    let name = &input.ident;
    let variants = &data_enum.variants;
    
    let mut constant_count = 0;
    
    for variant in variants.iter() {
        match &variant.fields {
            Fields::Unit => constant_count += 1,
            _ => break,
        }
    }
    
    let into_arms = variants.iter().enumerate().map(|(idx, variant)| {
        let variant_name = &variant.ident;
        
        match &variant.fields {
            Fields::Unit => {
                quote! {
                    #name::#variant_name => riot_core::Value::int(#idx as isize),
                }
            }
            Fields::Unnamed(fields) => {
                let field_count = fields.unnamed.len();
                let tag = idx - constant_count;
                let field_names: Vec<_> = (0..field_count)
                    .map(|i| syn::Ident::new(&format!("f{}", i), variant_name.span()))
                    .collect();
                
                quote! {
                    #name::#variant_name(#(#field_names),*) => {
                        unsafe {
                            let layout = std::alloc::Layout::from_size_align_unchecked(
                                std::mem::size_of::<riot_core::BlockHeader>() + 
                                #field_count * std::mem::size_of::<riot_core::Value>(),
                                std::mem::align_of::<riot_core::BlockHeader>()
                            );
                            
                            let ptr = std::alloc::alloc(layout) as *mut riot_core::Block;
                            
                            let header = riot_core::BlockHeader::new(
                                #field_count,
                                #tag as u8,
                                riot_core::GcColor::White
                            );
                            
                            std::ptr::write(
                                ptr as *mut riot_core::BlockHeader,
                                header
                            );
                            
                            let block = &mut *ptr;
                            
                            let mut idx = 0;
                            #(
                                block.set_field(idx, #field_names.into());
                                idx += 1;
                            )*
                            
                            riot_core::Value::from_block_ptr(ptr)
                        }
                    },
                }
            }
            Fields::Named(_) => panic!("Named fields in enum variants not supported"),
        }
    });
    
    let unit_try_from_arms = variants.iter().take(constant_count).enumerate().map(|(idx, variant)| {
        let variant_name = &variant.ident;
        quote! {
            #idx => Ok(#name::#variant_name),
        }
    });
    
    let block_try_from_arms = variants.iter().skip(constant_count).enumerate().map(|(idx, variant)| {
        let variant_name = &variant.ident;
        
        if let Fields::Unnamed(fields) = &variant.fields {
            let field_count = fields.unnamed.len();
            let field_names: Vec<_> = (0..field_count)
                .map(|i| syn::Ident::new(&format!("f{}", i), variant_name.span()))
                .collect();
            let field_indices: Vec<_> = (0..field_count).collect();
            
            quote! {
                (#idx, #field_count) => {
                    unsafe {
                        #(
                            let #field_names = block.field(#field_indices).try_into()
                                .map_err(|_| "Field conversion failed")?;
                        )*
                        Ok(#name::#variant_name(#(#field_names),*))
                    }
                },
            }
        } else {
            unreachable!()
        }
    });
    
    let into_impl = quote! {
        impl Into<riot_core::Value> for #name {
            fn into(self) -> riot_core::Value {
                match self {
                    #(#into_arms)*
                }
            }
        }
    };
    
    let try_from_impl = quote! {
        impl TryFrom<riot_core::Value> for #name {
            type Error = &'static str;
            
            fn try_from(value: riot_core::Value) -> Result<Self, Self::Error> {
                if value.is_int() {
                    let tag = value.as_int();
                    if tag < 0 || tag >= #constant_count as isize {
                        return Err("Invalid constant constructor tag");
                    }
                    
                    match tag as usize {
                        #(#unit_try_from_arms)*
                        _ => Err("Unknown variant")
                    }
                } else if value.is_block() {
                    let block_ptr = value.as_block().ok_or("Not a block")?;
                    let block = unsafe { &*block_ptr };
                    
                    let tag = block.tag() as usize;
                    let size = block.size();
                    
                    match (tag, size) {
                        #(#block_try_from_arms)*
                        _ => Err("Unknown variant or wrong size")
                    }
                } else {
                    Err("Invalid value type")
                }
            }
        }
    };
    
    let expanded = quote! {
        #into_impl
        #try_from_impl
    };
    
    TokenStream::from(expanded)
}
