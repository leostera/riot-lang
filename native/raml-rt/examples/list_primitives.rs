use raml_rt::runtime::PrimitiveTable;

fn main() {
    let table = PrimitiveTable::new();
    let mut prims = table.list_primitives();
    prims.sort();
    
    println!("RAML Runtime - Registered Primitives");
    println!("=====================================");
    println!("Total: {} primitives\n", prims.len());
    
    for (i, prim) in prims.iter().enumerate() {
        println!("{:3}. {}", i + 1, prim);
    }
}
