// TinyML's initial string helper surface. More operations use Host intrinsics.

let concat = fn (left: string) -> fn (right: string) -> left + right
