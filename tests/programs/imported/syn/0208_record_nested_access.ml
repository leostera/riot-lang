fn main() {
  let obj = Obj { field: Obj { subfield: 7 } };
  let x = obj.field.subfield;
  dbg(x)
}
