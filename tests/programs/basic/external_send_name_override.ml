external send : String -> unit = "riot_prim_println"

fn main() {
  send("not an actor send")
}
