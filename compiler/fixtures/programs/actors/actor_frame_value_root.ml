external gc_collect : i64 = "riot_rt_gc_collect"

fn main() {
  let label = string_concat("kee", "p");
  let actor_id = spawn {
    gc_collect();
    receive { msg -> dbg(label) }
  };
  gc_collect();
  send(actor_id, "go")
}
