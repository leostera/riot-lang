fn main() {
  let worker = spawn { receive { 1 -> () } };
  send(worker, "oops")
}
