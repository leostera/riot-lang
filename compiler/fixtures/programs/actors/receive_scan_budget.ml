fn main() {
  let worker = spawn {
    receive {
      "go" -> dbg("matched")
    };
    receive {
      msg -> dbg(msg)
    }
  };
  send(worker, "skip01");
  send(worker, "skip02");
  send(worker, "skip03");
  send(worker, "skip04");
  send(worker, "skip05");
  send(worker, "skip06");
  send(worker, "skip07");
  send(worker, "skip08");
  send(worker, "skip09");
  send(worker, "skip10");
  send(worker, "skip11");
  send(worker, "skip12");
  send(worker, "skip13");
  send(worker, "skip14");
  send(worker, "skip15");
  send(worker, "skip16");
  send(worker, "skip17");
  send(worker, "skip18");
  send(worker, "skip19");
  send(worker, "skip20");
  send(worker, "skip21");
  send(worker, "skip22");
  send(worker, "skip23");
  send(worker, "skip24");
  send(worker, "skip25");
  send(worker, "skip26");
  send(worker, "skip27");
  send(worker, "skip28");
  send(worker, "skip29");
  send(worker, "skip30");
  send(worker, "skip31");
  send(worker, "skip32");
  send(worker, "skip33");
  send(worker, "go")
}
