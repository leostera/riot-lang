open Global

type t = {
  description: string;
  cases: Bench_case.t list;
  config: Bench_case.bench_config;
}

let compare = fun description cases -> {description;cases;config = Bench_case.default_config;}

let compare_with_config = fun ~config description cases -> {description;cases;config;}
