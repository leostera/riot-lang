fn pair(name: string) -> (string, i64) { (name, 42) }

fn numbers() -> i64 list { [1, 2, 3] }

fn main() {
  dbg("riot" == "riot");
  dbg(pair("riot") == pair("riot"));
  dbg(numbers() == [1, 2, 3]);
  dbg(Point { x: 10, y: 20 } == Point { x: 10, y: 20 });
  dbg(Point { x: 10, y: 20 } == Point { x: 10, y: 21 })
}
