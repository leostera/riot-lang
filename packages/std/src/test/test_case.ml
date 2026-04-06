open Global

type ctx = Test_context.t

type test_result =
  Pass
  | Fail of string
  | Error of exn

type test_type =
  | UnitTest
  | Property of { examples: int }

type size =
  | Small
  | Long

type reliability =
  | Stable
  | Flaky of { retry_attempts: int }

type t = {
  name: string;
  test_type: test_type;
  size: size;
  reliability: reliability;
  fn: ctx -> (unit, string) result;
  skip: bool;
}

let case = fun ?(size = Small) ?(reliability = Stable) name fn ->
  {
    name;
    test_type = UnitTest;
    size;
    reliability;
    fn;
    skip = false;
  }

let property = fun ?(size = Small) ?(reliability = Stable) name ~examples fn ->
  {
    name;
    test_type = Property { examples };
    size;
    reliability;
    fn;
    skip = false;
  }

let skip = fun ?(size = Small) ?(reliability = Stable) name fn ->
  {
    name;
    test_type = UnitTest;
    size;
    reliability;
    fn;
    skip = true;
  }

let todo = fun ?(size = Small) ?(reliability = Stable) name ->
  {
    name;
    test_type = UnitTest;
    size;
    reliability;
    fn = (fun _ctx -> Result.Error "todo");
    skip = false;
  }
