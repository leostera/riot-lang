open Global

type test_result = Pass | Fail of string | Error of exn

type test_type = 
  | UnitTest
  | Property of { examples: int }

type t = { 
  name : string; 
  test_type : test_type;
  fn : unit -> (unit, string) result; 
  skip : bool 
}

let case name fn = { 
  name; 
  test_type = UnitTest;
  fn; 
  skip = false 
}

let property name ~examples fn = { 
  name; 
  test_type = Property { examples };
  fn; 
  skip = false 
}

let skip name fn = { 
  name; 
  test_type = UnitTest;
  fn; 
  skip = true 
}

let todo name = { 
  name; 
  test_type = UnitTest;
  fn = (fun () -> Result.Error "todo"); 
  skip = false 
}
