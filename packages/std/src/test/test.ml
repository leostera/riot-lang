module Runner = Runner
module Assertions = Assertions
module Cli = Cli

type test_result = Test_case.test_result

type test_type =
  | UnitTest
  | Property of { examples: int }

type test_case = Test_case.t

let case = Test_case.case

let property = Test_case.property

let skip = Test_case.skip

let todo = Test_case.todo

include Assertions
