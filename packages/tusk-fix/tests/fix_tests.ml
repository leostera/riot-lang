open Std

let synthetic_token ~span ~text =
  let green =
    Syn.Ceibo.Green.make_token ~kind:Syn.SyntaxKind.WHITESPACE ~text
      ~width:(String.length text)
  in
  Syn.Ceibo.Red.new_token green span

let tests =
  [
    Test.case "apply single operation" (fun () ->
        let source = "open Stdlib\n" in
        let span = Syn.Ceibo.Span.make ~start:5 ~end_:11 in
        let token = synthetic_token ~span ~text:"Stdlib" in
        let actual =
          Tusk_fix.Fix.apply_operation ~source
            (Tusk_fix.Fix.replace_token_with_text ~target:token ~text:"Std")
          |> Result.expect ~msg:"apply single operation failed"
        in
        Test.assert_equal ~expected:"open Std\n" ~actual;
        Ok ());
    Test.case "apply multiple operations in descending span order" (fun () ->
        let source = "open Stdlib\nlet q : int Queue.t = Queue.create ()\n" in
        let stdlib_token =
          synthetic_token
            ~span:(Syn.Ceibo.Span.make ~start:5 ~end_:11)
            ~text:"Stdlib"
        in
        let queue_type_token =
          synthetic_token
            ~span:(Syn.Ceibo.Span.make ~start:24 ~end_:29)
            ~text:"Queue"
        in
        let queue_value_token =
          synthetic_token
            ~span:(Syn.Ceibo.Span.make ~start:34 ~end_:39)
            ~text:"Queue"
        in
        let fixes =
          [
            Tusk_fix.Fix.make ~title:"replace Queue"
              ~operations:
                [
                  Tusk_fix.Fix.replace_token_with_text ~target:queue_type_token
                    ~text:"Std.Collections.Queue";
                  Tusk_fix.Fix.replace_token_with_text
                    ~target:queue_value_token ~text:"Std.Collections.Queue";
                ];
            Tusk_fix.Fix.make ~title:"replace Stdlib"
              ~operations:
                [
                  Tusk_fix.Fix.replace_token_with_text ~target:stdlib_token
                    ~text:"Std";
                ];
          ]
        in
        let actual =
          Tusk_fix.Fix.apply_fixes ~source fixes
          |> Result.expect ~msg:"apply multiple edits failed"
        in
        let expected =
          "open Std\nlet q : int Std.Collections.Queue.t = \
           Std.Collections.Queue.create ()\n"
        in
        Test.assert_equal ~expected ~actual;
        Ok ());
    Test.case "reject overlapping operations" (fun () ->
        let source = "open Stdlib\n" in
        let left = synthetic_token ~span:(Syn.Ceibo.Span.make ~start:5 ~end_:8) ~text:"Std" in
        let right = synthetic_token ~span:(Syn.Ceibo.Span.make ~start:7 ~end_:11) ~text:"lib" in
        let fix =
          Tusk_fix.Fix.make ~title:"bad overlap"
            ~operations:
              [
                Tusk_fix.Fix.replace_token_with_text ~target:left ~text:"Std";
                Tusk_fix.Fix.replace_token_with_text ~target:right ~text:"Std";
              ]
        in
        Test.assert_error (Tusk_fix.Fix.apply_fix ~source fix);
        Ok ());
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"tusk-fix:fix" ~tests ~args:Env.args)
    ~args:Env.args ()
