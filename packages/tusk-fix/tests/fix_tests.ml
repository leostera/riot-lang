open Std

let tests =
  [
    Test.case "apply single edit" (fun () ->
        let source = "open Stdlib\n" in
        let span = Syn.Ceibo.Span.make ~start:5 ~end_:11 in
        let edit = Tusk_fix.Fix.make_text_edit ~span ~new_text:"Std" in
        let actual =
          Tusk_fix.Fix.apply_edit ~source edit
          |> Result.expect ~msg:"apply single edit failed"
        in
        Test.assert_equal ~expected:"open Std\n" ~actual;
        Ok ());
    Test.case "apply multiple edits in descending span order" (fun () ->
        let source = "open Stdlib\nlet q : int Queue.t = Queue.create ()\n" in
        let fixes =
          [
            Tusk_fix.Fix.make ~title:"replace Queue"
              ~edits:
                [
                  Tusk_fix.Fix.make_text_edit
                    ~span:(Syn.Ceibo.Span.make ~start:24 ~end_:29)
                    ~new_text:"Std.Collections.Queue";
                  Tusk_fix.Fix.make_text_edit
                    ~span:(Syn.Ceibo.Span.make ~start:34 ~end_:39)
                    ~new_text:"Std.Collections.Queue";
                ];
            Tusk_fix.Fix.make ~title:"replace Stdlib"
              ~edits:
                [
                  Tusk_fix.Fix.make_text_edit
                    ~span:(Syn.Ceibo.Span.make ~start:5 ~end_:11)
                    ~new_text:"Std";
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
    Test.case "reject overlapping edits" (fun () ->
        let source = "open Stdlib\n" in
        let fix =
          Tusk_fix.Fix.make ~title:"bad overlap"
            ~edits:
              [
                Tusk_fix.Fix.make_text_edit
                  ~span:(Syn.Ceibo.Span.make ~start:5 ~end_:8)
                  ~new_text:"Std";
                Tusk_fix.Fix.make_text_edit
                  ~span:(Syn.Ceibo.Span.make ~start:7 ~end_:11)
                  ~new_text:"Std";
              ]
        in
        Test.assert_error (Tusk_fix.Fix.apply_fix ~source fix);
        Ok ());
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"tusk-fix:fix" ~tests ~args:Env.args)
    ~args:Env.args ()
