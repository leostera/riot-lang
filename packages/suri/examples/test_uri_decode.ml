open Std

let () =
  Miniriot.run ~main:(fun ~args:_ ->
    println "Testing Net.Uri.percent_decode:";
    println "=================================";
    
    let test_cases = [
      ("Screenshot%202025-11-05%20at%2018.19.04.png", "Screenshot with spaces");
      ("/browse/assets/Screenshot%202025-11-05%20at%2018.19.04.png", "Full path");
      ("Hello%20World", "Simple spaces");
      ("test%2Fpath", "Encoded slash");
      ("assets/Screenshot%202025-11-05%20at%2018.19.04.png", "Relative path");
    ] in
    
    List.iter (fun (input, desc) ->
      let decoded = Net.Uri.percent_decode input in
      println "";
      println desc;
      println (String.concat "" ["  Input:   '"; input; "'"]);
      println (String.concat "" ["  Decoded: '"; decoded; "'"]);
      println (String.concat "" ["  Length input: "; string_of_int (String.length input)]);
      println (String.concat "" ["  Length decoded: "; string_of_int (String.length decoded)])
    ) test_cases;
    
    println "";
    println "=================================";
    Ok ()
  ) ~args:Env.args ()
