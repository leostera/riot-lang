open Std
module Asm = Asm.AArch64
module Doc = Asm.Doc
module HashSet = Collections.HashSet

let ( let* ) = Result.and_then

type string_constant = {
  label: string;
  value: string;
}

let value_register = Asm.Register.x 9

let address_register = Asm.Register.x 10

let callee_register = Asm.Register.x 16

let is_ascii_digit = fun char -> char >= '0' && char <= '9'

let is_ascii_lowercase = fun char -> char >= 'a' && char <= 'z'

let is_ascii_uppercase = fun char -> char >= 'A' && char <= 'Z'

let is_macho_symbol_char = fun char ->
  is_ascii_digit char || is_ascii_lowercase char || is_ascii_uppercase char || char = '_' || char = '.'

let hex_digit = fun value ->
  if value < 10 then
    Char.chr (Char.code '0' + value)
  else
    Char.chr (Char.code 'a' + (value - 10))

let hex_escape = fun code ->
  String.init 2
    (fun index ->
      if index = 0 then
        hex_digit (code lsr 4)
      else
        hex_digit (code land 0x0f))

let encode_symbol_name = fun name ->
  let rec loop index parts =
    if index = String.length name then
      String.concat "" (List.rev parts)
    else
      loop (index + 1) (hex_escape (Char.code name.[index]) :: parts)
  in
  loop 0 []

let mangle_symbol = fun name ->
  if String.for_all is_macho_symbol_char name then
    format Format.[ str "_"; str name ]
  else
    format Format.[ str "_raml$"; str (encode_symbol_name name) ]

let procedure_symbol = fun (procedure: Lir.Procedure.t) ->
  match procedure.kind with
  | Lir.Procedure.Entry -> "_main"
  | Lir.Procedure.Function -> mangle_symbol procedure.name

let home_of_name = fun (layout: Lir.Frame.t) name ->
  layout.homes |> List.find_map
    (fun (binding: Lir.Home_binding.t) ->
      if String.equal binding.name name then
        Some binding.home
      else
        None) |> Option.expect ~msg:(format Format.[ str "missing home for register "; str name ])

let home_address = fun home ->
  match home with
  | Lir.Home.Register _ -> panic "aarch64 emitter: register home has no stack address"
  | Lir.Home.Stack_slot slot -> Asm.Address.offset ~base:Asm.Register.sp ~offset:slot.offset

let register_of_home = fun home ->
  let parse_index name =
    if String.length name < 2 then
      None
    else if name.[0] = 'x' then
      String.sub name 1 (String.length name - 1) |> int_of_string_opt
    else
      None
  in
  match home with
  | Lir.Home.Register name -> (
      match parse_index name with
      | Some index when index >= 0 && index <= 28 -> Ok (Asm.Register.x index)
      | _ -> Error (format Format.[ str "unsupported physical register home: "; str name ])
    )
  | Lir.Home.Stack_slot _ -> Error "stack slot does not name a physical register"

let instruction = Doc.Item.instruction

let directive = fun name ?(args = []) () -> Doc.Item.directive name ~args ()

let label = Doc.Item.label

let blank = Doc.Item.blank

let octal_escape = fun code ->
  let digit shift = (code lsr shift) land 0b111 in
  format Format.[ str "\\"; int (digit 6); int (digit 3); int (digit 0); ]

let escape_string = fun value ->
  let rec loop index parts =
    if index = String.length value then
      String.concat "" (List.rev parts)
    else
      let char = String.get value index in
      let escaped =
        match char with
        | '"' ->
            "\\\""
        | '\\' ->
            "\\\\"
        | '\n' ->
            "\\n"
        | '\r' ->
            "\\r"
        | '\t' ->
            "\\t"
        | char ->
            let code = Char.code char in
            if code >= 32 then
              if code < 127 then
                String.make 1 char
              else
                octal_escape code
            else
              octal_escape code
      in
      loop (index + 1) (escaped :: parts)
  in
  loop 0 []

let int64_literal_of_literal = fun literal ->
  match literal with
  | Lir.Literal.Unit -> 0L
  | Lir.Literal.Bool value ->
      if value then
        1L
      else
        0L
  | Lir.Literal.Int value -> Int64.of_int value
  | Lir.Literal.Float value -> Int64.bits_of_float value
  | Lir.Literal.String _ -> 0L

let move_int64_into = fun register value ->
  let chunk shift = Int64.(to_int (logand (shift_right_logical value shift) 0xffffL)) in
  let parts = [ (0, chunk 0); (16, chunk 16); (32, chunk 32); (48, chunk 48) ] in
  match
    List.find_opt
      (fun (_, imm) ->
        if imm = 0 then
          false
        else
          true)
      parts
  with
  | None -> [ instruction (Asm.Instruction.Movz { dst = register; imm = 0; shift = 0 }) ]
  | Some (first_shift, first_imm) ->
      instruction (Asm.Instruction.Movz { dst = register; imm = first_imm; shift = first_shift })
      :: (
        parts |> List.filter_map
          (fun (shift, imm) ->
            if shift = first_shift then
              None
            else if imm = 0 then
              None
            else
              Some (instruction (Asm.Instruction.Movk { dst = register; imm; shift })))
      )

let load_symbol_address = fun register symbol ->
  [
    instruction (Asm.Instruction.Adrp { dst = register; symbol });
    instruction (Asm.Instruction.Add_pageoff { dst = register; lhs = register; symbol });
  ]

let load_global_value = fun register symbol ->
  load_symbol_address address_register symbol
  @ [
    instruction
      (Asm.Instruction.Ldr {
        dst = register;
        address = Asm.Address.offset ~base:address_register ~offset:0
      })
  ]

let store_global_value = fun symbol register ->
  load_symbol_address address_register symbol
  @ [
    instruction
      (Asm.Instruction.Str {
        src = register;
        address = Asm.Address.offset ~base:address_register ~offset:0
      })
  ]

let rec materialize_operand = fun layout strings register operand ->
  match operand with
  | Lir.Operand.Register name ->
      Error (format Format.[ str "unassigned virtual register reached emitter: "; str name ])
  | Lir.Operand.Home home -> (
      match home with
      | Lir.Home.Stack_slot _ -> Ok [
        instruction (Asm.Instruction.Ldr { dst = register; address = home_address home })
      ]
      | Lir.Home.Register _ ->
          let* src = register_of_home home in
          if src = register then
            Ok []
          else
            Ok [ instruction (Asm.Instruction.Mov { dst = register; src }) ]
    )
  | Lir.Operand.Global name ->
      Ok (load_global_value register (mangle_symbol name))
  | Lir.Operand.Symbol_address name ->
      Ok (load_symbol_address register (mangle_symbol name))
  | Lir.Operand.Literal literal ->
      materialize_literal layout strings register literal

and materialize_literal = fun _layout strings register literal ->
  match literal with
  | Lir.Literal.String value -> (
      match
        List.find_map
          (fun constant ->
            if String.equal constant.value value then
              Some constant.label
            else
              None)
          strings
      with
      | Some label -> Ok (load_symbol_address register label)
      | None -> Ok (load_symbol_address register (mangle_symbol "__missing_string_literal"))
    )
  | _ -> Ok (move_int64_into register (int64_literal_of_literal literal))

let store_destination = fun layout destination register ->
  match destination with
  | Lir.Destination.Home home -> (
      match home with
      | Lir.Home.Stack_slot _ -> Ok [
        instruction (Asm.Instruction.Str { src = register; address = home_address home })
      ]
      | Lir.Home.Register _ ->
          let* dst = register_of_home home in
          if dst = register then
            Ok []
          else
            Ok [ instruction (Asm.Instruction.Mov { dst; src = register }) ]
    )
  | Lir.Destination.Register name -> Error (format
    Format.[ str "unassigned virtual destination reached emitter: "; str name ])

let emit_prologue = fun (layout: Lir.Frame.t) ->
  if not layout.frame_required then
    []
  else
    let prologue = [
      instruction
        (Asm.Instruction.Stp {
          src1 = Asm.Register.fp;
          src2 = Asm.Register.lr;
          address = Asm.Address.pre_index ~base:Asm.Register.sp ~offset:(-16)
        });
      instruction (Asm.Instruction.Mov { dst = Asm.Register.fp; src = Asm.Register.sp });
    ] in
    if layout.frame_size = 0 then
      prologue
    else
      prologue
      @ [
        instruction
          (Asm.Instruction.Sub_imm {
            dst = Asm.Register.sp;
            lhs = Asm.Register.sp;
            imm = layout.frame_size
          })
      ]

let emit_epilogue = fun (layout: Lir.Frame.t) ->
  if not layout.frame_required then
    [ instruction Asm.Instruction.Ret ]
  else
    let body =
      if layout.frame_size = 0 then
        []
      else
        [
          instruction
            (Asm.Instruction.Add_imm {
              dst = Asm.Register.sp;
              lhs = Asm.Register.sp;
              imm = layout.frame_size
            });
        ]
    in
    body
    @ [
      instruction
        (Asm.Instruction.Ldp {
          dst1 = Asm.Register.fp;
          dst2 = Asm.Register.lr;
          address = Asm.Address.post_index ~base:Asm.Register.sp ~offset:16
        });
      instruction Asm.Instruction.Ret;
    ]

let emit_parameter_saves = fun layout params ->
  if List.length params > 8 then
    Error "aarch64-apple-darwin emitter supports at most 8 parameters per procedure"
  else
    let rec loop index params =
      match params with
      | [] -> Ok []
      | name :: rest ->
          let home = home_of_name layout name in
          let* current =
            match home with
            | Lir.Home.Stack_slot _ -> Ok [
              instruction
                (Asm.Instruction.Str { src = Asm.Register.x index; address = home_address home })
            ]
            | Lir.Home.Register _ ->
                let* dst = register_of_home home in
                if dst = Asm.Register.x index then
                  Ok []
                else
                  Ok [ instruction (Asm.Instruction.Mov { dst; src = Asm.Register.x index }) ]
          in
          let* next = loop (index + 1) rest in
          Ok (current @ next)
    in
    loop 0 params

let emit_call_arguments = fun layout strings arguments ->
  if List.length arguments > 8 then
    Error "aarch64-apple-darwin emitter supports at most 8 call arguments"
  else
    let rec loop index arguments =
      match arguments with
      | [] -> Ok []
      | argument :: rest ->
          let* current = materialize_operand layout strings (Asm.Register.x index) argument in
          let* next = loop (index + 1) rest in
          Ok (current @ next)
    in
    loop 0 arguments

let emit_callee = fun layout strings callee ->
  match callee with
  | Lir.Callee.Direct name -> Ok ([], instruction (Asm.Instruction.Bl (mangle_symbol name)))
  | Lir.Callee.Indirect operand ->
      let* load = materialize_operand layout strings callee_register operand in
      Ok (load, instruction (Asm.Instruction.Blr callee_register))

let emit_return = fun layout strings operand ->
  let* body =
    match operand with
    | None -> Ok []
    | Some operand -> materialize_operand layout strings (Asm.Register.x 0) operand
  in
  Ok (body @ emit_epilogue layout)

let emit_instruction = fun layout strings (procedure: Lir.Procedure.t) instruction_ ->
  match instruction_ with
  | Lir.Instruction.Label name ->
      if String.equal name procedure.name then
        Ok []
      else
        Ok [ label name ]
  | Lir.Instruction.Comment _ ->
      Ok []
  | Lir.Instruction.Move { dst; src } ->
      let* body = materialize_operand layout strings value_register src in
      let* store = store_destination layout dst value_register in
      Ok (body @ store)
  | Lir.Instruction.Store_global { symbol; src } ->
      let* body = materialize_operand layout strings value_register src in
      Ok (body @ store_global_value (mangle_symbol symbol) value_register)
  | Lir.Instruction.Call { dst; callee; arguments } ->
      let* argument_setup = emit_call_arguments layout strings arguments in
      let* (callee_setup, call_instruction) = emit_callee layout strings callee in
      let store_result =
        match dst with
        | None -> Ok []
        | Some dst -> store_destination layout dst (Asm.Register.x 0)
      in
      let* store_result = store_result in
      Ok (argument_setup @ callee_setup @ [ call_instruction ] @ store_result)
  | Lir.Instruction.Branch_if_zero { operand; target } ->
      let* body = materialize_operand layout strings value_register operand in
      Ok (body @ [ instruction (Asm.Instruction.Cbz { src = value_register; label = target }) ])
  | Lir.Instruction.Jump target ->
      Ok [ instruction (Asm.Instruction.B target) ]
  | Lir.Instruction.Return operand ->
      emit_return layout strings operand

let has_explicit_return = fun (procedure: Lir.Procedure.t) ->
  List.exists
    (fun instruction_ ->
      match instruction_ with
      | Lir.Instruction.Return _ -> true
      | _ -> false)
    procedure.body

let emit_default_return = fun layout (procedure: Lir.Procedure.t) ->
  match procedure.kind with
  | Lir.Procedure.Function -> emit_epilogue layout
  | Lir.Procedure.Entry -> move_int64_into (Asm.Register.x 0) 0L @ emit_epilogue layout

let emit_procedure = fun strings (procedure: Lir.Procedure.t) ->
  let layout = procedure.frame in
  let* parameter_saves = emit_parameter_saves layout procedure.params in
  let* body =
    List.fold_left
      (fun acc instruction_ ->
        let* acc = acc in
        let* emitted = emit_instruction layout strings procedure instruction_ in
        Ok (acc @ emitted))
      (Ok [])
      procedure.body
  in
  let symbol = procedure_symbol procedure in
  let default_return =
    if has_explicit_return procedure then
      []
    else
      emit_default_return layout procedure
  in
  Ok ([
    directive ".globl" ~args:[ symbol ] ();
    directive ".p2align" ~args:[ "2" ] ();
    label symbol;
  ]
  @ emit_prologue layout
  @ parameter_saves
  @ body
  @ default_return
  @ [ blank ])

let procedure_uses_poll_stub = fun (procedure: Lir.Procedure.t) ->
  List.exists
    (fun instruction_ ->
      match instruction_ with
      | Lir.Instruction.Call { callee=Lir.Callee.Direct "raml_poll"; _ } -> true
      | _ -> false)
    procedure.body

let program_uses_poll_stub = fun (program: Lir.Program.t) ->
  List.exists procedure_uses_poll_stub program.procedures

let emit_poll_stub = fun () ->
  let symbol = mangle_symbol "raml_poll" in
  [ directive ".p2align" ~args:[ "2" ] (); label symbol; instruction Asm.Instruction.Ret; blank; ]

let add_string_constant = fun constants value ->
  match
    List.find_map
      (fun constant ->
        if String.equal constant.value value then
          Some constant
        else
          None)
      constants
  with
  | Some _ -> constants
  | None -> constants
  @ [ { label = format Format.[ str "L__raml_str_"; int (List.length constants) ]; value }; ]

let rec collect_literal_strings = fun constants literal ->
  match literal with
  | Lir.Literal.String value -> add_string_constant constants value
  | _ -> constants

let collect_operand_strings = fun constants operand ->
  match operand with
  | Lir.Operand.Literal literal -> collect_literal_strings constants literal
  | Lir.Operand.Symbol_address _ -> constants
  | Lir.Operand.Home _ -> constants
  | _ -> constants

let collect_instruction_strings = fun constants instruction_ ->
  match instruction_ with
  | Lir.Instruction.Label _ ->
      constants
  | Lir.Instruction.Comment _ ->
      constants
  | Lir.Instruction.Move { src; _ } ->
      collect_operand_strings constants src
  | Lir.Instruction.Store_global { src; _ } ->
      collect_operand_strings constants src
  | Lir.Instruction.Call { callee; arguments; _ } ->
      let constants =
        match callee with
        | Lir.Callee.Direct _ -> constants
        | Lir.Callee.Indirect operand -> collect_operand_strings constants operand
      in
      List.fold_left collect_operand_strings constants arguments
  | Lir.Instruction.Branch_if_zero { operand; _ } ->
      collect_operand_strings constants operand
  | Lir.Instruction.Jump _ ->
      constants
  | Lir.Instruction.Return operand ->
      Option.map (collect_operand_strings constants) operand |> Option.unwrap_or ~default:constants

let string_constants_of_program = fun (program: Lir.Program.t) ->
  List.fold_left
    (fun constants (procedure: Lir.Procedure.t) ->
      List.fold_left collect_instruction_strings constants procedure.body)
    []
    program.procedures

type ordered_names = {
  seen: string HashSet.t;
  ordered_rev: string list;
}

let empty_names = fun () -> { seen = HashSet.create (); ordered_rev = [] }

let add_name = fun names name ->
  if HashSet.contains names.seen name then
    names
  else
    (
      let _ = HashSet.insert names.seen name in
      { names with ordered_rev = name :: names.ordered_rev }
    )

let ordered_names = fun names -> List.rev names.ordered_rev

let global_symbols_of_program = fun (program: Lir.Program.t) ->
  List.fold_left
    (fun symbols (procedure: Lir.Procedure.t) ->
      List.fold_left
        (fun symbols instruction_ ->
          match instruction_ with
          | Lir.Instruction.Store_global { symbol; _ } -> add_name symbols symbol
          | _ -> symbols)
        symbols
        procedure.body)
    (empty_names ())
    program.procedures |> ordered_names

let emit_string_constants = fun strings ->
  match strings with
  | [] -> []
  | _ -> [ directive ".section" ~args:[ "__TEXT"; "__cstring"; "cstring_literals" ] (); ]
  @ List.concat_map
    (fun constant ->
      [
        directive ".p2align" ~args:[ "0" ] ();
        label constant.label;
        directive
          ".asciz"
          ~args:[ format Format.[ str "\""; str (escape_string constant.value); str "\"" ] ]
          ();
      ])
    strings
  @ [ blank ]

let emit_global_data = fun globals ->
  match globals with
  | [] -> []
  | _ ->
      [ directive ".data" () ] @ List.concat_map
        (fun symbol ->
          let symbol = mangle_symbol symbol in
          [
            directive ".globl" ~args:[ symbol ] ();
            directive ".p2align" ~args:[ "3" ] ();
            label symbol;
            directive ".quad" ~args:[ "0" ] ();
          ])
        globals @ [ blank ]

let emit_text = fun strings (program: Lir.Program.t) ->
  let* procedures =
    List.fold_left
      (fun acc procedure ->
        let* acc = acc in
        let* emitted = emit_procedure strings procedure in
        Ok (acc @ emitted))
      (Ok [])
      program.procedures
  in
  let poll_stub =
    if program_uses_poll_stub program then
      emit_poll_stub ()
    else
      []
  in
  Ok ([ directive ".text" (); ] @ poll_stub @ procedures)

let emit_program = fun (program: Lir.Program.t) ->
  let strings = string_constants_of_program program in
  let globals = global_symbols_of_program program in
  let* text = emit_text strings program in
  let document = emit_string_constants strings @ emit_global_data globals @ text in
  Ok (Doc.Document.of_items document |> Asm.to_string)
