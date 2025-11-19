open Std

(** Simple string formatting helper for raml compiler output.
    
    This provides basic sprintf-style formatting since Printf/Format
    modules aren't available in the nostdlib environment. *)

(** Format a string with arguments. Only supports %s and %d placeholders. *)
let format fmt =
  (* This is a simplified implementation that handles common cases *)
  let rec build_string parts = function
    | [] -> String.concat "" (List.rev parts)
    | arg :: rest -> build_string (arg :: parts) rest
  in
  fun args -> build_string [] (fmt :: args)

(** sprintf for single %s *)
let sprintf1 template arg1 =
  let parts = String.split_on_char '%' template in
  match parts with
  | [before; after] ->
      let after_cleaned = 
        if String.length after > 0 && String.get after 0 = 's' 
        then String.sub after 1 (String.length after - 1)
        else if String.length after > 0 && String.get after 0 = 'd'
        then String.sub after 1 (String.length after - 1)
        else after
      in
      before ^ arg1 ^ after_cleaned
  | _ -> template ^ arg1

(** sprintf for two args *)
let sprintf2 template arg1 arg2 =
  sprintf1 (sprintf1 template arg1) arg2

(** sprintf for three args *)  
let sprintf3 template arg1 arg2 arg3 =
  sprintf1 (sprintf2 template arg1 arg2) arg3

(** sprintf for four args *)
let sprintf4 template arg1 arg2 arg3 arg4 =
  sprintf1 (sprintf3 template arg1 arg2 arg3) arg4

(** General sprintf - tries to match %s and %d placeholders with arguments *)
let sprintf template args =
  let rec replace_placeholders str = function
    | [] -> str
    | arg :: rest ->
        (* Find first %s or %d and replace it *)
        let parts = String.split_on_char '%' str in
        let rec process_parts acc = function
          | [] -> String.concat "%" (List.rev acc)
          | part :: remaining ->
              if String.length part > 0 then
                let first_char = String.get part 0 in
                if first_char = 's' || first_char = 'd' then
                  let after = String.sub part 1 (String.length part - 1) in
                  let before_parts = List.rev acc in
                  let before = String.concat "%" before_parts in
                  let remaining_str = String.concat "%" (after :: remaining) in
                  before ^ arg ^ remaining_str
                else
                  process_parts (part :: acc) remaining
              else
                process_parts (part :: acc) remaining
        in
        let new_str = process_parts [] parts in
        replace_placeholders new_str rest
  in
  replace_placeholders template args
