open Std
open Util

(* Join strings horizontally with alignment *)
let join_horizontal ~pos strs =
  if strs = [] then ""
  else
    (* Calculate max height *)
    let heights = List.map (fun s -> List.length (Ansi.split_lines s)) strs in
    let max_height = List.fold_left max 0 heights in
    
    (* Pad each string to max_height *)
    let pad_to_height str =
      let lines = Ansi.split_lines str in
      let current_height = List.length lines in
      if current_height >= max_height then lines
      else
        let padding_needed = max_height - current_height in
        match pos with
        | `Top -> lines @ List.make ~len:padding_needed ~fn:(fun _ -> "")
        | `Bottom -> List.make ~len:padding_needed ~fn:(fun _ -> "") @ lines
        | `Center ->
            let top_pad = padding_needed / 2 in
            let bottom_pad = padding_needed - top_pad in
            List.make ~len:top_pad ~fn:(fun _ -> "") @ lines @ List.make ~len:bottom_pad ~fn:(fun _ -> "")
    in
    
    let padded = List.map pad_to_height strs in
    
    (* Transpose and join each row *)
    let rec transpose = function
      | [] -> []
      | [] :: _ -> []
      | rows ->
          let heads = List.filter_map (function [] -> None | h :: _ -> Some h) rows in
          let tails = List.filter_map (function [] -> None | _ :: t -> Some t) rows in
          heads :: transpose tails
    in
    
    let rows = transpose padded in
    let joined_rows = List.map (String.concat "") rows in
    String.concat "\n" joined_rows

(* Join strings vertically with alignment *)
let join_vertical ~pos strs =
  if strs = [] then ""
  else
    (* Calculate max width for each string *)
    let widths = List.map (fun s ->
      let lines = Ansi.split_lines s in
      List.fold_left (fun acc line -> max acc (Ansi.width line)) 0 lines
    ) strs in
    let max_width = List.fold_left max 0 widths in
    
    (* Align each string to max_width *)
    let align_str str =
      let lines = Ansi.split_lines str in
      List.map (fun line ->
        match pos with
        | `Left -> Ansi.pad_right ~width:max_width ' ' line
        | `Right -> Ansi.pad_left ~width:max_width ' ' line
        | `Center -> Ansi.pad_center ~width:max_width ' ' line
      ) lines
    in
    
    let aligned = List.concat_map align_str strs in
    String.concat "\n" aligned

(* Place string at specific position in a box *)
let place ~width:box_width ~height:box_height ~h_pos ~v_pos str =
  let lines = Ansi.split_lines str in
  let content_height = List.length lines in
  let content_width = List.fold_left (fun acc line -> max acc (Ansi.width line)) 0 lines in
  
  (* Calculate vertical position (0.0 = top, 0.5 = center, 1.0 = bottom) *)
  let v_padding = box_height - content_height in
  let top_lines = int_of_float (float_of_int v_padding *. v_pos) in
  let bottom_lines = v_padding - top_lines in
  
  (* Calculate horizontal position (0.0 = left, 0.5 = center, 1.0 = right) *)
  let h_padding = box_width - content_width in
  let left_pad = int_of_float (float_of_int h_padding *. h_pos) in
  
  (* Build result *)
  let empty_line = String.make box_width ' ' in
  let top = List.make ~len:(max 0 top_lines) ~fn:(fun _ -> empty_line) in
  let bottom = List.make ~len:(max 0 bottom_lines) ~fn:(fun _ -> empty_line) in
  
  let content = List.map (fun line ->
    let line_width = Ansi.width line in
    let line_padding = box_width - line_width in
    let line_left = int_of_float (float_of_int line_padding *. h_pos) in
    let line_right = line_padding - line_left in
    String.make (max 0 line_left) ' ' ^ line ^ String.make (max 0 line_right) ' '
  ) lines in
  
  String.concat "\n" (top @ content @ bottom)
