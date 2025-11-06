open Std

(** Scene Graph Differential Rendering
    
    Compare scene graphs to determine if rendering is needed.
    This is much cheaper than comparing rendered output.
*)

(** Compare two scene graphs for equality *)
let rec scene_equal a b =
  (* First check structural equality *)
  let open Scene in
  a.rect = b.rect &&
  a.z_index = b.z_index &&
  a.clip = b.clip &&
  (* Then check content *)
  match a.content, b.content with
  | TextNode a_text, TextNode b_text ->
      a_text.text = b_text.text &&
      style_equal a_text.style b_text.style
      
  | Container a_cont, Container b_cont ->
      (* Early return if different number of children *)
      let a_len = List.length a_cont.children in
      let b_len = List.length b_cont.children in
      a_len = b_len &&
      Option.equal style_equal a_cont.style b_cont.style &&
      List.for_all2 scene_equal a_cont.children b_cont.children
      
  | _ -> false

and style_equal a b =
  let open Scene in
  a.fg = b.fg &&
  a.bg = b.bg &&
  a.bold = b.bold &&
  a.italic = b.italic &&
  a.underline = b.underline &&
  a.strikethrough = b.strikethrough &&
  a.reverse = b.reverse

(** Fast hash-based comparison for quick equality checks *)
let scene_hash scene =
  let open Scene in
  let rec hash_node node acc =
    let h = Hashtbl.hash (
      node.rect.x,
      node.rect.y,
      node.rect.width,
      node.rect.height,
      node.z_index
    ) in
    let acc = acc lxor h in
    match node.content with
    | TextNode { text; style } ->
        let text_hash = Hashtbl.hash text in
        let style_hash = Hashtbl.hash style in
        acc lxor text_hash lxor style_hash
        
    | Container { style; children } ->
        let style_hash = match style with
          | None -> 0
          | Some s -> Hashtbl.hash s
        in
        let acc = acc lxor style_hash in
        List.fold_left (fun acc child -> hash_node child acc) acc children
  in
  hash_node scene 0

(** Diff result type *)
type diff_result = 
  | Same            (* Scenes are identical *)
  | FullRedraw      (* Everything changed *) 
  | PartialDiff of Scene.rect list (* Specific regions changed *)

(** Find changed regions between two scenes *)
let find_changed_regions prev curr =
  let open Scene in
  let changed_regions = ref [] in
  
  let rec diff_nodes prev_node curr_node =
    if not (scene_equal prev_node curr_node) then begin
      (* Add this region as changed *)
      changed_regions := curr_node.rect :: !changed_regions;
      
      (* Still recurse into children to get more granular changes *)
      match prev_node.content, curr_node.content with
      | Container prev_cont, Container curr_cont when 
          List.length prev_cont.children = List.length curr_cont.children ->
          (* Only recurse if same number of children *)
          List.iter2 diff_nodes prev_cont.children curr_cont.children
      | _ -> 
          () (* Different content types or different number of children - no point recursing *)
    end
  in
  
  (* Start diffing from root *)
  diff_nodes prev curr;
  !changed_regions

(** Compute diff between two scenes *)
let diff ?(use_hash=true) prev_scene curr_scene =
  (* Quick hash check first if enabled *)
  if use_hash then
    let prev_hash = scene_hash prev_scene in
    let curr_hash = scene_hash curr_scene in
    if prev_hash = curr_hash then
      Same
    else if scene_equal prev_scene curr_scene then
      (* Hash collision, but scenes are actually the same *)
      Same
    else
      (* Find specific regions that changed *)
      let regions = find_changed_regions prev_scene curr_scene in
      if List.length regions = 0 then Same
      else PartialDiff regions
  else
    (* Full equality check *)
    if scene_equal prev_scene curr_scene then
      Same
    else
      FullRedraw

(** Metrics for diff performance *)
type diff_metrics = {
  mutable comparisons : int;
  mutable hash_hits : int;
  mutable hash_misses : int;
  mutable partial_diffs : int;
  mutable full_rerenders : int;
  mutable skipped_renders : int;
}

let global_metrics = {
  comparisons = 0;
  hash_hits = 0;
  hash_misses = 0;
  partial_diffs = 0;
  full_rerenders = 0;
  skipped_renders = 0;
}

let reset_metrics () =
  global_metrics.comparisons <- 0;
  global_metrics.hash_hits <- 0;
  global_metrics.hash_misses <- 0;
  global_metrics.partial_diffs <- 0;
  global_metrics.full_rerenders <- 0;
  global_metrics.skipped_renders <- 0

let get_metrics_string () =
  format {|Scene Diff Metrics:
  Total comparisons: %d
  Hash hits: %d (%.1f%%)
  Hash misses: %d
  Partial diffs: %d
  Full rerenders: %d
  Skipped renders: %d
  Skip rate: %.1f%%|}
    global_metrics.comparisons
    global_metrics.hash_hits
    (if global_metrics.comparisons > 0 then
      float_of_int global_metrics.hash_hits *. 100.0 /. float_of_int global_metrics.comparisons
    else 0.0)
    global_metrics.hash_misses
    global_metrics.partial_diffs
    global_metrics.full_rerenders
    global_metrics.skipped_renders
    (if global_metrics.comparisons > 0 then
      float_of_int global_metrics.skipped_renders *. 100.0 /. float_of_int global_metrics.comparisons
    else 0.0)

(** Smart diff with metrics tracking *)
let diff_with_metrics prev_scene curr_scene =
  global_metrics.comparisons <- global_metrics.comparisons + 1;
  
  let result = diff ~use_hash:true prev_scene curr_scene in
  
  (match result with
  | Same ->
      global_metrics.hash_hits <- global_metrics.hash_hits + 1;
      global_metrics.skipped_renders <- global_metrics.skipped_renders + 1
  | PartialDiff _ ->
      global_metrics.hash_misses <- global_metrics.hash_misses + 1;
      global_metrics.partial_diffs <- global_metrics.partial_diffs + 1
  | FullRedraw ->
      global_metrics.hash_misses <- global_metrics.hash_misses + 1;
      global_metrics.full_rerenders <- global_metrics.full_rerenders + 1);
  
  result