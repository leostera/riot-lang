(**
   * Example: Paginator
   *
   * This example demonstrates:
   * - Using the Paginator component
   * - Navigating through pages of content
   * - Showing page indicators
   *
   * Key concepts:
   * - Page-based navigation
   * - Content chunking
   * - Page status display
   *
   * Controls:
   * - Left/Right arrows - Previous/Next page
   * - 1-9 - Jump to page
   * - Home/End - First/Last page
   * - q/Escape - Quit
*)
open Std
open Minttea
open Minttea.Component

(* Model *)

type model = {
  current_page: int;
  pages: string list;
}

(* Generate sample pages *)

let generate_pages = fun () ->
  let pages = ref [] in
  (* Page 1: Introduction *)
  pages := "📖 WELCOME TO THE PAGINATOR EXAMPLE\n\
     \n\
     This example demonstrates page-based navigation\n\
     through multiple screens of content.\n\
     \n\
     You can navigate between pages using:\n\
     • Arrow keys (← →)\n\
     • Number keys (1-9)\n\
     • Home/End keys\n\
     \n\
     Current page: 1 of 5"
  :: !pages;
  (* Page 2: Features *)
  pages := "✨ KEY FEATURES\n\
     \n\
     The paginator component provides:\n\
     \n\
     • Simple page navigation\n\
     • Page indicators (dots)\n\
     • Direct page jumping\n\
     • Circular navigation option\n\
     • Customizable page content\n\
     \n\
     Current page: 2 of 5"
  :: !pages;
  (* Page 3: Use Cases *)
  pages := "💡 COMMON USE CASES\n\
     \n\
     Paginators are perfect for:\n\
     \n\
     • Onboarding flows\n\
     • Multi-step forms\n\
     • Tutorial sequences\n\
     • Help documentation\n\
     • Settings screens\n\
     • Image galleries\n\
     \n\
     Current page: 3 of 5"
  :: !pages;
  (* Page 4: Tips *)
  pages := "💭 USAGE TIPS\n\
     \n\
     Best practices:\n\
     \n\
     • Keep pages focused on one topic\n\
     • Show clear progress indicators\n\
     • Allow both forward and backward navigation\n\
     • Consider adding 'Skip' option for long flows\n\
     • Save progress for multi-step forms\n\
     \n\
     Current page: 4 of 5"
  :: !pages;
  (* Page 5: Conclusion *)
  pages := "🎉 THAT'S ALL!\n\
     \n\
     You've reached the end of the paginator demo.\n\
     \n\
     Feel free to navigate back to review any page,\n\
     or press 'q' to quit.\n\
     \n\
     Thank you for trying the Minttea Paginator!\n\
     \n\
     Current page: 5 of 5"
  :: !pages;
  List.rev !pages

(* Initialize *)

let init = fun model -> (model, Command.Noop)

(* Update *)

let update = fun event model ->
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Escape, _) -> (model, Command.Quit)
  | Event.KeyDown (Event.Left, _) ->
      let current_page = max 0 (model.current_page - 1) in
      ({ model with current_page }, Command.Noop)
  | Event.KeyDown (Event.Right, _) ->
      let max_page = List.length model.pages - 1 in
      let current_page = min max_page (model.current_page + 1) in
      ({ model with current_page }, Command.Noop)
  | Event.KeyDown (Event.Home, _) -> ({ model with current_page = 0 }, Command.Noop)
  | Event.KeyDown (Event.End, _) ->
      let current_page = List.length model.pages - 1 in
      ({ model with current_page }, Command.Noop)
  | Event.KeyDown (Event.Key s, _) when String.length s = 1 -> (
      match String.get_unchecked s ~at:0 with
      | '1' .. '5' as c ->
          let page = Char.code c - Char.code '0' - 1 in
          let max_page = List.length model.pages - 1 in
          let current_page = min max_page (max 0 page) in
          ({ model with current_page }, Command.Noop)
      | _ -> (model, Command.Noop)
    )
  | _ -> (model, Command.Noop)

(* View *)

let view = fun model ->
  let open Element in
  let page_content =
    List.get model.pages ~at:model.current_page
    |> Option.unwrap_or ~default:"Page not found"
  in
  (* Create page indicator dots *)
  let total_pages = List.length model.pages in
  let dots =
    List.init
      ~count:total_pages
      ~fn:(fun i ->
        if i = model.current_page then
          "●"
        else
          "○")
    |> String.concat " "
  in
  column
    ~style:Style.(empty
    |> padding (Style.Padding.all 1))
    [
      container
        ~style:Style.(empty
        |> border ~width:1 ~color:(`rgb (100, 150, 200)) ()
        |> padding (Style.Padding.all 2)
        |> min_height 15.0)
        [ text page_content ];
      text "";
      text
        ~style:Style.(empty
        |> align ~x:Center ~y:Middle)
        dots;
      text "";
      row
        ~style:Style.(empty
        |> fg (`rgb (100, 100, 100)))
        [
          text "← → Navigate";
          text " • ";
          text "1-5 Jump to page";
          text " • ";
          text "Home/End First/Last";
          text " • ";
          text "q Quit";
        ];
    ]

(* Create and run the app *)

let app = App.make ~init ~update ~view ()

(* Run it *)

let main ~args:_ =
  let pages = generate_pages () in
  let initial_model = { current_page = 0; pages } in
  let config = Minttea.config () in
  Minttea.run ~config initial_model app

let () = Runtime.run ~main ~args:Env.args ()
