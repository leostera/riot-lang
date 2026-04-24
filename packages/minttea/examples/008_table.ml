(**
 * Example: Table Display
 * 
 * This example demonstrates:
 * - Using the Table component
 * - Column definitions with widths
 * - Row navigation
 * - Scrolling through large datasets
 * 
 * Key concepts:
 * - Creating tables with columns and rows
 * - Handling keyboard navigation
 * - Getting selected row data
 * 
 * Controls:
 * - Up/Down arrows - Navigate rows
 * - Enter - Show selected row details
 * - q/Escape - Quit
 *)
open Std
open Minttea
open Minttea.Component

(* Sample data type *)

type user = {
  id: int;
  name: string;
  email: string;
  status: string;
  joined: string;
}

(* Model *)

type model = {
  table: Table.t;
  selected_user: user option;
  users: user list;
}

(* Sample data *)

let users = [
  {
    id = 1;
    name = "Alice Johnson";
    email = "alice@example.com";
    status = "Active";
    joined = "2024-01-15";
  };
  {
    id = 2;
    name = "Bob Smith";
    email = "bob@example.com";
    status = "Active";
    joined = "2024-02-20";
  };
  {
    id = 3;
    name = "Charlie Brown";
    email = "charlie@example.com";
    status = "Inactive";
    joined = "2023-12-10";
  };
  {
    id = 4;
    name = "Diana Prince";
    email = "diana@example.com";
    status = "Active";
    joined = "2024-03-05";
  };
  {
    id = 5;
    name = "Eve Anderson";
    email = "eve@example.com";
    status = "Pending";
    joined = "2024-04-12";
  };
  {
    id = 6;
    name = "Frank Miller";
    email = "frank@example.com";
    status = "Active";
    joined = "2023-11-28";
  };
  {
    id = 7;
    name = "Grace Hopper";
    email = "grace@example.com";
    status = "Active";
    joined = "2024-01-08";
  };
  {
    id = 8;
    name = "Henry Ford";
    email = "henry@example.com";
    status = "Inactive";
    joined = "2023-10-15";
  };
  {
    id = 9;
    name = "Iris West";
    email = "iris@example.com";
    status = "Active";
    joined = "2024-02-14";
  };
  {
    id = 10;
    name = "Jack Ryan";
    email = "jack@example.com";
    status = "Active";
    joined = "2024-03-22";
  };
  {
    id = 11;
    name = "Karen Page";
    email = "karen@example.com";
    status = "Pending";
    joined = "2024-04-30";
  };
  {
    id = 12;
    name = "Leo Messi";
    email = "leo@example.com";
    status = "Active";
    joined = "2024-05-01";
  };
  {
    id = 13;
    name = "Maria Garcia";
    email = "maria@example.com";
    status = "Active";
    joined = "2023-09-20";
  };
  {
    id = 14;
    name = "Nathan Drake";
    email = "nathan@example.com";
    status = "Inactive";
    joined = "2023-08-15";
  };
  {
    id = 15;
    name = "Olivia Pope";
    email = "olivia@example.com";
    status = "Active";
    joined = "2024-06-10";
  };
]

(* Convert user to table row *)

let user_to_row = fun user ->
  [ Int.to_string user.id; user.name; user.email; user.status; user.joined; ]

(* Initialize *)

let init = fun model -> (model, Command.Noop)

(* Update *)

let update = fun event model ->
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Escape, _) ->
      (model, Command.Quit)
  | Event.KeyDown (Event.Up, _) ->
      let table =
        match Table.selected_index model.table with
        | Some idx when idx > 0 -> Table.select model.table (idx - 1)
        | _ -> model.table
      in
      ({ model with table }, Command.Noop)
  | Event.KeyDown (Event.Down, _) ->
      let table =
        match Table.selected_index model.table with
        | Some idx -> Table.select model.table (idx + 1)
        | None -> Table.select model.table 0
      in
      ({ model with table }, Command.Noop)
  | Event.KeyDown (Event.Enter, _) ->
      (* Get selected row index and find corresponding user *)
      let selected_user =
        match Table.selected_index model.table with
        | Some idx -> List.get model.users ~at:idx
        | _ -> None
      in
      ({ model with selected_user }, Command.Noop)
  | Event.KeyDown (Event.Key "c", _) -> (* Clear selection *)
    ({ model with selected_user = None }, Command.Noop)
  | _ ->
      (model, Command.Noop)

(* View *)

let view = fun model ->
  let open Element in
    column ~style:Style.(empty |> padding (Padding.all 1))
      [
        text ~style:Style.(empty |> bold |> fg (`rgb (100, 200, 255))) "User Management System";
        text "";
        text (Table.view model.table);
        text "";
        (
          match model.selected_user with
          | Some user -> column
            ~style:Style.(empty
            |> border ~width:1 ~color:(`rgb (0, 255, 0)) ()
            |> padding (Padding.all 1))
            [
              text ~style:Style.(empty |> bold) "Selected User Details:";
              text "";
              text ("ID:     " ^ Int.to_string user.id);
              text ("Name:   " ^ user.name);
              text ("Email:  " ^ user.email);
              text ("Status: " ^ user.status);
              text ("Joined: " ^ user.joined);
              text "";
              text ~style:Style.(empty |> fg (`rgb (100, 100, 100))) "Press 'c' to clear selection";
            ]
          | None -> text ~style:Style.(empty |> fg (`rgb (100, 100, 100))) "Press Enter to view user details"
        );
        text "";
        text ~style:Style.(empty |> fg (`rgb (100, 100, 100))) "↑↓ Navigate • Enter: Select • q: Quit";
      ]

(* Create and run the app *)

let app = App.make ~init ~update ~view ()

(* Run it *)

let () =
  (* Define table columns *)
  let columns = [
    Table.column ~title:"ID" ~width:5;
    Table.column ~title:"Name" ~width:20;
    Table.column ~title:"Email" ~width:25;
    Table.column ~title:"Status" ~width:10;
    Table.column ~title:"Joined" ~width:12;
  ] in
  (* Convert users to rows *)
  let rows = List.map users ~fn:user_to_row in
  (* Create table *)
  let table = Table.make columns rows |> Table.set_height ~height:10 |> Table.focus in
  let initial_model = { table; selected_user = None; users } in
  let config = Minttea.config () in
  Minttea.start ~config app initial_model
