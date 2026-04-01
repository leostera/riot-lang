(**
 * Example: Text Input
 * 
 * This example demonstrates:
 * - Using the TextInput component
 * - Setting placeholder text
 * - Handling input validation
 * - Password masking
 * 
 * Key concepts:
 * - Immutable component updates
 * - Input validation
 * - Different echo modes
 * 
 * Controls:
 * - Type to enter text
 * - Tab - Switch between fields
 * - Enter - Submit form
 * - Escape/q - Quit
 *)
open Std
open Minttea
open Minttea.Component

(* Which field is focused *)

type focus =
  | NameField
  | EmailField
  | PasswordField
  | SubmitButton

(* Model with multiple text inputs *)

type model = {
  name_input: Textinput.t;
  email_input: Textinput.t;
  password_input: Textinput.t;
  focus: focus;
  submitted: bool;
  error: string option;
}

(* Email validation helper *)

let validate_email = fun email -> String.contains email "@" && String.length email > 3

(* Initialize with empty inputs *)

let init = fun model -> (model, Command.Noop)

(* Handle tab key to switch focus *)

let switch_focus = fun model ->
  match model.focus with
  | NameField -> {
    model
    with focus = EmailField;
    name_input = Textinput.blur model.name_input;
    email_input = Textinput.focus model.email_input;
  }
  | EmailField -> {
    model
    with focus = PasswordField;
    email_input = Textinput.blur model.email_input;
    password_input = Textinput.focus model.password_input;
  }
  | PasswordField -> {
    model
    with focus = SubmitButton;
    password_input = Textinput.blur model.password_input;
  }
  | SubmitButton -> {model with focus = NameField;name_input = Textinput.focus model.name_input;}

(* Handle form submission *)

let submit_form = fun model ->
  let name = Textinput.value model.name_input in
  let email = Textinput.value model.email_input in
  let password = Textinput.value model.password_input in
  (* Validate *)
  if String.length name = 0 then
    {model with error = Some "Name is required";}
  else if not (validate_email email) then
    {model with error = Some "Invalid email address";}
  else if String.length password < 6 then
    {model with error = Some "Password must be at least 6 characters";}
  else
    {model with submitted = true;error = None;}

(* Update function *)

let update = fun event model ->
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Escape, _) when not model.submitted ->
      (model, Command.Quit)
  | Event.KeyDown (Event.Tab, _) ->
      (switch_focus model, Command.Noop)
  | Event.KeyDown (Event.Enter, _) ->
      if model.focus = SubmitButton then
        let new_model = submit_form model in
        if new_model.submitted then
          (new_model, Command.Quit)
        else
          (new_model, Command.Noop)
      else
        (switch_focus model, Command.Noop)
  | Event.KeyDown (Event.Key s, _) when String.length s = 1 ->
      (* Handle character input for the focused field *)
      let new_model =
        match model.focus with
        | NameField ->
            let current = Textinput.value model.name_input in
            let input = Textinput.set_value model.name_input ~value:((current ^ s)) in
            {model with name_input = input;error = None;}
        | EmailField ->
            let current = Textinput.value model.email_input in
            let input = Textinput.set_value model.email_input ~value:((current ^ s)) in
            {model with email_input = input;error = None;}
        | PasswordField ->
            let current = Textinput.value model.password_input in
            let input = Textinput.set_value model.password_input ~value:((current ^ s)) in
            {model with password_input = input;error = None;}
        | SubmitButton ->
            model
      in
      (new_model, Command.Noop)
  | Event.KeyDown (Event.Backspace, _) ->
      (* Handle backspace for the focused field *)
      let new_model =
        match model.focus with
        | NameField ->
            let current = Textinput.value model.name_input in
            let len = String.length current in
            let value =
              if len > 0 then
                String.sub current 0 (len - 1)
              else
                ""
            in
            let input = Textinput.set_value model.name_input ~value in
            {model with name_input = input;error = None;}
        | EmailField ->
            let current = Textinput.value model.email_input in
            let len = String.length current in
            let value =
              if len > 0 then
                String.sub current 0 (len - 1)
              else
                ""
            in
            let input = Textinput.set_value model.email_input ~value in
            {model with email_input = input;error = None;}
        | PasswordField ->
            let current = Textinput.value model.password_input in
            let len = String.length current in
            let value =
              if len > 0 then
                String.sub current 0 (len - 1)
              else
                ""
            in
            let input = Textinput.set_value model.password_input ~value in
            {model with password_input = input;error = None;}
        | SubmitButton ->
            model
      in
      (new_model, Command.Noop)
  | _ ->
      (model, Command.Noop)

(* View function *)

let view = fun model ->
  let open Element in
    if model.submitted then
      column
        ~style:Style.(empty |> padding (Padding.all 2))
        [
          text ~style:Style.(empty |> fg (`rgb (0, 255, 0)) |> bold) "✓ Form submitted successfully!";
          text "";
          text ("Name: " ^ Textinput.value model.name_input);
          text ("Email: " ^ Textinput.value model.email_input);
          text "Password: ••••••••";
        ]
    else
      (* Form screen *)
      let highlight_style focused =
        if focused then
          Style.(empty |> fg (`rgb (255, 200, 0)))
        else
          Style.empty
      in
      column ~style:Style.(empty |> padding (Padding.all 2))
        [
          text ~style:Style.(empty |> bold |> fg (`rgb (100, 200, 255))) "User Registration Form";
          text "";
          container
            ~style:(highlight_style (model.focus = NameField))
            [ text (Textinput.view model.name_input) ];
          container
            ~style:(highlight_style (model.focus = EmailField))
            [ text (Textinput.view model.email_input) ];
          container
            ~style:(highlight_style (model.focus = PasswordField))
            [ text (Textinput.view model.password_input) ];
          text "";
          text
            ~style:((
              if model.focus = SubmitButton then
                Style.(empty
                |> bg (`rgb (62, 103, 224))
                |> fg (`rgb (255, 255, 255))
                |> bold
                |> padding (Padding.symmetric ~h:2 ~v:1))
              else
                Style.(empty
                |> bg (`rgb (40, 40, 40))
                |> fg (`rgb (150, 150, 150))
                |> padding (Padding.symmetric ~h:2 ~v:1))
            ))
            " Submit (Enter) ";
          (
            match model.error with
            | Some msg -> column
              [ text ""; text ~style:Style.(empty |> fg (`rgb (255, 0, 0))) ("⚠ " ^ msg); ]
            | None -> empty
          );
          text "";
          text ~style:Style.(empty |> fg (`rgb (100, 100, 100))) "Tab: Next field • Enter: Submit • Escape: Quit";
        ]

(* Create and run the app *)

let app = App.make ~init ~update ~view ()

(* Run it *)

let () =
  let initial_model = {
    name_input = Textinput.make ()
    |> Textinput.set_placeholder ~placeholder:"John Doe"
    |> Textinput.set_prompt ~prompt:"Name: "
    |> Textinput.set_width ~width:30
    |> Textinput.focus;
    email_input = Textinput.make ()
    |> Textinput.set_placeholder ~placeholder:"john@example.com"
    |> Textinput.set_prompt ~prompt:"Email: "
    |> Textinput.set_width ~width:30;
    password_input = Textinput.make ()
    |> Textinput.set_placeholder ~placeholder:"********"
    |> Textinput.set_prompt ~prompt:"Password: "
    |> Textinput.set_width ~width:30
    |> Textinput.set_echo_mode ~mode:Textinput.Password
    |> Textinput.set_echo_char ~char:'*';
    focus = NameField;
    submitted = false;
    error = None;
  }
  in
  let config = Minttea.config () in
  Minttea.start ~config app initial_model
