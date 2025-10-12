open Std

type function_label = string

type constant =
  | Const_int of int
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_float of float
  | Const_string of string
  | Const_block of int * constant list

type primitive =
  | Pidentity
  | Pnot
  | Pnegint
  | Paddint
  | Psubint
  | Pmulint
  | Pdivint
  | Pmodint
  | Pandint
  | Porint
  | Pxorint
  | Plslint
  | Plsrint
  | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Pabsfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pfloatcomp of comparison
  | Pstringlength
  | Pstringrefu
  | Pstringsetu
  | Pstringrefs
  | Pstringsets
  | Pmakeblock of int * mutable_flag
  | Pfield of int
  | Psetfield of int * bool
  | Pfloatfield of int
  | Psetfloatfield of int
  | Pduprecord of int
  | Plazyforce
  | Pccall of prim_desc
  | Praise of raise_kind
  | Psequand
  | Psequor
  | Pbittest
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer * boxed_integer
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of boxed_integer
  | Pmodbint of boxed_integer
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * comparison
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  | Pbigarraydim of int
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool
  | Pstring_load_64 of bool
  | Pbytes_load_16 of bool
  | Pbytes_load_32 of bool
  | Pbytes_load_64 of bool
  | Pbytes_set_16 of bool
  | Pbytes_set_32 of bool
  | Pbytes_set_64 of bool
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool
  | Pbigstring_load_64 of bool
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  | Pctconst of compile_time_constant
  | Pbswap16
  | Pbbswap of boxed_integer
  | Pint_as_pointer
  | Popaque

and comparison = Ceq | Cne | Clt | Cgt | Cle | Cge
and mutable_flag = Immutable | Mutable
and raise_kind = Raise_regular | Raise_reraise | Raise_notrace
and boxed_integer = Pnativeint | Pint32 | Pint64

and bigarray_kind =
  | Pbigarray_unknown
  | Pbigarray_float32
  | Pbigarray_float64
  | Pbigarray_sint8
  | Pbigarray_uint8
  | Pbigarray_sint16
  | Pbigarray_uint16
  | Pbigarray_int32
  | Pbigarray_int64
  | Pbigarray_caml_int
  | Pbigarray_native_int
  | Pbigarray_complex32
  | Pbigarray_complex64

and bigarray_layout =
  | Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and compile_time_constant =
  | Big_endian
  | Word_size
  | Int_size
  | Max_wosize
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin
  | Backend_type

and prim_desc = {
  prim_name : string;
  prim_arity : int;
  prim_alloc : bool;
  prim_native_name : string;
  prim_native_float : bool;
}

type variable = { var_name : string; var_id : int }

type closure_function = {
  label : function_label;
  arity : int;
  params : variable list;
  body : ulambda;
  dbg : string option;
}

and closure = { functions : closure_function list; free_vars : variable list }

and ulambda =
  | Uvar of variable
  | Uconst of constant
  | Udirect_apply of function_label * ulambda list
  | Ugeneric_apply of ulambda * ulambda list
  | Uclosure of closure
  | Uoffset of ulambda * int
  | Ulet of variable * ulambda * ulambda
  | Uletrec of (variable * ulambda) list * ulambda
  | Uprim of primitive * ulambda list
  | Uswitch of ulambda * ulambda_switch
  | Ustringswitch of ulambda * (string * ulambda) list * ulambda option
  | Ustaticfail of int * ulambda list
  | Ucatch of int * variable list * ulambda * ulambda
  | Utrywith of ulambda * variable * ulambda
  | Uifthenelse of ulambda * ulambda * ulambda
  | Usequence of ulambda * ulambda
  | Uwhile of ulambda * ulambda
  | Ufor of variable * ulambda * ulambda * direction_flag * ulambda
  | Uassign of variable * ulambda
  | Usend of ulambda * ulambda * ulambda list
  | Uunreachable

and ulambda_switch = {
  us_index_consts : int array;
  us_actions_consts : ulambda array;
  us_index_blocks : int array;
  us_actions_blocks : ulambda array;
}

and direction_flag = Upto | Downto

let var_counter = ref 0

let fresh_var name =
  let id = !var_counter in
  var_counter := id + 1;
  { var_name = name; var_id = id }

let convert_identifier (id : Lambda.Ir.Identifier.t) : variable =
  match id with
  | Local { name; stamp } -> { var_name = name; var_id = stamp }
  | Scoped { name; stamp; _ } -> { var_name = name; var_id = stamp }
  | Global name -> { var_name = name; var_id = -1 }
  | Predef { name; stamp } -> { var_name = name; var_id = stamp }

let rec from_lambda (lam : Lambda.Ir.lambda) : ulambda =
  match lam with
  | Lambda.Ir.Var id ->
      let var = convert_identifier id in
      Uvar var
  | Lambda.Ir.Const c -> Uconst (convert_constant c)
  | Lambda.Ir.Apply { func; args; _ } -> (
      let ufunc = from_lambda func in
      let uargs = List.map from_lambda args in
      match func with
      | Lambda.Ir.Var _ -> Ugeneric_apply (ufunc, uargs)
      | _ -> Ugeneric_apply (ufunc, uargs))
  | Lambda.Ir.Function { params; body; _ } ->
      let label = format "fun_%d" !var_counter in
      var_counter := !var_counter + 1;
      let uvars = List.map convert_identifier params in
      let ubody = from_lambda body in
      let func =
        {
          label;
          arity = List.length params;
          params = uvars;
          body = ubody;
          dbg = None;
        }
      in
      Uclosure { functions = [ func ]; free_vars = [] }
  | Lambda.Ir.Let { id; value; body; _ } ->
      let var = convert_identifier id in
      let uvalue = from_lambda value in
      let ubody = from_lambda body in
      Ulet (var, uvalue, ubody)
  | Lambda.Ir.LetRec { bindings; body; _ } ->
      let ubindings =
        List.map
          (fun (id, lam) ->
            let var = convert_identifier id in
            let ulam = from_lambda lam in
            (var, ulam))
          bindings
      in
      let ubody = from_lambda body in
      Uletrec (ubindings, ubody)
  | Lambda.Ir.Prim (op, args) ->
      let uargs = List.map from_lambda args in
      Uprim (convert_primitive op, uargs)
  | Lambda.Ir.IfThenElse (cond, then_, else_opt) ->
      let ucond = from_lambda cond in
      let uthen = from_lambda then_ in
      let uelse =
        match else_opt with
        | Some e -> from_lambda e
        | None -> Uconst (Const_int 0)
      in
      Uifthenelse (ucond, uthen, uelse)
  | Lambda.Ir.Sequence (expr1, expr2) ->
      let uexpr1 = from_lambda expr1 in
      let uexpr2 = from_lambda expr2 in
      Usequence (uexpr1, uexpr2)
  | Lambda.Ir.While { condition; body; _ } ->
      let ucond = from_lambda condition in
      let ubody = from_lambda body in
      Uwhile (ucond, ubody)
  | Lambda.Ir.For { id; start; stop; direction; body; _ } ->
      let var = convert_identifier id in
      let ustart = from_lambda start in
      let ustop = from_lambda stop in
      let udir =
        match direction with
        | Lambda.Ir.Upto -> Upto
        | Lambda.Ir.Downto -> Downto
      in
      let ubody = from_lambda body in
      Ufor (var, ustart, ustop, udir, ubody)
  | Lambda.Ir.Switch _ ->
      failwith "TODO: Pattern match compilation (Switch) not implemented yet"
  | Lambda.Ir.StaticRaise (n, args) ->
      let uargs = List.map from_lambda args in
      Ustaticfail (n, uargs)
  | Lambda.Ir.StaticCatch (body, (n, vars), handler) ->
      let ubody = from_lambda body in
      let uvars = List.map convert_identifier vars in
      let uhandler = from_lambda handler in
      Ucatch (n, uvars, ubody, uhandler)

and convert_constant = function
  | Lambda.Ir.Const_int i -> Const_int i
  | Lambda.Ir.Const_float f -> Const_float f
  | Lambda.Ir.Const_string s -> Const_string s
  | Lambda.Ir.Const_block (tag, consts) ->
      Const_block (tag, List.map convert_constant consts)

and convert_primitive = function
  | Lambda.Ir.Pnot -> Pnot
  | Lambda.Ir.Pint_neg -> Pnegint
  | Lambda.Ir.Pint_add -> Paddint
  | Lambda.Ir.Pint_sub -> Psubint
  | Lambda.Ir.Pint_mul -> Pmulint
  | Lambda.Ir.Pint_div -> Pdivint
  | Lambda.Ir.Pint_mod -> Pmodint
  | Lambda.Ir.Pint_lt -> Pintcomp Clt
  | Lambda.Ir.Pint_le -> Pintcomp Cle
  | Lambda.Ir.Pint_gt -> Pintcomp Cgt
  | Lambda.Ir.Pint_ge -> Pintcomp Cge
  | Lambda.Ir.Pint_eq -> Pintcomp Ceq
  | Lambda.Ir.Pint_ne -> Pintcomp Cne
  | Lambda.Ir.Pmakeblock tag -> Pmakeblock (tag, Immutable)
  | Lambda.Ir.Pfield n -> Pfield n
  | Lambda.Ir.Psetfield n -> Psetfield (n, true)
  | Lambda.Ir.Pmakearray -> failwith "TODO: Pmakearray"
  | Lambda.Ir.Parraylength -> failwith "TODO: Parraylength"
  | Lambda.Ir.Parrayrefu -> failwith "TODO: Parrayrefu"
  | Lambda.Ir.Parraysetu -> failwith "TODO: Parraysetu"

let rec pp fmt = function
  | Uvar v -> Format.fprintf fmt "%s/%d" v.var_name v.var_id
  | Uconst c -> pp_constant fmt c
  | Udirect_apply (label, args) ->
      Format.fprintf fmt "(apply_direct %s %a)" label pp_list args
  | Ugeneric_apply (func, args) ->
      Format.fprintf fmt "(apply %a %a)" pp func pp_list args
  | Uclosure { functions; free_vars } ->
      Format.fprintf fmt "(closure [%a] free:[%a])" pp_functions functions
        pp_vars free_vars
  | Uoffset (e, n) -> Format.fprintf fmt "(offset %a %d)" pp e n
  | Ulet (v, e1, e2) ->
      Format.fprintf fmt "(let %s/%d %a %a)" v.var_name v.var_id pp e1 pp e2
  | Uletrec (bindings, body) ->
      Format.fprintf fmt "(letrec [%a] %a)" pp_bindings bindings pp body
  | Uprim (prim, args) ->
      Format.fprintf fmt "(prim %a %a)" pp_primitive prim pp_list args
  | Uswitch _ -> Format.fprintf fmt "(switch ...)"
  | Ustringswitch _ -> Format.fprintf fmt "(stringswitch ...)"
  | Ustaticfail (n, args) ->
      Format.fprintf fmt "(staticfail %d %a)" n pp_list args
  | Ucatch (n, vars, body, handler) ->
      Format.fprintf fmt "(catch %d [%a] %a %a)" n pp_vars vars pp body pp
        handler
  | Utrywith (body, exn, handler) ->
      Format.fprintf fmt "(try %a with %s/%d -> %a)" pp body exn.var_name
        exn.var_id pp handler
  | Uifthenelse (c, t, e) -> Format.fprintf fmt "(if %a %a %a)" pp c pp t pp e
  | Usequence (e1, e2) -> Format.fprintf fmt "(seq %a %a)" pp e1 pp e2
  | Uwhile (c, b) -> Format.fprintf fmt "(while %a %a)" pp c pp b
  | Ufor (v, start, stop, dir, body) ->
      Format.fprintf fmt "(for %s/%d %a %a %s %a)" v.var_name v.var_id pp start
        pp stop
        (match dir with Upto -> "to" | Downto -> "downto")
        pp body
  | Uassign (v, e) ->
      Format.fprintf fmt "(assign %s/%d %a)" v.var_name v.var_id pp e
  | Usend (obj, meth, args) ->
      Format.fprintf fmt "(send %a %a %a)" pp obj pp meth pp_list args
  | Uunreachable -> Format.fprintf fmt "unreachable"

and pp_constant fmt = function
  | Const_int i -> Format.fprintf fmt "%d" i
  | Const_int32 i -> Format.fprintf fmt "%ldl" i
  | Const_int64 i -> Format.fprintf fmt "%LdL" i
  | Const_float f -> Format.fprintf fmt "%f" f
  | Const_string s -> Format.fprintf fmt "%S" s
  | Const_block (tag, consts) ->
      Format.fprintf fmt "[%d: %a]" tag pp_const_list consts

and pp_const_list fmt consts =
  match consts with
  | [] -> ()
  | [ c ] -> pp_constant fmt c
  | c :: cs -> Format.fprintf fmt "%a, %a" pp_constant c pp_const_list cs

and pp_list fmt exprs =
  match exprs with
  | [] -> ()
  | [ e ] -> pp fmt e
  | e :: es -> Format.fprintf fmt "%a %a" pp e pp_list es

and pp_vars fmt vars =
  match vars with
  | [] -> ()
  | [ v ] -> Format.fprintf fmt "%s/%d" v.var_name v.var_id
  | v :: vs -> Format.fprintf fmt "%s/%d, %a" v.var_name v.var_id pp_vars vs

and pp_bindings fmt bindings =
  match bindings with
  | [] -> ()
  | [ (v, e) ] -> Format.fprintf fmt "%s/%d = %a" v.var_name v.var_id pp e
  | (v, e) :: bs ->
      Format.fprintf fmt "%s/%d = %a; %a" v.var_name v.var_id pp e pp_bindings
        bs

and pp_functions fmt funcs =
  match funcs with
  | [] -> ()
  | [ f ] -> pp_function fmt f
  | f :: fs -> Format.fprintf fmt "%a; %a" pp_function f pp_functions fs

and pp_function fmt { label; arity; params; body; _ } =
  Format.fprintf fmt "%s/%d(%a) = %a" label arity pp_vars params pp body

and pp_primitive fmt = function
  | Pidentity -> Format.fprintf fmt "identity"
  | Pnot -> Format.fprintf fmt "not"
  | Pnegint -> Format.fprintf fmt "~"
  | Paddint -> Format.fprintf fmt "+"
  | Psubint -> Format.fprintf fmt "-"
  | Pmulint -> Format.fprintf fmt "*"
  | Pdivint -> Format.fprintf fmt "/"
  | Pmodint -> Format.fprintf fmt "mod"
  | Pandint -> Format.fprintf fmt "land"
  | Porint -> Format.fprintf fmt "lor"
  | Pxorint -> Format.fprintf fmt "lxor"
  | Plslint -> Format.fprintf fmt "lsl"
  | Plsrint -> Format.fprintf fmt "lsr"
  | Pasrint -> Format.fprintf fmt "asr"
  | Pintcomp c -> Format.fprintf fmt "%a" pp_comparison c
  | Poffsetint n -> Format.fprintf fmt "offset(%d)" n
  | _ -> Format.fprintf fmt "<prim>"

and pp_comparison fmt = function
  | Ceq -> Format.fprintf fmt "="
  | Cne -> Format.fprintf fmt "<>"
  | Clt -> Format.fprintf fmt "<"
  | Cgt -> Format.fprintf fmt ">"
  | Cle -> Format.fprintf fmt "<="
  | Cge -> Format.fprintf fmt ">="
