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

val from_lambda : Lambda.Ir.lambda -> ulambda
val pp : Format.formatter -> ulambda -> unit
