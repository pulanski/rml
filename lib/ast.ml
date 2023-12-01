type program = func list

and func = {
  proto: prototype;
  body: stmt list;
}

and mutable_flag =
  | Mutable
  | Immutable

and prototype = {
  name: string;
  params: string list;
  (* params: (string * data_type) list; *)
}

and stmt =
  | Expr of expr
  | Return of expr option
  | VarDecl of mutable_flag * string * data_type option * expr

and expr =
  | Variable of string
  | Call of string * expr list
  | BinOp of binop * expr * expr
  | Tensor of expr list
  | Literal of data_type

and binop =
  | Add
  | Sub
  | Mul
  | Div

and data_type =
  | I8 of int
  | I16 of int
  | I32 of int
  | I64 of int
  | U8 of int
  | U16 of int
  | U32 of int
  | U64 of int
  | F32 of float
  | F64 of float
  | Char of char
  | String of string
  | Bool of bool
  | TTensor of tensor_type
  | Array of data_type
  | Custom of string  (* For user-defined types *)

and tensor_type = {
  shape: shape;
}

and shape = int list
