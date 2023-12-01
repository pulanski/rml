type program = func list

and func = {
  proto: prototype;
  body: stmt list;
}

and prototype = {
  name: string;
  params: string list;
  (* params: (string * data_type) list; *)
}

and stmt =
  | Expr of expr
  | Return of expr option
  | VarDecl of string * expr
  (* | VarDecl of string * data_type option * expr *)

and expr =
  | Number of float
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
  | Int
  | Float
  | String
  | Bool
  | TTensor of tensor_type
  | Array of data_type
  | Custom of string  (* For user-defined types *)

and tensor_type = {
  shape: shape;
}

and shape = int list
