type location = {
  file: string option;
  line: int;
  col: int;
}

type program = func list

and func = {
  proto: prototype;
  body: stmt list;
}

and prototype = {
  plocation: location;
  name: string;
  params: string list;
}

and tensor_type = {
  shape: shape;
}

and shape = int list

and stmt =
  | Expr of expr
  | Return of expr option
  | VarDecl of string * tensor_type option * expr

and expr =
  | Number of float
  | Variable of string
  | Call of string * expr list
  | BinOp of binop * expr * expr
  | Tensor of expr list

and binop =
  | Add
  | Sub
  | Mul
  | Div