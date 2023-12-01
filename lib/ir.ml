type ir_shape = int list

type ir_program = ir_func list

and ir_param = {
  name: string;
  param_type: string;
}

and ir_func = {
  func_name: string;
  params: ir_param list;
  body: ir_stmt list;
}

(* and ir_func = {
  name: string;
  params: ir_expr list;
  body: ir_stmt list;
} *)

and ir_stmt =
  | IRExpr of ir_expr
  | IRReturn of ir_expr
  | IRVarDecl of string * ir_expr

and ir_expr =
  | IRNumber of float
  | IRVariable of string
  | IRCall of string * ir_expr list
  | IRBinOp of ir_binop * ir_expr * ir_expr
  | IRTensor of ir_shape * ir_expr list
  | IRLiteral of ir_shape * float list

and ir_binop =
  | IRAdd
  | IRSub
  | IRMul
  | IRDiv