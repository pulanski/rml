type ir_shape = int list

type ir_program = ir_func list

and ir_func = {
  name: string;
  params: string list;
  body: ir_stmt list;
}

and ir_stmt =
  | IRExpr of ir_expr
  | IRReturn of ir_expr
  | IRVarDecl of string * ir_expr

and ir_expr =
  | IRNumber of float
  | IRVariable of string
  | IRCall of string * ir_expr list
  | IRBinOp of ir_binop * ir_expr * ir_expr
  | IRTensor of ir_shape * ir_expr list (* Include shape in tensor variant *)

and ir_binop =
  | IRAdd
  | IRSub
  | IRMul
  | IRDiv