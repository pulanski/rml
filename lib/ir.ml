type ir_shape = int list

type ir_type =
  | IRVoidTy
  | IRIntTy
  | IRFloatTy
  | IRBoolTy
  | IRCharTy
  | IRStringTy
  | IRTensorTy
  | IRFuncTy
  | IRTypeVar
  (* TODO: propagate data to here, maybe *)
  (* | IRTensorTy of ir_shape * ir_type
  | IRFuncTy of ir_type list * ir_type
  | IRTypeVar of string *)
  (* | ...  // Other types as needed *)

type ir_program = ir_func list

and ir_param = {
  name: string;
  param_type: ir_type;
}

and ir_func = {
  func_name: string;
  params: ir_param list;
  return_type: ir_type;
  body: ir_stmt list;
}

and ir_stmt =
  | IRExpr of ir_expr
  | IRReturn of ir_expr
  | IRVarDecl of string * ir_expr
  | IRIf of ir_expr * ir_stmt list * ir_stmt list  (* if condition then block else block *)
  | IRFor of string * ir_expr * ir_stmt list       (* for variable in iterable do block *)
  | IRMatch of ir_expr * ir_match_case list        (* match expression with cases *)
  | IRWhile of ir_expr * ir_stmt list              (* while condition do block *)
  | IRLoop of ir_stmt list                         (* loop block *)

and ir_match_case =
  | IRCase of ir_pattern * ir_stmt list            (* case pattern -> block *)

and ir_pattern =
  | IRLiteralPattern of ir_expr
  | IRVariablePattern of string
  | IRTuplePattern of ir_pattern list
  | IRCustomPattern of string * ir_pattern list    (* For matching user-defined types *)

and ir_expr =
  | IRU8 of int
  | IRU16 of int
  | IRU32 of int
  | IRU64 of int
  | IRI8 of int
  | IRI16 of int
  | IRI32 of int
  | IRI64 of int
  | IRF32 of float
  | IRF64 of float
  | IRChar of char
  | IRString of string
  | IRBool of bool
  | IRVariable of string
  | IRCall of string * ir_expr list
  | IRBinOp of ir_binop * ir_expr * ir_expr
  | IRTensor of ir_shape * ir_expr list

and ir_binop =
  | IRAdd
  | IRSub
  | IRMul
  | IRDiv
  | IRMod
  | IRPow
  | IRAnd
  | IROr
  | IRXor
  | IRShl
  | IRShr
  | IRLt
  | IRLte
  | IRGt
  | IRGte
  | IREq
  | IRNeq
  | IRLeq
  | IRGeq