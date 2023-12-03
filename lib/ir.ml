type ir_program = ir_item list

and ir_item =
  | IRFunc of ir_func
  | IRStructDef of ir_struct_def
  | IREnumDef of ir_enum_def
  | IRTraitDef of ir_trait_def
  | IRModuleDef of ir_module_def
  (* Add other item types as necessary *)

and ir_case = {
  ir_case_pattern: ir_pattern;
  ir_case_body: ir_stmt list;
}

and ir_function_signature = {
  func_sig_name: string;
  func_sig_params: ir_type list;
  func_sig_return_type: ir_type;
}

and ir_trait_def = {
  ir_trait_name: string;
  ir_methods: ir_function_signature list;
  (* Other trait details *)
}

and ir_module_def = {
  ir_module_name: string;
  ir_module_items: ir_item list;
  (* Other module details *)
}

and ir_struct_def = {
  ir_struct_name: string;
  ir_fields: (string * ir_type) list;
}

and ir_enum_def = {
  ir_enum_name: string;
  ir_variants: (string * ir_type option) list;
}

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
  | IRBreak
  | IRContinue

and ir_match_case =
  | IRCase of ir_pattern * ir_stmt list            (* case pattern -> block *)

and ir_pattern =
  | IRLiteralPattern of ir_expr
  | IRVariablePattern of string
  | IRTuplePattern of ir_pattern list
  | IRCustomPattern of string * ir_pattern list    (* For matching user-defined types *)

and ir_expr =
  | IRLiteral of ir_literal
  | IRVariable of string
  | IRCall of string * ir_expr list
  | IRBinOp of ir_binop * ir_expr * ir_expr
  | IRTensor of ir_shape * ir_expr list

and ir_literal =
  | IRInt of int
  | IRFloat of float
  | IRBool of bool
  | IRChar of char
  | IRString of string
  | IRNull (* TODO: do we need/want this? *)
  | IRVoid

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

and ir_shape = int list

and ir_type =
  | IRVoidTy
  | IRIntTy
  | IRFloatTy
  | IRBoolTy
  | IRCharTy
  | IRStringTy
  | IRTensorTy
  | IRFuncTy
  | IRTypeVar
  | IRArrayTy of ir_type
  | IRCustomTy of string
  (* TODO: propagate data to here, maybe *)
  (* | IRTensorTy of ir_shape * ir_type
  | IRFuncTy of ir_type list * ir_type
  | IRTypeVar of string *)
  (* | ...  // Other types as needed *)