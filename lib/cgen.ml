open Ir

let emit_c_type = function
  | IRVoidTy -> "void"
  | IRIntTy -> "int"
  | IRFloatTy -> "float"
  | IRBoolTy -> "bool"
  | IRCharTy -> "char"
  | IRStringTy -> "char*"
  | IRTensorTy -> "TODO: impl me"
  | IRFuncTy -> "TODO: impl me"
  | IRTypeVar -> "TODO: impl me"

let rec emit_c_expr = function
  | IRU8 x -> string_of_int x
  | IRU16 x -> string_of_int x
  | IRU32 x -> string_of_int x
  | IRU64 x -> string_of_int x
  | IRI8 x -> string_of_int x
  | IRI16 x -> string_of_int x
  | IRI32 x -> string_of_int x
  | IRI64 x -> string_of_int x
  | IRF32 x -> string_of_float x
  | IRF64 x -> string_of_float x
  | IRChar x -> "'" ^ Char.escaped x ^ "'"
  | IRString x -> "\"" ^ x ^ "\""
  | IRBool x -> if x then "1" else "0"
  | IRVariable x -> x
  | IRCall (func_name, args) ->
      let args_c = String.concat ", " (List.map emit_c_expr args) in
      Printf.sprintf "%s(%s)" func_name args_c
  | IRBinOp (op, lhs, rhs) ->
      let lhs_c = emit_c_expr lhs in
      let rhs_c = emit_c_expr rhs in
      Printf.sprintf "(%s %s %s)" lhs_c (emit_c_binop op) rhs_c
  | IRTensor (_dims, values) ->
      let values_c = String.concat ", " (List.map emit_c_expr values) in
      Printf.sprintf "tensor(%s) TODO: impl me" values_c

and emit_c_binop = function
  | IRAdd -> "+"
  | IRSub -> "-"
  | IRMul -> "*"
  | IRDiv -> "/"
  | IRMod -> "%"
  | IREq -> "=="
  | IRNeq -> "!="
  | IRLt -> "<"
  | IRLte -> "<="
  | IRGt -> ">"
  | IRGte -> ">="
  | IRAnd -> "&&"
  | IROr -> "||"
  | IRPow -> "^"
  | IRXor -> "^"
  | IRShl -> "<<"
  | IRShr -> ">>"
  | IRLeq -> "<="
  | IRGeq -> ">="
  (* Add cases for other binary operators *)

let rec emit_c_stmt = function
  | IRExpr expr -> emit_c_expr expr ^ ";"
  | IRReturn expr -> "return " ^ emit_c_expr expr ^ ";"
  | IRVarDecl (name, expr) ->
      "int " ^ name ^ " = " ^ emit_c_expr expr ^ ";"
  | IRIf (cond, then_stmts, else_stmts) ->
      let cond_c = emit_c_expr cond in
      let then_c = String.concat "\n  " (List.map emit_c_stmt then_stmts) in
      if List.length else_stmts = 0 then
        Printf.sprintf "if (%s) {\n  %s\n}" cond_c then_c
      else
        let else_c = String.concat "\n  " (List.map emit_c_stmt else_stmts) in
        Printf.sprintf "if (%s) {\n  %s\n} else {\n  %s\n}" cond_c then_c else_c
  | IRFor (name, expr, body) ->
      let expr_c = emit_c_expr expr in
      let body_c = String.concat "\n  " (List.map emit_c_stmt body) in
      Printf.sprintf "for (int %s = 0; %s; %s++) {\n  %s\n}" name expr_c name body_c
  | IRMatch (expr, cases) ->
      let expr_c = emit_c_expr expr in
      let cases_c = String.concat "\n  " (List.map emit_c_case cases) in
      Printf.sprintf "switch (%s) {\n  %s\n}" expr_c cases_c
  | IRWhile (cond, body) ->
      let cond_c = emit_c_expr cond in
      let body_c = String.concat "\n  " (List.map emit_c_stmt body) in
      Printf.sprintf "while (%s) {\n  %s\n}" cond_c body_c
  | IRLoop body ->
      let body_c = String.concat "\n  " (List.map emit_c_stmt body) in
      Printf.sprintf "while (1) {\n  %s\n}" body_c
  (* | IRBreak -> "break;" *)
  (* Extend with more statement types as needed *)

and emit_c_case case =
  match case with
  | IRCase (pattern, body) ->
      let pattern_c = emit_c_pattern pattern in
      let body_c = String.concat "\n  " (List.map emit_c_stmt body) in
      Printf.sprintf "case %s:\n  %s\n  break;" pattern_c body_c

and emit_c_pattern = function
  | IRLiteralPattern expr -> emit_c_expr expr
  | IRVariablePattern name -> name
  | IRTuplePattern patterns ->
      let patterns_c = String.concat ", " (List.map emit_c_pattern patterns) in
      Printf.sprintf "(%s)" patterns_c
  | IRCustomPattern (name, patterns) ->
      let patterns_c = String.concat ", " (List.map emit_c_pattern patterns) in
      Printf.sprintf "%s(%s)" name patterns_c

and emit_c_function func =
  let params_str = String.concat ", " (List.map (fun p -> (emit_c_type p.param_type) ^ " " ^ p.name) func.params) in
  let return_type_str = emit_c_type func.return_type in
  let body_str = String.concat "\n  " (List.map emit_c_stmt func.body) in
  Printf.sprintf "%s %s(%s) {\n  %s\n}" return_type_str func.func_name params_str body_str

(* let emit_c ir_program =
  let functions_str = String.concat "\n" (List.map emit_c_function ir_program) in
  "// Generated C Program\n\n" ^ functions_str *)

and emit_c_item = function
  | IRFunc func -> emit_c_function func
  | IRStructDef struct_def -> emit_c_struct struct_def
  | IREnumDef enum_def -> emit_c_enum enum_def
  | IRTraitDef _trait_def -> "TODO: impl me"
  (* | IRImplDef impl_def -> "TODO: impl me" *)
  | IRModuleDef _module_def -> "TODO: impl me"
  (* Add other item types as necessary *)

and emit_c_struct struct_def =
  let fields_str = String.concat ";\n  " (List.map (fun (name, ty) -> (emit_c_type ty) ^ " " ^ name) struct_def.ir_fields) in
  Printf.sprintf "struct %s {\n  %s;\n};" struct_def.ir_struct_name fields_str

and emit_c_enum enum_def =
  let variants_str = String.concat ",\n  " (List.map fst enum_def.ir_variants) in
  Printf.sprintf "enum %s {\n  %s\n};" enum_def.ir_enum_name variants_str

and emit_c ir_program =
  let items_str = String.concat "\n" (List.map emit_c_item ir_program) in
  "// Generated C Program\n\n" ^ items_str