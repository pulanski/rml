open Ir

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
  | IRBool x -> string_of_bool x
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
  (* Add cases for other binary operators *)


let emit_c_stmt = function
  | IRExpr expr -> emit_c_expr expr ^ ";"
  | IRReturn expr -> "return " ^ emit_c_expr expr ^ ";"
  | IRVarDecl (name, expr) ->
      "int " ^ name ^ " = " ^ emit_c_expr expr ^ ";"
  (* Extend with more statement types as needed *)


let emit_c_function func =
  let params_str = String.concat ", " (List.map (fun p -> "int " ^ p.name) func.params) in
  let body_str = String.concat "\n  " (List.map emit_c_stmt func.body) in
  Printf.sprintf "int %s(%s) {\n  %s\n}" func.func_name params_str body_str


let emit_c ir_program =
  let functions_str = String.concat "\n" (List.map emit_c_function ir_program) in
  "// Generated C Program\n\n" ^ functions_str
