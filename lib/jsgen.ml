open Ir

let rec emit_js_expr = function
  (* Translate IR expressions to JavaScript expressions *)
    (* | IRArray elems ->
      let elems_str = String.concat ", " (List.map emit_js_expr elems) in
      Printf.sprintf "[%s]" elems_str *)
  | IRVariable v -> v
  | IRCall (name, args) ->
    let args_str = String.concat ", " (List.map emit_js_expr args) in
    Printf.sprintf "%s(%s)" name args_str
  | IRBinOp (op, e1, e2) ->
    let op = emit_js_binop op in
    Printf.sprintf "%s %s %s" (emit_js_expr e1) op (emit_js_expr e2)
  (* | IRLiteral lit ->
      match lit with
      | IRInt i -> string_of_int i
      | IRFloat f -> string_of_float f
      | IRBool b -> string_of_bool b
      | IRString s -> "\"" ^ s ^ "\""
      | IRChar c -> "'" ^ Char.escaped c ^ "'"
      | IRNull -> "null"
      | IRVoid -> "undefined" *)
  | IRTensor (_shape, elems) ->
    let elems_str = String.concat ", " (List.map emit_js_expr elems) in
    Printf.sprintf "[%s]" elems_str
  | _ -> failwith "Not yet implemented"

  (* TODO: Add support for the following *)
  (* | IRIndex (e, i) -> Printf.sprintf "%s[%s]" (emit_js_expr e) (emit_js_expr i) *)
  (* | IRIndexSlice (e, i1, i2) -> Printf.sprintf "%s[%s:%s]" (emit_js_expr e) (emit_js_expr i1) (emit_js_expr i2) *)
  (* | IRIndexSliceStep (e, i1, i2, i3) -> Printf.sprintf "%s[%s:%s:%s]" (emit_js_expr e) (emit_js_expr i1) (emit_js_expr i2) (emit_js_expr i3) *)
  (* | IRIndexAssign (e1, i, e2) -> Printf.sprintf "%s[%s] = %s" (emit_js_expr e1) (emit_js_expr i) (emit_js_expr e2) *)
  (* | IRIndexSliceAssign (e1, i1, i2, e2) -> Printf.sprintf "%s[%s:%s] = %s" (emit_js_expr e1) (emit_js_expr i1) (emit_js_expr i2) (emit_js_expr e2) *)
  (* | IRIndexSliceStepAssign (e1, i1, i2, i3, e2) -> Printf.sprintf "%s[%s:%s:%s] = %s" (emit_js_expr e1) (emit_js_expr i1) (emit_js_expr i2) (emit_js_expr i3) (emit_js_expr e2) *)
  (* | IRShape e -> Printf.sprintf "%s.shape" (emit_js_expr e) *)
  (* | IRReshape (e, shape) -> Printf.sprintf "%s.reshape(%s)" (emit_js_expr e) (emit_js_expr shape) *)
  (* | IRTranspose e -> Printf.sprintf "%s.transpose()" (emit_js_expr e) *)
  (* | IRMap (e, f) -> Printf.sprintf "%s.map(%s)" (emit_js_expr e) (emit_js_expr f) *)
  (* | IRReduce (e, f) -> Printf.sprintf "%s.reduce(%s)" (emit_js_expr e) *)

and emit_js_binop = function
  | IRAdd -> "+"
  | IRSub -> "-"
  | IRMul -> "*"
  | IRDiv -> "/"
  | IRMod -> "%"
  | IRAnd -> "&&"
  | IROr -> "||"
  | IREq -> "=="
  | IRNeq -> "!="
  | IRLt -> "<"
  | IRLte -> "<="
  | IRGt -> ">"
  | IRGte -> ">="
  | IRPow -> "**"
  | IRXor -> "^"
  | IRShl -> "<<"
  | IRShr -> ">>"
  | IRLeq -> "<="
  | IRGeq -> ">="
  (*| IRConcat -> "+"
  | IRIndex -> "[]"
   *)



let emit_js_stmt stmt =
  (* Convert IR statements to JavaScript code *)
  match stmt with
  | IRVarDecl (name, e) -> Printf.sprintf "const %s = %s;" name (emit_js_expr e)
  | IRExpr e -> Printf.sprintf "%s;" (emit_js_expr e)
  | _ -> failwith "Not implemented"
  (* | IRVarAssign (name, e) -> Printf.sprintf "%s = %s;" name (emit_js_expr e) *)
  (* | IRPrint e -> Printf.sprintf "console.log(%s);" (emit_js_expr e)
  | IRIf (e, s1, s2) ->
    Printf.sprintf "if (%s) {\n  %s\n} else {\n  %s\n}" (emit_js_expr e) (emit_js_stmt s1) (emit_js_stmt s2) *)

let emit_js_function func =
  let params_str = String.concat ", " (List.map (fun p -> p.name) func.params) in
  let body_str = String.concat "\n  " (List.map emit_js_stmt func.body) in
  Printf.sprintf "function %s(%s) {\n  %s\n}" func.func_name params_str body_str


let emit_js ir_program =
  let functions_str = String.concat "\n" (List.map emit_js_function ir_program) in
  "// Generated JavaScript Program\n\n" ^ functions_str

