open Ir

let counter = ref 0
let var_map = Hashtbl.create 10

let fresh_var () =
  let var = Printf.sprintf "%%%d" !counter in
  incr counter;
  var

let reset_counter () =
  counter := 0;
  Hashtbl.clear var_map

let get_ssa_var name =
  try Hashtbl.find var_map name
  with Not_found -> name (* or handle error *)

let rec emit_mlir_expr = function
  | IRVariable x -> get_ssa_var x
  | IRBinOp (op, lhs, rhs) ->
      let lhs_ssa = emit_mlir_expr lhs in
      let rhs_ssa = emit_mlir_expr rhs in
      let result_var = fresh_var () in
      Printf.sprintf "%s = %s %s, %s : f64" result_var (emit_op op) lhs_ssa rhs_ssa
  | IRBreak -> "break"
  | IRContinue -> "continue"
  | IRCall (func_name, args) ->
      let args_str = String.concat ", " (List.map emit_mlir_expr args) in
      Printf.sprintf "%s(%s)" func_name args_str
  | IRLiteral x ->
      (match x with
      (* TODO: here check the size of the num and then dynamically generate the type here *)
        | IRInt x -> Printf.sprintf "arith.constant %d : i64" x
        | IRFloat x -> Printf.sprintf "arith.constant %f : f64" x
        | IRBool x -> Printf.sprintf "arith.constant %b : i1" x
        | IRChar x -> Printf.sprintf "arith.constant %d : i8" (Char.code x)
        | IRString x -> Printf.sprintf "arith.constant \"%s\" : string" x
        | IRNull -> "arith.constant null : i64"
        | IRVoid -> "arith.constant null : i64")
  | IRTensor (shape, elements) ->
    let shape_str = String.concat "x" (List.map string_of_int shape) in
    let elements_str = String.concat ", " (List.map emit_mlir_expr elements) in
    Printf.sprintf "rml.constant dense<[%s]> : tensor<%sxf64>" elements_str shape_str
    (* | IRTuple elements ->
      let elements_str = String.concat ", " (List.map emit_mlir_expr elements) in
      Printf.sprintf "(%s)" elements_str
    | IRCustom (name, elements) ->
      let elements_str = String.concat ", " (List.map emit_mlir_expr elements) in
      Printf.sprintf "%s(%s)" name elements_str *)
    | _ -> failwith "not implemented"

and emit_op = function
  | IRAdd -> "rml.add"
  | IRSub -> "rml.sub"
  | IRMul -> "rml.mul"
  | IRDiv -> "rml.div"
  | IRMod -> "rml.mod"
  | IREq -> "rml.eq"
  | IRNeq -> "rml.neq"
  | IRLt -> "rml.lt"
  | IRLte -> "rml.lte"
  | IRGt -> "rml.gt"
  | IRGte -> "rml.gte"
  | IRAnd -> "rml.and"
  | IROr -> "rml.or"
  | IRPow -> "rml.pow"
  | IRXor -> "rml.xor"
  | IRShl -> "rml.shl"
  | IRShr -> "rml.shr"
  | IRLeq -> "rml.leq"
  | IRGeq -> "rml.geq"
  (* | IRNot -> "rml.not"
  | IRNeg -> "rml.neg" *)

let rec emit_mlir_stmt = function
  | IRExpr expr -> emit_mlir_expr expr
  | IRVarDecl (_name, expr) ->
      let var_name = fresh_var () in
      Printf.sprintf "%s = %s" var_name (emit_mlir_expr expr)
  | IRIf (cond, then_stmts, else_stmts) ->
      let cond_ssa = emit_mlir_expr cond in
      let then_str = String.concat "\n  " (List.map emit_mlir_stmt then_stmts) in
      let else_str = String.concat "\n  " (List.map emit_mlir_stmt else_stmts) in
      Printf.sprintf "if %s {\n  %s\n} else {\n  %s\n}" cond_ssa then_str else_str
  | IRWhile (cond, body) ->
      let cond_ssa = emit_mlir_expr cond in
      let body_str = String.concat "\n  " (List.map emit_mlir_stmt body) in
      Printf.sprintf "while %s {\n  %s\n}" cond_ssa body_str
  | IRLoop body ->
      let body_str = String.concat "\n  " (List.map emit_mlir_stmt body) in
      Printf.sprintf "loop {\n  %s\n}" body_str
  | IRFor (name, expr, body) ->
      let expr_ssa = emit_mlir_expr expr in
      let body_str = String.concat "\n  " (List.map emit_mlir_stmt body) in
      Printf.sprintf "for %s in %s {\n  %s\n}" name expr_ssa body_str
  | IRMatch (expr, cases) ->
      let expr_ssa = emit_mlir_expr expr in
      let cases_str = String.concat "\n  " (List.map emit_mlir_case cases) in
      Printf.sprintf "match %s {\n  %s\n}" expr_ssa cases_str
  | IREmpty -> ""

and emit_mlir_case = function
  | IRCase (pattern, stmts) ->
      let pattern_str = emit_mlir_pattern pattern in
      let stmts_str = String.concat "\n  " (List.map emit_mlir_stmt stmts) in
      Printf.sprintf "%s {\n  %s\n}" pattern_str stmts_str

and emit_mlir_pattern = function
  | IRLiteralPattern expr -> emit_mlir_expr expr
  | IRVariablePattern name -> name
  | IRTuplePattern patterns ->
      let patterns_str = String.concat ", " (List.map emit_mlir_pattern patterns) in
      Printf.sprintf "(%s)" patterns_str
  | IRCustomPattern (name, patterns) ->
      let patterns_str = String.concat ", " (List.map emit_mlir_pattern patterns) in
      Printf.sprintf "%s(%s)" name patterns_str
  (* | IRWildcard -> "_"
  | IRVarPattern name -> name
  | IRConstPattern expr -> emit_mlir_expr expr
  | IRBinOpPattern (op, lhs, rhs) ->
      let lhs_ssa = emit_mlir_pattern lhs in
      let rhs_ssa = emit_mlir_pattern rhs in
      Printf.sprintf "%s %s %s" lhs_ssa (emit_op op) rhs_ssa
  | IRCallPattern (func_name, args) ->
      let args_str = String.concat ", " (List.map emit_mlir_pattern args) in
      Printf.sprintf "%s(%s)" func_name args_str
  | IRTensorPattern (shape, elements) ->
    let shape_str = String.concat "x" (List.map string_of_int shape) in
    let elements_str = String.concat ", " (List.map emit_mlir_pattern elements) in
    Printf.sprintf "rml.constant dense<[%s]> : tensor<%sxf64>" elements_str shape_str
  | _ -> failwith "not implemented" *)

let emit_mlir_type = function
  | IRVoidTy -> "void"
  | IRBoolTy -> "i1"
  | IRFloatTy -> "f64"
  | IRIntTy -> "i64"
  | IRCharTy -> "i8"
  | IRStringTy -> "string"
  | _ -> failwith "not implemented"
  (* TODO: *)
  (* | IRTensorTy shape -> Printf.sprintf "tensor<%sxf64>" (String.concat "x" (List.map string_of_int shape))
  | IRFuncTy (param_types, return_type) ->
      let param_types_str = String.concat ", " (List.map emit_mlir_type param_types) in
      let return_type_str = emit_mlir_type return_type in
      Printf.sprintf "(%s) -> %s" param_types_str return_type_str *)

let emit_mlir_function func =
  reset_counter ();
  let stmts_str = String.concat "\n  " (List.map emit_mlir_stmt func.body) in
  (* Extract names from function parameters *)
  let param_names = List.map (fun p -> Printf.sprintf "%s: %s" p.name (emit_mlir_type p.param_type)) func.params in
  let params_str = String.concat ", " param_names in
  Printf.sprintf "func.func @%s(%s) {\n  %s\n}\n" func.func_name params_str stmts_str

let emit_mlir ir_program =
  let header = "// MLIR module generated by rml compiler\n" in
  let functions_str = String.concat "\n" (List.map emit_mlir_function ir_program) in
  header ^ "module {\n" ^ functions_str ^ "}\n"
