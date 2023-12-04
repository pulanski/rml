open Ir
(* open Sema *)

let emit_c_type = function
  | IRVoidTy -> "void"
  | IRIntTy -> "int"
  | IRFloatTy -> "float"
  | IRBoolTy -> "bool"
  | IRCharTy -> "char"
  | IRStringTy -> "char*"
  | IRTensorTy -> "" (* TODO: impl me *)
  | _ -> failwith "TODO: Emit C type"
  (* | IRFuncTy (params, return_ty) ->
      let params_str = String.concat ", " (List.map emit_c_type params) in
      let return_ty_str = emit_c_type return_ty in
      Printf.sprintf "%s (%s)" return_ty_str params_str *)

let rec emit_c_expr = function
  | IRBreak -> "break"
  | IRContinue -> "continue"
  | IRNot expr -> "!" ^ emit_c_expr expr
  | IRNegation expr -> "-" ^ emit_c_expr expr
  | IRArray arr_list ->
      let arr_list_c = String.concat ", " (List.map emit_c_expr arr_list) in
      Printf.sprintf "[%s]" arr_list_c
  | IRArrayRepeat (expr, count) ->
      let expr_c = emit_c_expr expr in
      let count_c = emit_c_expr count in
      Printf.sprintf "[%s; %s]" expr_c count_c
  | IRArrayRange (start, end_) ->
      let start_c = emit_c_expr start in
      let end_c = emit_c_expr end_ in
      Printf.sprintf "[%s..%s]" start_c end_c
  | IRVariable x -> x
  | IRPath ir_segment_list ->
      let segments_c = String.concat "." (List.map (fun segment -> match segment with
        | IRPathExprSegment (expr, _) -> (match expr with
          | IRIdent name -> name
          | IRSelf -> "self"
          | IRSuper -> "super"
          | IRCrate -> "crate")
        | IRPathIdentSegment name -> match name with
          | IRIdent name -> name
          | IRSelf -> "self"
          | IRSuper -> "super"
          | IRCrate -> "crate"
      ) ir_segment_list) in
      Printf.sprintf "%s" segments_c
  | IRReturn expr -> "return " ^ emit_c_expr expr
  | IRTuple exprs ->
      let exprs_c = String.concat ", " (List.map emit_c_expr exprs) in
      Printf.sprintf "(%s)" exprs_c
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
  | IRStructInit (struct_name, fields) ->
      let fields_c = String.concat ", " (List.map (fun (name, expr) -> name ^ " = " ^ emit_c_expr expr) fields) in
      Printf.sprintf "(struct %s) { %s }" struct_name fields_c
  | IREnumInit (enum_name, variant_name, _expr) ->
      Printf.sprintf "(enum %s) %s" enum_name variant_name
  | IRLambda (params, body) ->
      let params_str = String.concat ", " (List.map (fun p -> (emit_c_type p.param_type) ^ " " ^ p.name) params) in
      let body_c = String.concat "\n  " (List.map emit_c_stmt body) in
      Printf.sprintf "(%s) {\n  %s\n}" params_str body_c
  | IRRange (start, end_) ->
      let start_c = emit_c_expr start in
      let end_c = emit_c_expr end_ in
      Printf.sprintf "range(%s, %s)" start_c end_c
  | IRLiteral x ->
    match x with
    | IRInt x -> string_of_int x
    | IRFloat x -> string_of_float x
    | IRBool x -> if x then "1" else "0"
    | IRChar x -> "'" ^ Char.escaped x ^ "'"
    | IRString x -> "\"" ^ x ^ "\""
    | IRNull -> "NULL"
    | IRVoid -> "void"

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

and emit_c_stmt = function
  | IRExpr expr -> emit_c_expr expr ^ ";"
  | IREmpty -> "/* empty statement */"
  | IRVarDecl (name, expr) -> "int " ^ name ^ " = " ^ emit_c_expr expr ^ ";"
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

and emit_c_trait trait_def =
  let method_declarations = List.map (fun method_sig ->
    Printf.sprintf "    %s (*%s)();" (emit_c_type method_sig.func_sig_return_type) method_sig.func_sig_name
  ) trait_def.ir_methods in
  Printf.sprintf "struct %s {\n%s\n};" trait_def.ir_trait_name (String.concat "\n" method_declarations)

and emit_c_item for_header = function
  | IRFunc func -> emit_c_function func
  | IRStructDef struct_def -> if for_header then emit_c_struct struct_def else "// Struct definition omitted in source file\n"
  | IREnumDef enum_def -> emit_c_enum enum_def
  | IRTraitDef trait_def -> emit_c_trait trait_def
  | IRModuleDef _module_def -> "// Ignore module definition as C does not have a module system.\n"
  (* | IRImplDef impl_def -> "TODO: impl me" *)
  (* Add other item types as necessary *)

and emit_c_struct struct_def =
  let fields_str = String.concat ";\n  " (List.map (fun (name, ty) -> (emit_c_type ty) ^ " " ^ name) struct_def.ir_fields) in
  Printf.sprintf "struct %s {\n  %s;\n};" struct_def.ir_struct_name fields_str

and emit_c_enum enum_def =
  let variants_str = String.concat ",\n  " (List.map fst enum_def.ir_variants) in
  Printf.sprintf "enum %s {\n  %s\n};" enum_def.ir_enum_name variants_str

and emit_c for_header ir_program =
  let items_str = String.concat "\n" (List.map (emit_c_item for_header) ir_program) in
  "// Generated C Program\n\n" ^ items_str

(* Emit C header *)
and emit_c_header ir_program input_file =
   let buffer = Buffer.create 1024 in
  (* Extract base name without extension for header guard *)
  let base_name = Filename.remove_extension (Filename.basename input_file) in
  let header_guard = String.uppercase_ascii base_name ^ "_H" in
  Buffer.add_string buffer (Printf.sprintf "#ifndef %s\n#define %s\n\n" header_guard header_guard);

  (* Add struct declarations to header file *)
  let add_struct_declaration struct_def buffer =
    let fields_str = String.concat ";\n  " (List.map (fun (name, ty) -> (emit_c_type ty) ^ " " ^ name) struct_def.ir_fields) in
    Printf.sprintf "struct %s {\n  %s;\n};\n\n" struct_def.ir_struct_name fields_str |> Buffer.add_string buffer
  in

  (* Add function declarations to header file *)
  let add_function_declaration func buffer =
    let params_str = String.concat ", " (List.map (fun p -> (emit_c_type p.param_type) ^ " " ^ p.name) func.params) in
    let return_type_str = emit_c_type func.return_type in
    Printf.sprintf "%s %s(%s);\n" return_type_str func.func_name params_str |> Buffer.add_string buffer
  in

  (* Iterate over IR items and add declarations *)
  List.iter (function
    | IRStructDef struct_def -> add_struct_declaration struct_def buffer
    | IRFunc func -> add_function_declaration func buffer
    | _ -> ()  (* Ignore other IR items for header file *)
  ) ir_program;

  Buffer.add_string buffer (Printf.sprintf "\n#endif /* %s */\n" header_guard);
  Buffer.contents buffer


(* Emit C source *)
and emit_c_source ir_program input_file =
  let buffer = Buffer.create 1024 in
  (* Use the base name of the file for the include statement *)
  let header_file_name = (Filename.remove_extension (Filename.basename input_file)) ^ ".h" in
  Buffer.add_string buffer (Printf.sprintf "#include \"%s\"\n\n" header_file_name);

  let source_contents = emit_c false ir_program in
  Buffer.add_string buffer source_contents;
  Buffer.contents buffer

and convert_function_signature func =
  let params_str = String.concat ", " (List.map (fun p -> (emit_c_type p.param_type) ^ " " ^ p.name) func.params) in
  emit_c_type func.return_type ^ " " ^ func.func_name ^ "(" ^ params_str ^ ")"

and convert_function_body func =
  let body_str = String.concat "\n  " (List.map emit_c_stmt func.body) in
  "{\n  " ^ body_str ^ "\n}"
