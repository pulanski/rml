open Ast

module StringMap = Map.Make(String)

type environment = {
  vars: ty StringMap.t;
  structs: struct_def StringMap.t;
  enums: enum_def StringMap.t;
  traits: trait_def StringMap.t;
}

let data_type_to_ty = function
  | I8 _ -> IntTy
  | I16 _ -> IntTy
  | I32 _ -> IntTy
  | I64 _ -> IntTy
  | U8 _ -> IntTy
  | U16 _ -> IntTy
  | U32 _ -> IntTy
  | U64 _ -> IntTy
  | F32 _ -> FloatTy
  | F64 _ -> FloatTy
  | Char _ -> CharTy
  | String _ -> StringTy
  | Bool _ -> BoolTy
  | TTensor _ -> TensorTy
  | Function _proto -> FuncTy ([], VoidTy)
  (* | Array dt -> ArrayTy (data_type_to_ty dt)
  | Custom _ -> CustomTy *)
  | _ -> failwith "Type not implemented"

let rec type_of_expr env = function
  | Variable v ->
      (try StringMap.find v env.vars
       with Not_found -> failwith ("Unbound variable " ^ v))
  | Call (f, args) ->
      let arg_types = List.map (type_of_expr env) args in
      type_of_call env f arg_types
  | BinOp (op, lhs, rhs) ->
      type_of_binop env op lhs rhs
  | Tensor elems ->
      (* Assume all elements of tensor have same type; check first element *)
      (match elems with
      | [] -> failwith "Empty tensor not allowed"
      | e::_ -> type_of_expr env e)
  | Literal l -> type_of_literal l
  | Lambda lambda -> type_of_lambda env lambda
  | _ -> failwith "Type not implemented"
  (* Add other cases as necessary *)

and type_of_call env f arg_types =
  let f_type =
    try StringMap.find f env.vars
    with Not_found -> failwith ("Unbound function " ^ f)
  in
  match f_type with
  | FuncTy (param_types, return_type) ->
      if List.length param_types <> List.length arg_types then
        failwith "Incorrect number of arguments in function call";
      List.iter2 (fun expected_type arg_type ->
        if expected_type <> arg_type then
          failwith "Type mismatch in function call arguments") param_types arg_types;
      return_type
  | _ -> failwith (f ^ " is not a function")

and type_of_binop env op lhs rhs =
  let lhs_type = type_of_expr env lhs in
  let rhs_type = type_of_expr env rhs in
  match op with
  | Add | Sub | Mul | Div | Mod ->
      if lhs_type <> IntTy || rhs_type <> IntTy then
        failwith "Type mismatch in arithmetic operation";
      IntTy
  | Eq | Neq | Lt | Gt | Leq | Geq ->
      if lhs_type <> rhs_type then
        failwith "Type mismatch in comparison operation";
      BoolTy
  | And | Or ->
      if lhs_type <> BoolTy || rhs_type <> BoolTy then
        failwith "Type mismatch in boolean operation";
      BoolTy
  | _ -> failwith "Invalid binary operator"

and type_of_lambda _env { lparams = _; lbody = _ } =
  (* Implement type checking for lambda expressions *)
  (* Extend environment with lambda parameters and check body *)
  failwith "Lambda not implemented"
  (* let env' = List.fold_left (fun env (name, ty) -> StringMap.add name ty env) env lparams in
  type_of_expr env' lbody *)

and type_of_literal = function
  | LitChar _ -> CharTy
  | LitString _ -> StringTy
  | LitRawString _ -> StringTy
  | LitByte _ -> IntTy
  | LitRawByteString _ -> StringTy
  | LitByteString _ -> StringTy
  | LitInteger _ -> IntTy
  | LitFloat _ -> FloatTy
  | LitBool _ -> BoolTy
  (* TODO: *)
  (*
  | LitTensor _ -> TensorTy
  | LitArray _ -> ArrayTy
  | LitFunction _ -> FuncTy ([], VoidTy)
  *)

let rec analyze_program env = function
  | [] -> ()
  | item :: rest -> (
      match item with
      | FunctionItem func ->
          analyze_func env func
      | StructItem struct_def ->
          let env = analyze_struct_def env struct_def
          in analyze_program env rest
      | EnumItem enum_def ->
          let env = analyze_enum_def env enum_def
          in analyze_program env rest
      | TraitItem trait_def ->
          let env = analyze_trait_def env trait_def in analyze_program env rest
      | ModuleItem module_def ->
          let env = analyze_module_def env module_def in analyze_program env rest
      | UseItem _ -> analyze_program env rest (* TODO: add support for ensuring that the module is defined *)
      | TypeAliasItem _ -> analyze_program env rest (* TODO: add support for type aliases *)
      )

and analyze_trait_def env trait_def =
  if StringMap.mem trait_def.trait_name env.traits then
    failwith ("Duplicate trait definition: " ^ trait_def.trait_name);
  (* Analyze each trait item *)
  let env = { env with traits = StringMap.add trait_def.trait_name trait_def env.traits } in
  env

and analyze_module_def env module_def =
  let module_env = { env with vars = StringMap.empty; structs = StringMap.empty; enums = StringMap.empty } in
  let _ = analyze_program module_env module_def.module_items in
  (* Optionally, update the parent environment with exports from the module *)
  env

and analyze_struct_def env struct_def =
  if StringMap.mem struct_def.struct_name env.structs then
    failwith ("Duplicate struct definition: " ^ struct_def.struct_name);
  (* Optionally, check field types here *)
  let env = { env with structs = StringMap.add struct_def.struct_name struct_def env.structs } in
  env

and analyze_enum_def env enum_def =
  if StringMap.mem enum_def.enum_name env.enums then
    failwith ("Duplicate enum definition: " ^ enum_def.enum_name);
  (* Optionally, check variant types here *)
  let env = { env with enums = StringMap.add enum_def.enum_name enum_def env.enums } in
  env

and analyze_struct_init env struct_name field_inits =
  let struct_def =
    try StringMap.find struct_name env.structs
    with Not_found -> failwith ("Undefined struct: " ^ struct_name)
  in
  List.iter (fun (field_name, init_expr) ->
    let field_type =
      try List.assoc field_name struct_def.fields
      with Not_found -> failwith ("Undefined field: " ^ field_name)
    in
    let init_type = type_of_expr env init_expr in
    if field_type <> init_type then
      failwith ("Type mismatch in field initialization for " ^ field_name)
  ) field_inits;
  env

and analyze_enum_init env enum_name variant value_opt =
  let enum_def =
    try StringMap.find enum_name env.enums
    with Not_found -> failwith ("Undefined enum: " ^ enum_name)
  in
  match List.assoc_opt variant enum_def.variants with
  | Some expected_type_opt ->
      (match expected_type_opt, value_opt with
       | Some expected_type, Some value ->
           let value_type = type_of_expr env value in
           if expected_type <> value_type then
             failwith ("Type mismatch in enum variant value for " ^ variant)
       | None, None -> ()
       | _ -> failwith ("Incorrect usage of enum variant " ^ variant))
  | None -> failwith ("Undefined variant: " ^ variant)

and analyze_stmt_list env stmts =
  List.fold_left (fun acc_env stmt -> analyze_stmt acc_env stmt) env stmts

(* and update_env env name ty =
  StringMap.add name ty env *)
and update_env env name ty =
    (* Update the environment with a new or modified variable *)
  { env with vars = StringMap.add name ty env.vars }

and analyze_func env func =
  let { proto = { name = _; params; _ }; body; func_attributes = _ } = func in
  let func_env = List.fold_left (fun acc_env (name, ty) ->
    update_env acc_env name ty) env params in
  ignore (analyze_stmt_list func_env body); ()  (* Ignore the returned environment *)

and analyze_stmt env = function
  | VarDecl (_mut, name, opt_type, expr) ->
      let expr_type = type_of_expr env expr in
      (match opt_type with
       | Some dt ->
           let ty = data_type_to_ty dt in
           if ty <> expr_type then failwith "Type mismatch in variable declaration"
       | None -> ());
      update_env env name expr_type
  | Expr expr ->
      ignore (type_of_expr env expr); env
  | Return (Some expr) ->
      ignore (type_of_expr env expr); env
  | Return None -> env
    | If (cond, then_stmts, else_stmts) ->
      ignore (type_of_expr env cond);
      let _then_env = analyze_stmt_list env then_stmts in
      let _else_env = analyze_stmt_list env else_stmts in
      (* TODO: Decide how to merge then_env and else_env if necessary *)
      env  (* or the merged environment *)
  | While (cond, body) ->
      ignore (type_of_expr env cond);
      let _body_env = analyze_stmt_list env body in
      env  (* TODO: or body_env if want to consider changes in the loop *)
  | _ -> failwith "Not implemented"

and analyze_expr env expr =
  match expr with
  | Variable v ->
      ignore (try StringMap.find v env.vars
              with Not_found -> failwith ("Unbound variable " ^ v))
  | Call (f, args) ->
      let arg_types = List.map (type_of_expr env) args in
      ignore (type_of_call env f arg_types)
  | BinOp (op, lhs, rhs) ->
      ignore (type_of_binop env op lhs rhs)
  | Tensor elems ->
      ignore (List.map (analyze_expr env) elems)
  | Literal _ -> ()
  | Lambda lambda -> analyze_lambda env lambda
  | _ -> failwith "Type not implemented"
  (* | StructInit (struct_name, field_inits) ->
      analyze_struct_init env struct_name field_inits
  | EnumInit (enum_name, variant, value_opt) ->
      analyze_enum_init env enum_name variant value_opt *)
  (* Add other cases as necessary *)

and analyze_lambda env { lparams; lbody } =
  (* Extend environment with lambda parameters and analyze body *)
  let env' = List.fold_left (fun env (name, ty) -> update_env env name ty) env lparams in
  analyze_expr env' lbody

let type_check_program program =
  let env = {
    vars = StringMap.empty;
    structs = StringMap.empty;
    enums = StringMap.empty;
    traits = StringMap.empty;
  } in
  analyze_program env program
