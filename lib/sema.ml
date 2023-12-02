open Ast
module StringMap = Map.Make(String)

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
      (try StringMap.find v env
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
  (* Add other cases as necessary *)

and type_of_call env f arg_types =
  let f_type =
    try StringMap.find f env
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
      IntTy  (* Assuming these operations return an integer *)
  | Eq | Neq | Lt | Gt | Leq | Geq ->
      if lhs_type <> rhs_type then
        failwith "Type mismatch in comparison operation";
      BoolTy  (* Comparison operations return a boolean *)
  | And | Or ->
      if lhs_type <> BoolTy || rhs_type <> BoolTy then
        failwith "Type mismatch in boolean operation";
      BoolTy
  | _ -> failwith "Invalid binary operator"

and type_of_lambda env { lparams; lbody } =
  (* Implement type checking for lambda expressions *)
  (* Extend environment with lambda parameters and check body *)
  let env' = List.fold_left (fun env (name, ty) -> StringMap.add name ty env) env lparams in
  type_of_expr env' lbody

and type_of_literal = function
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
  | _ -> failwith "Type not implemented"
  (* | Array _ -> ArrayTy
  | Function _ -> FuncTy *)
  (* ... *)

let rec analyze_program env = function
  | [] -> ()
  | func :: rest ->
      analyze_func env func;
      analyze_program env rest

and analyze_stmt_list env stmts =
  List.iter (analyze_stmt env) stmts

and update_env env name ty =
  (* Update the environment with a new or modified variable *)
  StringMap.add name ty env

and type_of_binop env op lhs rhs =
  let lhs_type = type_of_expr env lhs in
  let rhs_type = type_of_expr env rhs in
  (* Check that lhs_type and rhs_type are appropriate for the operator op *)
    match op with
    | Add | Sub | Mul | Div | Mod ->
        if lhs_type <> IntTy || rhs_type <> IntTy then failwith "Type mismatch in arithmetic operation"
    | Eq | Neq | Lt | Gt | Leq | Geq ->
        if lhs_type <> rhs_type then failwith "Type mismatch in comparison operation"
    | And | Or ->
        if lhs_type <> BoolTy || rhs_type <> BoolTy then failwith "Type mismatch in boolean operation"
    | _ -> failwith "Invalid binary operator"

and analyze_func env func =
let { proto = { name = _; params; _ }; body } = func in
  let env = List.fold_left (fun env (name, ty) -> StringMap.add name ty env) env params in
  analyze_stmt_list env body;
  (* Optionally, check the return type of the function *)

and analyze_stmt env = function
  | Expr expr -> ignore (type_of_expr env expr)
  | Return None -> ()
  | Return (Some expr) -> ignore (type_of_expr env expr)
   | VarDecl (_mut, name, opt_type, expr) ->
      let expr_type = type_of_expr env expr in
      (match opt_type with
       | Some dt ->
           let ty = data_type_to_ty dt in
           if ty <> expr_type then failwith "Type mismatch in variable declaration"
       | None -> ());
      let env = update_env env name expr_type in
        analyze_expr env expr
  | If (cond, then_stmt, else_stmt) ->
      let cond_type = type_of_expr env cond in
      if cond_type <> BoolTy then failwith "Condition in if statement must be boolean";
      analyze_stmt_list env then_stmt;
      analyze_stmt_list env else_stmt
  | For (_name, _expr, _body) ->
      (* Here handle the for loop variable and its scope *)
      failwith "For loop not implemented"
  | Match (_expr, _cases) ->
      (* Implement match statement type checking *)
        failwith "Match not implemented"
  | While (cond, body) ->
      let cond_type = type_of_expr env cond in
      if cond_type <> BoolTy then failwith "Condition in while statement must be boolean";
      analyze_stmt_list env body
  | Loop body ->
      analyze_stmt_list env body

and analyze_expr env expr =
  match expr with
  | Variable v ->
      ignore (try StringMap.find v env
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
  (* Add other cases as necessary *)

and analyze_lambda env { lparams; lbody } =
  (* Extend environment with lambda parameters and analyze body *)
  let env' = List.fold_left (fun env (name, ty) -> update_env env name ty) env lparams in
  analyze_expr env' lbody

and type_of_call _env _f _arg_types =
    (* TODO: Implement function call type checking, including higher-order functions *)
    failwith "Function call type checking not implemented"
    (* let f_type = type_of_expr env f in
    match f_type with
    | FuncTy (param_types, _return_type) ->
        (* Check that arg_types match param_types *)
        if List.length param_types <> List.length arg_types then failwith "Incorrect number of arguments";
        List.iter2 (fun param_type arg_type ->
            if param_type <> arg_type then failwith "Type mismatch in function call") param_types arg_types;
    | _ -> failwith "Type mismatch in function call" *)

let type_check_program program =
  let env = StringMap.empty in
  analyze_program env program
