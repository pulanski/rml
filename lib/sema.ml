open Ast

let rec analyze_program = function
  | [] -> ()
  | func :: rest ->
      analyze_func func;
      analyze_program rest

and analyze_func func =
  (* Here you might want to add scope handling for function parameters *)
  analyze_stmt_list func.body

and analyze_stmt_list = function
  | [] -> ()
  | stmt :: rest ->
      analyze_stmt stmt;
      analyze_stmt_list rest

and analyze_stmt = function
  | Expr expr -> analyze_expr expr
  | Return None -> ()
  | Return (Some expr) -> analyze_expr expr
  | VarDecl (_, _, _, expr) -> analyze_expr expr
  | If (cond, then_stmt, else_stmt) ->
      analyze_expr cond;
      analyze_stmt_list then_stmt;
      analyze_stmt_list else_stmt
  | For (_name, expr, body) ->
      analyze_expr expr;
      analyze_stmt_list body
  | Match (_expr, _cases) ->
      failwith "Match not implemented"
  | While (cond, body) ->
      analyze_expr cond;
      analyze_stmt_list body
  (* | Break -> () *)
  | Loop body ->
      analyze_stmt_list body

and analyze_expr = function
  | Variable _ -> ()  (* Here you might want to check if variable is declared *)
  | Call (_, args) -> List.iter analyze_expr args
  | BinOp (_, lhs, rhs) ->
      analyze_expr lhs;
      analyze_expr rhs
  | Tensor elements -> List.iter analyze_expr elements
  | Literal _ -> ()
