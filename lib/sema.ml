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
  | VarDecl (_, _, expr) -> analyze_expr expr

and analyze_expr = function
  | Number _ -> ()
  | Variable _ -> ()  (* Here you might want to check if variable is declared *)
  | Call (_, args) -> List.iter analyze_expr args
  | BinOp (_, lhs, rhs) ->
      analyze_expr lhs;
      analyze_expr rhs
  | Tensor elements -> List.iter analyze_expr elements
