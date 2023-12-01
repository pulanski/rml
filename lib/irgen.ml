open Ast
open Ir

(* Calculate the shape of a tensor *)
let rec calculate_shape (elements: expr list) : ir_shape =
  match elements with
  | [] -> []
  | Tensor elems :: _ -> List.length elements :: calculate_shape elems
  | _ -> [List.length elements]  (* Assuming all elements at this level are scalars *)

let rec generate_ir (program: program) : ir_program =
  ir_of_program program
and ir_of_program (program: program) : ir_program =
  List.map ir_of_func program

and ir_of_func (func: func) : ir_func =
  { name = func.proto.name;
    params = func.proto.params;
    body = List.map ir_of_stmt func.body }

and ir_of_stmt (statement: stmt) : ir_stmt =
  match statement with
  | Expr expr -> IRExpr (ir_of_expr expr)
  | Return (Some expr) -> IRReturn (ir_of_expr expr)
  | Return None -> IRReturn (IRNumber 0.0)
  | VarDecl (name, _, expr) -> IRVarDecl (name, ir_of_expr expr)

and ir_of_expr (expression: expr) : ir_expr =
  match expression with
  | Number n -> IRNumber n
  | Variable x -> IRVariable x
  | Call (name, args) -> IRCall (name, List.map ir_of_expr args)
  | BinOp (op, lhs, rhs) -> IRBinOp (ir_of_binop op, ir_of_expr lhs, ir_of_expr rhs)
  | Tensor elements -> let shape = calculate_shape elements in
      IRTensor (shape, List.map ir_of_expr elements)

and ir_of_binop (binop: binop) : ir_binop =
  match binop with
  | Add -> IRAdd
  | Sub -> IRSub
  | Mul -> IRMul
  | Div -> IRDiv
