open Ast
open Ir

(* Calculate the shape of a tensor *)
let rec calculate_shape (elements: expr list) : ir_shape =
  match elements with
  | [] -> []
  | Tensor elems :: _ -> List.length elements :: calculate_shape elems
  | _ -> [List.length elements]

let rec generate_ir (program: program) : ir_program =
  ir_of_program program
and ir_of_program (program: program) : ir_program =
  List.map ir_of_func program

and convert_param (param: string) : ir_param =
  { name = param; param_type = IRIntTy }  (* Assuming default type as IRIntTy *)

and ir_of_type (ty: ty) : ir_type =
  match ty with
  | IntTy -> IRIntTy
  | FloatTy -> IRFloatTy
  | VoidTy -> IRVoidTy
  (* | CharTy -> IRCharTy *)
  (* | StringTy -> IRStringTy *)
  | BoolTy -> IRBoolTy
  (* | TensorTy -> IRTensorTy *)

and ir_of_func (func: func) : ir_func =
  { func_name = func.proto.name;  params = List.map convert_param func.proto.params; body = List.map ir_of_stmt func.body; return_type = ir_of_type func.proto.return_type }

and ir_of_stmt (statement: stmt) : ir_stmt =
  match statement with
  | Expr expr -> IRExpr (ir_of_expr expr)
  | Return (Some expr) -> IRReturn (ir_of_expr expr)
  | Return None -> IRReturn (IRU8 0)
  | VarDecl (_, name, _, expr) -> IRVarDecl (name, ir_of_expr expr)
  | If (cond, then_stmts, else_stmts) -> IRIf (ir_of_expr cond, List.map ir_of_stmt then_stmts, List.map ir_of_stmt else_stmts)
  (* | While (cond, stmts) -> IRWhile (ir_of_expr cond, List.map ir_of_stmt stmts) *)
  | For (name, expr, body) -> IRFor (name, ir_of_expr expr, List.map ir_of_stmt body)
  | While (cond, body) -> IRWhile (ir_of_expr cond, List.map ir_of_stmt body)
  | Loop (body) -> IRLoop (List.map ir_of_stmt body)
  (* | Match (expr, cases) -> IRMatch (ir_of_expr expr, List.map ir_of_case cases) *)
  | _ -> failwith "Not implemented"

and ir_of_expr (expression: expr) : ir_expr =
  match expression with
  | Variable name -> IRVariable name
  | Call (name, args) -> IRCall (name, List.map ir_of_expr args)
  | BinOp (binop, left, right) -> IRBinOp (ir_of_binop binop, ir_of_expr left, ir_of_expr right)
  | Tensor elements -> IRTensor (calculate_shape elements, List.map ir_of_expr elements)
  | Literal x -> match x with
    | U8 x -> IRU8 x
    | U16 x -> IRU16 x
    | U32 x -> IRU32 x
    | U64 x -> IRU64 x
    | I8 x -> IRI8 x
    | I16 x -> IRI16 x
    | I32 x -> IRI32 x
    | I64 x -> IRI64 x
    | F32 x -> IRF32 x
    | F64 x -> IRF64 x
    | Char x -> IRChar x
    | String x -> IRString x
    | Bool x -> IRBool x
    | _ -> failwith "Not implemented"
    (* | Array x -> IRArray x
    | Custom x -> IRCustom x *)

and ir_of_binop (binop: binop) : ir_binop =
  match binop with
  | Add -> IRAdd
  | Sub -> IRSub
  | Mul -> IRMul
  | Div -> IRDiv
  | Mod -> IRMod
  | Pow -> IRPow
  | Eq -> IREq
  | Neq -> IRNeq
  | Lt -> IRLt
  | Gt -> IRGt
  | Leq -> IRLeq
  | Geq -> IRGeq
  | And -> IRAnd
  | Or -> IROr

(*
    and ir_of_func (func: func) : ir_func =
  let param_types = List.map (fun _ -> "tensor<*xf64>") func.proto.params in
  let params_with_types = List.mapi
    (fun i param -> { name = Printf.sprintf "arg%d" i; param_type = param })
    param_types in
  {
    func_name = func.proto.name;
    params = params_with_types;
    body = List.map ir_of_stmt func.body
  } *)