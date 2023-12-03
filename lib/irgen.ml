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
  List.map ir_of_item program

and ir_of_item (item: item) : ir_item =
  match item with
  | FunctionItem func -> IRFunc (ir_of_func func)
  | StructItem struct_def -> ir_of_struct struct_def
  | EnumItem enum_def -> ir_of_enum enum_def
  | TraitItem trait_def -> ir_of_trait trait_def
  | ModuleItem module_def -> ir_of_module module_def

and ir_of_trait (trait_def: trait_def) : ir_item =
  IRTraitDef {
    ir_trait_name = trait_def.trait_name;
    ir_methods = List.map (fun trait_item ->
      match trait_item with
      | TraitFunc method_def -> {
          func_sig_name = method_def.func_proto.name;
          func_sig_params = List.map ir_of_type (List.map snd method_def.func_proto.params);
          func_sig_return_type = ir_of_type method_def.func_proto.return_type
        }
      | _ -> failwith "Non-function items in trait not supported yet"
    ) trait_def.items
  }



and ir_of_module (_module_def: module_def) : ir_item =
  let module_name = _module_def.module_name in
  let module_items = _module_def.module_items in
  IRModuleDef {
    ir_module_name = module_name;
    ir_module_items = List.map ir_of_item module_items
  }

and ir_of_struct (struct_def: struct_def) : ir_item =
  IRStructDef {
    ir_struct_name = struct_def.struct_name;
    ir_fields = List.map (fun (name, ty) -> (name, ir_of_type ty)) struct_def.fields
  }

and ir_of_enum (enum_def: enum_def) : ir_item =
  IREnumDef {
    ir_enum_name = enum_def.enum_name;
    ir_variants = List.map (fun (name, ty_opt) -> (name, Option.map ir_of_type ty_opt)) enum_def.variants
  }

and ir_of_func (func: func) : ir_func =
  { func_name = func.proto.name;
    params = List.map ir_of_param func.proto.params;
    body = List.map ir_of_stmt func.body;
    return_type = ir_of_type func.proto.return_type }

and ir_of_type (ty: ty) : ir_type =
  match ty with
  | IntTy -> IRIntTy
  | FloatTy -> IRFloatTy
  | VoidTy -> IRVoidTy
  | CharTy -> IRCharTy
  | StringTy -> IRStringTy
  | BoolTy -> IRBoolTy
  | TensorTy -> IRTensorTy
  | FuncTy _ -> IRFuncTy

and ir_of_param ((name, ty): (string * ty)) : ir_param =
  { name = name; param_type = ir_of_type ty }

and ir_of_stmt (statement: stmt) : ir_stmt =
  match statement with
  | Expr expr -> IRExpr (ir_of_expr expr)
  | Return (Some expr) -> IRReturn (ir_of_expr expr)
  | Return None -> IRReturn (IRU8 0)
  | VarDecl (_, name, _, expr) -> IRVarDecl (name, ir_of_expr expr)
  | If (cond, then_stmts, else_stmts) -> IRIf (ir_of_expr cond, List.map ir_of_stmt then_stmts, List.map ir_of_stmt else_stmts)
  | For (name, expr, body) -> IRFor (name, ir_of_expr expr, List.map ir_of_stmt body)
  | While (cond, body) -> IRWhile (ir_of_expr cond, List.map ir_of_stmt body)
  | Loop (body) -> IRLoop (List.map ir_of_stmt body)
  | Match (expr, cases) -> IRMatch (ir_of_expr expr, List.map ir_of_case cases)
  | Break -> IRBreak
  | Continue -> IRContinue

and ir_of_case (case: case) : ir_match_case =
  match case with
  | Case (pattern, stmts) -> IRCase (ir_of_pattern pattern, List.map ir_of_stmt stmts)

and ir_of_pattern (pattern: pattern) : ir_pattern =
  match pattern with
  | LiteralPattern lit -> IRLiteralPattern (ir_of_expr (Literal lit))
  | VariablePattern name -> IRVariablePattern name
  | TuplePattern pats -> IRTuplePattern (List.map ir_of_pattern pats)
  | CustomPattern (name, pats) -> IRCustomPattern (name, List.map ir_of_pattern pats)

and ir_of_expr (expression: expr) : ir_expr =
  match expression with
  | Variable name -> IRVariable name
  | Call (name, args) -> IRCall (name, List.map ir_of_expr args)
  | BinOp (binop, left, right) -> IRBinOp (ir_of_binop binop, ir_of_expr left, ir_of_expr right)
  | Tensor elements -> IRTensor (calculate_shape elements, List.map ir_of_expr elements)
  | Literal x -> (match x with
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
    (* | Array x -> IRArray x
    | Custom x -> IRCustom x *)
    | _ -> failwith "Literal: Not implemented")
  | _ -> failwith "Expr: Not implemented"

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