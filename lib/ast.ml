type program = item list

and item =
  | FunctionItem of func
  | StructItem of struct_def
  | EnumItem of enum_def
  | TraitItem of trait_def
  | ModuleItem of module_def

and case =
  | Case of pattern * stmt list

and module_def = {
  module_name: string;
  module_items: item list;
  module_attributes: attribute list;
}

and trait_def = {
  trait_name: string;
  items: trait_item list;
  trait_attributes: attribute list;
}

and trait_item =
  | TraitFunc of trait_func
  | TraitType of trait_type
  | TraitConst of trait_const

and trait_func = {
  func_proto: prototype;
  default_impl: stmt list option;
}

and trait_type = {
  type_name: string;
  type_def: ty option;
}

and trait_const = {
  const_name: string;
  const_type: ty;
  const_value: expr option;
}

and func = {
  proto: prototype;
  body: stmt list;
  func_attributes: attribute list;
}

and mutable_flag =
  | Mutable
  | Immutable

and prototype = {
  name: string;
  params: (string * ty) list;
  return_type: ty;
}

and struct_def = {
  struct_name: string;
  fields: (string * ty) list;  (* Field name and its type *)
  struct_attributes: attribute list;
}

and enum_def = {
  enum_name: string;
  variants: (string * ty option) list;  (* Variant name and optional associated type *)
  enum_attributes: attribute list;
}

and stmt =
  | Expr of expr
  | Return of expr option
  | VarDecl of mutable_flag * string * data_type option * expr
  | If of expr * stmt list * stmt list
  | For of string * expr * stmt list
  | Match of expr * case list
  | While of expr * stmt list
  | Loop of stmt list
  | Break
  | Continue

and ty =
  | VoidTy
  | CharTy
  | StringTy
  | IntTy
  | FloatTy
  | BoolTy
  | TensorTy
  | FuncTy of ty list * ty  (* Function type: list of argument types and return type *)     (* case pattern -> block *)

and pattern =
  | LiteralPattern of data_type
  | VariablePattern of string
  | TuplePattern of pattern list
  | CustomPattern of string * pattern list  (* For matching user-defined types *)

and attribute =
  | InnerAttribute of attr
  | OuterAttribute of attr

and attr = {
  path: simple_path;
  input: attr_input option;
}

and attr_input =
  | AttrExpr of expr
  (* possibly other types of input *)

and simple_path = simple_path_segment list

and simple_path_segment = string  (* possibly extend this to include more complex path segments *)

and expr =
  | Variable of string
  | Call of string * expr list
  (* | Call of expr * expr list  Function call with function expression and arguments *)
  | StructInit of string * (string * expr) list  (* Struct name and field initializations *)
  | EnumInit of string * string * expr option  (* Enum name, variant, and optional value *)
  | BinOp of binop * expr * expr
  | Tensor of expr list
  | Lambda of lambda  (* Lambda expression for anonymous functions *)
  | Literal of data_type
  (* | Tuple of expr list *)
  | RangeExpr of range_expr

and range_expr =
  | Range of expr * expr
  | RangeFrom of expr
  | RangeTo of expr
  | RangeFull
  | RangeInclusive of expr * expr
  | RangeToInclusive of expr

and lambda = {
  lparams: (string * ty) list;  (* Parameters with types *)
  lbody: expr;                  (* Body of the lambda function *)
}

and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | And
  | Or

and data_type =
  | I8 of int
  | I16 of int
  | I32 of int
  | I64 of int
  | U8 of int
  | U16 of int
  | U32 of int
  | U64 of int
  | F32 of float
  | F64 of float
  | Char of char
  | String of string
  | Bool of bool
  | TTensor of tensor_type
  | Array of data_type
  | Custom of string  (* For user-defined types *)
  | Function of prototype

and tensor_type = {
  shape: shape;
}

and shape = int list
