type program = item list

and item =
  | FunctionItem of func
  | StructItem of struct_def
  | EnumItem of enum_def
  | TraitItem of trait_def
  | ModuleItem of module_def
  | UseItem of use_tree
  | TypeAliasItem of type_alias

and type_alias =
  | TypeAlias of string * ty

and use_tree =
  | SimplePathUseTree of simple_path
  | NestedUseTree of simple_path * use_tree list
  | GlobUseTree of simple_path
  | RenamedUseTree of simple_path * string
  (* | UseDecl of use_tree *)

and case =
  | Case of pattern * stmt list

and module_def = {
  module_name: string;
  module_items: item list;
  module_attributes: attribute list;
}

and literal =
  | LitChar of char
  | LitString of string
  | LitRawString of string
  | LitByte of char
  | LitByteString of string
  | LitRawByteString of string
  | LitInteger of int
  | LitFloat of float
  | LitBool of bool

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
  | VarDecl of mutable_flag * string * data_type option * expr
  | If of expr * stmt list * stmt list
  | For of string * expr * stmt list
  | Match of expr * case list
  | While of expr * stmt list
  | Loop of stmt list
  (* | Break
  | Continue *)
  | ItemDecl of item
  | Empty

and ty =
  | VoidTy
  | CharTy
  | StringTy
  | IntTy
  | FloatTy
  | BoolTy
  | TensorTy
  | ArrayTy of ty
  | FuncTy of ty list * ty  (* Function type: list of argument types and return type *)     (* case pattern -> block *)
  | CustomTy of string  (* For user-defined types *)

and pattern =
  | LiteralPattern of data_type
  | VariablePattern of string
  | TuplePattern of pattern list
  | CustomPattern of string * pattern list  (* For matching user-defined types *)

and visibility =
  | Public
  | Private
  | PubCrate
  | PubSelf
  | PubSuper
  | PubPath of simple_path

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

and path_expr =
  | PathInExpr of path
  | QualifiedPathInExpr of qualified_path

and path = path_segment list
and path_segment =
  | PathIdentSegment of path_ident_segment
  | PathExprSegment of path_ident_segment * generic_arg list (* Represents generic arguments in a path *)

and path_ident_segment =
  | PathIdent of string
  | Self
  | Super
  | Crate

and generic_arg =
  | TyArg of ty
  | LitArg of literal

and qualified_path =
  | QualifiedPath of ty * path (* Represents a fully qualified path with a type and a path *)

and expr =
  | Literal of literal
  | PathExpr of path_expr
  | Continue
  | Break
  | QualifiedPathExpr of qualified_path
  | Variable of string
  | Call of string * expr list
  (* | Call of expr * expr list  Function call with function expression and arguments *)
  | StructInit of string * (string * expr) list  (* Struct name and field initializations *)
  | EnumInit of string * string * expr option  (* Enum name, variant, and optional value *)
  | BinOp of binop * expr * expr
  | Tensor of expr list
  | Lambda of lambda  (* Lambda expression for anonymous functions *)
  (* | Literal of data_type <- TODO: figure out how we want to do user-defined types *)
  | RangeExpr of range_expr
  | ArrayExpr of array_expr
  | IndexExpr of expr * expr
  | Negation of expr
  | Not of expr
  | Borrow of expr
  | BorrowMut of expr
  | Deref of expr
  | Underscore
  | ErrorProp of expr
  | CompoundAssign of binop * expr * expr
  | TupleExpr of expr list
  | TupleIndexExpr of expr * int
  | MethodCall of expr * string * expr list
  | Return of expr option
  | FieldAccess of expr * string

and array_expr =
  | ArrayLit of expr list
  | ArrayRepeat of expr * expr
  | ArrayRange of range_expr

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
  | And
  | Or
  | Xor
  | Shl
  | Shr
  | Pow
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq

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

let transform_last_expr_to_return stmt_list =
  match List.rev stmt_list with
  | Expr (Return _) :: _ -> stmt_list  (* If last expr is already a return, do nothing *)
  | Expr expr :: rest -> List.rev (Expr (Return (Some expr)) :: rest)  (* Wrap last expr in return *)
  | _ -> stmt_list  (* No transformation if last stmt is not an expr *)
