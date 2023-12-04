%{
  open Ast
%}

%token PLUS MINUS STAR DIV LANGLE RANGLE
%token PLUSEQ MINUSEQ STAREQ DIVEQ MODEQ ANDEQ OREQ XOREQ SHLEQ SHREQ UNDERSCORE
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE SEMICOLON EQ COMMA COLON RARROW PATH_SEP BANG HASH CARET AMP PIPE SHL SHR PIPEPIPE AMPAMP
%token U32 F32
%token RETURN FN LET IF ELSE MUT FOR WHILE BREAK CONTINUE IN MATCH CASE TRUE FALSE VOID BOOL LOOP STRUCT ENUM TYPE TRAIT CONST IMPL USE AS PUB SELF SUPER MOD DO CRATE
%token DOT DOTDOT DOTDOTEQ QUESTION EQEQ NOT_EQ LTEQ GTEQ
%token <string> IDENT
%token <string> STRING_LITERAL
%token <string> RAW_STRING_LITERAL
%token <string> BYTE_LITERAL
%token <string> BYTE_STRING_LITERAL
%token <string> RAW_BYTE_STRING_LITERAL
%token <string> CHAR_LITERAL
%token <string> INTEGER_LITERAL
%token <string> FLOAT_LITERAL
%token EOF

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <Ast.program> program
%type <Ast.expr> expr
%type <Ast.func> func

%%

/* Main Program Structure */
program: item_list EOF { $1 }

item_list:
  | item item_list { $1 :: $2 }
  | { [] }

item:
  | function_item { $1 }
  | struct_item { $1 }
  | enum_item { $1 }
  | trait_item { $1 }
  | module_item { $1 }
  | use_decl { UseItem $1 }
  | type_alias { TypeAliasItem $1 }

/* Function Definitions */
function_item:
  | attributes func { FunctionItem { $2 with func_attributes = $1 } }

func:
| FN IDENT LPAREN params RPAREN RARROW return_type block {
    { proto = { name = $2; params = $4; return_type = $7 }; body = $8; func_attributes = [] }
}

params:
| param COMMA params { $1 :: $3 }
| param { [$1] }
|  { [] }

param:
| IDENT COLON ty { ($1, $3) }
| IDENT { ($1, IntTy) }

/* Struct Definitions */
struct_item:
  | attributes struct_def { StructItem { $2 with struct_attributes = $1 } }

struct_def:
  | STRUCT IDENT LBRACE field_defs RBRACE { { struct_name = $2; fields = $4; struct_attributes = [] } }

field_defs:
  | field_def COMMA field_defs { $1 :: $3 }
  | field_def { [$1] }
  |  { [] }

field_def:
  | IDENT COLON ty { ($1, $3) }

/* Enum Definitions */
enum_item:
  | attributes enum_def { EnumItem { $2 with enum_attributes = $1 } }

enum_def:
  | ENUM IDENT LBRACE variant_defs RBRACE { { enum_name = $2; variants = $4; enum_attributes = [] } }

variant_defs:
  | variant_def COMMA variant_defs { $1 :: $3 }
  | variant_def { [$1] }
  |  { [] }

variant_def:
  | IDENT { ($1, None) }
  // | IDENT OF ty { ($1, Some $3) } // TODO: figure out what we want syntax to be here

/* Trait Definitions */
trait_item:
  | attributes trait_def { TraitItem { $2 with trait_attributes = $1 } }

trait_def:
  | TRAIT IDENT LBRACE associated_items RBRACE { { trait_name = $2; items = $4; trait_attributes = [] } }

associated_items:
  | associated_item associated_items { $1 :: $2 }
  | { [] }

associated_item:
  | trait_func { TraitFunc $1 }
  | trait_type { TraitType $1 }
  | trait_const { TraitConst $1 }

trait_func:
  | FN IDENT LPAREN params RPAREN RARROW return_type SEMICOLON {
      { func_proto = { name = $2; params = $4; return_type = $7 }; default_impl = None }
    }
  | FN IDENT LPAREN params RPAREN RARROW return_type block {
      { func_proto = { name = $2; params = $4; return_type = $7 }; default_impl = Some $8 }
    }

trait_type:
  | TYPE IDENT SEMICOLON { { type_name = $2; type_def = None } }

trait_const:
  | CONST IDENT COLON ty SEMICOLON { { const_name = $2; const_type = $4; const_value = None } }
  | CONST IDENT COLON ty EQ expr SEMICOLON { { const_name = $2; const_type = $4; const_value = Some $6 } }

/* Module Definitions */
module_item:
  | attributes module_def { ModuleItem { $2 with module_attributes = $1 } }

module_def:
  | MOD IDENT LBRACE item_list RBRACE {
      { module_name = $2; module_items = $4; module_attributes = [] }
  }

/* Use Declarations */
use_decl:
  | USE use_tree SEMICOLON { UseDecl $2 }

use_tree:
  | simple_path { SimplePathUseTree $1 }
  | simple_path PATH_SEP LBRACE use_tree_list RBRACE { NestedUseTree ($1, $4) }
  | simple_path PATH_SEP STAR { GlobUseTree $1 }
  | simple_path AS IDENT { RenamedUseTree ($1, $3) }

use_tree_list:
  | use_tree COMMA use_tree_list { $1 :: $3 }
  | use_tree { [$1] }

type_alias:
| TYPE IDENT EQ ty SEMICOLON { TypeAlias ($2, $4) }

/* Expressions and Statements */

// Expressions
// https://doc.rust-lang.org/stable/reference/expressions.html
expr:
| literal_expr { Literal ($1) }
| path_expr { PathExpr ($1) }
| operator_expr { $1 }
| grouped_expr { $1 }
| array_expr { ArrayExpr ($1) }
// | await_expr { $1 } TODO: support async/await
| index_expr { $1 }
| tuple_expr { TupleExpr ($1) }
| tuple_index_expr { $1 }
// struct_expr { $1 } TODO: support struct expressions
| call_expr { $1 }
| method_call_expr { $1 }
| field_expr { $1 }
// | closure_expr { $1 } TODO: support closures
// | async_block_expr { $1 } TODO: support async blocks
| continue_expr { $1 }
| break_expr { $1 }
// | loop_expr { $1 } TODO: support loop
| range_expr { RangeExpr ($1) }
| return_expr { $1 }
// | underscore_expr { $1 } TODO: support underscore
// | macro_invocation { $1 } TODO: support macro invocations
// | IDENT LBRACE field_init_list RBRACE { StructInit ($1, $3) }
// | yield_expr { $1 } TODO: support yield

break_expr:
| BREAK { Break }

continue_expr:
| CONTINUE { Continue }

path_expr:
| path_in_expr { PathInExpr ($1) }
| qualified_path_in_expr { QualifiedPathInExpr ($1) }

path_in_expr:
| PATH_SEP path_expr_segment path_expr_segments { $2 :: $3 }
| path_expr_segment path_expr_segments { $1 :: $2 }

path_expr_segments:
| PATH_SEP path_expr_segment path_expr_segments { $2 :: $3 }
| { [] }

path_expr_segment:
| path_ident_segment { PathIdentSegment $1 }
| path_ident_segment PATH_SEP generic_args { PathExprSegment ($1, $3) }

path_ident_segment:
| IDENT { PathIdent $1 }
| SUPER { Super }
| SELF { Self }
| CRATE { Crate }

generic_args:
| LANGLE RANGLE { [] }
| LANGLE generic_arg_list RANGLE { $2 }

generic_arg_list:
| generic_arg COMMA generic_arg_list { $1 :: $3 }

generic_arg:
| ty { TyArg $1 }
// TODO: refactor to GenericArgsConst
| literal_expr { LitArg $1 }
| UNDERSCORE literal_expr { LitArg $2 }

qualified_path_in_expr:
| qualified_path_type PATH_SEP path_expr_segment path_expr_segments { QualifiedPath ($1, $3 :: $4) }

qualified_path_type:
| LANGLE ty RANGLE { $2 }
| LANGLE ty AS ty RANGLE { $2 }

return_expr:
| RETURN expr { Return (Some $2) }
| RETURN { Return None }

operator_expr:
| borrow_expr { $1 }
| deref_expr { $1 }
| error_propagation_expr { $1 }
| negation_expr { $1 }
| arith_or_logical_binary_expr { $1 }
| comparison_expr { $1 }
| lazy_bool_expr { $1 }
// | type_cast_expr { $1 } TODO: support type casts
| compound_assign_expr { $1 }

field_expr:
| expr DOT IDENT { FieldAccess ($1, $3) }

method_call_expr:
| expr DOT IDENT LPAREN expr_list RPAREN { MethodCall ($1, $3, $5) }
// | expr DOT path_expr_segment LPAREN call_params RPAREN { MethodCall ($1, $3, $5) }

call_expr:
| IDENT LPAREN expr_list RPAREN { Call ($1, $3) } // TODO: add support for trailing commas here and in other places
// | expr LPAREN expr_list RPAREN { Call ($1, $3) }

tuple_index_expr:
| expr DOT INTEGER_LITERAL { TupleIndexExpr ($1, int_of_string $3) }

tuple_expr:
| LPAREN expr_list RPAREN { $2 }

grouped_expr:
| LPAREN expr RPAREN { $2 }

compound_assign_expr:
| expr PLUSEQ expr { CompoundAssign (Add, $1, $3) }
| expr MINUSEQ expr { CompoundAssign (Sub, $1, $3) }
| expr STAREQ expr { CompoundAssign (Mul, $1, $3) }
| expr DIVEQ expr { CompoundAssign (Div, $1, $3) }
| expr MODEQ expr { CompoundAssign (Mod, $1, $3) }
| expr ANDEQ expr { CompoundAssign (And, $1, $3) }
| expr OREQ expr { CompoundAssign (Or, $1, $3) }
| expr XOREQ expr { CompoundAssign (Xor, $1, $3) }
| expr SHLEQ expr { CompoundAssign (Shl, $1, $3) }
| expr SHREQ expr { CompoundAssign (Shr, $1, $3) }

lazy_bool_expr:
| expr PIPEPIPE expr { BinOp (Or, $1, $3) }
| expr AMPAMP expr { BinOp (And, $1, $3) }

comparison_expr:
| expr EQEQ expr { BinOp (Eq, $1, $3) }
| expr NOT_EQ expr { BinOp (Neq, $1, $3) }
| expr LANGLE expr { BinOp (Lt, $1, $3) }
| expr RANGLE expr { BinOp (Gt, $1, $3) }
| expr LTEQ expr { BinOp (Leq, $1, $3) }
| expr GTEQ expr { BinOp (Geq, $1, $3) }

error_propagation_expr:
| expr QUESTION { ErrorProp ($1) }

deref_expr:
| STAR expr { Deref ($2) }

negation_expr:
| MINUS expr %prec UMINUS { Negation ($2) }
| BANG expr { Not ($2) }

borrow_expr:
| AMP expr { Borrow ($2) }
| AMP MUT expr { BorrowMut ($3) }

array_expr:
  | LBRACKET expr_list RBRACKET { ArrayLit($2) }
  | LBRACKET expr SEMICOLON expr RBRACKET { ArrayRepeat($2, $4) }

index_expr:
  | expr LBRACKET expr RBRACKET { IndexExpr($1, $3) }

literal_expr:
  | TRUE { LitBool true }
  | FALSE { LitBool false }
  | CHAR_LITERAL { LitChar (String.get $1 0) }
  | STRING_LITERAL { LitString $1 }
  | RAW_STRING_LITERAL { LitRawString $1 }
  | BYTE_STRING_LITERAL { LitByteString $1 }
  | RAW_BYTE_STRING_LITERAL { LitRawByteString $1 }
  | BYTE_LITERAL { LitByte (String.get $1 0) }
  | INTEGER_LITERAL { LitInteger (int_of_string $1) }
  | FLOAT_LITERAL { LitFloat (float_of_string $1) }

arith_or_logical_binary_expr:
  | expr PLUS expr { BinOp (Add, $1, $3) }
  | expr MINUS expr { BinOp (Sub, $1, $3) }
  | expr STAR expr { BinOp (Mul, $1, $3) }
  | expr DIV expr { BinOp (Div, $1, $3) }
  | expr MOD expr { BinOp (Mod, $1, $3) }
  | expr LANGLE expr { BinOp (Lt, $1, $3) }
  | expr RANGLE expr { BinOp (Gt, $1, $3) }
  | expr AMP expr { BinOp (And, $1, $3) }
  | expr PIPE expr { BinOp (Or, $1, $3) }
  | expr CARET expr { BinOp (Xor, $1, $3) }
  | expr SHL expr { BinOp (Shl, $1, $3) }
  | expr SHR expr { BinOp (Shr, $1, $3) }

range_expr:
| expr DOTDOT expr { Range($1, $3) }                  (* start..end *)
  | expr DOTDOT { RangeFrom($1) }                       (* start.. *)
  | DOTDOT expr { RangeTo($2) }                         (* ..end *)
  | DOTDOT { RangeFull }                                (* .. *)
  | expr DOTDOTEQ expr { RangeInclusive($1, $3) }       (* start..=end *)
  | DOTDOTEQ expr { RangeToInclusive($2) }              (* ..=end *)

// Statements
// https://doc.rust-lang.org/stable/reference/statements.html
stmt:
| SEMICOLON { Empty }
| declaration_stmt { $1 }
| if_stmt { $1 }
| WHILE expr block { While ($2, $3) }
| LOOP block { Loop ($2) }
| FOR IDENT IN expr block { For ($2, $4, $5) }
| MATCH expr LBRACE match_cases RBRACE { Match ($2, $4) }
| expr { Expr ($1) }
| expr SEMICOLON { Expr ($1) }

/* Declaration Statements */
declaration_stmt:
  | let_statement { $1 }
  | item_declaration { $1 }

item_declaration:
  | item { ItemDecl $1 }

let_statement:
| LET opt_mut IDENT opt_type EQ expr SEMICOLON { VarDecl ($2, $3, $4, $6) }

/* Types */
ty:
| U32 { IntTy }
| F32 { FloatTy }
| BOOL { BoolTy }
// | tensor_type { TensorTy $1 }
| tensor_type { TensorTy }
| VOID { VoidTy }
// TODO: support first-class functions
// | FN LPAREN params RPAREN RARROW ty { FuncTy ($3, $6) }

return_type:
| VOID { VoidTy }
| U32 { IntTy }
| F32 { FloatTy }
| BOOL { BoolTy }

block:
| LBRACE stmt_list RBRACE { $2 }

stmt_list:
| stmt stmt_list { $1 :: $2 }
|  { [] }

if_stmt:
  | IF expr block else_clause { If ($2, $3, $4) }
;

else_clause:
| ELSE block { $2 }
| ELSE if_stmt { [$2] }
| (* empty *) { [] }

match_cases:
| match_case match_cases { $1 :: $2 }
| match_case { [$1] }

match_case:
| CASE pattern RARROW block { Case ($2, $4) }

pattern:
| IDENT { VariablePattern $1 }
(* Other patterns *)

opt_tensor_type:
| (* empty *) { None }
| tensor_type { Some (TTensor $1) }

tensor_type:
| LANGLE shape RANGLE { { shape = $2 } }

shape:
| INTEGER_LITERAL { [(int_of_string $1)] }
| INTEGER_LITERAL COMMA shape { (int_of_string $1) :: $3 }

field_init_list:
  | field_init COMMA field_init_list { $1 :: $3 }
  | field_init { [$1] }
  |  { [] }

field_init:
  | IDENT EQ expr { ($1, $3) }

opt_mut:
| MUT { Mutable }
| { Immutable }

opt_type:
| COLON U32 { None } (* TODO: support other types *)
| { None }

tensor_list:
| expr { [$1] }
| expr COMMA tensor_list { $1 :: $3 }

expr_list:
| expr { [$1] }
| expr COMMA expr_list { $1 :: $3 }

tensor_literal:
| opt_literal_list { $1 }
| INTEGER_LITERAL { [$1] }

opt_literal_list:
| (* empty *) { [] }
| literal_list { $1 }

literal_list:
| tensor_literal { $1 }
| tensor_literal COMMA opt_literal_list { $1 :: $3 }

attributes:
  | attribute attributes { $1 :: $2 }
  | { [] }

// TODO: support block exprs
//   Syntax
// BlockExpression :
//    {
//       InnerAttribute*
//       Statements?
//    }

// Statements :
//       Statement+
//    | Statement+ ExpressionWithoutBlock
//    | ExpressionWithoutBlock

// block_expr:
//   | inner_attrs stmt_list { BlockExpr ($2) }
//   | inner_attrs stmt_list expr { BlockExpr ($2 @ [Expr $3]) }
//   | inner_attrs expr { BlockExpr ([Expr $2]) }

attribute:
  | inner_attribute { $1 }
  | outer_attribute { $1 }

inner_attribute:
  | HASH BANG LBRACKET attr RBRACKET { InnerAttribute $4 }

outer_attribute:
  | HASH LBRACKET attr RBRACKET { OuterAttribute $3 }

attr:
  | simple_path attr_input { { path = $1; input = Some $2 } }
  | simple_path { { path = $1; input = None } }

attr_input:
  | EQ expr { AttrExpr $2 }
  (* possibly other types of input *)

visibility:
  | PUB { Public }
  | PUB LPAREN CRATE RPAREN { PubCrate }
  | PUB LPAREN SELF RPAREN { PubSelf }
  | PUB LPAREN SUPER RPAREN { PubSuper }
  | PUB LPAREN simple_path RPAREN { PubPath $3 }

simple_path:
  | simple_path_segment { [$1] }
  | simple_path PATH_SEP simple_path_segment { $1 @ [$3] }

simple_path_segment:
  | IDENT { $1 }