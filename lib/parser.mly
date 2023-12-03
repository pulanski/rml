%{
  open Ast
%}

%token <float> NUMBER
%token PLUS MINUS MULT DIV LANGLE RANGLE
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE SEMICOLON EQ COMMA COLON RARROW PATH_SEP BANG HASH CARET AMP PIPE
%token U32 F32
%token RETURN FN LET IF ELSE MUT FOR WHILE BREAK CONTINUE IN MATCH CASE TRUE FALSE VOID BOOL LOOP STRUCT ENUM TYPE TRAIT CONST IMPL USE AS PUB SELF SUPER MOD DO
%token <string> IDENT
%token EOF

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <Ast.program> program
%type <Ast.expr> expr
%type <Ast.func> func

%%

program:
  | item_list EOF { $1 }

item_list:
  | item item_list { $1 :: $2 }
  |  { [] }

item:
  | attributes func { FunctionItem { $2 with func_attributes = $1 } }
  | attributes struct_def { StructItem { $2 with struct_attributes = $1 } }
  | attributes enum_def { EnumItem { $2 with enum_attributes = $1 } }
  | attributes trait_def { TraitItem { $2 with trait_attributes = $1 } }
  // | attributes impl_def { ImplItem { $2 with impl_attributes = $1 } }
  | attributes module_def { ModuleItem { $2 with module_attributes = $1 } }

module_def:
  | MOD IDENT LBRACE item_list RBRACE {
      { module_name = $2; module_items = $4; module_attributes = [] }
  }

trait_def:
  | TRAIT IDENT LBRACE trait_items RBRACE { { trait_name = $2; items = $4; trait_attributes = [] } }

trait_items:
  | trait_item trait_items { $1 :: $2 }
  | { [] }

trait_item:
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

func:
| FN IDENT LPAREN params RPAREN RARROW return_type block {
    { proto = { name = $2; params = $4; return_type = $7 }; body = $8; func_attributes = [] }
}

return_type:
| VOID { VoidTy }
| U32 { IntTy }
| F32 { FloatTy }
| BOOL { BoolTy }

params:
| param COMMA params { $1 :: $3 }
| param { [$1] }
|  { [] }

param:
| IDENT COLON ty { ($1, $3) }
| IDENT { ($1, IntTy) }

struct_def:
  | STRUCT IDENT LBRACE field_defs RBRACE { { struct_name = $2; fields = $4; struct_attributes = [] } }

field_defs:
  | field_def COMMA field_defs { $1 :: $3 }
  | field_def { [$1] }
  |  { [] }

field_def:
  | IDENT COLON ty { ($1, $3) }

enum_def:
  | ENUM IDENT LBRACE variant_defs RBRACE { { enum_name = $2; variants = $4; enum_attributes = [] } }

variant_defs:
  | variant_def COMMA variant_defs { $1 :: $3 }
  | variant_def { [$1] }
  |  { [] }

variant_def:
  | IDENT { ($1, None) }
  // | IDENT OF ty { ($1, Some $3) } // TODO: figure out what we want syntax to be here

ty:
| U32 { IntTy }
| F32 { FloatTy }
| BOOL { BoolTy }
// | tensor_type { TensorTy $1 }
| tensor_type { TensorTy }
| VOID { VoidTy }
// TODO: support first-class functions
// | FN LPAREN params RPAREN RARROW ty { FuncTy ($3, $6) }

block:
| LBRACE stmt_list RBRACE { $2 }

stmt_list:
| stmt stmt_list { $1 :: $2 }
|  { [] }

stmt:
| LET opt_mut IDENT opt_type EQ expr SEMICOLON { VarDecl ($2, $3, $4, $6) }
| RETURN expr SEMICOLON { Return (Some $2) }
| if_stmt { $1 }
| WHILE expr block { While ($2, $3) }
| LOOP block { Loop ($2) }
| FOR IDENT IN expr block { For ($2, $4, $5) }
| MATCH expr LBRACE match_cases RBRACE { Match ($2, $4) }
| expr { Expr ($1) }

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
| NUMBER { LiteralPattern (F32 $1) }
(* Other patterns *)

opt_tensor_type:
| (* empty *) { None }
| tensor_type { Some (TTensor $1) }

tensor_type:
| LANGLE shape RANGLE { { shape = $2 } }

shape:
| NUMBER { [int_of_float $1] }
| NUMBER COMMA shape { (int_of_float $1) :: $3 }

expr:
| NUMBER { Literal (F32 $1) }
| IDENT { Variable $1 }
| TRUE { Literal (Bool true) }
| FALSE { Literal (Bool false) }
| IDENT LPAREN expr_list RPAREN { Call ($1, $3) }
| LPAREN expr RPAREN { $2 }
| expr PLUS expr { BinOp (Add, $1, $3) }
| expr MINUS expr { BinOp (Sub, $1, $3) }
| expr MULT expr { BinOp (Mul, $1, $3) }
| expr DIV expr { BinOp (Div, $1, $3) }
| expr LANGLE expr { BinOp (Lt, $1, $3) }
| expr RANGLE expr { BinOp (Gt, $1, $3) }
| LBRACKET tensor_list RBRACKET { Tensor ($2) }
| IDENT LBRACE field_init_list RBRACE { StructInit ($1, $3) }

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
| NUMBER { [$1] }

opt_literal_list:
| (* empty *) { [] }
| literal_list { $1 }

literal_list:
| tensor_literal { $1 }
| tensor_literal COMMA opt_literal_list { $1 :: $3 }

attributes:
  | attribute attributes { $1 :: $2 }
  | { [] }

attribute:
  | HASH BANG LBRACKET attr RBRACKET { InnerAttribute $4 }
  | HASH BANG LBRACKET attr RBRACKET { OuterAttribute $4 }

attr:
  | simple_path attr_input { { path = $1; input = Some $2 } }
  | simple_path { { path = $1; input = None } }

attr_input:
  | EQ expr { AttrExpr $2 }
  (* possibly other types of input *)

simple_path:
  | simple_path_segment { [$1] }
  | simple_path PATH_SEP simple_path_segment { $1 @ [$3] }

simple_path_segment:
  | IDENT { $1 }