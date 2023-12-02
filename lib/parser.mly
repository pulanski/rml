%{
  open Ast
%}

%token <float> NUMBER
%token PLUS MINUS MULT DIV LANGLE RANGLE
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE SEMICOLON EQUAL COMMA COLON RARROW
%token U32 F32
%token RETURN FN VAR LET IF ELSE MUT FOR WHILE BREAK CONTINUE IN MATCH CASE TRUE FALSE VOID BOOL LOOP
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
| func_list EOF { $1 }

func_list:
| func func_list { $1 :: $2 }
|  { [] }

func:
| FN IDENT LPAREN params RPAREN RARROW return_type block {
    { proto = { name = $2; params = $4; return_type = $7 }; body = $8 }
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
| LET opt_mut IDENT opt_type EQUAL expr SEMICOLON { VarDecl ($2, $3, $4, $6) }
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