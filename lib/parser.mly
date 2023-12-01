%{
  open Ast
%}

%token <float> NUMBER
%token PLUS MINUS MULT DIV LANGLE RANGLE
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE SEMICOLON EQUAL COMMA COLON
%token U32 F32
%token RETURN FN VAR LET IF ELSE MUT
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
| FN IDENT LPAREN params RPAREN block { { proto = { name = $2; params = $4 }; body = $6 } }

params:
| IDENT { [$1] }
| IDENT COMMA params { $1 :: $3 }
|  { [] }

block:
| LBRACE stmt_list RBRACE { $2 }

stmt_list:
| stmt SEMICOLON stmt_list { $1 :: $3 }
|  { [] }

stmt:
| LET opt_mut IDENT opt_type EQUAL expr { VarDecl ($2, $3, $4, $6) }
| RETURN expr { Return (Some $2) }
| expr { Expr ($1) }

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
| IDENT LPAREN expr_list RPAREN { Call ($1, $3) }
| LPAREN expr RPAREN { $2 }
| expr PLUS expr { BinOp (Add, $1, $3) }
| expr MINUS expr { BinOp (Sub, $1, $3) }
| expr MULT expr { BinOp (Mul, $1, $3) }
| expr DIV expr { BinOp (Div, $1, $3) }
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