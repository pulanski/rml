%{
  open Ast
%}

%token <float> NUMBER
%token PLUS MINUS MULT DIV LANGLE RANGLE
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE SEMICOLON EQUAL COMMA
%token RETURN DEF VAR
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
| DEF IDENT LPAREN params RPAREN block { { proto = { name = $2; params = $4 }; body = $6 } }

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
| VAR IDENT EQUAL expr { VarDecl ($2, $4) }
// | VAR IDENT opt_tensor_type EQUAL expr { VarDecl ($2, $3, $5) }
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
| NUMBER { Number $1 }
| IDENT { Variable $1 }
| IDENT LPAREN expr_list RPAREN { Call ($1, $3) }
| LPAREN expr RPAREN { $2 }
| expr PLUS expr { BinOp (Add, $1, $3) }
| expr MINUS expr { BinOp (Sub, $1, $3) }
| expr MULT expr { BinOp (Mul, $1, $3) }
| expr DIV expr { BinOp (Div, $1, $3) }
| LBRACKET tensor_list RBRACKET { Tensor ($2) }

tensor_list:
| expr { [$1] }
| expr COMMA tensor_list { $1 :: $3 }

expr_list:
| expr { [$1] }
| expr COMMA expr_list { $1 :: $3 }

// tensorLiteral ::= [ literalList ] | number NOTE: [ ] means optional
tensor_literal:
| opt_literal_list { $1 }
| NUMBER { [$1] }

opt_literal_list:
| (* empty *) { [] }
| literal_list { $1 }

// literalList ::= tensorLiteral | tensorLiteral, literalList
literal_list:
| tensor_literal { $1 }
| tensor_literal COMMA opt_literal_list { $1 :: $3 }