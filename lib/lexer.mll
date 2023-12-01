{
  open Parser
}

rule token = parse
  | [' ' '\t' '\r' '\n']  { token lexbuf }  (* Skip whitespace *)
  | '#' [^ '\n']* '\n'    { token lexbuf }  (* Skip comments *)
  | '+'                   { PLUS }
  | '-'                   { MINUS }
  | '*'                   { MULT }
  | '/'                   { DIV }
  | '<'                   { LANGLE }
  | '>'                   { RANGLE }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '['                   { LBRACKET }
  | ']'                   { RBRACKET }
  | '{'                   { LBRACE }
  | '}'                   { RBRACE }
  | ';'                   { SEMICOLON }
  | ':'                   { COLON }
  | '='                   { EQUAL }
  | ","                   { COMMA }
  | "return"              { RETURN }
  | "if"                  { IF }
  | "else"                { ELSE }
  | "fn"                  { FN }
  | "mut"                 { MUT }
  | "u32"                 { U32 }
  | "f32"                 { F32 }
  | "var"                 { VAR }
  | "let"                 { LET }
  | ['0'-'9']+ as lxm     { NUMBER(float_of_string lxm) }
  | ['0'-'9']+ '.' ['0'-'9']* as lxm { NUMBER(float_of_string lxm) }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { IDENT(lxm) }
  | eof                   { EOF }
  | _ as char {
    let pos = Lexing.lexeme_start_p lexbuf in
    let pos_info = Printf.sprintf "File \"%s\", line %d, character %d"
                   pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in
    failwith (Printf.sprintf "Unrecognized character '%s' at %s" (Char.escaped char) pos_info)
  }
