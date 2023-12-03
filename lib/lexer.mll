{
  open Parser
}

rule token = parse
  | [' ' '\t' '\r' '\n']  { token lexbuf }  (* Skip whitespace *)
  | "//" [^ '\n']* '\n'    { token lexbuf }  (* Skip comments *)
  | "//!" [^ '\n']* '\n'  { token lexbuf }   (* Skip //! comments *)
  | "/*"                  { comment lexbuf } (* Skip block comments *)
  | "::"                  { PATH_SEP }
  | '+'                   { PLUS }
  | '-'                   { MINUS }
  | "+="                  { PLUSEQ }
  | "-="                  { MINUSEQ }
  | "*="                  { STAREQ }
  | "/="                  { DIVEQ }
  | "%="                  { MODEQ }
  | "&="                  { ANDEQ }
  | "|="                  { OREQ }
  | "^="                  { XOREQ }
  | "<<="                 { SHLEQ }
  | ">>="                 { SHREQ }
  | '.'                   { DOT }
  | ".."                  { DOTDOT }
  | "..="                 { DOTDOTEQ }
  | "<<"                  { SHL }
  | ">>"                  { SHR }
  | '#'                   { HASH }
  | '^'                   { CARET }
  | '!'                   { BANG }
  | '&'                   { AMP }
  | "&&"                  { AMPAMP }
  | '|'                   { PIPE }
  | "||"                  { PIPEPIPE }
  | '?'                   { QUESTION }
  | "=="                  { EQEQ }
  | "!="                  { NOT_EQ }
  | '*'                   { STAR }
  | '/'                   { DIV }
  | '<'                   { LANGLE }
  | '>'                   { RANGLE }
  | "<="                  { LTEQ }
  | ">="                  { GTEQ }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '['                   { LBRACKET }
  | ']'                   { RBRACKET }
  | '{'                   { LBRACE }
  | '}'                   { RBRACE }
  | "->"                  { RARROW }
  | ';'                   { SEMICOLON }
  | ':'                   { COLON }
  | '='                   { EQ }
  | ","                   { COMMA }
  | "return"              { RETURN }
  | "if"                  { IF }
  | "else"                { ELSE }
  | "fn"                  { FN }
  | "struct"              { STRUCT }
  | "enum"                { ENUM }
  | "impl"                { IMPL }
  | "type"                { TYPE }
  | "use"                 { USE }
  | "as"                  { AS }
  | "pub"                 { PUB }
  | "self"                { SELF }
  | "super"               { SUPER }
  | "trait"               { TRAIT }
  | "while"               { WHILE }
  | "do"                  { DO }
  | "for"                 { FOR }
  | "loop"                { LOOP }
  | "in"                  { IN }
  | "break"               { BREAK }
  | "continue"            { CONTINUE }
  | "match"               { MATCH }
  | "case"                { CASE }
  | "void"                { VOID }
  | "bool"                { BOOL }
  | "true"                { TRUE }
  | "false"               { FALSE }
  | "mut"                 { MUT }
  | "u32"                 { U32 }
  | "f32"                 { F32 }
  | "mod"                 { MOD }
  | "let"                 { LET }
  (* /* Number literals */ *)
  | ['0'-'9']+ as lxm { INTEGER_LITERAL(lxm) }
  (* | ['0'-'9']+ '.' ['0'-'9']* as lxm { FLOAT_LITERAL(float_of_string lxm) } *)
  | ['0'-'9']+ '.' ['0'-'9']* as lxm { FLOAT_LITERAL(lxm) }
  (* /* Character and String literals */ *)
  | '\'' ( [^'\\'] | '\\' _ ) '\'' { CHAR_LITERAL(Lexing.lexeme lexbuf) }
  | '\"' ( [^'\\' '\n'] | '\\' _ )* '\"' { STRING_LITERAL(Lexing.lexeme lexbuf) }
  (* /* Raw String literals */ *)
  | "r\"" [^'\"']* "\"r" { RAW_STRING_LITERAL(Lexing.lexeme lexbuf) }
  (* /* Byte and Byte String literals */ *)
  | "b'" ( [^'\\'] | '\\' _ ) "'" { BYTE_LITERAL(Lexing.lexeme lexbuf) }
  | "b\"" ( [^'\\'] | '\\' _ )* "\"" { BYTE_STRING_LITERAL(Lexing.lexeme lexbuf) }
  (* /* Raw Byte String literals */ *)
  | "br\"" [^'\"']* "\"br" { RAW_BYTE_STRING_LITERAL(Lexing.lexeme lexbuf) }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { IDENT(lxm) }
  | eof                   { EOF }
  | _ as char {
    let pos = Lexing.lexeme_start_p lexbuf in
    let pos_info = Printf.sprintf "File \"%s\", line %d, character %d"
                   pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in
    failwith (Printf.sprintf "Unrecognized character '%s' at %s" (Char.escaped char) pos_info)
  }

and comment = parse
  | "*/"            { token lexbuf }
  | _               { comment lexbuf }
  | eof             { failwith "Unclosed comment" }