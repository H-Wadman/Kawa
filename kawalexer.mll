{

  open Lexing
  open Kawaparser
  open Kawa

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 18 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "true",       TRUE;
      "false",      FALSE;
      "var",        VAR;
      "attribute",  ATTRIBUTE;
      "method",     METHOD;
      "class",      CLASS;
      "new",        NEW;
      "this",       THIS;
      "if",         IF;
      "else",       ELSE;
      "while",      WHILE;
      "return",    RETURN;
      "print",      PRINT;
      "int",        TINT;
      "bool",       TBOOL;
      "void",       TVOID;
      "main",       MAIN;
      "extends",    EXTENDS;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)

  
  let make_pos lb = { start_p=lexeme_start_p lb; end_p=lexeme_end_p lb}

}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*

rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }

  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "["  { LSQB }
  | "]"  { RSQB }

  (*Binary op symbols*)
  | "+"  { PLUS (make_pos lexbuf)}
  | "-"  { MINUS (make_pos lexbuf)}
  | "*"  { STAR (make_pos lexbuf)}
  | "/"  { SLASH (make_pos lexbuf)}
  | "%"  { PERCENT (make_pos lexbuf)}
  | "=="  { EQ (make_pos lexbuf)}
  | "!="  { NEQ (make_pos lexbuf)}
  | "<"  { LT (make_pos lexbuf)}
  | ">"  { GT (make_pos lexbuf)}
  | "<="  { LE (make_pos lexbuf)}
  | ">="  { GE (make_pos lexbuf)}
  | "&&" { AND (make_pos lexbuf)}
  | "||" { OR (make_pos lexbuf)}

  | "!"  { NOT (make_pos lexbuf) }

  | "="  { ASSIGN }

  | "."  { DOT }
  | "," { COMMA }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
