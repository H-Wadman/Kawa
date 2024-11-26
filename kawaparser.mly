%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT

(*Keywords except types*)
%token TRUE FALSE VAR ATTRIBUTE METHOD CLASS NEW THIS IF ELSE
%token WHILE RETURN PRINT TINT TBOOL TVOID MAIN EXTENDS
(*Symbols*)
%token PLUS MINUS STAR SLASH PERCENT EQ NEQ LT GT LE GE AND OR NOT ASSIGN

%token LPAR RPAR BEGIN END SEMI
%token EOF

%start program
%type <Kawa.program> program

%type <Kawa.var_decl> variable_declaration
%type <Kawa.attr_decl> attribute_declaration

%%

variable_declaration:
| VAR t=typ id=IDENT SEMI
  {
    match t with
  | TVoid -> failwith "Cannot declare a variable of type void"
  | _ -> (id, var_typ)
  }
;

attribute_declaration:
  | ATTRIBUTE t=typ id=IDENT SEMI
    {
      match t with
    | TVoid -> failwith "Cannot declare an attribute of type void"
    | _ -> (id, attr_typ)
    }

typ:
  | TVOID { TVoid }
  | TINT { TInt }
  | TBOOL { TBool }
  | id=IDENT { TClass(id) }
;

program:
| MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals=[]; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
;

expression:
| n=INT { Int(n) }
;

