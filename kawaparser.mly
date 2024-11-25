%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
(*Keywords except types*)
%token TRUE FALSE VAR ATTRIBUTE METHOD CLASS NEW THIS IF ELSE WHILE RETURN PRINT TINT TBOOL TVOID MAIN

%token LPAR RPAR BEGIN END SEMI
%token EOF

%start program
%type <Kawa.program> program

%%

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
