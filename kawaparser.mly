%{
  open Lexing
  open Kawa

  type cstructor = string * expr list
  type decl = typ * string
%}

%token <int> INT
%token <string> IDENT

(*Keywords except types*)
%token TRUE FALSE VAR ATTRIBUTE METHOD CLASS NEW THIS IF ELSE
%token WHILE RETURN PRINT TINT TBOOL TVOID MAIN EXTENDS
(*Symbols*)
%token PLUS MINUS STAR SLASH PERCENT EQ NEQ LT GT LE GE AND OR NOT ASSIGN

%token DOT COMMA

%token LPAR RPAR BEGIN END SEMI
%token EOF

%nonassoc RETURN

%left OR
%left AND
%left EQ NEQ
%left LT GT LE GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%left DOT

%start program
%type <Kawa.program> program

%type <Kawa.var_decl> variable_decl
%type <Kawa.attr_decl> attribute_decl
%type <Kawa.class_def> class_def

%%

decl:
  t=typ id=IDENT { t, id }

variable_decl:
| VAR d=decl SEMI
  {
    let (t, id) = d in
    match t with
  | TVoid -> failwith "Cannot declare a variable of type void";
  | _ -> (id, t)
  }
;

attribute_decl:
  | ATTRIBUTE d=decl SEMI
    {
      let (t, id) = d in
      (match t with
    | TVoid -> failwith "Cannot declare an attribute of type void";
    | _ -> (id, t))
    }
;

method_def:
  | METHOD d=decl LPAR args=separated_list(COMMA, decl) RPAR BEGIN
    local=list(variable_decl) code=list(instruction) END
  {
    let (t, id) = d in
    let permute tup_lst =
      List.map (fun (one, two) -> (two, one)) tup_lst in
      {method_name=id; code; params=(permute args); locals=local; return=t}
  }
;

class_def:
| CLASS id=IDENT BEGIN attr=list(attribute_decl) meth=list(method_def)
    { {class_name=id; attributes=attr; methods=meth; parent=None} }
| CLASS id=IDENT EXTENDS prnt=IDENT BEGIN attr=list(attribute_decl) meth=list(method_def)
    { {class_name=id; attributes=attr; methods=meth; parent=(Some (prnt)) } }
;

typ:
  | TVOID { TVoid }
  | TINT { TInt }
  | TBOOL { TBool }
  | id=IDENT { TClass(id) }
;

program:
| var_decl=list(variable_decl)
  cls_def=list(class_def)
MAIN BEGIN main=list(instruction) END EOF
    { {classes=cls_def; globals=var_decl; main} }
;

instruction:
  | PRINT LPAR e=expression RPAR SEMI { Print(e) }
  | m=mem ASSIGN e=expression SEMI { Set(m, e) }
  | IF LPAR e=expression RPAR BEGIN ins_if=list(instruction) ELSE BEGIN ins_else=list(instruction) END { If (e, ins_if, ins_else) }
  | WHILE LPAR e=expression RPAR BEGIN inst=list(instruction) END { While (e, inst) }
  | RETURN e=expression { Return (e) } %prec RETURN
  | e=expression SEMI { Expr(e) }
;

mem:
  | id=IDENT { Var(id) }
  | expr=expression DOT id=IDENT { Field (expr, id) }
;
unop:
  | NOT { Not }
  (* Will give priorty issues later with Sub *)
  | MINUS { Opp }
;

expression:
| n=INT { Int(n) }
| b=TRUE { Bool(true) }
| b=FALSE { Bool(false) }
| THIS { This }
| m=mem { Get (m) }
| op=unop expr=expression { Unop (op, expr)}
| e1=expression PLUS e2=expression { Binop (Add, e1, e2) } %prec PLUS
| e1=expression MINUS e2=expression { Binop (Sub, e1, e2) } %prec MINUS
| e1=expression STAR e2=expression { Binop (Mul, e1, e2) } %prec STAR
| e1=expression SLASH e2=expression { Binop (Div, e1, e2) } %prec SLASH
| e1=expression PERCENT e2=expression { Binop (Rem, e1, e2) } %prec PERCENT
| e1=expression EQ e2=expression { Binop (Eq, e1, e2) } %prec EQ
| e1=expression NEQ e2=expression { Binop (Neq, e1, e2) } %prec NEQ
| e1=expression LT e2=expression { Binop (Lt, e1, e2) } %prec LT
| e1=expression GT e2=expression { Binop (Gt, e1, e2) } %prec GT
| e1=expression LE e2=expression { Binop (Le, e1, e2) } %prec LE
| e1=expression GE e2=expression { Binop (Ge, e1, e2) } %prec GE
| e1=expression AND e2=expression { Binop (And, e1, e2) } %prec AND
| e1=expression OR e2=expression { Binop (Or, e1, e2) } %prec OR
| LPAR e=expression RPAR { e }
| NEW id=IDENT { New (id) }
| NEW id=IDENT LPAR args=separated_list(COMMA, expression) RPAR { NewCstr (id, args) }
| e=expression DOT id=IDENT LPAR args=separated_list(COMMA, expression) RPAR { MethCall (e, id, args) }
;