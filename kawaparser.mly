%{
  open Lexing
  open Kawa

  type cstructor = string * expr list
  type decl = typ * string
  type member_decl = | AttrDecl of attr_decl | MethDecl of method_def
  type method_content = VarDecl of var_decl | Code of instr
%}

%token <int> INT
%token <string> IDENT

(*Keywords except types*)
(*%token VAR ATTRIBUTE METHOD*)

%token TRUE FALSE CLASS NEW THIS IF ELSE
%token WHILE RETURN PRINT TINT TBOOL TVOID MAIN EXTENDS
(*Symbols*)
%token <Kawa.loc> PLUS MINUS STAR SLASH PERCENT EQ NEQ LT GT LE GE AND OR 
%token <Kawa.loc> NOT 
%token ASSIGN

%token DOT COMMA

%token AT
%token LPAR RPAR BEGIN END SEMI LSQB RSQB
%token EOF

%left OR
%left AND
%left EQ NEQ
%left LT GT LE GE
%left PLUS MINUS
%left STAR SLASH PERCENT

%nonassoc UNARY NEWARR

%left DOT LSQB



%start program
%type <Kawa.program> program

%type <Kawa.var_decl> variable_decl
%type <Kawa.attr_decl> attribute_decl
%type <Kawa.class_def> class_def

%%

decl:
  t=typ id=IDENT { t, id }

variable_decl:
| d=decl SEMI
  {
    let (t, id) = d in
    match t with
  | TVoid -> failwith "Cannot declare a variable of type void";
  | _ -> (id, t, None)
  }
| d=decl ASSIGN e=expression SEMI
  {
    let (t, id) = d in
    match t with
  | TVoid -> failwith "Cannot declare a variable of type void";
  | _ -> (id, t, Some e)
  }
;

attribute_decl:
  | d=decl SEMI
    {
      let (t, id) = d in
      (match t with
    | TVoid -> failwith "Cannot declare an attribute of type void";
    | _ -> (id, t))
    }
;

local_typ:
  | TVOID { TVoid }
  | TINT { TInt }
  | TBOOL { TBool }
  | id=IDENT { TClass(id) }
  | AT t=typ 
    { 
      match t with TArray(t, n) -> t | _ -> failwith "Array syntax used for non array type"
    }
;

local_decl:
  t=local_typ id=IDENT { t, id }

local_variable_decl:
| d=local_decl SEMI
  {
    let (t, id) = d in
    match t with
  | TVoid -> failwith "Cannot declare a variable of type void";
  | _ -> (id, t, None)
  }
| d=local_decl ASSIGN e=expression SEMI
  {
    let (t, id) = d in
    match t with
  | TVoid -> failwith "Cannot declare a variable of type void";
  | _ -> (id, t, Some e)
  }
;

method_content:
  | v=local_variable_decl { VarDecl v }
  | i=instruction { Code i }

method_def:
  | d=decl LPAR args=separated_list(COMMA, decl) RPAR BEGIN
    content=list(method_content) END
  {
    (*local=list(variable_decl) code=list(instruction)*)
    let rec verify_order lst =
      let rec aux l = 
        match l with 
        | [] -> ()
        | hd :: tl -> (match hd with 
          | VarDecl _ -> failwith "Variable declaration must be at the beginning of a method"
          | _ -> aux tl) 
      in
      match lst with 
      | [] -> () 
      | hd :: tl -> (match hd with 
        | VarDecl _ -> verify_order tl
        | _ -> aux tl)
    in
    verify_order content;

    let local, code = List.partition_map (
      fun x -> match x with 
      | VarDecl v -> Left v
      | Code i -> Right i) content in
    let (t, id) = d in
    let permute tup_lst =
      List.map (fun (one, two) -> (two, one)) tup_lst in
      {method_name=id; code; params=(permute args); locals=local; return=t; tag=None}
  }
;

member_decl:
  | attr=attribute_decl { AttrDecl attr }
  | meth=method_def { MethDecl meth }

class_def:
| CLASS id=IDENT BEGIN mems=list(member_decl) END
    { 
      let (attrs, meths) = List.partition_map (
        fun x -> match x with 
        | AttrDecl a -> Left a
        | MethDecl m -> Right m) mems in
      {class_name=id; attributes=attrs; methods=meths; parent=None} 
    }
| CLASS id=IDENT EXTENDS prnt=IDENT BEGIN mems=list(member_decl) END
    { 
      let (attrs, meths) = List.partition_map (
        fun x -> match x with 
        | AttrDecl a -> Left a
        | MethDecl m -> Right m) mems in
      {class_name=id; attributes=attrs; methods=meths; parent=(Some prnt)} 
    }
;

typ:
  | TVOID { TVoid }
  | TINT { TInt }
  | TBOOL { TBool }
  | id=IDENT { TClass(id) }
  | t=typ LSQB n=INT RSQB { TArray(t, n) }
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
  | IF LPAR e=expression RPAR BEGIN ins_if=list(instruction) END ELSE BEGIN ins_else=list(instruction) END { If (e, ins_if, ins_else) }
  | WHILE LPAR e=expression RPAR BEGIN inst=list(instruction) END { While (e, inst) }
  | RETURN e=expression SEMI { Return (e) }
  | e=expression SEMI { Expr(e) }
;

mem:
  | id=IDENT { Var(id) }
  | expr=expression DOT id=IDENT { Field (expr, id) }
  | e=expression LSQB idx=expression RSQB { ArrAccess(e, idx) }
;

unop:
  | l=NOT { Not, l }
  | l=MINUS { Opp, l }
;

%inline binop:
  | l=PLUS { Add, l }
  | l=MINUS { Sub, l }
  | l=STAR { Mul, l }
  | l=SLASH { Div, l }
  | l=PERCENT { Rem, l }
  | l=EQ { Eq, l }
  | l=NEQ { Neq, l }
  | l=LT { Lt, l }
  | l=GT { Gt, l }
  | l=LE { Le, l }
  | l=GE { Ge, l }
  | l=AND { And, l }
  | l=OR { Or, l }
;


expression:
| n=INT { Int(n) }
| b=TRUE { Bool(true) }
| b=FALSE { Bool(false) }
| THIS { This }
| m=mem { Get (m) }
| op=unop expr=expression { Unop (op, expr)} %prec UNARY
| e1=expression op=binop e2=expression { Binop (op, e1, e2) }
| LPAR e=expression RPAR { e }
| NEW id=IDENT LPAR args=separated_list(COMMA, expression) RPAR { NewCstr (id, args, ref None) }
| LSQB elts=separated_list(COMMA, expression) RSQB { Arr (Array.of_list elts) }
| NEW t=typ { match t with | TArray (t, n) -> NewArray (t, n) | TClass id -> New (id)
  | _ -> failwith "Cannot only declare new of class or array" } %prec NEWARR
| e=expression DOT id=IDENT LPAR args=separated_list(COMMA, expression) RPAR { MethCall (e, id, args, ref None) }
;