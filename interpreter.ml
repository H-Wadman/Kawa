open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  
  let rec eval_call f this args =
    let lenv = Hashtbl.create 1 in
    List.iter (fun local -> Hashtbl.add lenv (fst local) Null) f.locals;
    List.iter2 (fun param v -> Hashtbl.add lenv (fst param) v) f.params args;
    try exec_seq f.code lenv; Null
    with Return v -> v
  and exec_seq s lenv =
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false
        
    and eval_args args = List.map (fun e -> eval e) args 
    and eval (e: expr): value = match e with
      | Int n  -> VInt n
      | Bool b -> VBool b
      | Unop (Opp, expr) -> VInt (- (evali expr))
      | Unop (Not, expr) -> VBool (not (evalb expr))
      | Binop (bin, e1, e2) -> eval_binop bin e1 e2
      | Get (Var s) -> 
        (match Hashtbl.find_opt lenv s with
        | Some v -> v 
        | None -> Hashtbl.find env s)
      | Get (Field (o, attr)) -> Hashtbl.find (evalo o).fields attr
      | This -> failwith "No idea"
      | New cls -> 
        let fields = Hashtbl.create 1 in
        let c_def = List.find (fun c -> c.class_name = cls) p.classes in
        List.iter (fun a -> Hashtbl.add fields (fst a) Null) c_def.attributes;
        VObj {cls; fields}
      | NewCstr (cls, args) ->
          let c_def = List.find (fun c -> c.class_name = cls) p.classes in
          let obj = evalo (New cls) in
          let cstr = List.find (fun m -> m.method_name = "constructor") c_def.methods in
          eval_call cstr obj (eval_args args)
      | MethCall (e, name, args) ->
        let o = evalo e in
        let c_def = List.find (fun c -> c.class_name = o.cls) p.classes in
        let meth = List.find (fun m -> m.method_name = name) c_def.methods in
        eval_call meth o (eval_args args)
    and eval_binop bin e1 e2 =
      match bin with 
      | Add -> VInt (evali e1 + evali e2)
      | Sub -> VInt (evali e1 - evali e2)
      | Mul -> VInt (evali e1 * evali e2)
      | Div -> VInt (evali e1 / evali e2)
      | Rem -> VInt (evali e1 mod evali e2)
      | Lt -> VBool (evali e1 < evali e2)
      | Le -> VBool (evali e1 <= evali e2)
      | Gt -> VBool (evali e1 > evali e2)
      | Ge -> VBool (evali e1 >= evali e2)
      | Eq -> let v1 = eval e1 in (match v1 with | VObj o -> VBool (v1 == eval e2) | _ -> VBool (v1 = eval e2)) 
      | Neq -> VBool (eval e1 <> eval e2)
      | And -> VBool (evalb e1 && evalb e2)
      | Or -> VBool (evalb e1 || evalb e2)
    in
    let rec exec (i: instr): unit = match i with
      | Print e -> Printf.printf "%d\n" (evali e)
      | Set (Var s, e) -> Hashtbl.replace env s (eval e);
      | Set (Field (o, s), e) -> Hashtbl.replace (evalo o).fields s (eval e)
      | If (cond, br1, br2) -> if evalb cond then exec_seq br1 else exec_seq br2
      | While (cond, br) -> if evalb cond then exec_seq br; exec i
      | Return e -> raise (Return (eval e))
      | Expr e -> ignore (eval e)
    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in
  
  exec_seq p.main (Hashtbl.create 1)
