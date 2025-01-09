open Kawa

type value =
  | VInt of int
  | VBool of bool
  | VObj of obj
  | VArray of value array
  | Null

and obj =
  { cls : string
  ; fields : (string, value) Hashtbl.t
  }

exception Error of string
exception Return of value

let exec_prog (p : program) : unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  let rec eval_call f this args =
    let lenv = Hashtbl.create 1 in
    List.iter2 (fun param v -> Hashtbl.add lenv (fst param) v) f.params args;
    List.iter (fun local -> Hashtbl.add lenv (fst local) Null) f.locals;
    Hashtbl.add lenv "this" (VObj this);
    try
      exec_seq f.code lenv;
      Null
    with
    | Return v -> v
  and exec_seq s lenv =
    let rec evali e =
      match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e =
      match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e =
      match eval e with
      | VObj o -> o
      | _ -> assert false
    and evala e =
      match eval e with
      | VArray a -> a
      | _ -> assert false
    and eval_args args = List.map (fun e -> eval e) args
    and eval (e : expr) : value =
      match e with
      | Int n -> VInt n
      | Bool b -> VBool b
      | Unop (Opp, expr) -> VInt (-evali expr)
      | Unop (Not, expr) -> VBool (not (evalb expr))
      | Binop (bin, e1, e2) -> eval_binop bin e1 e2
      | NewArray (_, n) -> VArray (Array.make n Null)
      | Arr a -> VArray (Array.map eval a)
      | Get (Var s) ->
        (match Hashtbl.find_opt lenv s with
         | Some v -> v
         | None -> Hashtbl.find env s)
      | Get (Field (o, attr)) -> Hashtbl.find (evalo o).fields attr
      | Get (ArrAccess (e, ix)) ->
        let a = evala e in
        a.(evali ix)
      | This -> Hashtbl.find lenv "this"
      | New cls ->
        let fields = Hashtbl.create 1 in
        let c_def = List.find (fun c -> c.class_name = cls) p.classes in
        List.iter (fun a -> Hashtbl.add fields (fst a) Null) c_def.attributes;
        VObj { cls; fields }
      | NewCstr (cls, args, tag) ->
        let c_def = List.find (fun c -> c.class_name = cls) p.classes in
        let obj = evalo (New cls) in
        assert (Option.is_some !tag);
        let cstr =
          List.find
            (fun m ->
              assert (Option.is_some m.tag);
              m.method_name = "constructor" && m.tag = !tag)
            c_def.methods
        in
        eval_call cstr obj (eval_args args) |> ignore;
        VObj obj
      | MethCall (e, name, args, tag) ->
        assert (Option.is_some !tag);
        let o = evalo e in
        let rec find_method c m =
          let c_def = List.find (fun c_def -> c_def.class_name = c) p.classes in
          let comp meth =
            assert (Option.is_some meth.tag);
            meth.method_name = m && meth.tag = !tag
          in
          if List.exists comp c_def.methods
          then List.find comp c_def.methods
          else (
            try find_method (Option.get c_def.parent) m with
            | Invalid_argument _ ->
              failwith (Printf.sprintf "Method %s not defined for class %s" m c))
        in
        let meth = find_method o.cls name in
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
      | Eq ->
        let v1 = eval e1 in
        (match v1 with
         | VObj o -> VBool (v1 == eval e2)
         | _ -> VBool (v1 = eval e2))
      | Neq -> VBool (eval e1 <> eval e2)
      | And -> VBool (evalb e1 && evalb e2)
      | Or -> VBool (evalb e1 || evalb e2)
    in
    let rec exec (i : instr) : unit =
      match i with
      | Print e -> Printf.printf "Debug: %d\n%!" (evali e)
      | Set (Var s, e) ->
        if Hashtbl.mem lenv s
        then Hashtbl.replace lenv s (eval e)
        else Hashtbl.replace env s (eval e)
      | Set (Field (o, s), e) -> Hashtbl.replace (evalo o).fields s (eval e)
      | Set (ArrAccess (a, ix), e) -> (evala a).(evali ix) <- eval e
      | If (cond, br1, br2) -> if evalb cond then exec_seq br1 else exec_seq br2
      | While (cond, br) ->
        if evalb cond
        then (
          exec_seq br;
          exec i)
      | Return e -> raise (Return (eval e))
      | Expr e -> ignore (eval e)
    and exec_seq s = List.iter exec s in
    exec_seq s
  in
  exec_seq p.main (Hashtbl.create 1)
;;
