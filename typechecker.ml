open Kawa

exception Error of string

let error s = raise (Error s)

let type_error ty_actual ty_expected =
  error
    (Printf.sprintf
       "expected %s, got %s"
       (typ_to_string ty_expected)
       (typ_to_string ty_actual))
;;

module Env = Map.Make (String)

type tenv = typ Env.t

let add_env l tenv = List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let check_subclass t base class_list =
  let get_class c = List.find (fun x -> x.class_name = c) class_list in
  let rec aux c_def base =
    if c_def.class_name <> base
    then (
      match c_def.parent with
      | None ->
        failwith (Printf.sprintf "%s is not a subclass of %s" c_def.class_name base)
      | Some p -> aux (get_class p) base)
  in
  match t, base with
  | TInt, _ | TBool, _ | TVoid, _ ->
    if t <> base then failwith "Basic types cannot be subclasses"
  | TClass c, TClass c2 -> aux (get_class c) c2
  | _, _ -> failwith "Class cannot inherit from basic types"
;;

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in
  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ
  and type_expr e tenv =
    match e with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Unop (Opp, e) ->
      check e TInt tenv;
      TInt
    | Unop (Not, e) ->
      check e TBool tenv;
      TBool
    | Binop (op, e1, e2) -> check_binop op e1 e2
    | Get m -> type_mem_access m tenv
    | This -> Env.find "this" tenv
    | New cls ->
      if List.exists (fun c -> c.class_name = cls) p.classes
      then TClass cls
      else failwith (Printf.sprintf "New called with class %s which does not exist" cls)
    | NewCstr (cls, args) -> check_constructor cls args tenv
    | MethCall (e, m, args) -> check_meth_call e m args tenv
  and check_binop op e1 e2 =
    match op with
    | Add ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TInt
    | Sub ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TInt
    | Mul ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TInt
    | Div ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TInt
    | Rem ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TInt
    | Lt ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TBool
    | Le ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TBool
    | Gt ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TBool
    | Ge ->
      check e1 TInt tenv;
      check e2 TInt tenv;
      TBool
    | Eq | Neq ->
      let t1, t2 = type_expr e1 tenv, type_expr e2 tenv in
      if t1 = t2 && t1 <> TVoid then TBool else failwith ""
    | And ->
      check e1 TBool tenv;
      check e2 TBool tenv;
      TBool
    | Or ->
      check e1 TBool tenv;
      check e2 TBool tenv;
      TBool
  and check_meth_call e m args tenv =
    let t = type_expr e tenv in
    match t with
    | TClass c ->
      (match List.find_opt (fun def -> def.class_name = c) p.classes with
       | None -> failwith "Method called on undefined class"
       | Some cdef ->
         let mdef = List.find_opt (fun mdef -> mdef.method_name = m) cdef.methods in
         (match mdef with
          | None -> failwith (Printf.sprintf "Method %s not defined for class %s" m c)
          | Some mdef ->
            if List.length mdef.params <> List.length args
            then
              failwith
                (Printf.sprintf
                   "Method %s of class %s was applied with the wrong number of arguments"
                   m
                   c)
            else
              List.iter2
                (fun arg param ->
                  check_subclass (type_expr arg tenv) (snd param) p.classes)
                args
                mdef.params;
            mdef.return))
    | _ -> error "method call on non-object"
  and check_constructor cls args tenv =
    let cdef = List.find (fun def -> def.class_name = cls) p.classes in
    let cstr =
      List.find_opt (fun mdef -> mdef.method_name = "constructor") cdef.methods
    in
    match cstr with
    | None -> failwith (Printf.sprintf "Constructor for class %s not found" cls)
    | Some cstr ->
      if List.length cstr.params <> List.length args
      then
        failwith
          (Printf.sprintf
             "Constructor of class %s was applied with the wrong number of arguments"
             cls)
      else
        List.iter2
          (fun arg param -> check_subclass (type_expr arg tenv) (snd param) p.classes)
          args
          cstr.params;
      if cstr.return = TVoid then TClass cls else failwith "Constructor must return void"
  and type_mem_access m tenv =
    match m with
    | Var x ->
      Env.find_opt x tenv
      |> (function
       | Some t -> t
       | None -> failwith (Printf.sprintf "Variable %s not found" x))
    | Field (e, x) ->
      (match type_expr e tenv with
       | TClass c ->
         let cls = List.find (fun def -> def.class_name = c) p.classes in
         cls.attributes |> List.find (fun attr -> fst attr = x) |> fun attr -> snd attr
       | _ -> error "Cannot access field on non-class type")
  in
  let rec check_instr i ret tenv =
    let check_seq = List.iter (fun i -> check_instr i ret tenv) in
    match i with
    | Print e -> check e TInt tenv
    | Set (m, e) ->
      let t = type_mem_access m tenv in
      check_subclass (type_expr e tenv) t p.classes
    | If (e, seq1, seq2) ->
      check e TBool tenv;
      check_seq seq1;
      check_seq seq2
    | While (e, seq) -> check e TBool tenv; check_seq seq
    | Return e -> check e ret tenv
    | Expr e -> check e TVoid tenv
  and check_seq s ret tenv = List.iter (fun i -> check_instr i ret tenv) s in
  check_seq p.main TVoid tenv
;;
