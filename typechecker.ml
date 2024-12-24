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
    | Get (Var x) -> Env.find x tenv
    | Get (Field (e, x)) ->
      (match type_expr e tenv with
       | TClass c ->
         let cls = List.find (fun def -> def.class_name = c) p.classes in
         cls.attributes |> List.find (fun attr -> fst attr = x) |> fun attr -> snd attr
       | _ -> error "Cannot access field on non-class type")
    | This -> Env.find "this" tenv
    | New cls -> TClass cls
    | NewCstr (cls, args) -> failwith "not ready"
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
            else List.iter2 (fun arg param -> check arg (snd param) tenv) args mdef.params;
            mdef.return))
    | _ -> error "method call on non-object"
  and type_mem_access m tenv =
    match m with
    | Var x -> Env.find x tenv
    | Field (e, x) ->
      (match type_expr e tenv with
       | TClass c -> failwith "not ready"
       | _ -> failwith "case not implemented in type_mem_access")
  in
  let rec check_instr i ret tenv =
    match i with
    | Print e -> check e TInt tenv
    | _ -> failwith "case not implemented in check_instr"
  and check_seq s ret tenv = List.iter (fun i -> check_instr i ret tenv) s in
  check_seq p.main TVoid tenv
;;
