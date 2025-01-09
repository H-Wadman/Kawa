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

let lev s1 s2 =
  let a, b = String.to_seq s1 |> List.of_seq, String.to_seq s2 |> List.of_seq in
  let rec aux a b =
    match a, b with
    | [], _ -> List.length b
    | _, [] -> List.length a
    | x :: xs, y :: ys ->
      if x = y then aux xs ys else 1 + min (min (aux xs b) (aux a ys)) (aux xs ys)
  in
  aux a b
;;

assert (lev "kitten" "sitting" = 3)

let add_env l tenv = List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

(*let ht = Hashtbl.create 1 in*)
(* class_def * method_name -> int*)
module StrSet = Set.Make (String)

let tag_methods cls_def =
  let meths = cls_def.methods in
  let m_names = List.map (fun m -> m.method_name) meths in
  let s = StrSet.of_list m_names in
  StrSet.iter
    (fun m_name ->
      (*Printf.printf "Tagging %s: %s\n%!" cls_def.class_name m_name;*)
      let lst = List.filter (fun def -> def.method_name = m_name) cls_def.methods in
      List.iteri (fun i elt -> elt.tag <- Some i) lst)
    s
;;

let get_method_signature m args =
  let rec fmt_params args res =
    match args with 
    | [] -> res
    | hd :: tl -> res ^ ", " ^ typ_to_string hd |> fmt_params tl
  in
  let params = match args with 
  | [] -> "()"
  | hd :: tl -> fmt_params tl (typ_to_string hd)
  in
  Printf.sprintf "%s(%s)" m params
;;

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
  | TInt, _ | TBool, _ | TVoid, _ | TArray (_, _), _ ->
    if t <> base then failwith "Basic types cannot be subclasses"
  | TClass c, TClass c2 -> aux (get_class c) c2
  | _, _ -> failwith "Class cannot inherit from non-class types"
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
    | Binop (op, e1, e2) -> check_binop op e1 e2 tenv
    | Get m -> type_mem_access m tenv
    | This -> Env.find "this" tenv
    | New cls ->
      if List.exists (fun c -> c.class_name = cls) p.classes
      then TClass cls
      else failwith (Printf.sprintf "New called with class %s which does not exist" cls)
    | NewCstr (cls, args, tag) -> check_constructor cls args tag tenv
    | MethCall (e, m, args, tag) -> check_meth_call e m args tag tenv
    | NewArray (t, n) -> TArray (t, n)
    | Arr arr ->
      if Array.length arr = 0
      then failwith "Cannot assign empty array"
      else TArray (type_expr arr.(0) tenv, Array.length arr)
  and type_array_access e tenv =
    match type_expr e tenv with
    | TArray (t, _) -> t
    | _ -> failwith "Cannot subscript non-array"
  and check_binop op e1 e2 tenv =
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
  and find_overload cls m args tenv =
    let c_def = List.find (fun c -> c.class_name = cls) p.classes in
    let meths = List.filter (fun meth -> meth.method_name = m) c_def.methods in
    (*All possible method calls*)
    let candidates =
      List.filter
        (fun m_def ->
          if List.length m_def.params <> List.length args
          then false
          else
            List.for_all2
              (fun par a ->
                try
                  check_subclass (snd par) (type_expr a tenv) p.classes;
                  true
                with
                | _ -> false)
              m_def.params
              args)
        meths
    in
    let score m_def =
      List.fold_left2
        (fun acc par a -> if snd par = type_expr a tenv then acc + 1 else acc)
        0
        m_def.params
        args
    in
    let sorted = List.sort (fun a b -> compare (score a) (score b)) candidates in
    match sorted with
    | [ hd ] -> hd
    | hd :: hd' :: _ ->
      if score hd = score hd' then failwith "Ambiguous method call" else hd
    | [] -> (match c_def.parent with 
    | None -> failwith (Printf.sprintf "No matching function definition %s found for %s" (get_method_signature m (List.map (fun a -> type_expr a tenv) args)) cls)
    | Some p -> find_overload p m args tenv)
  and check_meth_call e m args tag tenv =
    (*let rec find_method c m =
      let cdef = List.find (fun def -> def.class_name = c) p.classes in
      assert (Option.is_some tag); (*Testing, remove later*)
      let comp meth = meth.method_name = m && meth.tag = tag in
      if List.exists comp cdef.methods
      then List.find comp cdef.methods
      else (
      try find_method (Option.get cdef.parent) m with
      | Invalid_argument _ ->
      failwith (Printf.sprintf "Method %s not defined for class %s" m c))
      in*)
    assert (Option.is_none !tag);
    (* Remove later *)
    let t = type_expr e tenv in
    match t with
    | TClass c ->
      let mdef = find_overload c m args tenv in
      assert (Option.is_some mdef.tag);
      tag := mdef.tag;
      List.iter2
        (fun arg param -> check_subclass (type_expr arg tenv) (snd param) p.classes)
        args
        mdef.params;
      mdef.return
    | _ -> error "method call on non-object"
  and check_constructor cls args tag tenv =
    assert (Option.is_none !tag);
    (*Remove later*)
    let cstr = find_overload cls "constructor" args tenv in
    tag := cstr.tag;
    List.iter2
      (fun arg param -> check_subclass (type_expr arg tenv) (snd param) p.classes)
      args
      cstr.params;
    if cstr.return = TVoid then TClass cls else failwith "Constructor must return void"
  and type_mem_access m tenv =
    match m with
    | ArrAccess (e, e2) ->
      check e2 TInt tenv;
      type_array_access e tenv
    | Var x ->
      Env.find_opt x tenv
      |> (function
       | Some t -> t
       | None ->
         if Env.is_empty tenv
         then failwith (Printf.sprintf "Variable %s not found" x)
         else (
           let min v _ acc =
             let l = lev v x in
             if l <= fst acc then l, v else acc
           in
           let sugg = Env.fold min tenv (Int.max_int, "") |> snd in
           failwith (Printf.sprintf "Variable %s not found, did you mean %s?" x sugg)))
    | Field (e, a) ->
      let rec find_attr c a =
        let cdef = List.find (fun def -> def.class_name = c) p.classes in
        let comp attr = fst attr = a in
        if List.exists comp cdef.attributes
        then List.find comp cdef.attributes
        else (
          try find_attr (Option.get cdef.parent) a with
          | Invalid_argument _ ->
            failwith (Printf.sprintf "Field %s not defined for class %s" a c))
      in
      (match type_expr e tenv with
       | TClass c ->
         let attr = find_attr c a in
         snd attr
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
      (try
         check e TBool tenv;
         check_seq seq1;
         check_seq seq2
       with
       | _ -> ())
    | While (e, seq) ->
      check e TBool tenv;
      check_seq seq
    | Return e ->
      assert (Env.exists (fun elt _ -> elt = "this") tenv);
      check e ret tenv
    | Expr e -> check e TVoid tenv
  and check_seq s ret tenv = List.iter (fun i -> check_instr i ret tenv) s
  and check_mdef mdef tenv =
    check_seq mdef.code mdef.return (add_env mdef.locals tenv |> add_env mdef.params)
  and check_class cdef tenv =
    List.iter
      (fun m -> check_mdef m (Env.add "this" (TClass cdef.class_name) tenv))
      cdef.methods
  in
  List.iter tag_methods p.classes;
  check_seq p.main TVoid tenv;
  List.iter (fun c -> check_class c tenv) p.classes
;;
