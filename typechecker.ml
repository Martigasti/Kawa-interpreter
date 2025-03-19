open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

(*  Pour les classes *)
module ClassEnv = Map.Make(String)
type cenv = { classes: class_def ClassEnv.t; }

(* Pour créer une liste avec les variables globales *)
let add_env l tenv =
  List.fold_left (fun env (x, t) ->
      Env.add x t env
  ) tenv l

let add_class_env cenv classes = 
  List.fold_left (fun env c -> ClassEnv.add c.class_name c env) cenv classes

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in
  let cenv = {classes = add_class_env ClassEnv.empty p.classes } in 

  (* Fonction pour tester l'héritage *)
  let rec is_subclass c1 c2 =
    if c1 = c2 then true 
    else
      match ClassEnv.find_opt c1 cenv.classes with 
      | None -> false
      | Some cdef ->
        (match cdef.parent with
        | None -> false
        | Some parent_name -> 
          is_subclass parent_name c2)
  in
  
  (* Fonction pour tester si un type est un subtype de l'autre *)
  let rec subtype t1 t2 = 
    match t1, t2 with 
    | TVoid, TVoid -> true 
    | TInt, TInt -> true
    | TBool, TBool -> true
    | TClass c1, TClass c2 -> 
      is_subclass c1 c2 
    | _ -> false
  in

  (* Fonction pour vérifier que attr existe dans class_name (avec heritage inclus) *)
  let rec find_attribute_type class_name attr = 
    match ClassEnv.find_opt class_name cenv.classes with 
    | None -> error ("Classe introuvable : " ^ class_name)
    | Some cdef ->
      match List.assoc_opt attr cdef.attributes with 
      | Some t -> t 
      | None -> 
        (match cdef.parent with 
        | None ->
           error (Printf.sprintf "Attribut %s non trouve dans la classe %s ni ses parents" attr class_name)
        | Some p ->
          find_attribute_type p attr)
  in

  let rec find_method_def class_name meth_name = 
    match ClassEnv.find_opt class_name cenv.classes with 
    | None ->  
      error (Printf.sprintf "Classe %s non definie, methode %s impossible de trouver." class_name meth_name)
    | Some cdef -> 
      match List.find_opt (fun m -> m.method_name = meth_name) cdef.methods with 
      | Some mdef -> mdef 
      | None ->
        (* Si on ne trouve pas, on rémonte si la classe a un parent *)
        match cdef.parent with 
        | None -> 
          error (Printf.sprintf "Methode %s non trouvee dans la classe %s ni chez ses parents." meth_name class_name)
        | Some parent_name -> 
          find_method_def parent_name meth_name
  in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if not (subtype typ_e typ) then type_error typ_e typ

  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Get mem -> type_mem_access mem tenv
    | Binop(b, e1, e2) -> begin
      let t1 = type_expr e1 tenv in 
      let t2 = type_expr e2 tenv in 
      match b with 
      (* Verifie que le binop soit correct *)
      | Add | Sub | Mul | Div | Rem -> 
        (* Verifie que les types soient des entiers *)
        if t1 = TInt && t2 = TInt then TInt 
        (* On cherche de quel type provient l'erreur *)
        else if t1 <> TInt then type_error t1 TInt
        else type_error t2 TInt
      | Lt | Le | Gt | Ge ->
        if t1 = TInt && t2 = TInt then TBool
        else if t1 <> TInt then type_error t1 TInt
        else type_error t2 TInt
      | And | Or -> 
        if t1 = TBool && t2 = TBool then TBool
        else if t1 <> TBool then type_error t1 TBool 
        else type_error t2 TBool 
      | Eq | Neq ->
        if t1 = t2 then TBool
        else error ("types t1 et t2 differents")
      | StrEq | StrNeq -> 
        if t1 = TInt && t2 = TInt then TBool
        else if t1 = TBool && t2 = TBool then TBool 
        else
          match (t1, t2) with 
          | (TClass c1, TClass c2) -> 
            if c1 = c2 then TBool
            else error (Printf.sprintf "Impossible de comparer deux classes differentes : %s et %s" c1 c2)
          | _ -> error "Egalite structurelle impossible sur ces types"
      end


    | Unop(u, e) -> begin
      let t = type_expr e tenv in 
      match u with 
      | Not -> if t = TBool then TBool else type_error t TBool
      | Opp -> if t = TInt then TInt else type_error t TInt
      end

    | New class_name -> begin
      match ClassEnv.find_opt class_name cenv.classes with
      | Some _ -> TClass class_name
      | None -> error ("Classe non definie: " ^ class_name)
      end

    | NewCstr (class_name, args) -> begin (* Initialise la classe avec plusieurs attributs *)
      match ClassEnv.find_opt class_name cenv.classes with 
      | Some class_def -> 
        (* On vérifie la présence d'un constructeur *)
        let constructeur = 
          try List.find (fun m -> m.method_name = "constructor") class_def.methods
          with Not_found -> error ("Constructeur non trouve pour la classe " ^ class_name)
        in   
        let param_types = List.map snd constructeur.params in 
        (* On verifie que le nombre de parametres passes est equivalent au nombre attendu *)
        if List.length param_types <> List.length args then 
          error ("Mauvais nombre d'arguments pour le constructeur de " ^ class_name );
        (* On verifie que les types des deux listes soient les memes *)
        List.iter2 (fun param_type arg -> check arg param_type tenv) param_types args;
        (* Si tout est bien, on renvoie la class *)
        TClass class_name
      | None -> error ("Classe non definie : " ^ class_name)
      end

    | MethCall (obj, id, args) -> begin 
      (* On vérifie que le type de obj soit une TClass *)
      let obj_type = type_expr obj tenv in 
      match obj_type with 
      | TClass class_name -> 
        (* On cherche la définition de la méthode *)
        let method_def = find_method_def class_name id in 

        (* On vérifie le nombre d'arguments *)
        let param_types = List.map snd method_def.params in 
        let arg_types = List.map (fun e -> type_expr e tenv) args in 
        if List.length param_types <> List.length arg_types then 
          error (Printf.sprintf "Mauvais nombre d'arguments pour la methode %s (classe %s)" id class_name);
        (* On vérifie le sous_typage entre chaque paramètre et argument *)
        List.iter2 (fun param_type arg_type -> 
          if not (subtype arg_type param_type) then 
            type_error arg_type param_type 
        ) param_types arg_types;

        (* On donne le type qui renvoie la methode*)
        method_def.return

      | _ -> error (Printf.sprintf "Impossible d'appeler la methode %s sur un objet pas de type class." id)
      end

    | This -> 
      (* On vérifie l'existenc d'un "this" dans l'env *)
      if Env.mem "this" tenv then 
        Env.find "this" tenv
      else error ("Utilisation de 'this' hors d'une methode de classe")


  and type_mem_access m tenv = match m with
  | Var id -> begin
      if Env.mem id tenv then
        Env.find id tenv
      else
        error ("Variable non trouvee: " ^ id)
  
    end
  | Field (obj, nom_attr) -> begin (* Objet d'une classe et son attribut *)

    let obj_type = type_expr obj tenv in 
    match obj_type with 
    | TClass class_name -> 
      find_attribute_type class_name nom_attr
      
    | _ -> error ("Acces a l'attribut d'un objet qui n'est pas une classe")
    end

  in

  let rec check_instr i ret tenv = match i with
    | Print e -> 
      let typ_e = type_expr e tenv in
      if typ_e = TInt || typ_e = TBool then
        check e typ_e tenv
      else
        error ("Print attend entier ou boleen, mais a " ^ typ_to_string typ_e)
    | Set (Var id, e) -> 
      if not (Env.mem id tenv) then
        error ("Variable non declaree: " ^ id);
      (* On vérifie que le type soit le meme que celui déclaré dans tenv *)
      let t_var = Env.find id tenv in
      check e t_var tenv
    | Set (Field (objet, id), e) -> 
        (* On vérifie que l'objet est une instance d'une classe *)
      let obj_type = type_expr objet tenv in
      begin match obj_type with
      | TClass class_name -> 
        (* On cherche les attributs de la classe *)
        let attr_type = find_attribute_type class_name id in

        (* On check le type de e pour vérifier la cohérence *)
        let expr_type = type_expr e tenv in
        if not (subtype expr_type attr_type) then
          type_error expr_type attr_type
      | _ -> error ("Tentative de modification d'un attribut sur un objet non-classe")
      end

    | If (cond, i1, i2) ->
      (* On verifie que la condition soit un bool *)
      check cond TBool tenv;
      (* On verifie recursivement ses branches *)
      check_seq i1 ret tenv;
      check_seq i2 ret tenv
    | While (cond, instr) ->
      check cond TBool tenv;
      check_seq instr ret tenv
    | Return (e) ->
      (* On vérifie si l'expression renvoie le type attendu *)
      check e ret tenv
    | Expr e -> 
      ignore (type_expr e tenv);


  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in

  
  (* On vérifie si les parents des classes existent *) 
  ClassEnv.iter
    (fun (class_name : string) (cdef : class_def) ->
      match cdef.parent with
      | None -> ()
      | Some parent_name ->
        if not (ClassEnv.mem parent_name cenv.classes) then
          error (Printf.sprintf "La classe %s herite d'une classe %s non definie." class_name parent_name)
    ) cenv.classes;


  (* Vérifications extras pour les méthodes *)
  let check_method global_env class_name mdef =
    (* Vérifie que tous les chemins de la méthode renvoient quelque chose *)
    let rec check_return_instr i ret tenv =
      match i with
      | Return _ ->
          true  
      | If(_, s1, s2) ->
          (* Vrai si les deux branches renvoient quelque chose *)
             check_return_seq s1 ret tenv && check_return_seq s2 ret tenv
      | While(_, s) ->
          false
      | _ ->
          false
  (* Cette fonction auxiliaire sert à regarder la liste d'instructions *)
    and check_return_seq s ret tenv =
      match s with
      | [] -> false
      | i :: tl ->
          if check_return_instr i ret tenv then
            (* Si l'instruction actuelle renvoie quelque chose, alors true *)
            true
          else
            (* Sinon, on regarde la suite *)
            check_return_seq tl ret tenv
    in

    (* On construit l'environnement local de la méthode (l'environnement global est inclus) *)
    let local_env = global_env in
    let local_env = Env.add "this" (TClass class_name) local_env in
    let local_env = add_env mdef.params local_env in
    let local_env = add_env mdef.locals local_env in
    (* On verifie si le type renvoyé est correcte, mais ne verifie pas si on oublie un return *)
    check_seq mdef.code mdef.return local_env;

    (* Si la methode renvoie quelque chose, mais un de ses chemins ne renvoie rien alors erreur *)
    if mdef.return <> TVoid && not (check_return_seq mdef.code mdef.return local_env) then
      error (Printf.sprintf
               "La methode %s, appartenant a la classe %s, est declaree %s mais n'a pas de return sur tous ses chemins."
               mdef.method_name class_name (typ_to_string mdef.return))
  in

  (* On vérifie chaque méthode de chaque classe. *)
  ClassEnv.iter
    (fun class_name cdef ->
       List.iter (fun mdef -> check_method tenv class_name mdef)
                 cdef.methods
    )
    cenv.classes;

  check_seq p.main TVoid tenv
