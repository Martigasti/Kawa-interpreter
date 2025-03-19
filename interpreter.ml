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

(* Fonction pour recuperer tous les attributs d'une classe et ses parents *)
let rec collect_attributes classes class_name = 
  match List.find_opt (fun c -> c.class_name = class_name) classes with 
  | None -> 
    raise (Error ("Classe introuvable : " ^ class_name ))
  |Some cdef -> 
    let attrs = cdef.attributes in 
    match cdef.parent with 
    | None -> attrs
    | Some parent_name -> 
      let parent_attrs = collect_attributes classes parent_name in 
      (* On concatène les attributs du parent et les actuels *)
      parent_attrs @ attrs  

(* Fonction pour rechercher une méthode en remontant dans ses parents *)
let rec find_method_def classes class_name meth_name = 
  match List.find_opt (fun c -> c.class_name = class_name) classes with 
  | None -> raise (Error ("Classe " ^ class_name ^ " non definie."))
  | Some cdef ->
    match List.find_opt (fun m -> m.method_name = meth_name) cdef.methods with 
    | Some mdef -> mdef 
    | None -> (* On remonte au parent *)
      match cdef.parent with 
      | Some p -> find_method_def classes p meth_name
      | None -> raise (Error (Printf.sprintf "Methode %s non trouve dan la classe %s" meth_name class_name))

  

let exec_prog (p: program): unit =

  let env = Hashtbl.create 16 in

  (* On initialise les variables globales *)
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;

  let rec exec_seq s lenv =
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false
        
    and eval (e: expr): value = match e with
      | Int n  -> VInt n 
      | Bool b -> VBool b
      | Binop (b, e1, e2) -> eval_binop b e1 e2  
      | Unop (u, e) -> eval_unop u e
      | Get mem -> eval_mem mem 
      | New class_name -> begin 
        match List.find_opt (fun c -> c.class_name = class_name) p.classes with
        | Some class_def -> 
            (* On récupère les attributs de la classe et ses parents (si elle a) *)
            let all_attrs = collect_attributes p.classes class_name in 

            (* On crée l'objet *)
            let fields = Hashtbl.create 16 in
            List.iter (fun (name, _) -> Hashtbl.add fields name Null) all_attrs;
            VObj { cls = class_name; fields }
        | None -> raise (Error (Printf.sprintf "classe %s non trouvee" class_name))
        end

      | MethCall (obj, id, args) -> begin
        let VObj instance = eval obj in

        let find_class class_name =
          match List.find_opt (fun c -> c.class_name = class_name) p.classes with
          | Some class_def -> class_def
          | None -> raise (Error (Printf.sprintf "classe %s non trouvee" class_name))
        in
        
        (* On recupére la définition de la méthode *)
        let method_def =
          List.find (fun m -> m.method_name = id) (find_class instance.cls).methods
        in

        (* On évalue les arguments *)
        let arg_values = List.map eval args in

        (* On crée un environnement local pour executer la méthode *)
        let local_env = Hashtbl.create 16 in
        List.iter2 (fun (param_name, _) arg_value ->
          Hashtbl.add local_env param_name arg_value
        ) method_def.params arg_values;

        (* On ajoute l'objet courant "this" *)
        Hashtbl.add local_env "this" (VObj instance);

        (* On éxecute le corps de la méthode *)
        try
          exec_seq method_def.code local_env;
          Null (* Dans le cas d'un void *)
        with
        | Return value -> value 
      end

      | This -> begin
        try Hashtbl.find lenv "this" 
        with 
        | Not_found -> raise (Error ("'this' pas défini dans l'environnement local")) 
        end

      | NewCstr (class_name, args) ->
        (* On récupère les attributs *)
        let all_attrs = collect_attributes p.classes class_name in 

        (* On cherche la définition de la classe *)
        let cdef = 
          match List.find_opt (fun c -> c.class_name = class_name) p.classes with 
          | None -> raise (Error ("Classe introuvable : " ^ class_name)) (* cas devrait être impossible *)
          | Some cd -> cd 
        in 
        (* On crée une Hashtbl pour stocker ses attributs *)
        let fields = Hashtbl.create 16 in 
        List.iter (fun (attr_name, _) -> Hashtbl.add fields attr_name Null) all_attrs;
        let instance = { cls = class_name; fields } in
        let vobj = VObj instance in 

        (* On appele la méthode constructeur sur cet objet crée *)
        begin
          match List.find_opt (fun m -> m.method_name = "constructor") cdef.methods with
          | Some constructor_def ->
              let arg_values = List.map eval args in
              let local_env = Hashtbl.create 16 in
              List.iter2 (fun (pname, _) arg_val ->
                Hashtbl.add local_env pname arg_val
              ) constructor_def.params arg_values;
              (* On ajoute l'objet crée sous la clé 'this' *)
              Hashtbl.add local_env "this" vobj;
                
              (* On execute le constructeur *)
              exec_seq constructor_def.code local_env;
              vobj
          | None ->
              (* Cas qui devrait etre impossible (par le typechecker) *)
              vobj
        end

    and eval_binop b e1 e2 =

      (* Extension : fonction auxiliaire pour égalité structurelle *)
      let rec struct_eq v1 v2 = 
        match v1, v2 with 
        | VInt n1, VInt n2 -> n1 = n2
        | VBool b1, VBool b2 -> b1 = b2
        | VObj o1, VObj o2 -> 
          if o1.cls <> o2.cls then false 
          else begin 
            (* On itére sur ses attributs *)
            let same = ref true in 
            Hashtbl.iter (fun field_nom val1 -> 
              if !same then begin 
                let val2 = Hashtbl.find o2.fields field_nom in 
                if not (struct_eq val1 val2) then same := false 
              end
            ) o1.fields;
            !same
          end
        | _, _ -> false 

      in

      match b with 
      | Add -> VInt (evali e1 + evali e2)
      | Sub -> VInt (evali e1 - evali e2)
      | Mul -> VInt (evali e1 * evali e2)
      | Div -> 
        let n2 = evali e2 in 
        if n2 = 0 then raise (Error "Division par zero")
        else VInt (evali e1 / n2)
      | Rem -> 
        let n2 = evali e2 in 
        if n2 = 0 then raise (Error "Modulo par zero")
        else VInt (evali e1 mod n2)
      | Lt -> VBool (evali e1 < evali e2)
      | Le -> VBool (evali e1 <= evali e2)
      | Gt -> VBool (evali e1 > evali e2)
      | Ge -> VBool (evali e1 >= evali e2)
      | Eq ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        (match (v1, v2) with
         | VInt n1, VInt n2 -> VBool (n1 == n2)
         | VBool b1, VBool b2 -> VBool (b1 == b2)
         | _ -> raise (Error "types t1 et t2 differents"))
    
     | Neq ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        (match (v1, v2) with
         | VInt n1, VInt n2 -> VBool (n1 != n2)
         | VBool b1, VBool b2 -> VBool (b1 != b2)
         | _ -> raise (Error "types t1 et t2 differents"))
      | And -> VBool (evalb e1 && evalb e2)
      | Or -> VBool (evalb e1 || evalb e2)

      | StrEq -> 
        let v1 = eval e1 in 
        let v2 = eval e2 in 
        VBool (struct_eq v1 v2) 
      | StrNeq -> 
        let v1 = eval e1 in 
        let v2 = eval e2 in 
        VBool (not (struct_eq v1 v2)) 
    
    
    and eval_unop u e = 
      match u with 
      | Not -> VBool (not (evalb e))
      | Opp -> VInt (0 - evali e)

    and eval_mem mem =
      match mem with
      | Var id -> 
        (* On cherche dans 'lenv' *)
        (try Hashtbl.find lenv id
        with Not_found ->
        (* On cherche dans 'env' *)
          (try Hashtbl.find env id
           with Not_found -> raise (Error ("Variable non définie : " ^ id))))
      | Field (objet, nom_attr) -> begin
          match eval objet with
          | VObj obj -> 
              (try Hashtbl.find obj.fields nom_attr
               with Not_found -> raise (Error ("Attribut " ^ nom_attr ^ " non trouve dans l'objet de classe " ^ obj.cls)))
          | _ -> raise (Error ("Accès à un attribut sur une valeur qui n'est pas un objet"))
        end
          

        
        
    in
  
    let rec exec (i: instr): unit = match i with
      | Print e -> begin
        (match eval e with
        | VInt n -> Printf.printf "%d\n" n
        | VBool b -> Printf.printf "%b\n" b 
        | _ -> failwith "case not implemented in print")
        end

      | Set (Var id, e) -> begin
        let v = eval e in 
        Hashtbl.replace env id v 
        end
      | Set (Field(obj, id), e) -> begin 
        match eval obj with 
        | VObj instance ->  
          let value = eval e in 
          Hashtbl.replace instance.fields id value 
        end

      | If (cond, i1, i2) -> begin
        match (eval cond) with 
        | VBool true -> exec_seq i1
        | VBool false -> exec_seq i2
        end

      | While (cond, instr) -> begin
        (* On crée un loop pour executer les instructions autant de fois que nécessaire *)
        let rec loop() = 
          match (eval cond) with 
          | VBool false -> () 
          | VBool true -> 
            exec_seq instr;
            loop();
        in
        loop()
        end
      
      | Return e -> begin
        let v = eval e in 
        raise (Return v)
        end
      
      | Expr e -> ignore (eval e)

    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in
  
  exec_seq p.main (Hashtbl.create 1)
