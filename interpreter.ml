(** Représentation des arbres de syntaxe abstraite du langage. *)
type expr =
  | Let of string * expr * expr
  | Fun of string * expr
  | Apply of expr * expr
  | Var of string

(** [string_of_expr expr] convertit une expression en forme d’arbre de syntaxe
    abstraite vers sa forme textuelle. *)
let rec string_of_expr (expr : expr) : string =
  match expr with
  | Var name -> name
  | Let (name, value, body) ->
      Printf.sprintf "(let (%s %s) %s)" name (string_of_expr value)
        (string_of_expr body)
  | Fun (name, body) -> Printf.sprintf "(fun %s %s)" name (string_of_expr body)
  | Apply (func, arg) ->
      Printf.sprintf "(%s %s)" (string_of_expr func) (string_of_expr arg)

(*---------*)
(* ANALYSE *)
(*---------*)

(** [string_of_char c] donne la chaîne de caractères contenant [c]. *)
let string_of_char c = String.make 1 c

(** [explode str] convertit une chaîne de caractères en
    une liste de caractères. *)
let explode str =
  let rec explode_inner cur_index chars =
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [ new_char ])
    else chars
  in
  explode_inner 0 []

(** [explode str] convertit une liste de caractères en
    une chaîne de caractères. *)
let rec implode chars =
  match chars with [] -> "" | h :: t -> string_of_char h ^ implode t

(** [parse_expr input] analyse l’entrée [input] et retourne une erreur si
    elle ne correspond pas à une expression du langage, ou bien une paire
    [(expr, rest)] s’il existe un préfixe [prefix] tel que
    [prefix @ rest = input] et [expr] est l’arbre de syntaxe abstraite
    correspondant à [prefix]. *)
let rec parse_expr (input : char list) : (expr * char list) option =
  match input with
  | '(' :: 'l' :: 'e' :: 't' :: ' ' :: _ -> parse_let input
  | '(' :: 'f' :: 'u' :: 'n' :: ' ' :: _ -> parse_fun input
  | '(' :: _ -> parse_apply input
  | _ -> parse_var input

(** [parse_let input] analyse l’entrée [input] pour y trouver un préfixe
    correspondant à une expression de la forme '(let (name value) body)'. Une
    erreur est retournée s’il n’y a pas de telle expression au début de [input]
    ou si elle est mal formée. *)
and parse_let (input : char list) : (expr * char list) option =
    match input with 
     | '(' :: 'l' :: 'e' :: 't' :: ' ' :: rest -> 
             match rest with 
             | '(' :: rest2 -> 
                     match parse_ident rest2 with 
                | Some(name, ' ' ::rest3) -> 
                        match parse_expr rest3 with  
                     |Some(expr1, ')' :: ' ' :: rest4) ->
                             match parse_expr rest4 with  
                        |Some(expr2, rest5) -> Some(Let(name, expr1, expr2), rest5)
                        |_ -> None
                     |_ -> None
                |_ ->None
             | _ -> None
    | _ -> None


(** [parse_fun input] analyse l’entrée [input] pour y trouver un préfixe
    correspondant à une expression de la forme '(fun name body)'. Une erreur
    est retournée s’il n’y a pas de telle expression au début de [input] ou
    si elle est mal formée. *)
and parse_fun (input : char list) : (expr * char list) option =
    match input with 
     |'(' :: 'f' :: 'u' :: 'n' :: ' ' :: rest ->
        match parse_ident rest with 
         | Some(value, ' ' :: rest2) -> 
             match parse_expr rest2 with 
              | Some(expr, ')':: rest3) -> Some(Fun(value, expr), rest3)
              |_ -> None
         |_ -> None
    |_ -> None
        
(** [parse_apply input] analyse l’entrée [input] pour y trouver un préfixe
    correspondant à une expression de la forme '(func arg)'. Une erreur est
    retournée s’il n’y a pas de telle expression au début de [input] ou si elle
    est mal formée. *)
and parse_apply (input : char list) : (expr * char list) option =
   match input with
   |'(' :: rest ->
     match parse_expr rest with
         |Some(expr1, ' ' :: rest2) ->
             match parse_expr rest2 with
                 |Some (expr2, ')' :: rest3)-> Some(Apply(expr1, expr2), rest3)
                 |_ -> None
         |_ -> None
    |_ -> None 
 
(** [parse_var input] analyse l’entrée [input] pour y trouver un préfixe
    correspondant à une variable libre. Une erreur est retournée s’il n’y a pas
    de telle valeur au début de [input]. *)
and parse_var (input : char list) : (expr * char list) option =
    match parse_ident input with
     |Some(str, rest) -> Some(Var(str), rest)
     |None -> None

(** [parse_ident input] analyse l’entrée [input] pour y trouver un préfixe
    correspondant à un nom non-vide. Une erreur est retournée s’il n’y a pas de
    telle valeur au début de [input]. *)
and parse_ident (input : char list) : (string * char list) option =
  let is_letter x =
    ('a' <= x && x <= 'z')
    || ('A' <= x && x <= 'Z')
    || ('0' <= x && x <= '9')
    || x = '-'
  in match input with
  | letter :: rest when is_letter letter ->
      (match parse_ident rest with
       | Some(s, rest2) -> Some (string_of_char letter ^ s, rest2)  
       | None -> Some (string_of_char letter, rest))
  | _ -> None

(*------------*)
(* ÉVALUATION *)
(*------------*)

(** [remove values x] donne la liste [values] dans laquelle toutes les
    occurrences de [x] ont été supprimées. *)
let rec remove (values : 'a list) (x : 'a) : 'a list =
  match values with
  | [] -> []
  | head :: rest when head = x -> remove rest x
  | head :: rest -> head :: remove rest x

(** [free_vars expr] donne la liste des noms des variables libres dans
    l’expression [expr]. *)
let rec free_vars (expr : expr) : string list =
    match expr with
    |Var(str) -> [str]
    |Let(name,expr1,expr2) -> (free_vars expr1) @ (remove (free_vars expr2) name) 
    |Fun(str, expr1) -> remove (free_vars expr1) (str)
    |Apply(expr1, expr2) -> 
            (match expr1 with
            |Fun(str,expr3) -> free_vars (Let(str, expr2, expr3))
            |_ -> (free_vars expr1) @ (free_vars expr2)
            )
(** [fresh name values] donne une variation du nom [name] qui n’apparaît pas
    dans la liste [values]. *)
let rec fresh (name : string) (values : string list) : string =
    let rec in_List (str: string) (lst : string list) : bool = 
        match lst with 
         |[] -> false
         |head :: rest -> if head = str then true else in_List str rest
    in
    if in_List name values then
        fresh (name^"0") values
    else
        name

(** [substitute expr x y] applique les règles de substitution dans l’expression
    [expr] pour remplacer chaque occurrence de la variable nommée [x] par
    l’expression [y]. *)
let rec substitute (expr : expr) (x : string) (y : expr) : expr =
    match expr with
    |Var(value) -> if value = x then y  else Var(value)
    |Apply(expr1, expr2) -> Apply((substitute expr1 x y), (substitute expr2 x y ))
    |Fun(value, expr1) -> 
            if value = x then Fun(value, expr1)
            else
                let new_value = fresh value (value :: (free_vars expr1) @ (free_vars y)) in
                Fun(new_value, 
                    (substitute(substitute expr1 value (Var new_value)) x y ))
    |Let(value, expr1, expr2) -> 
            if value = x then Let(value,(substitute expr1 x y), expr2)
            else 
                let new_value = fresh value (value :: (free_vars expr1) @ (free_vars y)) in
                Let(new_value, (substitute expr1 x y),
                    (substitute(substitute expr2 value (Var new_value)) x y))

(** [eval expr] évalue l’expression [expr] en la réduisant le plus possible. *)
let rec eval (expr : expr) : expr =
    match expr with
    |Var(value) -> Var(value)
    |Fun(value,expr1) -> 
            Fun(value, eval expr1) 
    |Apply(expr1,expr2) ->
           (match expr1  with
            |Fun(value, expr3) -> eval (substitute expr3 value expr2)
            |_ -> Apply((eval expr1), (eval expr2))
           )
    |Let(value, expr1, expr2) -> 
            eval (Apply(Fun(value, expr2), expr1)) 

