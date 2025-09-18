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
  (* À COMPLÉTER! *)
  None

(** [parse_fun input] analyse l’entrée [input] pour y trouver un préfixe
    correspondant à une expression de la forme '(fun name body)'. Une erreur
    est retournée s’il n’y a pas de telle expression au début de [input] ou
    si elle est mal formée. *)
and parse_fun (input : char list) : (expr * char list) option =
  (* À COMPLÉTER! *)
  None

(** [parse_apply input] analyse l’entrée [input] pour y trouver un préfixe
    correspondant à une expression de la forme '(func arg)'. Une erreur est
    retournée s’il n’y a pas de telle expression au début de [input] ou si elle
    est mal formée. *)
and parse_apply (input : char list) : (expr * char list) option =
  (* À COMPLÉTER! *)
  None

(** [parse_var input] analyse l’entrée [input] pour y trouver un préfixe
    correspondant à une variable libre. Une erreur est retournée s’il n’y a pas
    de telle valeur au début de [input]. *)
and parse_var (input : char list) : (expr * char list) option =
  (* À COMPLÉTER! *)
  None

(** [parse_ident input] analyse l’entrée [input] pour y trouver un préfixe
    correspondant à un nom non-vide. Une erreur est retournée s’il n’y a pas de
    telle valeur au début de [input]. *)
and parse_ident (input : char list) : (string * char list) option =
  let is_letter x =
    ('a' <= x && x <= 'z')
    || ('A' <= x && x <= 'Z')
    || ('0' <= x && x <= '9')
    || x == '-'
  in
  (* À COMPLÉTER! *)
  None

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
  (* À COMPLÉTER! *)
  []

(** [fresh name values] donne une variation du nom [name] qui n’apparaît pas
    dans la liste [values]. *)
let fresh (name : string) (values : string list) : string =
  (* À COMPLÉTER! *)
  name

(** [substitute expr x y] applique les règles de substitution dans l’expression
    [expr] pour remplacer chaque occurrence de la variable nommée [x] par
    l’expression [y]. *)
let rec substitute (expr : expr) (x : string) (y : expr) : expr =
  (* À COMPLÉTER! *)
  expr

(** [eval expr] évalue l’expression [expr] en la réduisant le plus possible. *)
let rec eval (expr : expr) : expr =
  (* À COMPLÉTER! *)
  expr
