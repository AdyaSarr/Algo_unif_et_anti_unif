(* Types *)
type terme =
  | Constant of string
  | Variable of string
  | Fonction of string * terme list;;

type substitution = (string * terme) list;;

type unif_result =
  | Substitution of substitution
  | UnificationFailure;;

type anti_unif_result =
  | AntiUnifTree of anti_unif_tree

and anti_unif_tree =
  | AntiUnifTerm of terme
  | AntiUnifList of anti_unif_tree list

(* Fonctions *)
let rec occurs_check variable terme =
  match terme with
  | Constant _ -> false
  | Variable v -> v = variable
  | Fonction (_, args) ->
      List.exists (occurs_check variable) args

let rec apply_subst terme substitution =
  match terme with
  | Constant _ -> terme
  | Variable v ->
      (match List.assoc_opt v substitution with
      | Some t -> t
      | None -> terme)
  | Fonction (f, args) ->
      Fonction (f, List.map (fun t -> apply_subst t substitution) args);;

let rec unify_list terms1 terms2 substitution =
  match terms1, terms2 with
  | [], [] -> Substitution substitution
  | t1 :: rest1, t2 :: rest2 ->
      let t1' = apply_subst t1 substitution in
      let t2' = apply_subst t2 substitution in
      (match unify t1' t2' substitution with
      | Substitution sub ->
          let rest1' = List.map (fun t -> apply_subst t sub) rest1 in
          let rest2' = List.map (fun t -> apply_subst t sub) rest2 in
          unify_list rest1' rest2' sub
      | UnificationFailure -> UnificationFailure
      )
  | _, _ -> UnificationFailure


and unify term1 term2 substitution =
  match term1, term2 with
  | Constant c1, Constant c2 ->
      if c1 = c2 then Substitution substitution else UnificationFailure
  | Variable v, _ ->
      if occurs_check v term2 then UnificationFailure
      else Substitution ((v, term2) :: substitution)
  | _, Variable v ->
      if occurs_check v term1 then UnificationFailure
      else Substitution ((v, term1) :: substitution)
  | Fonction (f1, args1), Fonction (f2, args2) ->
      if f1 = f2 && List.length args1 = List.length args2 then
        unify_list args1 args2 substitution
      else
        UnificationFailure
  | Constant _, Fonction (_, _) -> UnificationFailure
  |Fonction (_, _), Constant _ -> UnificationFailure

let rec anti_unif_list terms1 terms2 =
  match terms1, terms2 with
  | [], [] -> AntiUnifList []
  | t1 :: rest1, t2 :: rest2 ->
      (match anti_unif t1 t2 with
      | AntiUnifTree tree ->
          let AntiUnifList subTrees = anti_unif_list rest1 rest2 in
          AntiUnifList (tree :: subTrees)
      )
  | _, _ -> raise (Invalid_argument "Invalid input")
  | _, _ -> AntiUnifList []

and anti_unif term1 term2 =
  match term1, term2 with
  | Constant c1, Constant c2 ->
      if c1 = c2 then AntiUnifTree (AntiUnifTerm term1)
      else AntiUnifTree (AntiUnifList [])
  | Variable _, _ -> AntiUnifTree (AntiUnifTerm term1)
  | _, Variable _ -> AntiUnifTree (AntiUnifTerm term2)
  | Fonction (f1, args1), Fonction (f2, args2) ->
    if f1 = f2 && List.length args1 = List.length args2 then
      let AntiUnifList subTrees = anti_unif_list args1 args2 in
      AntiUnifTree (AntiUnifList (AntiUnifTerm (Fonction (f1, [])) :: subTrees))
    else
      AntiUnifTree (AntiUnifList [])
  | (Fonction (_, _), Constant _) -> AntiUnifTree (AntiUnifList [])
  | Constant _, Fonction (_, _) -> AntiUnifTree (AntiUnifList [])


(* Exemple d'utilisation *)
let term1 = Fonction ("f", [Variable "X"; Constant "a"]);;
let term2 = Fonction ("f", [Constant "b"; Variable "Y"]);;
(*Autre exemple*)
let t1 = Fonction("f", [Variable "X"; Fonction("g", [Variable "Y"])]);;
let t2 = Fonction("f", [Variable "Z"; Fonction("g", [Variable "Z"])]);;
let t7 = Fonction("h", [Fonction("f", [Variable "X"]); Fonction("g", [Constant "a"; Variable "X"]); Fonction("f", [Constant "c"])]);;
let t8 = Fonction("h", [Variable "T"; Fonction("g", [Variable "U"; Variable "U"]); Fonction("f", [Variable "K"])]);;
let unification_result = unify t1 t2 [] in
  match unification_result with
  | Substitution sub -> (* Unification réussie *)
      print_endline "Unification réussie!";
      List.iter (fun (var, t) -> Printf.printf "%s -> %s\n" var (match t with Variable v -> v | _ -> "")) sub
  | UnificationFailure -> (* Échec de l'unification *)
      print_endline "Échec de l'unification!";;

let rec string_of_term term =
  match term with
  | Constant c -> c
  | Variable v -> v
  | Fonction (f, args) ->
      let args_str = String.concat ", " (List.map string_of_term args) in
      f ^ "(" ^ args_str ^ ")";;

let anti_unification_result = anti_unif t7 t8 in
match anti_unification_result with
| AntiUnifTree tree ->
    print_endline "Anti-unification réussie!";
    let rec print_anti_unif_tree_indent indent tree =
      match tree with
      | AntiUnifTerm term ->
          let indentation = String.make indent ' ' in
          print_endline (indentation ^ "Term: " ^ string_of_term term)
      | AntiUnifList subTrees ->
          let indentation = String.make indent ' ' in
          print_endline (indentation ^ "List:");
          List.iter (print_anti_unif_tree_indent (indent + 2)) subTrees
    in
    print_anti_unif_tree_indent 0 tree
| _ ->
    print_endline "Anti-unification échouée."
        
      
