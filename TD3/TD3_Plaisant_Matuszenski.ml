type contact = string;;

let l : contact list = ["Felleisen"; "Leroy"; "Abelson"; "Sussman"];;

let l' : contact list = ["Abelson"; "Felleisen"; "Leroy"; "Sussman"];;


(* ╺┓          ╺┳╸┏━┓╻   ┏━┓┏━┓┏━┓   ┏━┓╻  ┏━╸┏━╸╺┳╸╻┏━┓┏┓╻ *)
(*  ┃    ╺━╸    ┃ ┣┳┛┃   ┣━┛┣━┫┣┳┛   ┗━┓┃  ┣╸ ┃   ┃ ┃┃ ┃┃┗┫ *)
(* ╺┻╸          ╹ ╹┗╸╹   ╹  ╹ ╹╹┗╸   ┗━┛┗━╸┗━╸┗━╸ ╹ ╹┗━┛╹ ╹ *)


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠼⠄ *)


(* remove a contact from a contact list *)
let rec remove_contact (contlist : contact list) (cont : contact) : (contact list) =
    match contlist with
    | [] -> []
    | fst_contact::rest ->if fst_contact = cont
    then remove_contact rest cont else fst_contact :: remove_contact rest cont;;

(* get the minimum contact in a contact list *)
let rec minimum_contact (contlist : contact list) : contact=
    let rec minimum (current_min : contact) (contlist: contact list) : contact =
        match contlist with
        | [] -> current_min
        | fst_contact::rest -> (
            if fst_contact < current_min
            then minimum fst_contact rest
            else minimum current_min rest)
    in (minimum (List.hd contlist) (List.tl contlist));;

let rec select_min (contlist: contact list) : (contact * (contact list)) =
    if contlist = [] then raise (Invalid_argument "must not be empty")
    else
    let min = minimum_contact contlist
    in min, (remove_contact contlist min);;

(* Trie l en suivant l’ordre lexicographique. *)
let rec sort_contacts (l : contact list) : contact list =
    if l = [] then []
    else let (m, o) = select_min l
    in m :: sort_contacts o;;





(* ┏━┓         ╺┳╸┏━┓╻   ┏━┓┏━┓┏━┓╻╺┳┓┏━╸ *)
(* ┏━┛   ╺━╸    ┃ ┣┳┛┃   ┣┳┛┣━┫┣━┛┃ ┃┃┣╸  *)
(* ┗━╸          ╹ ╹┗╸╹   ╹┗╸╹ ╹╹  ╹╺┻┛┗━╸ *)

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠼⠄ *)

(* Sépare les éléments supérieurs ou inférieurs au pivot dans contlist *)
let partition_contacts (pivot: contact) (contlist: contact list) : ((contact list)*(contact list)) =
    let rec separate (pivot: contact) (alpha: contact list) (beta: contact list) (others: contact list) =
        if others = [] then (alpha, beta)
        else let element = (List.hd others)
        in if pivot < element
        then separate pivot alpha            (element::beta) (List.tl others)
        else separate pivot (element::alpha) beta            (List.tl others)
    in separate pivot [] [] contlist;;



(* Trie l en suivant l’ordre lexicographique. *)
let rec qsort_contacts (l : contact list) : contact list =
    match l with
    | [] -> []
    | c :: l' -> let (alpha, beta) = partition_contacts c l'
    in qsort_contacts alpha @ [c] @ qsort_contacts beta;;



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠮⠤ *)

(* Complexité de partition_contacts
   La fonction partition_contacts est en fait un wrapper pour la fonction separate.
   A chaque appel, cette fonction place un élément de son argument others
 *)


