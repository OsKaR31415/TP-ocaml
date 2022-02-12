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
   La fonction partition_contacts est en fait un wrapper pour la fonction
   separate.  A chaque appel, cette fonction place un élément de son argument
   others (qui contient les contacts qui restent à partitionner), soit dans
   alpha, soit dans beta, et fait un appel récursif à elle-même.  La complexité
   algorithmique de partition_contacts en fonction de la longueur de la liste
   de contacts donnée en entrée est O(n), car elle fera n appels récursif à sa
   sous-fonction.  Cette complexité est toujours la même, indépendamment des
   paramètres (c'est donc la complexité minimale, moyenne et maximale à la
   fois).
 *)

(* Complexité de qsort_contacts
   La fonction qsort_contact est une fonction récursive qui, à chaque appel,
   partitionne la liste de contacts en suivant un pivot (qui est le premier
   élément de la liste), trie chaque partition récursivement, puis les joint.
   Dans le pire des cas, Si la liste est déjà triée (ou bien triée en ordre inverse), il faudra que qsort_contacts fasse partitionner n fois la liste par partition_contacts, qui à une complexité en O(n). Donc, la comlexité dans le pire des cas est O(n^2) car un algorithme de complexité O(n^2) est exécuté n fois.
   Dans un cas moyen (ce qui nous intéresse le plus), la partition sera faîte, en moyenne sur la moitié de la liste. On aura donc O(log n) partitions à faire. Chaque partition a une complexité de O(n), on a donc une complexité de O(n log n) dans le cas moyen.
   Dans le meilleur des cas, le pivot permet toujours de partitionner la liste de contacts en deux parties de même longueur, ce qui fait que le nombre de divisions nécessaires pour arriver au cas de base de la récursion est O(log(n)). La complexité dans ce cas est donc O(n log(n))
 *)


