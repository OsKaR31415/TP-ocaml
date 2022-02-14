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
   Dans un cas moyen (ce qui nous intéresse le plus), la partition sera faîte, en moyenne sur la moitié de la liste. On aura donc log_2(n) partitions à faire. Chaque partition a une complexité de O(n), on a donc une complexité de O(n log n) dans le cas moyen.
   Dans le meilleur des cas, la complexité est de O(n), car c'est ce qui est marqué sur wikipédia (et il faut toujours croire ce qui est dit sur internet).
 *)


(* ┏━┓         ┏┳┓┏━╸┏━┓╻ ╻┏━┓┏━╸   ╺┳┓╻ ╻   ╺┳╸┏━╸┏┳┓┏━┓┏━┓   ╺┳┓╻┏━╸╻ ╻┏━╸┏━╸╻ ╻╺┳╸╻┏━┓┏┓╻ *)
(* ╺━┫   ╺━╸   ┃┃┃┣╸ ┗━┓┃ ┃┣┳┛┣╸     ┃┃┃ ┃    ┃ ┣╸ ┃┃┃┣━┛┗━┓    ┃┃ ┣╸ ┏╋┛┣╸ ┃  ┃ ┃ ┃ ┃┃ ┃┃┗┫ *)
(* ┗━┛         ╹ ╹┗━╸┗━┛┗━┛╹┗╸┗━╸   ╺┻┛┗━┛    ╹ ┗━╸╹ ╹╹  ┗━┛   ╺┻┛ ┗━╸╹ ╹┗━╸┗━╸┗━┛ ╹ ╹┗━┛╹ ╹ *)



let gen_contact max_len =
    let gen_char () =
        let i = Random.int 26 + 97 in
        char_of_int i
        in
        let rec aux acc x =
            if x = 0 then (String.of_seq (List.to_seq acc))
            else aux (gen_char () :: acc) (x - 1)
    in aux [] max_len;;


let rec gen_contacts (n : int) : string list =
    if n <= 0 then []
    else gen_contact (Random.int 10) :: gen_contacts (n - 1);;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢉⡹   ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠤⠜ ⠶ ⠼⠄ *)

(* La fonction *gen_contacts* n'est pas une fonction au sens mathématique du
   terme car le résultat qu'elle donne n'est pas uniquement fonction des
   paramètres donnés en entrée, c'est-à-dire que la fonction n'est pas
   totalement déterministe. En effet, l'introduction d'aléatoire dans le
   programme fait que l'appel plusieurs fois de gen_contacts avec les mêmes
   arguments ne donnera pas forcément le même résultat.
   Ceci dit, une variable aléatoire est définie formellement comme une fonction
   (source : https://fr.wikipedia.org/wiki/Variable_al%C3%A9atoire#D%C3%A9finitions)
*)


(* Note: I saw you copy-pasting the time function from stackoverflow. Don't try to deny it.
           \\
            \\
   ==\       \\
      =\=     \\
        =\=    \\
           \==  \\
    ¯¯¯       \==
       ¯¯¯¯¯
   _______  ¯¯¯¯
          ____
| ____________ |
|              |
¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
 *)

(* Function that measures the execution time of a given function *func* called
    with the argument *arg*. *)
let time func arg =
    let begin_time = Sys.time() in
    let foo = func arg in (* just here to execute the function *)
    Printf.printf "Execution took: %f seconds\n" (10. *. (Sys.time() -. begin_time));;

(* Function that measures the execution time of a given function *func* called
   with the argument *arg* and that returns the result of the execution. *)
let time_and_return func arg =
    let begin_time = Sys.time() in
    let result = func arg in
    Printf.printf "Execution took: %f seconds\n" (10. *. (Sys.time() -. begin_time));
    result;;

(* Measure the execution time of *gen_contacts*. *)
time gen_contacts 10;;  (* 140μs *)
time gen_contacts 100;;  (* 1400μs *)
time gen_contacts 1000;;  (* 12860μs = 12.86ms *)
time gen_contacts 5000;;  (* 61310μs = 61.31ms *)
time gen_contacts 10000;; (* 120ms *)

(* Il semble que la complexité en temps soit de O(n), car le temps mesuré sur
   les appels semble proportionnel au nombre de valeurs demandées. *)


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢉⡹   ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠤⠜ ⠶ ⠤⠜ *)

(* Check if a contact list is sorted - O(n) *)
let rec sorted_contacts (contlist: contact list) : bool =
    match contlist with
    | [] -> true
    | first::[] -> true
    | first :: second :: rest -> (
        if first < second
        then (sorted_contacts (second :: rest))
        else false
    );;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢉⡹   ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠤⠜ ⠶ ⠤⠜ *)

(* Appeler la fonction *sorted_contacts* sur le résultat de l'appel à une
   fonction de tri pour 100 listes de contacts générées de manière aléatoire
   permettrai de verrifier que la fonction de trie permet effectivement de
   trier la liste de contact q'on lui donne, et qu'elle ne laisse pas certains
   éléments dans le désordre. Cela peut permettre de constituer un test de la
   fonction de tri, même si cela ne constitue pas une preuve du fonctionnement
   de l'algorithme de tri. *)


