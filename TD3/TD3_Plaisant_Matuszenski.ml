type contact = string;;

let l : contact list = ["Felleisen"; "Leroy"; "Abelson"; "Sussman"];;

let l’ : contact list = ["Abelson"; "Felleisen"; "Leroy"; "Sussman"];;

(*1/ Tri par sélection*)

(* Trie l en suivant l’ordre lexicographique. *)
let rec sort_contacts (l : contact list) : contact list =
    if l = [] then []
    else let (m, o) = select_min l
    in m :: sort_contacts o;;
    
(*Cette implémentation s’appuie sur l’existence d’une fonction select_mi*)

let rec select_min (contlist : contact list) : (contact*(contact list)=
     match contlist with
     |[] -> (None,[])
     |hd :: tl -> if hd' <= hd -> hd then hd :: l;; 
