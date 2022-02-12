type contact = string;;

let l : contact list = ["Felleisen"; "Leroy"; "Abelson"; "Sussman"];;

let l’ : contact list = ["Abelson"; "Felleisen"; "Leroy"; "Sussman"];;


(* ╺┓          ╺┳╸┏━┓╻   ┏━┓┏━┓┏━┓   ┏━┓╻  ┏━╸┏━╸╺┳╸╻┏━┓┏┓╻ *)
(*  ┃    ╺━╸    ┃ ┣┳┛┃   ┣━┛┣━┫┣┳┛   ┗━┓┃  ┣╸ ┃   ┃ ┃┃ ┃┃┗┫ *)
(* ╺┻╸          ╹ ╹┗╸╹   ╹  ╹ ╹╹┗╸   ┗━┛┗━╸┗━╸┗━╸ ╹ ╹┗━┛╹ ╹ *)



(* Trie l en suivant l’ordre lexicographique. *)
let rec sort_contacts (l : contact list) : contact list =
    if l = [] then []
    else let (m, o) = select_min l
    in m :: sort_contacts o;;

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠼⠄ *)

let rec select_min (contlist : contact list) : (contact * (contact list))=
    let rec min (current_min : contact) (contlist : contact list) : (contact * (contact list)) =
        match contlist with
        | [] -> (current_min, contlist)
        | _ -> select_min ((if current_min < List.hd contlist then current_min else List.hd contlist); List.tl contlist)
    in (min (List.hd contlist, contlist), contlist);;





