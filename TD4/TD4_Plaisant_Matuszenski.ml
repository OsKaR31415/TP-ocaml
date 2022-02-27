
type contact = string;;

let cl: contact list = ["Felleisen"; "Leroy"; "Abelson"; "Sussman"];;



(* ╺┓          ┏━╸┏━┓┏┳┓┏━┓╺┳╸┏━┓┏━╸┏━╸   ╺┳┓┏━╸   ┏━╸┏━┓┏━┓┏━┓┏━╸╺┳╸┏━┓┏━╸┏━┓ *)
(*  ┃    ╺━╸   ┃  ┃ ┃┃┃┃┣━┛ ┃ ┣━┫┃╺┓┣╸     ┃┃┣╸    ┃  ┣━┫┣┳┛┣━┫┃   ┃ ┣┳┛┣╸ ┗━┓ *)
(* ╺┻╸         ┗━╸┗━┛╹ ╹╹   ╹ ╹ ╹┗━┛┗━╸   ╺┻┛┗━╸   ┗━╸╹ ╹╹┗╸╹ ╹┗━╸ ╹ ╹┗╸┗━╸┗━┛ *)

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠼⠄ *)

let rec count_chars (contlist: contact list): int =
    match contlist with
    | [] -> 0
    | hd::tl -> (String.length hd) + count_chars tl


(* tests unitaires *)
assert (count_chars [] = 0);;
assert (count_chars [""] = 0);;
assert (count_chars ["Sussman"] = 7);;
assert (count_chars ["Sussman"; "Leroy"] = 7 + 5);;
assert (count_chars ["12"; "345"; "6"; ""; "7"] = 7);;
assert (count_chars cl = 9 + 5 + 7 + 7);;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠮⠤ *)

let rec count_chars_term (contlist: contact list) (acc: int): int =
        match contlist with
        | [] -> acc
        | hd::tl -> (count_chars_term (tl) (acc + (String.length hd)));;


(* tests unitaires *)
assert (count_chars_term []                          0 = 0);;
assert (count_chars_term [""]                        0 = 0);;
assert (count_chars_term ["Sussman"]                 0 = 7);;
assert (count_chars_term ["Sussman"; "Leroy"]        0 = 7 + 5);;
assert (count_chars_term ["12"; "345"; "6"; ""; "7"] 0 = 7);;
assert (count_chars_term cl                          0 = 9 + 5 + 7 + 7);;



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠤⠜ *)

let count_chars (contlist: contact list): int =
    count_chars_term contlist 0;;

(* tests unitaires *)
assert (count_chars [] = 0);;
assert (count_chars [""] = 0);;
assert (count_chars ["Sussman"] = 7);;
assert (count_chars ["Sussman"; "Leroy"] = 7 + 5);;
assert (count_chars ["12"; "345"; "6"; ""; "7"] = 7);;
assert (count_chars cl = 9 + 5 + 7 + 7);;



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢇⣸ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶  ⠸ *)


let count_chars (contlist: contact list): int =
    let rec aux (contlist: contact list) (acc: int): int =
        match contlist with
        | [] -> acc
        | hd::tl -> (aux tl (acc + String.length hd))
    in aux contlist 0;;


(* tests unitaires *)
assert (count_chars [] = 0);;
assert (count_chars [""] = 0);;
assert (count_chars ["Sussman"] = 7);;
assert (count_chars ["Sussman"; "Leroy"] = 7 + 5);;
assert (count_chars ["12"; "345"; "6"; ""; "7"] = 7);;
assert (count_chars cl = 9 + 5 + 7 + 7);;



(* ┏━┓         ┏━╸┏━┓┏━┓┏┳┓┏━┓╺┳╸┏━┓┏━╸┏━╸   ╺┳┓┏━╸   ┏━╸┏━┓┏┓╻╺┳╸┏━┓┏━╸╺┳╸┏━┓ *)
(* ┏━┛   ╺━╸   ┣╸ ┃ ┃┣┳┛┃┃┃┣━┫ ┃ ┣━┫┃╺┓┣╸     ┃┃┣╸    ┃  ┃ ┃┃┗┫ ┃ ┣━┫┃   ┃ ┗━┓ *)
(* ┗━╸         ╹  ┗━┛╹┗╸╹ ╹╹ ╹ ╹ ╹ ╹┗━┛┗━╸   ╺┻┛┗━╸   ┗━╸┗━┛╹ ╹ ╹ ╹ ╹┗━╸ ╹ ┗━┛ *)


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠼⠄ *)


let rec format_contacts (contlist : contact list) : string =
    match contlist with
    | [] -> "tout le monde"
    | alone::[] -> alone
    | first::second::[] -> first ^ " et " ^ second
    | hd::tl -> hd ^ ", " ^ (format_contacts tl);;

(* tests unitaire *)
format_contacts [] = "tout le monde";;
format_contacts ["Sussman"] = "Sussman";;
format_contacts ["Sussman"; "Leroy"] = "Sussman et Leroy";;
format_contacts (List.tl cl) = "Leroy, Abelson et Sussman";;
format_contacts cl = "Felleisen, Leroy, Abelson et Sussman";;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠮⠤ *)

let rec format_contacts_term (contlist: contact list) (acc: string) =
    match contlist with
    | [] -> "tout le monde"
    | alone::[] -> if acc = "" then alone else acc ^ " et " ^ alone
    | hd::tl -> if acc = "" then format_contacts_term tl hd
    else format_contacts_term tl (acc ^ ", " ^ hd);;

(* tests unitaires *)
format_contacts_term [] ""                   = "tout le monde";;
format_contacts_term ["Sussman"] ""          = "Sussman";;
format_contacts_term ["Sussman"; "Leroy"] "" = "Sussman et Leroy";;
format_contacts_term (List.tl cl) ""         = "Leroy, Abelson et Sussman";;
format_contacts_term cl ""                   = "Felleisen, Leroy, Abelson et Sussman";;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠤⠜ *)

let format_contacts_term (contlist: contact list) =
    let rec aux (contlist: contact list) (acc: string) =
        match contlist with
        | [] -> "tout le monde"
        | alone::[] -> if acc = "" then alone else acc ^ " et " ^ alone
        | hd::tl -> if acc = "" then format_contacts_term tl hd
        else format_contacts_term tl (acc ^ ", " ^ hd)
    in aux contlist "";;

(* tests unitaires *)
format_contacts_term []                    = "tout le monde";;
format_contacts_term ["Sussman"]           = "Sussman";;
format_contacts_term ["Sussman"; "Leroy"]  = "Sussman et Leroy";;
format_contacts_term (List.tl cl)          = "Leroy, Abelson et Sussman";;
format_contacts_term cl                    = "Felleisen, Leroy, Abelson et Sussman";;


(* ┏━┓         ┏━┓╻ ╻┏━┓┏━┓┏━┓┏━╸┏━┓┏━┓╻┏━┓┏┓╻   ╺┳┓┏━╸   ┏━╸┏━┓┏┓╻╺┳╸┏━┓┏━╸╺┳╸ *)
(* ╺━┫   ╺━╸   ┗━┓┃ ┃┣━┛┣━┛┣┳┛┣╸ ┗━┓┗━┓┃┃ ┃┃┗┫    ┃┃┣╸    ┃  ┃ ┃┃┗┫ ┃ ┣━┫┃   ┃  *)
(* ┗━┛         ┗━┛┗━┛╹  ╹  ╹┗╸┗━╸┗━┛┗━┛╹┗━┛╹ ╹   ╺┻┛┗━╸   ┗━╸┗━┛╹ ╹ ╹ ╹ ╹┗━╸ ╹  *)

(* Enleve c de l qui peut avoir des doublons. *)
let rec remove_contact (c : contact) (l : contact list) : contact list =
    match l with
    | [] -> []
    | c' :: l' -> let next = remove_contact c l'
    in if c = c' then next else c' :: next;;

(* tests unitaires *)
remove_contact "Felleisen" [] = [];;
remove_contact "Sussman" ["Sussman"; "Sussman"] = [];;
remove_contact "van Rossum" cl = cl;;
remove_contact "Leroy" cl = ["Felleisen"; "Abelson"; "Sussman"];;



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢉⡹   ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠤⠜ ⠶ ⠼⠄ *)

let rec remove_contact_term (c: contact) (l: contact list) (acc: contact list) =
    match l with
    | [] -> acc
    | hd::tl -> if hd = c then remove_contact_term c tl acc
    else remove_contact_term c tl (acc @ hd::[]);;


(* tests unitaires *)
remove_contact_term "Felleisen" []                   [] = [];;
remove_contact_term "Sussman" ["Sussman"; "Sussman"] [] = [];;
remove_contact_term "van Rossum" cl                  [] = cl;;
remove_contact_term "Leroy" cl                       [] = ["Felleisen"; "Abelson"; "Sussman"];;




(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢉⡹   ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠤⠜ ⠶ ⠮⠤ *)


let remove_contact_term (c: contact) (l: contact list): (contact list) =
    let rec aux (c: contact) (l: contact list) (acc: contact list) =
        match l with
        | [] -> acc
        | hd::tl -> if hd = c then remove_contact_term c tl acc
        else remove_contact_term c tl (acc @ hd::[])
    in aux c l [];;


(* tests unitaires *)
remove_contact_term "Felleisen" []                   = [];;
remove_contact_term "Sussman" ["Sussman"; "Sussman"] = [];;
remove_contact_term "van Rossum" cl                  = cl;;
remove_contact_term "Leroy" cl                       = ["Felleisen"; "Abelson"; "Sussman"];;


