
type contact = string;;

let cl: contact list = ["Fellesein"; "Leroy"; "Abelson"; "Sussman"];;



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




