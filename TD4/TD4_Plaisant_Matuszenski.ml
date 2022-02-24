
type contact = string;;

let cl: contact list = ["Fellesein"; "Leroy"; "Abelson"; "Sussman"];;



(* ╺┓          ┏━╸┏━┓┏┳┓┏━┓╺┳╸┏━┓┏━╸┏━╸   ╺┳┓┏━╸   ┏━╸┏━┓┏━┓┏━┓┏━╸╺┳╸┏━┓┏━╸┏━┓ *)
(*  ┃    ╺━╸   ┃  ┃ ┃┃┃┃┣━┛ ┃ ┣━┫┃╺┓┣╸     ┃┃┣╸    ┃  ┣━┫┣┳┛┣━┫┃   ┃ ┣┳┛┣╸ ┗━┓ *)
(* ╺┻╸         ┗━╸┗━┛╹ ╹╹   ╹ ╹ ╹┗━┛┗━╸   ╺┻┛┗━╸   ┗━╸╹ ╹╹┗╸╹ ╹┗━╸ ╹ ╹┗╸┗━╸┗━┛ *)

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠼⠄ *)

let count_chars (contlist: contact list): int =
    let rec aux (contlist: contact list) (acc: int): int =
        match contlist with
        | [] -> acc
        | hd::tl -> (aux (tl) (String.length hd))
    in (aux contlist 1);;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠮⠤ *)



