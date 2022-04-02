
(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ╺┓  *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸     ┃  *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ╺┻╸ *)


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠼⠄ *)

(*
Les différences entre tab_sin et tab_sqrt sont leurs noms et l'utilisation de
[sin 0.] contre [sqrt 0.]. Et Donc comme nous dis les commentaire tab_sin
donnera les sinus de n à 0 et tab_sqrt les racines de n à 0

 *)

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠮⠤ *)

let rec tab_to_floats (f: float -> float) (n: int): float list =
    if n = 0 then [f 0.]
    else f (float_of_int n) :: tab_to_floats f (n-1);;

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠤⠜ *)


let tab_to_sin (n: int): float list =
    tab_to_floats sin n;;


let tab_to_sqrt (n: int): float list =
    tab_to_floats sqrt n;;

(* tests *)
tab_to_sin 5 = [(sin 5.); (sin 4.); (sin 3.); (sin 2.); (sin 1.); (sin 0.)];;
tab_to_sqrt 5 = [(sqrt 5.); (sqrt 4.); (sqrt 3.); (sqrt 2.); (sqrt 1.); (sqrt 0.)];;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢇⣸ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶  ⠸ *)

let rec tabulate (n : int) (f : int -> 'a) : 'a list =
    if n < 0 then []
    else f n :: tabulate (n - 1) f;;

let tab_to_floats (f: float -> 'a) (n: int) : 'a list =
    let f_from_int (i: int) : 'a = f (float_of_int i)
    in tabulate n f_from_int;;



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⣏⡉ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠤⠜ *)

let tab_repeat (n: int) (chr: string) : string list =
    let rec repeat (chr: string) (n: int) : string =
        match n with
        | 0 -> ""
        | 1 -> chr
        | _ -> chr ^ (repeat chr (n-1))
    in (tabulate n (repeat chr));;

tab_repeat 0 "a" = [""];;
tab_repeat 1 "e" = ["e"; ""];;
tab_repeat 2 "a" = ["aa"; "a"; ""];;
tab_repeat 5 "z" = ["zzzzz"; "zzzz"; "zzz"; "zz"; "z"; ""];;


(* ┏━┓         ╻ ╻┏┓╻╻╻ ╻┏━╸┏━┓┏━┓┏━┓╻  ╻╺┳╸┏━╸   ┏━╸╺┳╸   ┏━╸╻ ╻┏━┓┏━┓┏━╸┏━┓┏━┓╻╻ ╻╻╺┳╸┏━╸   ╺┳┓┏━╸   ┏━╸┏━┓╻  ╺┳┓ *)
(* ┏━┛   ╺━╸   ┃ ┃┃┗┫┃┃┏┛┣╸ ┣┳┛┗━┓┣━┫┃  ┃ ┃ ┣╸    ┣╸  ┃    ┣╸ ┏╋┛┣━┛┣┳┛┣╸ ┗━┓┗━┓┃┃┏┛┃ ┃ ┣╸     ┃┃┣╸    ┣╸ ┃ ┃┃   ┃┃ *)
(* ┗━╸         ┗━┛╹ ╹╹┗┛ ┗━╸╹┗╸┗━┛╹ ╹┗━╸╹ ╹ ┗━╸   ┗━╸ ╹    ┗━╸╹ ╹╹  ╹┗╸┗━╸┗━┛┗━┛╹┗┛ ╹ ╹ ┗━╸   ╺┻┛┗━╸   ╹  ┗━┛┗━╸╺┻┛ *)


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠼⠄ *)

let length (l: 'a list): int =
    List.fold_right (fun x y -> y + 1) l 0;;

length [] = 0;;
length [1];;
length [1; 9] = 2;;
length [1; 2; 3; 5; 8; 13] = 6;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠮⠤ *)

let map (f: 'a -> 'b) (l: 'a list): 'b list =
    List.fold_right (fun x y -> (f x)::y) l [];;

map (fun x -> x) [] = [];;
map (fun x -> x*x) [1; 2; 3; 4; 5; 6] = [1; 4; 9; 16; 25; 36];;
map (fun x -> x^"s") ["le"; "test"; "cool"] = ["les"; "tests"; "cools"];;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠤⠜ *)

let filter (f: 'a -> bool) (l: 'a list) : 'a list =
    List.fold_right (fun x y -> if f(x) then x::y else y) l [];;

filter (fun x -> x = 2) [1; 2; 1; 3; 1; 2; 1; 4; 1; 2; 1] = [2; 2; 2];;
filter (fun x -> (x mod 2) = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9] = [2; 4; 6; 8];;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢇⣸ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶  ⠸ *)

let for_all (f: 'a -> bool) (l: 'a list) : bool =
    List.fold_right (fun x y -> (f x) && y) l true;;

for_all (fun x -> x mod 2 = 0) [] = true;;
for_all (fun x -> x mod 2 = 0) [2; 4; 6; 12] = true;;
for_all (fun x -> x mod 2 = 0) [2; 3; 6; 12] = false;;
for_all (fun x -> x = 1 || x = 2) [1; 2; 1; 3; 1; 2; 1] = false;;
for_all (fun x -> x = 1 || x = 2) [1; 2; 1; 2; 1; 2; 1] = true;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⣏⡉ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠤⠜ *)

let rec partition (p: 'a -> bool) (l: 'a list) : ('a list) * ('a list) =
    match l with
    | [] -> ([], [])
    | [elt] -> if p elt then ([elt], []) else ([], [elt])
    | elt::tail -> if p elt
    then (elt::(fst (partition p tail)),       (snd (partition p tail)))
    else (     (fst (partition p tail)), elt:: (snd (partition p tail)));;

partition (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9] = ([2; 4; 6; 8], [1; 3; 5; 7; 9]);;
partition (fun x -> x mod 3 = 1) [1; 2; 3; 4; 5; 6; 7; 8; 9] = ([1; 4; 7], [2; 3; 5; 6; 8; 9]);;



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⣎⡁ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠣⠜ *)

let partition (p: 'a -> bool) (l: 'a list) : ('a list) * ('a list) =
    List.fold_right (fun x y -> if p x then (x::(fst y), snd y) else (fst y, x::(snd y))) l ([], []);;


partition (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9] = ([2; 4; 6; 8], [1; 3; 5; 7; 9]);;
partition (fun x -> x mod 3 = 1) [1; 2; 3; 4; 5; 6; 7; 8; 9] = ([1; 4; 7], [2; 3; 5; 6; 8; 9]);;



