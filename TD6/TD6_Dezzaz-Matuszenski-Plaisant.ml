
type 'a bintree =
    BEmpty | BNode of 'a * 'a bintree * 'a bintree;;


let t0 : int bintree = BNode (4,
    BNode (2, BEmpty, BNode(3, BEmpty, BEmpty)),
    BNode (8,
        BNode(6, BEmpty, BEmpty),
        BNode(10, BEmpty, BEmpty)));;

(* ╺┓          ╻ ╻┏━╸┏━┓╻┏━╸╻┏━╸┏━┓╺┳╸╻┏━┓┏┓╻   ╺┳┓┏━╸   ┏━┓┏━┓┏━┓┏━┓┏━┓╻┏━╸╺┳╸┏━╸┏━┓ *)
(*  ┃    ╺━╸   ┃┏┛┣╸ ┣┳┛┃┣╸ ┃┃  ┣━┫ ┃ ┃┃ ┃┃┗┫    ┃┃┣╸    ┣━┛┣┳┛┃ ┃┣━┛┣┳┛┃┣╸  ┃ ┣╸ ┗━┓ *)
(* ╺┻╸         ┗┛ ┗━╸╹┗╸╹╹  ╹┗━╸╹ ╹ ╹ ╹┗━┛╹ ╹   ╺┻┛┗━╸   ╹  ╹┗╸┗━┛╹  ╹┗╸╹┗━╸ ╹ ┗━╸┗━┛ *)

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠼⠄ *)

let dist (a: int) (b: int) : int =
    let d = a - b
    in if d > 0 then d else -d;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠮⠤ *)

let rec hauteur (tree: 'a bintree) : int =
    match tree with
    | BEmpty -> -1
    | BNode(_, left, right) -> max (1+hauteur(left)) (1+hauteur(right));;

(* tests *)
hauteur BEmpty = -1;;
hauteur t0 = 2;;


let rec is_balanced (tree: 'a bintree) (d: int): bool =
    match tree with
    | BEmpty -> true
    | BNode(_, left, right) ->
            if not ((is_balanced left d) && (is_balanced right d)) then false
            else d <= (max (hauteur left) (hauteur right));;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠤⠜ *)

(* Renvoie le plus petit element d'un ABR t. *)
let rec bst_min (t : 'a bintree) : 'a =
    match t with
    | BEmpty -> invalid_arg "empty tree"
    | BNode (x, BEmpty, _) -> x
    | BNode (_, l, _) -> bst_min l;;

(* Renvoie le plus grand element d'un ABR t. *)
let rec bst_max (t : 'a bintree) : 'a =
    match t with
    | BEmpty -> invalid_arg "empty tree"
    | BNode (x, _, BEmpty) -> x
    | BNode (_, _, r) -> bst_max r;;

let rec is_bst (tree : 'a bintree) : bool =
    match tree with
    | BEmpty -> true
    | BNode(_, BEmpty, BEmpty) -> true
    | BNode (x, BEmpty, right) -> is_bst right && x < (bst_max right)
    | BNode (x, left, BEmpty) -> is_bst left && x > (bst_min left)
    | BNode(x, left, right ) -> (is_bst left) && (is_bst right) && x > (bst_min left) && x < (bst_max right);;


(* tests *)
is_bst t0;;

let not_research_binary_tree : int bintree = BNode (4,
    BNode (2, BEmpty, BNode(3, BEmpty, BEmpty)),
    BNode (8,
        BNode(6, BEmpty, BEmpty),
        BNode(0, BEmpty, BEmpty)));;

is_bst not_research_binary_tree = false;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢇⣸ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶  ⠸ *)


(* Complexite algorithmique :
    De bst_min : O(log_2 n)
    De bst_max : O(log_2 n)
    De is_bst  : O(n log_2 n)
*)



(* ┏━┓         ╺┳╸┏━┓┏━┓┏┓╻┏━┓┏━╸┏━┓┏━┓┏┳┓┏━┓╺┳╸╻┏━┓┏┓╻┏━┓   ╺┳┓╻┏━┓┏━┓┏┓ ┏━┓┏━╸┏━┓ *)
(* ┏━┛   ╺━╸    ┃ ┣┳┛┣━┫┃┗┫┗━┓┣╸ ┃ ┃┣┳┛┃┃┃┣━┫ ┃ ┃┃ ┃┃┗┫┗━┓    ┃┃ ┣━┫┣┳┛┣┻┓┣┳┛┣╸ ┗━┓ *)
(* ┗━╸          ╹ ╹┗╸╹ ╹╹ ╹┗━┛╹  ┗━┛╹┗╸╹ ╹╹ ╹ ╹ ╹┗━┛╹ ╹┗━┛   ╺┻┛ ╹ ╹╹┗╸┗━┛╹┗╸┗━╸┗━┛ *)


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠼⠄ *)

let rec bst_nodes (tree: 'a bintree) : 'a list =
    match tree with
    | BEmpty -> []
    | BNode(x, left, right) -> (bst_nodes left) @ x :: (bst_nodes right);;


bst_nodes BEmpty = [];;
bst_nodes t0 = [2; 3; 4; 6; 8; 10];;

