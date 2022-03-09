
type ibintree = 
    IBEMPTY
  | IBNODE of int * ibintree * ibintree;;


let t0 = IBNODE (8,
                 IBNODE (4, IBEMPTY, IBEMPTY),
                 IBNODE (3, IBEMPTY, IBEMPTY));;


(* ╺┓          ┏━┓┏━┓┏━╸┏━┓┏┳┓┏┓ ╻ ╻╻  ┏━╸ *)
(*  ┃    ╺━╸   ┣━┛┣┳┛┣╸ ┣━┫┃┃┃┣┻┓┃ ┃┃  ┣╸  *)
(* ╺┻╸         ╹  ╹┗╸┗━╸╹ ╹╹ ╹┗━┛┗━┛┗━╸┗━╸ *)

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠼⠄ *)

let leaf (number: int): ibintree =
    IBNODE (number, IBEMPTY, IBEMPTY);;

leaf 3 = IBNODE (3, IBEMPTY, IBEMPTY);;
leaf 0 = IBNODE (0, IBEMPTY, IBEMPTY);;
leaf (-1) = IBNODE (-1, IBEMPTY, IBEMPTY);;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠮⠤ *)


let t1 = IBNODE (8, IBNODE (4, IBEMPTY, IBEMPTY),
                    IBNODE (3, IBNODE (0, IBEMPTY, IBEMPTY),
                               IBNODE (12, IBEMPTY, IBEMPTY)));;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠤⠜ *)

let is_parent (t: ibintree) =
    match t with
    | IBEMPTY -> false
    | IBNODE (_, IBEMPTY, IBEMPTY) -> false
    | _ -> true;;

is_parent IBEMPTY = False;;
is_parent (IBNODE (42, IBEMPTY, IBEMPTY)) = False;;
is_parent (leaf 9) = True;;
is_parent (IBNODE (7, IBEMPTY, leaf 5)) = True;;
is_parent (IBNODE (4, leaf 9, IBEMPTY)) = True;;
is_parent t0 = True;;



(* ┏━┓         ┏┳┓┏━┓┏┓╻╻┏━┓╻ ╻╻  ┏━┓╺┳╸╻┏━┓┏┓╻   ╺┳┓╻┏━┓┏━┓┏┓ ┏━┓┏━╸┏━┓ *)
(* ┏━┛   ╺━╸   ┃┃┃┣━┫┃┗┫┃┣━┛┃ ┃┃  ┣━┫ ┃ ┃┃ ┃┃┗┫    ┃┃ ┣━┫┣┳┛┣┻┓┣┳┛┣╸ ┗━┓ *)
(* ┗━╸         ╹ ╹╹ ╹╹ ╹╹╹  ┗━┛┗━╸╹ ╹ ╹ ╹┗━┛╹ ╹   ╺┻┛ ╹ ╹╹┗╸┗━┛╹┗╸┗━╸┗━┛ *)



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠼⠄ *)



(* Calcule la hauteur de t. *)
let rec bt_height (t : ibintree) : int =
    match t with
        | IBEMPTY -> -1
        | IBNODE (x, l, r) -> 1 + max(bt_height l) (bt_height r);;


bt_height IBEMPTY = -1;;
bt_height (IBNODE (3, IBEMPTY, IBEMPTY)) = 0;;
bt_height (IBNODE (3, IBNODE (2, IBEMPTY, IBEMPTY),
                      IBNODE(10,  IBEMPTY, IBEMPTY))) = 1;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠮⠤ *)

let rec nodes_sum (tree: ibintree): int =
    match tree with
    | IBEMPTY -> invalid_arg "Empty tree"
    | IBNODE (num, IBEMPTY, IBEMPTY) -> num
    | IBNODE (num, left, right) -> num + (nodes_sum left) + (nodes_sum right);;

(* test *)
nodes_sum IBEMPTY;; (* Error *)
nodes_sum (leaf 73) = 73;;
nodes_sum (IBNODE (3, (leaf 4), (leaf 5))) = 12;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠤⠜ *)

(* get the number of nodes in a tree *)
let rec nodes_count (tree: ibintree): int =
    match tree with
    | IBEMPTY -> 0
    | IBNODE (_, IBEMPTY, IBEMPTY) -> 1
    | IBNODE (_, left, right) -> 1 + (nodes_count left) + (nodes_count right);;

nodes_count IBEMPTY = 0;;
nodes_count (leaf 4) = 1;;
nodes_count t0 = 3;;
nodes_count t1 = 5;;

let nodes_mean (tree: ibintree): float =
    (float_of_int (nodes_sum tree)) /. (float_of_int (nodes_count tree));;

nodes_mean IBEMPTY;; (* error *)
nodes_mean (leaf 4) = 4.;;
nodes_mean t0 = 5.;;
nodes_mean t1 = 5.4;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢇⣸ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶  ⠸ *)

let rec node_succ (tree: ibintree): ibintree =
    match tree with
    | IBEMPTY -> IBEMPTY
    | IBNODE (num, left, right) -> IBNODE (num+1, node_succ left, node_succ right);;


(* tests *)
node_succ IBEMPTY = IBEMPTY;;
node_succ (leaf 4) = leaf 5;;
node_succ t0 = IBNODE (9, IBNODE (5, IBEMPTY, IBEMPTY),
                          IBNODE (4, IBEMPTY, IBEMPTY));;
node_succ t1 = IBNODE (9, IBNODE (5, IBEMPTY, IBEMPTY),
                          IBNODE (4, IBNODE (1, IBEMPTY, IBEMPTY),
                                     IBNODE (13, IBEMPTY, IBEMPTY)));;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⣏⡉ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠤⠜ *)

let rec count_leaves (tree: ibintree): int =
    match tree with
    | IBEMPTY -> 0
    | IBNODE (_, IBEMPTY, IBEMPTY) -> 1
    | IBNODE (_, left, right) -> (count_leaves left) + (count_leaves right);;


(* tests *)
count_leaves IBEMPTY = 0;;
count_leaves (leaf 4) = 1;;
count_leaves (IBNODE (4, leaf 2, leaf 3)) = 2;;
count_leaves t0 = 2;;
count_leaves t1 = 3;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⣎⡁ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠣⠜ *)

let rec leaves (tree: ibintree): (int list) =
    match tree with
    | IBEMPTY -> []
    | IBNODE (number, IBEMPTY, IBEMPTY) -> [number]
    | IBNODE (_, left, right) -> (leaves left) @ (leaves right);;


leaves IBEMPTY = [];;
leaves (leaf 5) = [5];;
leaves t0 = [4; 3];;
leaves t1 = [4; 0; 12];;



