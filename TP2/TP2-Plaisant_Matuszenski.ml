type euro = float;;

type sold_product = {
    id : int;
    name : string;
    price : euro;
    sales : int
};;


type sales_report = sold_product list;;

(* il semble tout à fait inacceptable que personne n'aie acheté _Learn Lisp_ ! *)
let c0 : sales_report = [
    {id = 34; name = "book A"; price = 11.9; sales = 12};
    {id = 49; name = "book B"; price = 18.6; sales = 5};
    {id = 47; name = "toothbrush R"; price = 48.7; sales = 2};
    {id = 18; name = "software A"; price = 9.99; sales = 6};
    {id = 24; name = "software B"; price = 109.0; sales = 2};
    {id = 33; name = "game X"; price = 60.9; sales = 18};
    {id = 190; name = "game Y"; price = 49.9; sales = 72};
    {id = 4; name = "Learn Lisp"; price = 63.5; sales = 0};
    {id = 9; name = "Learn OCaml"; price = 48.0; sales = 10};
    {id = 99; name = "pull-over X"; price = 29.0; sales = 1};
    {id = 101; name = "t-shirt Y"; price = 12.0; sales = 7}
];;

(* ╺┓          ┏━┓┏━┓┏━╸┏━┓┏┳┓┏┓ ╻ ╻╻  ┏━╸ *)
(*  ┃    ╺━╸   ┣━┛┣┳┛┣╸ ┣━┫┃┃┃┣┻┓┃ ┃┃  ┣╸ *) 
(* ╺┻╸         ╹  ╹┗╸┗━╸╹ ╹╹ ╹┗━┛┗━┛┗━╸┗━╸ *)

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠼⠄ *)

let popular_product (prod: sold_product): bool =
    prod.sales > 10;;

popular_product {id = 190; name = "game Y"; price = 49.9; sales = 72} = true;;
popular_product {id = 9; name = "Learn OCaml"; price = 48.0; sales = 10} = false;;
popular_product {id = 101; name = "t-shirt Y"; price = 12.0; sales = 7} = false;;



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠮⠤ *)

let product_revenue (prod: sold_product): float =
    (float_of_int prod.sales) *. prod.price;;



product_revenue {id = 99; name = "pull-over X"; price = 29.0; sales = 1} = 29.;;
product_revenue {id = 4; name = "Learn Lisp"; price = 63.5; sales = 0} = 0.;;
product_revenue {id = 101; name = "t-shirt Y"; price = 12.0; sales = 7} = 84.;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⢺    ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠼⠄ ⠶ ⠤⠜ *)

let ids_sales (report: sales_report) : (int * int) list =
    List.map (fun prod -> (prod.id, prod.sales)) report;;

ids_sales c0 = [(34, 12); (49, 5); (47, 2); (18, 6); (24, 2); (33, 18); (190, 72); (4, 0); (9, 10); (99, 1); (101, 7)];;



(* ┏━┓         ┏━┓┏┓╻┏━┓╻  ╻ ╻┏━┓┏━╸   ╺┳┓┏━╸┏━┓   ╻ ╻┏━╸┏┓╻╺┳╸┏━╸┏━┓ *)
(* ┏━┛   ╺━╸   ┣━┫┃┗┫┣━┫┃  ┗┳┛┗━┓┣╸     ┃┃┣╸ ┗━┓   ┃┏┛┣╸ ┃┗┫ ┃ ┣╸ ┗━┓ *)
(* ┗━╸         ╹ ╹╹ ╹╹ ╹┗━╸ ╹ ┗━┛┗━╸   ╺┻┛┗━╸┗━┛   ┗┛ ┗━╸╹ ╹ ╹ ┗━╸┗━┛ *)

(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢺  *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠼⠄ *)

let count_sales (report: sales_report): int =
    List.fold_right (fun prod acc -> prod.sales + acc) report 0;;

count_sales [] = 0;;
count_sales c0 = 135;;



(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⠊⡱ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠮⠤ *)


(* Version en comptant tous les produits *)
let total_revenue (report: sales_report): float =
    List.fold_right (+.) (List.map product_revenue report) 0.;;

total_revenue [] = 0.;;
total_revenue c0 = 5893.13999999999851;;


(* Version en comptant uniquement les produits populaires *)
let popular_revenue (report: sales_report): float =
    List.fold_right (+.) (List.map product_revenue (List.filter popular_product report)) 0.;;

popular_revenue [] = 0.;;
popular_revenue c0 = 4831.8;;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢉⡹ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠤⠜ *)

let rec take (n: int) (l: 'a list): 'a list =
    match l with
    | [] -> []
    | hd::tl -> if n > 0 then hd::(take (n-1) tl) else [];;


take 5 [] = [];; (* take 5 est un super morceau de jazz ! *)
take 3 [8; 7] = [8; 7];;
take 2 ["a"; "b"; "c"; "d"] = ["a"; "b"];;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⢇⣸ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶  ⠸ *)

let top3_sold (report: sales_report): string list =
    List.map (fun prod -> prod.name) (take 3 (List.sort (fun x y-> compare y.sales x.sales) report));;


top3_sold [] = [];;
top3_sold c0 = ["game Y"; "game X"; "book A"];;


(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⣏⡉ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠤⠜ *)


(* render one product alone as a string *)
let render_product (prod: sold_product): string =
    (String.make prod.sales '*') ^ "(" ^ prod.name ^ ")\n";;

let render_sales (report: sales_report): string =
    List.fold_right (^) (List.map render_product (List.sort (fun x y -> compare y.sales x.sales) report)) "";;

print_string (render_sales c0);;




(* ⣏⡉ ⢇⡸ ⣏⡉ ⣏⡱ ⡎⠑ ⡇ ⡎⠑ ⣏⡉   ⠊⡱   ⣎⡁ *)
(* ⠧⠤ ⠇⠸ ⠧⠤ ⠇⠱ ⠣⠔ ⠇ ⠣⠔ ⠧⠤   ⠮⠤ ⠶ ⠣⠜ *)


(* retirer le produit *prod* de la liste de produits *report* *)
let remove_product (report: sales_report) (prod: sold_product): sales_report =
    List.filter (fun p -> p != prod) report;;

(* Version récusive, parce que on a mal lu la consigne la première fois *)
let top_product_revenues_recursive (report: sales_report) =
    let rec aux (report: sales_report) (selected: sales_report) =
        if (total_revenue selected) > (8. /. 10.) *. (total_revenue report)
        then selected
        else let max_of_report = List.hd (List.sort (fun x y -> compare y.price x.price) report)
        in aux (remove_product report max_of_report) (max_of_report::selected)
    in aux report [];;

total_revenue (top_product_revenues_recursive c0);;
top_product_revenues_recursive c0;;
total_revenue c0;;



let top_product_revenues (report: sales_report) =
    let eighty_percent_total = 80. *. (total_revenue report) /. 100.
    in List.fold_right (fun prod selected -> if eighty_percent_total > total_revenue selected then prod :: selected else selected) (List.sort (fun x y -> compare y.price x.price) report) [];;

total_revenue (top_product_revenues c0);;
top_product_revenues c0;;
total_revenue c0;;
