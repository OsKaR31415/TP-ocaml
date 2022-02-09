type category = Book | Software | Game | Health | Fashion;;
type euro = float;;

type product = {
    id: int;
    name: string;
    categories: category list;
    price: euro;
};;

type catalog = product list;;

(* this sample has been taken from an alive cat, so it is really fresh *)
(*     |\__/⁰|   (`\  *)
(*   _.|o o '|_   ) ) *)
(* -(((---(((-------- *)
let cat_sample : catalog=[
    {id=34; name="book A"; price=11.9; categories=[Book]};
    {id=49; name="book B"; price=18.6; categories=[Book; Health]};
    {id=47; name="toothbrush R"; price=48.7; categories=[Health]};
    {id=18; name="software A"; price=9.99; categories=[Software]};
    {id=24; name="software B"; price=109.0; categories=[Software]};
    {id=33; name="game X"; price=60.9; categories=[Software; Game]};
    {id=190; name="game Y"; price=49.9; categories=[Software; Game]};
    {id=4; name="Learn Lisp"; price=63.5; categories=[Software; Book]};
    {id=9; name="Learn OCaml"; price=48.0; categories=[Software; Book]};
    {id=78; name="pants X"; price=109.0; categories=[Fashion]};
    {id=99; name="pull-over X"; price=29.0; categories=[Fashion]};
    {id=101; name="t-shirt Y"; price=12.0; categories=[Fashion]}
];;



let print_bool (boolean: bool) =
    if boolean then print_endline "true" else print_endline "false";;

(* ▗▌      ▜▘▛▀▖▛▀▘▙ ▌▀▛▘▜▘▛▀▘▜▘▞▀▖▙ ▌▀▛▘▞▀▖ ▛▀▘▀▛▘ ▛▀▖▛▀▖▜▘▌ ▌ *)
(*  ▌  ▄▄▖ ▐ ▌ ▌▙▄ ▌▌▌ ▌ ▐ ▙▄ ▐ ▙▄▌▌▌▌ ▌ ▚▄  ▙▄  ▌  ▙▄▘▙▄▘▐ ▝▞  *)
(*  ▌      ▐ ▌ ▌▌  ▌▝▌ ▌ ▐ ▌  ▐ ▌ ▌▌▝▌ ▌ ▖ ▌ ▌   ▌  ▌  ▌▚ ▐ ▞▝▖ *)
(* ▝▀      ▀▘▀▀ ▀▀▘▘ ▘ ▘ ▀▘▘  ▀▘▘ ▘▘ ▘ ▘ ▝▀  ▀▀▘ ▘  ▘  ▘ ▘▀▘▘ ▘ *)



(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ╺┓  ╺┓  *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸     ┃   ┃  *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ╺┻╸╹╺┻╸ *)

(* sometimes, products have good ids, sometimes not *)
let rec products_ids (catlg : catalog) : int list =
    match catlg with
    | [] -> []
    (* recurse *)
    | prod :: catlg_tail -> prod.id :: products_ids catlg_tail;;

(* tests *)
print_bool ((products_ids(cat_sample)) = [34; 49; 47; 18; 24; 33; 190; 4; 9; 78; 99; 101]);;



(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ╺┓  ┏━┓ *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸     ┃  ┏━┛ *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ╺┻╸╹┗━╸ *)

let rec cheaper_than (catlg: catalog) (price: euro) : catalog =
    match catlg with
    | [] -> []
    | fst_product :: catlg_tail -> if fst_product.price < price
    then fst_product :: (cheaper_than catlg_tail price)
    else (cheaper_than catlg_tail price);;

(* test *)
(cheaper_than cat_sample 20.);;



(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ╺┓  ┏━┓ *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸     ┃  ╺━┫ *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ╺┻╸╹┗━┛ *)

let rec sum_price (catlg: catalog): float =
    match catlg with
    | [] -> 0.
    | head :: catlg_tail -> head.price +. sum_price(catlg_tail);;

let mean_price(catlg : catalog) : float = (sum_price catlg) /. float_of_int(List.length catlg);;

(* tests *)
print_float (mean_price (cheaper_than cat_sample 20.));;
print_endline "";;  (* because print float doesn't put a carriage return *)



(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ╺┓  ╻ ╻ *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸     ┃  ┗━┫ *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ╺┻╸╹  ╹ *)

let rec most_expensive (catlg: catalog) : product =
    match catlg with
    (* the null product does not exist, so here is a one representing it *)
    | [] -> {id=0; name=""; categories=[]; price=0.}
    | [prod] -> prod
    | first :: second :: catlg -> if first.price >= second.price
    then most_expensive(first :: catlg)
    else most_expensive(second :: catlg);;

(* test *)
(most_expensive cat_sample);;



(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ┏━┓ ╺┓  *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸    ┏━┛  ┃  *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ┗━╸╹╺┻╸ *)

let rec has_category (catgry: category) (list_catgry: category list) : bool =
    match list_catgry with
    | [] -> false
    | [c] -> c == catgry
    | head :: tail -> if head == catgry then true
    else has_category catgry tail;;

(* tests *)
print_bool (has_category Book [Book;   Game]);;
print_bool (not (has_category Book [Health; Game]));;



(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ┏━┓ ┏━┓ *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸    ┏━┛ ┏━┛ *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ┗━╸╹┗━╸ *)

let rec in_category (prod : product) (catgry : category) =
    has_category catgry prod.categories;;

(* tests *)
print_bool (not(in_category {id=49; name="book B"; price=18.6; categories=[Book; Health]} Game));;
print_bool (in_category {id=49; name="book B"; price=18.6; categories=[Book; Health]} Book);;



(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ┏━┓ ┏━┓ *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸    ┏━┛ ╺━┫ *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ┗━╸╹┗━┛ *)

let rec category_product (catlg: catalog) (catgry : category) =
    match catlg with
    | [] -> []
    | prod :: catlg_tail -> if (in_category prod catgry)
    then prod :: (category_product catlg_tail catgry)
    else (category_product catlg_tail catgry);;

(* tests *)
(category_product cat_sample Book);;



(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ┏━┓ ╻ ╻ *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸    ┏━┛ ┗━┫ *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ┗━╸╹  ╹ *)

let rec dedup_category (categories: category list) =
    match categories with
    | [] -> []
    | catgry_hd :: catgry_tail -> 
            if (has_category catgry_hd catgry_tail)
            then (dedup_category catgry_tail)
            else catgry_hd :: (dedup_category catgry_tail);;

(*tests*)
print_bool ((dedup_category [Book; Book; Fashion; Book]) = [Fashion; Book]);;

(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ┏━┓ ┏━╸ *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸    ┏━┛ ┗━┓ *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ┗━╸╹┗━┛ *)

let rec join_categories (catlg: catalog) : category list =
    match catlg with
    | [] -> []
    | catlg_hd :: catlg_tail -> List.concat [catlg_hd.categories; join_categories(catlg_tail)];;

let all_categories (catlog : catalog) : category list =
    dedup_category(join_categories catlog);;

(* tests *)
print_bool ((all_categories cat_sample) = [Health; Game; Software; Book; Fashion]);;

(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ┏━┓ ┏━┓ *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸    ┏━┛ ┣━┓ *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ┗━╸╹┗━┛ *)


let rec products_by_category (catlg: catalog) (catgry: category) : (category*catalog) =
    match catlg with
    | [] -> (catgry, [])
    | prod :: catlg_tail ->
            (* if the product is in the category *)
            if (in_category prod catgry)
            (* the category isn't changed. prepend the product and recurse on
               the catalog tail *)
            then (catgry, prod :: snd (products_by_category catlg_tail catgry))
            (* else, the category isn't changed, recurse on the catalog tail
               without prepending the product *)
            else (catgry, snd (products_by_category catlg_tail catgry));;

(* tests *)
products_by_category cat_sample Game;;



(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ┏━┓ ┏━┓ *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸    ┏━┛   ┃ *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ┗━╸╹  ╹ *)

(* apply a function on each element of a list *)
let rec apply (f) (lst) =
    match lst with
    | [] -> []
    | first :: rest -> (f first) :: apply f rest;;

let rec product_groups (catlg: catalog) : (category*catalog) list =
    (apply (fun c -> products_by_category catlg c) (all_categories catlg));;


(* tests *)
product_groups cat_sample;;


