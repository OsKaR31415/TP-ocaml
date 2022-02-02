type category = Book | Software | Game | Health | Fashion;;
type euro = float;;

type product = {
    id: int;
    name: string;
    categories: category list;
    price: euro;
};;

type catalog = product list;;

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

(* ▗▌      ▜▘▛▀▖▛▀▘▙ ▌▀▛▘▜▘▛▀▘▜▘▞▀▖▙ ▌▀▛▘▞▀▖ ▛▀▘▀▛▘ ▛▀▖▛▀▖▜▘▌ ▌ *)
(*  ▌  ▄▄▖ ▐ ▌ ▌▙▄ ▌▌▌ ▌ ▐ ▙▄ ▐ ▙▄▌▌▌▌ ▌ ▚▄  ▙▄  ▌  ▙▄▘▙▄▘▐ ▝▞  *)
(*  ▌      ▐ ▌ ▌▌  ▌▝▌ ▌ ▐ ▌  ▐ ▌ ▌▌▝▌ ▌ ▖ ▌ ▌   ▌  ▌  ▌▚ ▐ ▞▝▖ *)
(* ▝▀      ▀▘▀▀ ▀▀▘▘ ▘ ▘ ▀▘▘  ▀▘▘ ▘▘ ▘ ▘ ▝▀  ▀▀▘ ▘  ▘  ▘ ▘▀▘▘ ▘ *)

(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ╺┓  ╺┓  *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸     ┃   ┃  *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ╺┻╸╹╺┻╸ *)

let rec products_ids (catlg : catalog) : int list =
    match catlg with
    | [] -> []
    | product :: catlg_tail -> product.id :: products_ids catlg_tail;;

(* tests *)
(products_ids(cat_sample)) == [34; 49; 47; 18; 24; 33; 190; 4; 9; 78; 99; 101];;

(* ┏━╸╻ ╻┏━╸┏━┓┏━╸╻┏━╸┏━╸   ╺┓  ┏━┓ *)
(* ┣╸ ┏╋┛┣╸ ┣┳┛┃  ┃┃  ┣╸     ┃  ┏━┛ *)
(* ┗━╸╹ ╹┗━╸╹┗╸┗━╸╹┗━╸┗━╸   ╺┻╸╹┗━╸ *)

let rec cheaper_than (catlg: catalog) (price: euro) : catalog =
    match catlg with
    | [] -> []
    | fst_product :: catlg_tail -> if fst_product.price < price
    then fst_product :: (cheaper_than catlg_tail price)
    else (cheaper_than catlg_tail price);;

(cheaper_than cat_sample 20.);;



