(*>* HOF - trees*>*)
type 'a tree = NULL | Node of 'a * 'a tree * 'a tree;;

let test = Node (1, Node (2, NULL, NULL), Node (3, NULL, NULL));;

(*>* sum of tree nodes *>*)
let rec sumtree t = match t with 
  NULL -> 0
  | Node (data, left, right) -> data + (sumtree left) + (sumtree right);;

(*>* total number of nodes *>*)
let rec countnodes t = match t with 
  NULL -> 0
  | Node (data, left, right) -> countnodes left + countnodes right +1;;

(*>* height of a tree *>*)
let rec htree t = match t with
  NULL -> 0 | Node (data, left, right) -> 1+ (if (htree left> htree right) then (htree left) else  (htree right));;
   
   
(*>* mirroring a tree  - with hof *>*)
let rec mirror t = match t with
  NULL -> NULL
  | Node(data, left, right) -> Node(data, right, left);;

(*>* tree fold *>*)
let rec fold_tree f t init = match t with 
  NULL -> init
  | Node( data, left, right) -> f data (fold_tree f left init) (fold_tree f right init);;

(*>* sum of tree nodes  - with hof *>*)
let sumtree_f t = fold_tree (fun x y z -> x+y+z) t 0;;
;;

(*>* total number of nodes  - with hof *>*)
let countnodes_f t = fold_tree (fun x y z  -> 1+y+z) t 0;;
;;

(*>* height of a tree -with hof *>*)

(*>* mirroring a tree  - with hof *>*)
let mirror_fold t = fold_tree (fun x y z -> Node(x,z,y)) t NULL;;

(*>* HOF - lists*>*)
let rec map (f:('a -> 'b)) (lst:'a list): ('b list ) = match lst with  
  | [] -> []
  | h::t -> f h :: map f t;;

let rec fold_right (f:('a -> 'b -> 'b)) (lst:'a list) (acc:'b) : 'b = match lst with 
  | [] -> acc 
  | h::t -> f h (fold_right f t acc);;

let rec fold_left (f:('a -> 'b -> 'a))(acc:'a) (lst:'b list) : ('a) = match lst with 
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t    ;;

(*>* what do these functions do? *>*)
let mystery1 l = fold_right (fun x r -> x::r) l [];;
let mystery2 l = fold_left (fun r x -> x::r) [] l;;
let mystery3 l = fold_left (fun x y -> x+y) 0 l;;
let mystery4 l = fold_left (fun r x -> r + 1) 0 l;;


