(** CS 440 **)
(** Fall 2024 **)
(** Homework 2 **)

(* Your name *)
let my_name = "Dylan Figueredo"
                
exception Unimplemented

(* IMPORTANT: Do not remove any lines beginning *>*, like the following one.
 * These are used by our autograder and need to be there for you to get points. 
 *)

(*>* HOF *>*)
let rec map (f:('a -> 'b)) (lst:'a list): ('b list ) = match lst with  
  | [] -> []
  | h::t -> f h :: map f t;;

let rec fold_right (f:('a -> 'b -> 'b)) (lst:'a list) (acc:'b) : 'b = match lst with 
  | [] -> acc 
  | h::t -> f h (fold_right f t acc);;

let rec fold_left (f:('a -> 'b -> 'a)) (acc:'a) (lst:'b list) : ('a) = match lst with 
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t;;


(*>* Trees *>*)
type 'a tree =
  | NULL
  | Node of 'a * 'a tree * 'a tree;;

(*>* Problem 1 *>*)
type 'a threetree = 
| NULL3
| Node3 of 'a * 'a threetree * 'a threetree * 'a threetree;;
  
(*>* Problem 2 *>*)
let rec insertlm (tr: 'a threetree) (data:'a) : ('a threetree) = match tr with
| NULL3 -> Node3 (data, NULL3, NULL3, NULL3)
| Node3 (n, l, m, r) -> Node3 (n, insertlm l data, m, r);;

(*>* Problem 3 *>*)
let rec isbst (tr:'a tree) : (bool) = match tr with
| NULL -> true
| Node (n, l, r) -> if isbst r then true else false;; 

(*>* Problem 4 *>*)
let rec treemap (tr:'a tree) (f:('a -> 'b)) : ('b tree) = match tr with
| NULL -> NULL
| Node (n, l, r) -> Node (f n, treemap l f, treemap r f);;

(*>* Problem 5 *>*)
let bexp (lst:int list) (m:int) = 
  let rec exp x b = if b = 0 then 1 else x * exp x (b-1)
  in fold_left (fun acc e -> acc + exp m e) 0 lst;;

(*>* Problem 6 *>*)
let lastel (lst:int list) : (int) =
raise Unimplemented

(*>* Problem 7 *>*)
let blistb (lst:bool list): (int) = 
raise Unimplemented




