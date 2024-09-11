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

let rec fold_left (f:('a -> 'b -> 'a))(acc:'a) (lst:'b list) : ('a) = match lst with 
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t    ;;


(*>* Trees *>*)
type 'a tree =
    NULL
  | Node of 'a * 'a tree * 'a tree ;;


(*>* Problem 1 *>*)
type 'a threetree ;;
  
(*>* Problem 2 *>*)
let rec insertlm (tr: 'a threetree) (data:'a) : ('a threetree) =
raise Unimplemented

(*>* Problem 3 *>*)
let rec isbst (tr:'a tree) : (bool) = 
raise Unimplemented

(*>* Problem 4 *>*)
let rec treemap  (tr:'a tree) (f:('a -> 'b)): ('b tree)=
raise Unimplemented

(*>* Problem 5 *>*)
let bexp (lst:int list) (m:int) =
raise Unimplemented

(*>* Problem 6 *>*)
let lastel (lst:int list) : (int) =
raise Unimplemented

(*>* Problem 7 *>*)
let blistb (lst:bool list): (int) = 
raise Unimplemented




