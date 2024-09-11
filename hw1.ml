(** CS 440 **)
(** Fall 2024 **)
(** Homework 1 **)

(* Your name *)
let my_name = "Dylan Figueredo"
                
exception Unimplemented

(* IMPORTANT: Do not remove any lines beginning *>*, like the following one.
 * These are used by our autograder and need to be there for you to get points. 
 *)


(*>* Problem 1 *>*)
let rec nthel (lst: 'a list) (n: int) : 'a = 
  match lst with 
  | [] -> -1
  | x :: t -> if n = 0 then x else nthel t (n - 1)
  
(*>* Problem 2 *>*)
let rec sliceone (lst: 'a list) (n: int) : 'a list =
  match lst with
  | [] -> []
  | x :: t -> if n > 0 then x :: sliceone t (n - 1) else []

(*>* Problem 3 *>*)let rec slicetwo (lst: 'a list) (n: int) : 'a list =
  match lst with
  | [] -> []
  | _ :: tl -> if n > 0 then slicetwo tl (n - 1) else lst

(*>* Problem 4 *>*)
let split (lst: 'a list) (n: int) : ('a list) * ('a list) =
  let rec helper (left, right, count) =
    match right with
    | [] -> (left, [])
    | hd :: tl ->
        if count > 0 then
          helper (left @ [hd], tl, count - 1)
        else
          (left, right)
  in
  helper ([], lst, n)


(*>* Problem 5 *>*)
let rec merge (lst1: int list) (lst2: int list) : int list =
  match lst1, lst2 with
  | [], lst -> lst
  | lst, [] -> lst
  | x :: t1, y :: t2 -> if x < y then x :: merge t1 lst2 else y :: merge lst1 t2


(*>* Problem 6 *>*)
let rec exp (n:int) (m:int) : int =
  if m = 0 then 1 else n * exp n (m-1);;

(*>* Problem 7 *>*)
let rec bexp (lst: int list) (n:int) : (int) =
  match lst with 
  | [] -> 0
  | x :: t -> exp n x + bexp t n  


(*>* Problem 8 *>*)
let rec funlist (lst: (int ->int) list) (n:int) : (int list) = 
  match lst with
  | [] -> []
  | func :: t -> func n :: funlist t n



