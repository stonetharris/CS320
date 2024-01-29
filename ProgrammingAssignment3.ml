(* Honor code comes here:

   First Name: Douglas
   Last Name: Moy
   BU ID: U71000048

   I pledge that this program represents my own
   program code and that I have coded on my own. I received
   help from no one in designing and debugging my program.
   I have read the course syllabus of CS 320 and have read the sections on Collaboration
   and Academic Misconduct. I also understand that I may be asked to meet the instructor
   or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
   may also ask you to solve a related problem. *)

(* IMPORTANT
   Problems that do not satisfy requirements will have points deducted from
   your Gradescope score during manual grading.

   Requirements for the assignemnt.
   * You may NOT add or remove the 'rec' keyword.
   * Your helper functions may NOT use the 'rec' keyword.
   * You may not use ANY library functions.
   * You may not use list operators such as '@'.
   * The standard library will be disabled during grading, code
     dependent on library code will fail to compile. *)

(* Problem 1.
   Implement the filter higher-order function. *)
let rec filter (f : 'a -> bool) (ls : 'a list) =
  match ls with
  | [] -> []
  | h :: t -> if f h then h :: filter f t else filter f t

(* Problem 2.
   Implement the map higher-order function. *)
let rec map (f : 'a -> 'b) (ls : 'a list) : 'b list =
  match ls with x :: ls -> f x :: map f ls | _ -> []

(* Problem 3.
   Implement the fold_left higher-order function. *)
let rec fold_left (f : 'a -> 'b -> 'a) (acc : 'a) (ls : 'b list) : 'a =
  match ls with [] -> acc | h :: t -> fold_left f (f acc h) t

(* Problem 4.
   Implement the fold_right higher order function. *)
let rec fold_right (f : 'a -> 'b -> 'b) (ls : 'a list) (acc : 'b) : 'b =
  match ls with [] -> acc | h :: t -> f h (fold_right f t acc)

(* Problem 5.
   Implement append in using fold_right. You may not use the 'rec' keyword. *)
let append (ls1 : 'a list) (ls2 : 'a list) : 'a list =
  fold_right (fun l1 l2 -> l1 :: l2) ls1 ls2

(* Problem 6.
   Implement a function nth_opt that finds the nth element in a list if it exists.

   Examples:
   nth_opt [1; 2; 3] 0 = Some 1
   nth_opt [1; 2; 3] 1 = Some 2
   nth_opt [1; 2; 3] 3 = None
   nth_opt [] 3 = None
*)

let length (ls : 'a list) : int = fold_right (fun x y -> y + 1) ls 0

let lastElement (ls : 'a list) : 'a option =
  match ls with [] -> None | _ -> Some (fold_left (fun x y -> y) 0 ls)

let nth_opt (ls : 'a list) (n : int) : 'a option =
  match ls with
  | [] -> None
  | _ ->
      if length ls < n + 1 then None
      else
        lastElement
          (fold_left
             (fun x y -> if length x <= n then append x [ y ] else x)
             [] ls)

(* Problem 7.
   Given an int list, add_all_opt attempts to add all elements in the
   list. If the list is empty, return None, otherwise return the sum tagged
   by Some.

   Examples:
   add_all_opt [1; 2] = Some 3
   add_all_opt [3; 4; 5] = Some 12
   add_all_opt [] = None *)
let add_all_opt ls =
  match ls with
  | [] -> None
  | h :: t -> Some (fold_right (fun x y -> x + y) ls 0)

(* Problem 8.
   Given an int list, add_all_even_opt attempts to add all even elements
   in the list. If the list is empty, return None, otherwise return the sum
   tagged by Some.

   Examples:
   add_all_even_opt [1; 2] = Some 2
   add_all_even_opt [3; 4; 5; 6] = Some 10
   add_all_even_opt [] = None *)
let add_all_even_opt (ls : int list) : int option =
  match ls with
  | [] -> None
  | _ -> (
      match filter (fun x -> if x mod 2 = 0 then true else false) ls with
      | [] -> None
      | h :: t ->
          Some (fold_right (fun x y -> if x mod 2 = 0 then x + y else y) ls 0))

(* Problem 9.
   Given an int list list, sum up all int elements. The int list list
   is guaranteed to be non-empty. The nested int lists may be of
   different lengths.

   Examples:
   sum_matrix [[1; 2]; [3; 4]] = 10
   sum_matrix [[1]; [3; 4]] = 8
   sum_matrix [[1; 2]; [3]] = 6 *)

let add_all (ls : int list) : int = fold_right (fun x y -> x + y) ls 0

let sum_matrix (lss : int list list) : int =
  match lss with
  | [] -> 0
  | h :: t -> fold_left (fun x y -> add_all y + x) 0 lss

(* Problem 10.
   Implement 'find_key' from pa2. *)
let find_key (dict : (string * int) list) (key : string) : int option =
  match dict with
  | [] -> None
  | _ -> (
      match filter (fun (a, b) -> a = key) dict with
      | [] -> None
      | (a, b) :: t -> Some b)

(* Problem 11.
   Implement 'to_freq' from pa2. *)

let numberOfString (ls : string list) (x : string) : int =
  fold_left (fun a b -> if b = x then a + 1 else a) 0 ls

let uniqueString (ls : string list) : string list =
  fold_left
    (fun x y -> if numberOfString x y = 0 then append [ y ] x else x)
    [] ls

let to_freq (ls : string list) : (string * int) list =
  map (fun z -> (z, numberOfString ls z)) (uniqueString ls)

(* Problem 12.
   Implement 'concat' from pa2. *)
let concat (lss : 'a list list) : 'a list =
  fold_left (fun x y -> append x y) [] lss

(* Problem 13.
   When given a list ls of type 'a list, and a function f of
   type 'a -> 'b list, flatMap maps f across all elements of
   ls uniformly and flattens the result into a 'a list.

   Example:
   flatMap [1; 2] (fun x -> [x; x+1]) = [1; 2; 2; 3]
   flatMap [4; 6; 8] factor = [2; 2; 2; 3; 2; 2; 2] *)
let flatMap (ls : 'a list) (f : 'a -> 'b list) : 'b list =
  fold_right (fun x y -> append (f x) y) ls []

(* Problem 14.
   Given 3 lists ls1, ls2 and ls3, triples enumerates a list
   containing triples of all their combinations (similar to theory homework 2).
   You do not need to worry about the order of the triples in the output.

   Example:
   triples [1; 2] [3] [4; 5] = [(1, 3, 4); (1, 3, 5); (2, 3, 4); (2, 3, 5) ]
   triples [1; 2] [3; 4] [5; 6] =
     [(1, 3, 5); (1, 3, 6); (1, 4, 5); (1, 4, 6); (2, 3, 5); (2, 3, 6); (2, 4, 5); (2, 4, 6)] *)
let triples (ls1 : 'a list) (ls2 : 'b list) (ls3 : 'c list) :
    ('a * 'b * 'c) list =
  concat
    (concat
       (map (fun a -> map (fun b -> map (fun c -> (a, b, c)) ls3) ls2) ls1))

(* Problem 15.
    Given an element x and a list ls, insert x into all possible positions
    in ls. Collect all results as a nested list output.

    Examples:
    insert 4 [1; 2; 3] = [[4; 1; 2; 3]; [1; 4; 2; 3]; [1; 2; 4; 3]; [1; 2; 3; 4]]
    insert 1 [4; 3; 2] = [[1; 4; 3; 2]; [4; 1; 3; 2]; [4; 3; 1; 2]; [4; 3; 2; 1]]
    insert 1 [] = [[1]] *)
let insertN (ls : 'a list) (n : int) (input : 'a) : 'a list =
  fold_left
    (fun x y ->
      if length ls + 1 == n then append ls [ input ]
      else if length x + 1 == n then append (append x [ input ]) [ y ]
      else if length x == n then append (append x [ input ]) [ y ]
      else append x [ y ])
    [] ls

let insert (x : 'a) (ls : 'a list) : 'a list list =
  match ls with
  | [] -> [ [ x ] ]
  | _ ->
      append
        (fold_left (fun a b -> insertN ls (length ls - length a) x :: a) [] ls)
        [ insertN ls (length ls + 1) x ]

(* Problem 16.
   Given a list ls, generate all possible permutations of ls.
   Collect these permutations a nested list output. The order
   of generated permutations does not matter.

   Examples:
   perm [] = [[]]
   perm [1] = [[1]]
   perm [1; 2] = [[1; 2]; [2; 1]]
   perm [1; 2; 3] = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]] *)

let listWithoutX (ls : int list) (x : int) : int list =
  fold_left (fun a b -> if b == x then a else append a [ b ]) [] ls

let xInList (ls : int list list) (x : int list) : int list list =
  fold_left (fun a b -> if b == x then append [ b ] a else a) [] ls

let reverseList (ls : int list) : int list = fold_left (fun a b -> b :: a) [] ls
let lastElementInt (ls : int list) : int = fold_left (fun x y -> y) 0 ls

let firstElementInt (ls : int list) : int list =
  fold_right (fun x y -> [ x ]) ls []

let tailFunction (ls : int list) : int list = match ls with h :: t -> t

let perm (ls : int list) : int list list =
  match ls with
  | [] -> [ [] ]
  | _ -> (
      match length ls with
      | 1 -> [ ls ]
      | _ ->
          fold_left
            (fun x y -> concat (map (fun z -> insert y z) x))
            [ firstElementInt ls ]
            (tailFunction ls))
