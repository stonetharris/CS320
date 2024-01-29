(* Honor code comes here:

   First Name: Stone
   Last Name: Harris
   BU ID: U41533031

   I pledge that this program represents my own program code and
   that I have coded on my own. I received help from no one in
   designing and debugging my program.  I have read the course
   syllabus of CS 320 and have read the sections on Collaboration
   and Academic Misconduct. I also understand that I may be asked
   to meet the instructor or the TF for a follow up interview on
   Zoom. I may be asked to explain my solution in person and may
   also ask you to solve a related problem. *)

(* Problem 1. *)
(* Determine the length of a list.
   Examples:
   length [] = 0
   length [1; 2; 3] = 3
*)
let rec length (l : 'a list): int =
  let rec aux sum l = match l with 
  | [] -> sum 
  | h :: t -> aux (sum + 1) t in
  aux 0 l

(*
   Using 'type' we may define aliases for types. In this case, anywhere we write
   'matrix' in our types OCaml will see 'float list list' instead.

   A float is a whole number with a decimal value as well. In OCaml, the
   operators to work with them have a trailing '.' e.g.:
   3. +. 4.2 = 7.2
   2. *. 1.4 = 2.8
*)
type matrix = float list list

(* Problem 2. *)
(* Determine if a matrix is square (n by n, for any n >= 0).
   Examples:
   is_square [[1.; 2.]] = false
   is_square [[1.; 2.]; [3.; 4.]] = true
*)

let rec is_square (m : matrix): bool =
  let height = (length m) in
    let rec all_rows_have h m =
        match m with
        | [] -> true
        | [r] -> length r = height
        | r :: rest -> length r = height && all_rows_have height rest
    in
      all_rows_have height m;;
  
(* Problem 3. *)
(* With an option type, we care about the value (possibly) stored inside. We
   will often write code that looks like the following:
   match my_option with
   | None -> None
   | Some(x) -> do calculation on x...

  To make avoid having to write this over and over, we can write a simple
  function which works with a function as an argument to make it easier! Write
  that function.

  Examples:
  let x = Some(3) in
    and_then x (fun y -> Some(y + 1))
  = Some(4)

  let x = None in
    and_then x (fun y -> Some(y + 1))
  = None
*)
let and_then (o : 'a option) (f : 'a -> 'b option) : 'b option =
  match o with
  | None -> None
  | Some(x) -> f x

(* Problem 4. *)
(* Add the elements of two lists together,
   if they are different lengths return None.

   Examples:
   add_lists [1.; 2.] [3.; 4.] = Some([4.; 6.])
   add_lists [] [1.2] = None
*)
let rec add_lists (xs : float list) (ys : float list): float list option =
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> Some([])
    | [], _ ->  None
    | _, [] -> None
    | x :: xs, y :: ys -> match aux xs ys with
      | None -> None
      | Some(l) ->  Some ((x +. y) :: l)
  in
  aux xs ys

(* Problem 5. *)
(* Add the elements of two matrices together,
   if they are different dimensions return None.

   Examples:
   add_matrices [[1.; 2.]; [3.; 4.]] [[0.; 0.5]; [1.4; 4.7]]
   = Some([[1.; 2.5]; [4.4; 8.7]])
*)

let rec add_matrices (m1 : matrix) (m2 : matrix): matrix option =

  let rec addrow (r1 : float list option) (r2 : float list option): float list option =
    match r1, r2 with
    | Some([]), Some([]) -> Some([])
    | Some([]), _ -> None
    | _, Some([]) -> None
    | Some(x :: rest), Some(x2 :: rest2) -> (match (addrow (Some(rest)) (Some(rest2))) with
                                            | None -> None
                                            | Some(sum_of_rests) -> Some( (x +. x2) :: sum_of_rests))
    | _,_ -> None
  in

  let rec addrows (rs1: matrix option) (rs2 : matrix option): matrix option =
    match rs1, rs2  with
    | Some([]), Some([]) -> Some([])
    | Some([]), _  -> None
    | _, Some([]) ->  None
    | Some(r1 :: rest1), Some(r2 :: rest2) ->  (match (addrow (Some(r1)) (Some(r2))) with
                                               | None -> None
                                               | Some(rsum) -> match (addrows (Some rest1) (Some rest2)) with
                                                               |  None -> None
                                                               |  Some(restsum) -> Some(rsum :: restsum))
    | _, _ -> None
  in
  addrows (Some m1) (Some m2) ;;


 (* Problem 6. *)
(* Scale each element of the list by the given constant.
   Examples:
   scale_list 3. [1.; 2.; 4.] = [3.; 6.; 12.]
*)
let rec scale_list (s : float) (l : float list): float list =
  let rec scale_list_with s l = 
    match l with 
    | [] -> []
    | x :: rest -> (s *. x) :: scale_list_with s rest
    in
    scale_list_with s l;;


(* Problem 7. *)
(* Scale each element of the matrix by the given constant.
   Examples:
   scale_matrix 3. [[1.; 2.]; [3.; 4.]] = [[3.; 6.]; [9.; 12.]]
*)

let rec scale_matrix (s : float) (m : matrix): matrix =
  let rec scale_matrix_with s m = 
    match m with 
    | [] -> []
    | r :: remrows -> (scale_list s r) :: scale_matrix_with s remrows
    in
    scale_matrix_with s m;;

(* Problem 8. *)
(* Convert the matrix into a list by flattening it.
   Examples:
   into_list [[1.]] = [1.]
   into_list [[1.; 2.]; [3.; 4.]] = [1.; 2.; 3.; 4.]
*)
let rec into_list (m : matrix): float list =
  failwith "Unimplemented"

(* Problem 9. *)
(* Transpose the matrix.

   Given a matrix of dimensions M x N, the transpose is a matrix with dimensions
   N x M produced by swapping columns and rows.

   For a 4x3 matrix:

   0  1  2  3
   4  5  6  7
   8  9  10 11
   ==>
   0 4 8
   1 5 9
   2 6 10
   3 7 11

   Examples:
   transpose [[1.; 2.]; [3.; 4.]] = [[1.; 3.]; [2.; 4.]]

   transpose
    [[0.; 4.; 8.]; [1.; 5.; 9.]; [2.; 6.; 10.;] [3.; 7.; 11.]]
   =
    [[0.; 1.; 2.; 3.]; [4.; 5.; 6.; 7.]; [8.; 9.; 10.; 11.]]

   Notes:
   * You may assume all nested int lists are of the same length (aka matrix is well-formed).
*)
let transpose (lss : matrix) : matrix =
  failwith "Unimplemented"
  (* three functions needed: one to get get first value of each row and one to store the remaining, third to construct the final product*)
  (* let rec get_first m = match m with
    | None -> None
    | [] -> []
    |  *)

(* Problem 10. *)
(* Generate the cofactor of the matrix.

   Given a matrix, invert the sign of its elements in an alternating
   ('checkerboard') fashion. This is used in the process of inverting matrices.
   Specifically, for a 4x3 matrix, we would want to apply the follow change
   (where '-' means to invert the sign):

    +  -  +  -
    -  +  -  +
    +  -  +  -

    0 -1 -2  3
   -4  5  6 -7
   -8  9 10 11
   ==>
    0  1 -2 -3
    4  5 -6 -7
   -8 -9 10 -11

  Examples:
  cofactor [[1.; 1.; 1.]]     = [[1.; -1.; 1.]]
  cofactor [[1.]; [1.]; [1.]] = [[1.]; [-1.]; [1.]]

  cofactor
    [[0.; -4.; -8.]; [-1.; 5.; 9.]; [-2.; 6.; 10.]; [3.; -7.; 11.]]
  =
    [[0.; 4.; -8.]; [1.; 5.; -9.]; [-2.; -6.; 10.]; [-3.; -7.; -11.]]

  cofactor [[2.; -2.; 0.]; [2.; 3.; -10.]; [2.; 3.; 0.]]
    = [[2.; 2.; 0.]; [-2.; 3.; 10.]; [2.; -3.; 0.]]
*)
let rec cofactor (m : matrix) : matrix =
  failwith "Unimplemented"
