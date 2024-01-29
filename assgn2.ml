(* Problem 1. *)
(* Determine the length of a list.
   Examples:
   length [] = 0
   length [1; 2; 3] = 3
*)
let rec length (l : 'a list): int =
  match l with
  | [] -> 0
  | _::rest -> 1 + length rest

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
  let len = length m in
  let rec loop (m : matrix): bool =
    match m with
    | [] -> true
    | c :: r ->
       let len_c = length c in
       len_c = len && loop r
  in
  loop m

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
  match (xs, ys) with
  | ([], []) -> Some([])
  | (x::xs, y::ys) ->
    and_then (add_lists xs ys) (fun r -> Some ((x +. y) :: r))
  | (_, _) -> None

(* Problem 5. *)
(* Add the elements of two matrices together,
   if they are different dimensions return None.

   Examples:
   add_matrices [[1.; 2.]; [3.; 4.]] [[0.; 0.5]; [1.4; 4.7]]
   = Some([[1.; 2.5]; [4.4; 8.7]])
*)
let rec add_matrices (m1 : matrix) (m2 : matrix): matrix option =
  match (m1, m2) with
  | ([], []) -> Some([])
  | (c1::r1, c2::r2) ->
     and_then (add_lists c1 c2)
       (fun c' ->
     and_then (add_matrices r1 r2)
       (fun r' -> Some (c' :: r')))
  | (_, _) -> None

(* Problem 6. *)
(* Scale each element of the list by the given constant.
   Examples:
   scale_list 3. [1.; 2.; 4.] = [3.; 6.; 12.]
*)
let rec scale_list (s : float) (l : float list): float list =
  match l with
  | [] -> []
  | x::rest -> (s *. x) :: scale_list s rest

(* Problem 7. *)
(* Scale each element of the matrix by the given constant.
   Examples:
   scale_matrix 3. [[1.; 2.]; [3.; 4.]] = [[3.; 6.]; [9.; 12.]]
*)
let rec scale_matrix (s : float) (m : matrix): matrix =
  match m with
  | [] -> []
  | x::rest -> (scale_list s x) :: scale_matrix s rest

(* Problem 8. *)
(* Convert the matrix into a list by flattening it.
   Examples:
   into_list [[1.]] = [1.]
   into_list [[1.; 2.]; [3.; 4.]] = [1.; 2.; 3.; 4.]
*)
let rec into_list (m : matrix): float list =
  let rec append l1 l2 =
    match l1 with
    | [] -> l2
    | x::rest -> x :: append rest l2
  in
  match m with
  | [] -> []
  | x::rest -> append x (into_list rest)

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
  let rec insert ls lss =
    match (ls, lss) with
    | x :: ls, ls' :: lss -> (x :: ls') :: insert ls lss
    | x :: ls, [] -> [ x ] :: insert ls []
    | _ -> []
  in
  let rec loop lss =
    match lss with
    | ls :: lss -> insert ls (loop lss)
    | [] -> []
  in
  loop lss

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
  let rec cofactor_list (sign : bool) (l : float list): float list =
    match l with
    | [] -> []
    | x::rest ->
       let rest = cofactor_list (not sign) rest in
       let x = if sign then x else -1. *. x in
       x :: rest
  in
  let rec loop (sign : bool) (m : matrix): matrix =
    match m with
    | [] -> []
    | x::rest ->
       let rest = loop (not sign) rest in
       let x = cofactor_list sign x in
       x :: rest
  in
  loop true m

