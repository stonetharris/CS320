let rec sumTo n = 
  if n = 0 then 0
  else n + sumTo (n - 1)

(* use pattern matching for sumTo *)
let rec sumTo n =
  match n with
  | 0 -> 0
  | n -> n + sumTo (n - 1)

(* power function 
  Examples: 
  power 2 0 = 1
  power 3 2 = 9
  power 2 5 = 32
  power 5 10 = 9765626 *)
let rec power a b =
  match b with
  | 0 -> 1
  | b -> power a (b - 1) * a

(* tail-recursive function *)
(* sum of the elements in the list 
  Examples:
  sumList [] = 0
  sumList [1] = 1
  sumList [1; 2] = 3
  sumList [1; 3; 5; 7] = 16 *)

let rec sumList ls =
  match ls with
    | [] -> 0
    | h :: t -> (sumList t) + h

let sumList2 ls =
  let rec helper ls =
    match ls with
    | [] -> 0
    | h :: t -> h + helper t
  in helper ls

let sumList3 ls =
  let rec helper ls acc =
    match ls with
    | [] -> acc
    | h :: t -> helper t (acc + h)
  in helper ls 0


(* sum of list1[i] - list2[i]
  Examples:
  sumDiff [][] = 0 - 0 = 0
  sumDiff [1][2] = 1 - 2 = 0
  sumDiff [1; 2] [-1; 2] = 1 - (-1) + 2 - 2 = 2
  sumDiff [1; 2][] = 1 - 0 + 2 - 0 = 3
  sumDiff [1][1; 2; 3; 4] = 
            1 - 1 + 0 - 2 + 0 - 3 + 0 - 4 = -9 *)

let sumDiff ls1 ls2 = 
  let rec helper ls1 ls2 acc =
    match (ls1, ls2) with
    | [],[] -> acc
    | [], head :: tail -> helper [] tail (acc - head)
    | head :: tail, [] -> helper [] tail (acc + head)
    | head :: tail, head2 :: tail2 -> helper tail tail2 (acc + head - head2)
  in helper ls1 ls2 0

let sumDiff2 ls1 ls2 =
  let sumLs1 = sumList ls1 in
  let sumLs2 = sumList ls2 in
  sumLs1 - sumLs2

(* Examples:
  string_of_intlist [] = "[]"
  string_of_intlist [1] = "[1]"
  string_of_intlist [1; 2] = "[1; 2]"
  string_of_intlist [1; 2; 3] = "[1; 2; 3]"
  string_of_intlist [1; 2; 3; 4] = "[1; 2; 3; 4]" *)

let string_of_intlist ls =
  let rec helper ls =
    match ls with
    | [] -> ""
    | [ x ] -> string_of_int x
    | head :: tail ->  string_of_int head ^ "; " ^ helper tail
  in "[" ^ helper ls ^ "]"


(* Tetranacci sequence
  Examples:
  t0 = 0
  t1 = 1
  t2 = 1
  t3 = 2
  t5 = t1 + t2 + t3 + t4
  ...
  tk = tk-1 + tk-2 + tk-3 + tk-4 (for any k > 3) *)

let tetranacci k =
  let rec helper k a b c d =
    match k with
    | 0 -> a
    | 1 -> b
    | 2 -> c
    | 3 -> d
    | k -> helper (k-1) b c d (a + b + c + d)
  in helper k 0 1 1 2


(* Examples:
  sqrt 100 = 10
  sqrt 81 = 9
  sqrt 144 = 12
  sqrt 15129 = 123 *)
let sqrt n =
  let rec bsearch a b =
    if b < a then
      a
    else
      let mid = (a + b) /2 in
        if mid * mid = n then
          mid
        else if mid * mid > n then
          bsearch a (mid - 1)
        else bsearch (mid + 1) b
  in bsearch 0 n


(* Examples:
  intlist_range 0 5 = [0; 1; 2; 3; 4]
  intlist_range 7 10 = [7; 8; 9]
  intlist_range 10 7 = []
  intlist_range (-3) 1 = [-3; -2; -1; 0] *)

let intlist_range x y =
  let rec helper a b acc =
    if a >= b then
      acc
    else helper a (b - 1) ((b - 1)::acc)
  in helper x y []


(* Examples:
  rev [] = []
  rev [1] = [1]
  rev [1; 2] = [2; 1]
  rev [1; 2; 3; 4] = [4; 3; 2; 1] *)

let rev ls =
  let rec helper ls acc =
    match ls with
    | head :: tail -> helper tail (head::acc)
    | [] -> acc
  in helper ls []


(* Examples:
  length [] = 0
  length [1] = 1
  length [1; 2] = 2
  length [1; 2; 3; 4] = 4 *)

let rec length ls =
  match ls with
  | [] -> 0
  | h :: t -> length t + 1


(* Examples:
  xs and ys have the same length
  xs = [x1; x2; x3; ... xk]
  ys = [y1; y2; y3; ... yk]
  dot_product xs ys = (x1 * y1) + (x2 * y2) + (x3 * y3) + ... + (xk * yk) *)

let rec dot_product xs ys =
    match (xs, ys) with
    | [],[] ->  0
    | h1::t1, h2::t2 -> dot_product t1 t2 + h1*h2


(* greatest common divisor
Examples:
  gcd 7 81 = 1
  gcd 9 81 = 9
  gcd 141 36 = 3
  gcd 84 144 = 12
  gcd 315 441 = 63 *)

let rec gcd x y =
  if x < y then
    gcd y x
  else 
    let r = x mod y in
    if r = 0 then
      y
    else gcd y r

