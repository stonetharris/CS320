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

(* Problem 1:
   The factorial is a recursively defined mathematical function.
   For any integer n which is 0 <= n, the factorial of n is equal
   to n multiplied by the factorial of n - 1. In other words,
   the factorial of n is the product of the first n non-negative
   integers. The factorial of 0 is a special cas e which is defined
   to be exactly 1.

   Examples:
   fact 0 = 1
   fact 2 = 1 * 2
   fact 3 = 1 * 2 * 3
   fact 8 = 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8

   Task:
   Complete the implementation of the fact function so that it
   computes the factorial for input n.

   Notes:
   - The input n is guaranteed to be non-negative.
   - You do not need to worry about integer overflow for large inputs. *)

let rec fact (n : int) =
  if n = 0 then 1
  else n * (fact (n-1));;

(* Problem 2:
   The fibonacci sequence is a sequence of numbers that commonly
   occurs in nature. This sequence begins with two initial numbers,
   0 and 1. In order to generate a new number in this sequence, the
   previous 2 numbers ared added together. So the number immediately
   after 0 and 1 is 1 since 0 + 1 = 1. The next number in the sequence
   will be 2 since 1 + 1 = 2.

   Examples:
   0 1 1 2 3 5 8 13 21 34 55 ...

   Task:
   Suppose 0 is the 0th fibonacci number and 1 is the 1st fibonacci,
   write a function fibo that computes the nth fibonacci number.

   fibonacci: 0 1 1 2 3 5 8 13 21 34 55 ... ?
   positiion: 0 1 2 3 4 5 6 7  8  9  10 ... n

   Notes:
   - The input n is guaranteed to be non-negative.
   - You do not need to worry about integer overflow or computational efficiency. *)

let rec fib (n : int) =
   match n with
   | (0 | 1 | 2) -> 1
   | n when n > 0 -> (fib (n-2) + fib (n-1));;

(* Problem 3.
   Prime numbers are integers that cannot be evenly divided by any
   number other than 1 and itself.

   Examples:
   2   prime
   4   not prime
   5   prime
   19  prime
   21  not prime
   187 not prime

   Task:
   Write a function is_prime that returns true if its input is
   a prime number, otherwise return false.

   Notes:
   - 2 <= n is guaranteed.
   - You do not need to worry about computational efficiency.

   Hints:
   - A recursive helper function can be beneficial.
   - The remainder operator in OCaml is written as mod, for example: 7 mod 2. *)

let is_prime (n : int) : bool =
   let rec aux num n =
      if n = 2 then true
      else if n = num then true
      else if n mod num = 0 then false
      else aux (num + 1) n
   in
   aux 2 n

(* Problem 4.
   Task:
   Given an non-negative integer n, generate a string filled with all prime
   numbers less than or equal to n. Each prime number in the string should be
   separated by a single space. The format for your output strings should
   follow precisely the format of output results shown in the examples.

   Examples:
   all_primes 2 = "2"
   all_primes 3 = "2 3"
   all_primes 10 = "2 3 5 7"
   all_primes 30 = "2 3 5 7 11 13 17 19 23 29"

   Hints:
   - all_primes can be used to check for primality
   - ints can be converted into their string representaion using string_of_int
   - ^ is the string concat operator *)

let rec all_primes (n : int) : string =
   if n >= 2 then 
      if is_prime (n) = true then all_primes (n - 1) ^ " " ^ string_of_int (n)
      else all_primes (n - 1)
   else 
      "";;


(* Problem 5.
   Task:
   Given an integer n, convert it to its binary representation in string
   form. The leftmost binary digit is the most significant bit.

   Examples:
   bin_of_int 0  = "0"
   bin_of_int 1  = "1"
   bin_of_int 6  = "110"
   bin_of_int 8  = "1000"
   bin_of_int 10 = "1010"
   bin_of_int 12 = "1100"
   bin_of_int 16 = "10000"

   Notes:
   - No extra spaces are allowed.
   - No extra 0s are allowed.
   - Formats of output must follow the examples precisely. *)

let rec bin_of_int (n : int) : string = 
   if n <= 1 then string_of_int (n)
      else bin_of_int (n/2) ^ string_of_int (n mod 2) ;;
