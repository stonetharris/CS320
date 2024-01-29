(* Honor code comes here:

   First Name: Stone
   Last Name: Harris
   BU ID: U41533031

   I pledge that this program represents my own program code and that I have
   coded on my own. I received help from no one in designing and debugging my
   program. I have read the course syllabus of CS 320 and have read the sections
   on Collaboration and Academic Misconduct. I also understand that I may be
   asked to meet the instructor or the TF for a follow up interview on Zoom. I
   may be asked to explain my solution in person and may also ask you to solve a
   related problem. *)

(*NOTE: There are no restrictions on what you can use*)


(*Writing a line to a file*)
let write_file_example (file_path: string) : unit =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "writing this line!" in
    close_out fp

type uletter = 
  Uletter of char

let uletter_of_char (c: char) : uletter option = 
  if 'A' <= c && c<= 'Z' then
    Some (Uletter c)
  else
    None

type lletter = 
  Lletter of char

let lletter_of_char (c : char) : lletter option = 
  if 'a' <= c && c<= 'z' then
    Some (Lletter c)
  else
    None

type letter = 
  Uletter of uletter
  | Lletter of lletter

let letter_of_char (c : char) : letter option =
  match lletter_of_char c with
  | Some lletter -> Some (Lletter lletter)
  | None -> match uletter_of_char c with
            | Some uletter -> Some (Uletter uletter)
            | None -> None

type digit =
  Digit of char

let digit_of_char (c : char) : digit option = 
  if '0' <= c && c <= '9' then
    Some (Digit c)
  else
    None

type char_after_first_in_name = 
  Letter of letter
  | Underscore of char
  | Digit of digit

let char_after_first_in_name_of_char (c : char) : char_after_first_in_name option =
  match letter_of_char c with 
  | Some letter -> Some(Letter letter)
  | None ->if '_' = c then Some (Underscore c)
          else
            match digit_of_char c with 
            | Some digit -> Some (Digit digit)
            | None -> None

type name =
  Name of (lletter * char_after_first_in_name list)

type const = 
  Str of string
  | Int of int
  | Name of name

let rec string_of_chars_after_first_in_name (rest : char_after_first_in_name list) : string =
  match rest with 
  | [] -> ""
  | Letter Uletter (Uletter uletter) :: tl -> (String.make 1 uletter) ^ string_of_chars_after_first_in_name tl
  | Letter Lletter (Lletter lletter) :: tl -> (String.make 1 lletter) ^ string_of_chars_after_first_in_name tl
  | Underscore underscore :: tl -> (String.make 1 underscore) ^ string_of_chars_after_first_in_name tl
  | Digit (Digit digit) :: tl -> (String.make 1 digit) ^ string_of_chars_after_first_in_name tl


let string_of_const (item : const) : string = 
  match item with
  | Str s -> "\"" ^ s ^ "\""
  | Int i -> string_of_int i
  | Name n -> match n with 
              | Name (Lletter lletter, rest) -> (String.make 1 lletter) ^ string_of_chars_after_first_in_name rest
              | _ -> failwith "TODO"

let write_stack_to_file (stack : const list) (output_path : string) : unit = 
  let rec helper (stack : const list) (output : out_channel) : unit = 
    (* if the stack is empty then stop, otherwise write the 
       top line and recur on the remaing lines *)
    match stack with
    | [] -> ()
    | item :: items -> 
      let () = Printf.fprintf output "%s\n" (string_of_const item) in
      helper items output
  in
  let fp = open_out output_path in 
  let () = helper stack fp in 
  close_out fp
    (*then call the helper and close channel when helper is done*)


(* let rec match_string (src : string t1) : prog = 
  match src with
  | ""::tl -> (Quit :: [])
  | "Pop"::tl -> Pop :: (match_string tl) *)

type command = 
  Quit
  | Push of const
  | Pop
  | Add
  | Sub
  | Mul
  | Div
  | Swap
  | Neg
  | Concat
  | And
  | Or
  | Not
  | Equal
  | Lte

let is_digit (c : char) : bool = ('0' <= c && c <= '9')

let rec are_digits (digits : string) : bool =
  if 0 = String.length digits then true
  else 
    let c = digits.[0] in
    let rest = (String.sub digits 1 ((String.length digits) - 1) ) in
    is_digit c && are_digits rest

let is_letter (c : char) : bool = 
  ('a' <= c && c <= 'z'
  ||
  'A' <= c && c <= 'Z'
  )

let rec are_letters (letters : string) : bool =
  if 0 = String.length letters then true
  else 
    let c = letters.[0] in
    let rest = (String.sub letters 1 ((String.length letters) - 1) ) in
      is_letter c && are_letters rest

let const_str_of_string (constant : string) : const option =
  let str_len = String.length constant in
  if 2 <= str_len then
    let first_char = constant.[0] in
    let last_char = constant.[str_len - 1] in
    let middle_chars = String.sub constant 1 (str_len - 2) in
    if (first_char = '"' && last_char = '"' && are_letters middle_chars) then 
      Some (Str middle_chars)
    else
      None
  else 
    None


let is_non_negative_int (constant : string) : bool =
  String.length constant > 0 && are_digits constant

let const_int_of_string (constant : string) : const option =
  let digits = 
    if constant.[0] = '-' && 0 < String.length constant then
      String.sub constant 1 (String.length constant - 1)
    else 
      constant 
  in 
  if is_non_negative_int digits then
    Some (Int (int_of_string constant))
  else
    None

let const_of_string (constant : string) : const = 
  match const_int_of_string constant with 
  | Some(i) -> i
  | None -> match const_str_of_string constant with 
            | Some(str) -> str
            | None -> failwith "unable to convert string to a constant"

let command_of_string (command: string) : command = 
  match String.split_on_char ' ' command with
  | ["Quit"] -> Quit
  | ["Push"; const] -> Push (const_of_string const)
  | ["Pop"] -> Pop
  | ["Add"] -> Add 
  | ["Sub"] -> Sub
  | ["Mul"] -> Mul
  | ["Div"] -> Div
  | ["Swap"] -> Swap
  | ["Neg"] -> Neg
  | ["Concat"] -> Concat
  | ["And"] -> And
  | ["Or"] -> Or
  | ["Not"] -> Not
  | ["Equal"] -> Equal
  | ["Lte"] -> Lte
  | _ -> failwith ("Error, invalid command: \"" ^ command ^ "\"")


let interpret1 (command : string) (stack : const list) : (const list * bool) = 
  match command_of_string command with 
  | Push const -> (const :: stack, true)
  | Pop -> (match stack with
            | [] -> failwith "Error"
            | _hd :: tl -> (tl, true))
  | Swap -> (match stack with 
            | h1 :: h2 :: tl -> ((h2 :: h1 :: tl), true)
            | _ -> failwith "Error")
  | Concat -> (match stack with 
              | (Str h1) :: (Str h2) :: tl -> ((Str (h1 ^ h2)) :: tl, true)
              | h1 :: h2 :: tl -> failwith "Error"
              | _ -> failwith "Error")
  | Add -> (match stack with
            | (Int h1) :: (Int h2) :: tl -> (Int ((h1 + h2)) :: tl, true)
            | _h1 :: _h2 :: _tl -> failwith "Error"
            | _ -> failwith "Error")
  | Sub -> (match stack with
            | (Int h1) :: (Int h2) :: tl -> (Int ((h1 - h2)) :: tl, true)
            | _h1 :: _h2 :: _tl -> failwith "Error"
            | _ -> failwith "Error")
  | Mul -> (match stack with
            | (Int h1) :: (Int h2) :: tl -> (Int ((h1 * h2)) :: tl, true)
            | _h1 :: _h2 :: _tl -> failwith "Error"
            | _ -> failwith "Error")
  | Div -> (match stack with
            | (Int h1) :: (Int 0) :: tl -> failwith "Div by zero"
            | (Int h1) :: (Int h2) :: tl -> (Int ((h1 / h2)) :: tl, true)
            | _h1 :: _h2 :: _tl -> failwith "Error"
            | _ -> failwith "Error")
  | Neg -> (match stack with
            | (Int h1) :: tl -> (Int (-h1) :: tl, true)
            | _h1 :: _h2 :: _tl -> failwith "Error"
            | _ -> failwith "Error")
  | And -> (match stack with 
            | Int 1 :: Int 1 :: tl -> (Int 1 :: tl, true)
            | Int 0 :: Int 1 :: tl -> (Int 0 :: tl, true)
            | Int 1 :: Int 0 :: tl -> (Int 0 :: tl, true)
            | Int 0 :: Int 0 :: tl -> (Int 0 :: tl, true)
            | _ -> failwith "Error, and on non booleans")
  | Or -> (match stack with 
          | Int 1 :: Int 1 :: tl -> (Int 1 :: tl, true)
          | Int 0 :: Int 1 :: tl -> (Int 1 :: tl, true)
          | Int 1 :: Int 0 :: tl -> (Int 1 :: tl, true)
          | Int 0 :: Int 0 :: tl -> (Int 0 :: tl, true)
          | _ -> failwith "Error, or on non booleans")
  | Not -> (match stack with 
            | Int 1 :: tl -> (Int 0 :: tl, true)
            | Int 0 :: tl -> (Int 0 :: tl, true)
            | _ -> failwith "Error, not on non booleans")
  | Quit -> (stack, false)
  | _ -> failwith "TODO: finish interpret1"


let rec interpret (commands : string list) (stack : const list) : const list = 
  (* if commands is empty  *)
  match commands with 
  | [] -> stack
  | command :: remaining_commands -> 
  match interpret1 command stack with
  | stack', true -> interpret remaining_commands stack'
  | stack', false -> stack'

  (* returned stack should be used to call i *)
  (* go thru all of the commands *)

let write_error_to_file (output_path : string) : unit = 
  let fp = open_out output_path in
  let () = Printf.fprintf fp "\"Error\"" in
    close_out fp

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
    

(*When it comes to parsing src, you should find it useful that fold_left can be
  defined and used with strings (like String.fold_left in new OCaml version).
  See String.get. These are suggestions though and you are welcome
  to use what you want :)  *)
let interpreter (src : string) (output_file_path: string): unit =
  try write_stack_to_file 
    (interpret (String.split_on_char '\n' src) []) output_file_path with 
    Failure _s -> write_error_to_file output_file_path

let interpret_file (filename : string) (output_path : string): unit = 
    interpreter (read_whole_file filename) output_path