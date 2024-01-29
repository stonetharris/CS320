(* INTERPRETER PART 2 SOLUTION *)

let and_then (f : 'a -> ('b, 'e) result) (res : ('a, 'e) result): ('b, 'e) result =
  Result.bind res f

let all_ok (ls : ('a, 'e) result list): ('a list, 'e) result =
  let combine_result a b =
    a |> and_then @@ fun a ->
    b |> and_then @@ fun b ->
    Ok(b :: a)
  in
  List.fold_left combine_result (Ok([])) ls |> Result.map List.rev

(*PROGRAM DEFINITIONS*)

type var = Var of string

type const =
  | Int of int
  | String of string
  | Name of var

type com =
  | Quit
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
  | Local of var
  | Global of var
  | BeginEnd of prog
  (* | Fun of (var * var * prog) list *)
  | Fun of var * var * prog
  | IfThenElse of prog * prog
  | CaseLeftRight of prog * prog
  | InjL
  | InjR
  | Tuple of const
  | Get of const
  | Call

and prog = com list

and env = (var * value) list

and value =
  | VInt of int
  | VString of string
  | VLeft of value
  | VRight of value
  | VTuple of value list
  | VClo of (var * var * prog) * env * (var * var * prog) list
  (* | VClo of (var * var * prog) list * env *)

type stack = value list
type log = string list


type state = stack * log * (* local *) env * (* global *) env

let int_of_bool (b : bool): int =
  if b then 1 else 0

let is_bool (n : int): bool =
  n = 0 || n = 1

let lookup_bind (x : var) (envs : env * env): value option =
  let rec lookup e =
    match e with
    | [] -> None
    | (y, v)::rst -> if y = x
                     then Some(v)
                     else lookup rst
  in
  let (local, global) = envs in
  match lookup local with
  | None -> lookup global
  | Some(v) -> Some(v)

let add_bind (x : var) (v : value) (e : env): env  =
  let updated, e =
      List.fold_left
        (fun (updated, acc) (y, v') ->
          if y = x
          then true, (y, v)::acc
          else updated, (y, v')::acc)
        (false, [])
        e
  in
  let e = List.rev e in
  if updated then e else (x, v)::e

let com_arity (c : com): int option =
  match c with
  | Quit | Push(_) | BeginEnd(_) | Fun(_) -> Some 0
  | Pop  | Neg | Not | Local(_) | Global(_) | InjL | InjR | CaseLeftRight(_) | IfThenElse(_) | Get(_) -> Some 1
  | Add  | Sub | Mul | Div | Swap | Concat | And | Or | Equal | Lte | Call -> Some 2
  | Tuple(_) -> None

(*PRINTERS*)
let string_of_const c =
  match c with
  | Int(i)       -> string_of_int i
  | String(s)    -> "\"" ^ s ^ "\""
  | Name(Var(v)) -> v

let rec string_of_value v =
  match v with
  | VInt(i)    -> string_of_int i
  | VString(s) -> "\"" ^ s ^ "\""
  | VLeft(v)   -> "Left " ^ (string_of_value v)
  | VRight(v)  -> "Right " ^ (string_of_value v)
  | VTuple(ls)  -> "(" ^ (string_of_values ls) ^ ")"
  (* | VClo([], _env) -> failwith "tried to convert empty closure to string" *)
  | VClo((Var(fname), Var(parameter), _prog), _env, _mutually_recs) -> "Clo (" ^ fname ^ " " ^ parameter ^ ")"
  (* | VClo((Var(fname), Var(parameter), _prog) :: _mutually_rec_funs, _env) -> "Clo (" ^ fname ^ " " ^ parameter ^ ")" *)
  (* currently ignoring following mutually rec funs^ *)

and string_of_values (vs : value list) : string = 
  match vs with
  | [] -> ""
  | x :: [] -> string_of_value x
  | x1 :: x2 :: rest -> string_of_value x1 ^ ", " ^ string_of_values (x2 :: rest)

let rec string_of_com (c : com) : string =
  match c with
  | Quit    -> "Quit"
  | Push(c) -> Printf.sprintf "Push %s" (string_of_const c)
  | Tuple(c) -> Printf.sprintf "Tuple %s" (string_of_const c)
  | Get(c) -> Printf.sprintf "Get %s" (string_of_const c)
  | Pop     -> "Pop"
  | Add     -> "Add"
  | Sub     -> "Sub"
  | Mul     -> "Mul"
  | Div     -> "Div"
  | Swap    -> "Swap"
  | Neg     -> "Neg"
  | Concat  -> "Concat"
  | And     -> "And"
  | Or      -> "Or"
  | Not     -> "Not"
  | Equal   -> "Equal"
  | Lte     -> "Lte"
  | InjL    -> "InjL"
  | InjR    -> "InjR"
  | Call    -> "Call"
  | Local (Var(v))   -> Printf.sprintf "Local %s" v
  | Global(Var(v))   -> Printf.sprintf "Local %s" v
  | Fun(Var(fname0), Var(parameter0), blk) ->
      "Fun " ^ fname0 ^ " " ^ parameter0 ^ " " ^ "\n"
    ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev blk)^ "\nEnd\n"
  (* | Fun[(Var(fname0), Var(parameter0), p0) ;
        (Var(fname1), Var(parameter1), p1)] ->
     "Fun " ^ fname0 ^ " " ^ parameter0 ^ " " ^ "\n"
   ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev p0)^ "\nEnd\n" *)
  (* | Fun(_) -> "Finish to_string for Fun() in string_of_com" *)
  | BeginEnd(p) ->
     "Begin\n"
   ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev p)
   ^ "\nEnd\n"
  | IfThenElse(t, e) ->
     "IfThen\n"
   ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev t)
   ^ "\nElse\n"
   ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev e)
   ^ "\nEnd\n"
  | CaseLeftRight(l, r) ->
  "CaseLeft\n"
 ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev l)
 ^ "\nRight\n"
 ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev r)
 ^ "\nEnd\n"

let rec string_of_list (p : 'a -> string) (ls : 'a list): string =
  match ls with
  | []       -> "[]"
  | fst::rst -> p fst ^ "::" ^ string_of_list p rst

(*TOKENIZING*)
let char_list_of_string (s : string): char list =
  List.init (String.length s) (String.get s)

type token =
  | NEWLINE
  | STRING of string
  | NUMBER of int
  | SYMBOL of string

let string_of_token tok =
  match tok with
  | NEWLINE   -> "\\n"
  | STRING(s) -> "\"" ^ s ^ "\""
  | NUMBER(n) -> string_of_int n
  | SYMBOL(s) -> s

let is_space (c : char): bool =
  c = ' ' || c = '\t'

let is_digit (c : char): bool =
  match c with | '0' .. '9' -> true | _ -> false

let is_alpha (c : char): bool =
  match c with | 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let int_of_char (c : char): int =
  int_of_string @@ Char.escaped c

type lexer_err =
  | UnclosedString of (* line number *) int
  | InvalidChar    of (* line number *) int * char
  | UnknownChar    of (* line number *) int * char

let string_of_lexer_err e =
  match e with
  | UnclosedString(i) -> Printf.sprintf "Unclosed string at line %i" i
  | InvalidChar(i, c) -> Printf.sprintf "Invalid char '%c' at line %i" c i
  | UnknownChar(i, c) -> Printf.sprintf "Unknown char '%c' at line %i" c i


let tokenize_string line_num (ls : char list): (string * char list, lexer_err) result =
  let rec helper ls acc =
    match ls with
    | [] -> Error(UnclosedString(line_num))
    | ch::rst ->
       if ch = '\"' then
         Ok(acc, rst)
       else if is_alpha ch then
         helper rst (acc ^ Char.escaped ch)
       else
         Error(InvalidChar(line_num, ch))
  in helper ls ""

let tokenize_number line_num (ls : char list): (int * char list, lexer_err) result =
  let rec helper ls acc =
    match ls with
    | [] -> Ok(acc, [])
    | ch::rst ->
       if ch = '\n' || is_space ch then
         Ok(acc, ls)
       else if is_digit ch then
         helper rst @@ acc * 10 + int_of_char ch
       else Error(InvalidChar(line_num, ch))
  in helper ls 0

let tokenize_symbol line_num (ls : char list): (string * char list, lexer_err) result =
  let rec helper ls acc =
    match ls with
    | [] -> Ok(acc, [])
    | ch::rst ->
       if ch = '\n' || is_space ch then
         Ok(acc, ls)
       else if is_alpha ch || ch = '_' || is_digit ch then
         helper rst (acc ^ Char.escaped ch)
       else
         Error(InvalidChar(line_num, ch))
  in helper ls ""

let tokenize_source (src : string): (token list, lexer_err) result =
  let rec helper line_num ls acc =
    match ls with
    | [] -> Ok(List.rev @@ acc)
    | ch::rst ->
       match ch with
       | '\n' -> helper (line_num + 1) rst (NEWLINE :: acc)
       | '\"' -> tokenize_string line_num rst |> and_then @@ fun (s, rst) ->
                 helper line_num rst (STRING(s) :: acc)
       | '-'  -> tokenize_number line_num rst |> and_then @@ fun (n, rst) ->
                 helper line_num rst (NUMBER(-1 * n) :: acc)
       | ch when is_digit ch
              -> tokenize_number line_num (ch::rst) |> and_then @@ fun (n, rst) ->
                 helper line_num rst (NUMBER(n) :: acc)
       | ch when is_alpha ch
              -> tokenize_symbol line_num ls |> and_then @@ fun (s, rst) ->
                 helper line_num rst (SYMBOL(s) :: acc)
       | ch when is_space ch -> helper line_num rst acc
       | ch -> Error(UnknownChar(line_num, ch))
  in helper 1 (char_list_of_string src) []


(*PARSING*)
type parse_err =
  | MissingArguments of (* line number *) int
  | InvalidCom       of (* line number *) int * token
  | ExpectedConst    of (* line number *) int * token
  | ExpectedInt      of (* line number *) int * token
  | ExpectedVar      of (* line number *) int * token
  | InvalidVar       of (* line number *) int * token
  | UnexpectedEOF    of (* line number *) int
  | MissingNewline   of (* line number *) int

let string_of_parse_err e =
  match e with
  | MissingArguments(i) ->
     Printf.sprintf "Missing arguments to command at line %i" i
  | InvalidCom(i, tok) ->
     Printf.sprintf "Invalid command at line %i, got: \"%s\"" i (string_of_token tok)
  | ExpectedConst(i, tok) ->
     Printf.sprintf "Expected constant at line %i, got: \"%s\"" i (string_of_token tok)
  | ExpectedInt(i, tok) ->
     Printf.sprintf "Expected int at line %i, got: \"%s\"" i (string_of_token tok)
  | ExpectedVar(i, tok) ->
     Printf.sprintf "Expected a variable name at line %i, got: \"%s\"" i (string_of_token tok)
  | InvalidVar(i, tok) ->
     Printf.sprintf "Invalid variable name at line %i, got: \"%s\"" i (string_of_token tok)
  | UnexpectedEOF(i) ->
     Printf.sprintf "Ran out of tokens at line %i" i
  | MissingNewline(i) ->
     Printf.sprintf "Missing newline on line %i" i

(* DEPENDS ON: Symbol tokens being valid variables with arbitrary case starting char *)
let make_var line_num (s : string): (var, parse_err) result =
  if String.length s <> 0 then
     match String.get s 0 with
     | 'a' .. 'z' -> Ok(Var(s))
     | _ -> Error(InvalidVar(line_num, SYMBOL(s)))
  else Error(InvalidVar(line_num, SYMBOL(s)))

(* Consume a newline from the token list, if it is required and not present, error. *)
let consume_newline (line_num : int) (required : bool) (ls : token list)
                  : (int * token list, parse_err) result =
  match ls with
  | [] -> Ok(line_num, [])
  | NEWLINE::tl -> Ok((line_num + 1, tl))
  |      hd::tl -> if required then
                     Error(MissingNewline(line_num))
                   else
                     Ok(line_num, hd::tl)

(* See: PA4 parse_sexpr *)
let rec parse_com line_num ls : (com * int * token list, parse_err) result =
  let parse_int line_num tok =
    match tok with
    | NUMBER(n) -> Ok(Int(n))
    | tok       -> Error(ExpectedInt(line_num, tok))
  in
  let parse_const line_num tok =
    match tok with
    | NUMBER(n) -> Ok(Int(n))
    | STRING(s) -> Ok(String(s))
    | SYMBOL(s) -> make_var line_num s |> Result.map (fun x -> Name(x))
    | tok       -> Error(ExpectedConst(line_num, tok))
  in
  (* let parse_var line_num toks : (var * token list, parse_err) result =
    match toks with
    | SYMBOL(s) :: rst -> (
        match make_var line_num s with
        | Ok(v) -> Ok(v, rst)
        | Error(p) -> Error(p)
    )
    | tok :: _rst -> Error(ExpectedVar(line_num, tok))
    | [] -> Error(UnexpectedEOF(line_num))
  in *)
  match ls with
  | SYMBOL("Push")  ::fst::rst ->
     parse_const line_num fst |> and_then @@ fun x ->
     Ok(Push(x), line_num, rst)
  | SYMBOL("Tuple") ::fst::rst ->
    parse_int line_num fst |> and_then @@ fun i ->
    Ok(Tuple(i), line_num, rst)
  | SYMBOL("Get") ::fst::rst ->
    parse_int line_num fst |> and_then @@ fun i ->
    Ok(Get(i), line_num, rst)
  | SYMBOL("Quit")  ::rst -> Ok(Quit, line_num, rst)
  | SYMBOL("Pop")   ::rst -> Ok(Pop, line_num, rst)
  | SYMBOL("Add")   ::rst -> Ok(Add, line_num, rst)
  | SYMBOL("Sub")   ::rst -> Ok(Sub, line_num, rst)
  | SYMBOL("Mul")   ::rst -> Ok(Mul, line_num, rst)
  | SYMBOL("Div")   ::rst -> Ok(Div, line_num, rst)
  | SYMBOL("Swap")  ::rst -> Ok(Swap, line_num, rst)
  | SYMBOL("Neg")   ::rst -> Ok(Neg, line_num, rst)
  | SYMBOL("Concat")::rst -> Ok(Concat, line_num, rst)
  | SYMBOL("And")   ::rst -> Ok(And, line_num, rst)
  | SYMBOL("Or")    ::rst -> Ok(Or, line_num, rst)
  | SYMBOL("Not")   ::rst -> Ok(Not, line_num, rst)
  | SYMBOL("Equal") ::rst -> Ok(Equal, line_num, rst)
  | SYMBOL("Lte")   ::rst -> Ok(Lte, line_num, rst)
  | SYMBOL("InjL")  ::rst -> Ok(InjL, line_num, rst)
  | SYMBOL("InjR")  ::rst -> Ok(InjR, line_num, rst)

  (* | SYMBOL("Fun") ::SYMBOL(fname)::SYMBOL(param)::rst ->
    make_var line_num fname |> and_then @@ fun fname0 -> 
    make_var line_num param |> and_then @@ fun param0 -> 
    consume_newline line_num false rst 
    |> and_then @@ fun (line_num, rst) ->
    parse_com_list line_num (SYMBOL("Mut")) rst 
    |> and_then @@ fun (blk0, line_num, rst) -> 
    parse_var line_num rst |> and_then @@ fun (fname1, rst) ->
    parse_var line_num rst |> and_then @@ fun (param1, rst) ->
    consume_newline line_num false rst |> and_then @@ fun (line_num, rst) -> 
    parse_com_list line_num (SYMBOL("End")) rst
    |> and_then @@ fun (blk1, line_num, rst) ->
    Ok(Fun [fname0, param0, blk0 ; fname1, param1, blk1], line_num, rst)
    
  | SYMBOL("Fun") ::c::rst -> Error(ExpectedVar(line_num, c)) *)

  | SYMBOL("Fun") ::SYMBOL(fname)::SYMBOL(param)::rst ->
     make_var line_num fname |> and_then @@ fun fname -> 
     make_var line_num param |> and_then @@ fun param -> 
     consume_newline line_num false rst 
     |> and_then @@ fun (line_num, rst) ->
     parse_com_list line_num (SYMBOL("End")) rst 
     |> and_then @@ fun (blk, line_num, rst) ->
     Ok(Fun (fname, param, blk), line_num, rst)

  | SYMBOL("Fun") ::c::rst -> Error(ExpectedVar(line_num, c))

  | SYMBOL("Local") ::SYMBOL(s)::rst ->
    make_var line_num s |> Result.map @@ fun x -> Local(x), line_num, rst
  | SYMBOL("Local") ::c::rst -> Error(ExpectedVar(line_num, c))

  | SYMBOL("Global")::SYMBOL(s)::rst ->
     make_var line_num s |> Result.map @@ fun x -> Global(x), line_num, rst
  | SYMBOL("Global")::c::rst -> Error(ExpectedVar(line_num, c))

  | SYMBOL("Begin") ::rst ->
     consume_newline line_num false rst
     |> and_then @@ fun (line_num, rst) ->
     parse_com_list line_num (SYMBOL("End")) rst
     |> and_then @@ fun (blk, line_num, rst) ->
     Ok(BeginEnd(blk), line_num, rst)

  | SYMBOL("IfThen")::rst ->
     consume_newline line_num false rst
       |> and_then @@ fun (line_num, rst) ->
     parse_com_list line_num (SYMBOL("Else")) rst
       |> and_then @@ fun (then_blk, line_num, rst) ->
     consume_newline line_num false rst
       |> and_then @@ fun (line_num, rst) ->
     parse_com_list line_num (SYMBOL("End")) rst
       |> and_then @@ fun (else_blk, line_num, rst) ->
     Ok(IfThenElse(then_blk, else_blk), line_num, rst)

  | SYMBOL("CaseLeft")::rst ->
    consume_newline line_num false rst
      |> and_then @@ fun (line_num, rst) ->
    parse_com_list line_num (SYMBOL("Right")) rst
      |> and_then @@ fun (left_blk, line_num, rst) ->
    consume_newline line_num false rst
      |> and_then @@ fun (line_num, rst) ->
    parse_com_list line_num (SYMBOL("End")) rst
      |> and_then @@ fun (right_blk, line_num, rst) ->
    Ok(CaseLeftRight(left_blk, right_blk), line_num, rst)

  | tok::_ -> Error(InvalidCom(line_num, tok))
  | [] -> Error(UnexpectedEOF(line_num))


(* See: PA4 parse_sexpr_list *)
and parse_com_list (line_num : int) (terminator : token) (ls : token list)
                 : (prog * int * token list, parse_err) result =
    match ls with
    | fst::rst when fst = terminator -> Ok([], line_num, rst)
    | _  -> parse_com line_num ls
              |> and_then @@ fun (fst, line_num, ls') ->
            consume_newline line_num true ls'
              |> and_then @@ fun (line_num, ls'') ->
            parse_com_list line_num terminator ls''
              |> and_then @@ fun (rst, line_num, ls''') ->
            Ok((fst::rst, line_num, ls'''))

(* See: PA4 parse *)
let parse_program (src : token list): (prog, parse_err) result  =
  let rec parse_all line_num ls acc  =
    match ls with
    | [] -> Ok(List.rev acc)
    | _  -> parse_com line_num ls
              |> and_then @@ fun (c, line_num, ls) ->
            consume_newline line_num true ls
              |>  and_then @@ fun (line_num, ls) ->
            parse_all line_num ls (c::acc)
  in
  parse_all 1 src []

(*EVALUATION*)

type eval_err =
  | InvalidVarArgArity of com * (* # got *) int * (* # should get *) int
  | InvalidArity of com * (* # got *) int
  | InvalidIndex of com * (* # got *) int (* # of items in tuple *) * int
  | WrongType    of com * (* args got *) value list
  | DivByZero    of int * int
  | UnboundVar   of var
  | NoValue      of com

let string_of_eval_err e =
  match e with
  | InvalidVarArgArity(c, i, s) ->
    Printf.sprintf "Got %i arguments to %s, expected %i" i (string_of_com c) s
  | InvalidIndex(c, i, s) ->
    Printf.sprintf "Got %i items in tuple for %s, expected %i or more" i (string_of_com c) s
  | InvalidArity(c, i) -> (match com_arity c with 
                          | Some arity -> 
                          Printf.sprintf 
                            "Got %i arguments to %s, expected %i" 
                            i 
                            (string_of_com c) 
                            arity

                          | None -> Printf.sprintf 
                            "Got %i arguments to %s, which has unkown arity" 
                            i 
                            (string_of_com c) 
                            )
  | WrongType(_, ls) ->
     Printf.sprintf "Got arguments of incorrect type: " ^ string_of_list string_of_value ls
  | DivByZero(m, n) ->
     Printf.sprintf "Got arguments to div: %i / %i" m n
  | UnboundVar(Var(s)) ->
     Printf.sprintf "Unbound variable %s" s
  | NoValue(c) ->
     Printf.sprintf "Expected return value from command %s" (string_of_com c)

let with_stack (f : stack -> (stack, 'e) result) (s, l, loc, glob : state): (state, 'e) result =
  f s |> Result.map (fun s -> s, l, loc, glob)

(*PROGRAM METHODS*)
let quit (stk, log, loc, glob : state): state =
  stk
  , (List.fold_right
       (fun elem acc -> string_of_value elem :: acc)
       stk
       [])
    @ log
  , loc
  , glob


let rec firstk k xs = match xs with
  | [] -> failwith "firstk"
  | x::xs -> if k=1 then [x] else x::firstk (k-1) xs;;

let rec dropk k xs = 
  match xs with 
  | [] -> []
  | h :: tl -> 
    if k = 0 then
      xs
    else 
      dropk (k - 1) tl

let rec nth (lst : value list) (n : int) : value option = 
  match lst, n with
  | [], _ -> None
  | _, n when n < 0 -> None
  | x :: _, 0 -> Some x
  | x :: xs, n -> nth xs (n-1)

let push (stk, log, loc, glob : state) (c : const): (state, eval_err) result =
  match c with
  | Name(v)   -> lookup_bind v (loc, glob)
              |> Option.to_result ~none:(UnboundVar(v))
              |> Result.map (fun x -> x::stk, log, loc, glob)
  | String(s) -> Ok(VString(s) :: stk, log, loc, glob)
  | Int(n)    -> Ok(VInt(n) :: stk, log, loc, glob)

let tuple (stk, log, loc, glob : state) (c : const): (state, eval_err) result =
  match c with
  | Int(n) -> let stklen = List.length stk in 
              let topk = firstk n stk in 
              let bottom = dropk n stk in 
              if n <= stklen then 
                Ok(VTuple(List.rev topk) :: bottom, log, loc, glob)
              else
                Error(InvalidVarArgArity(Tuple(c), stklen, n))
  | _ -> failwith "evaling tuple, parsed something other than int"

let get (stk, log, loc, glob : state) (c : const): (state, eval_err) result =
  match c with
  | Int(n) -> (match stk with 
              | [] -> Error(InvalidArity(Get(c), 0))
              | VTuple(t) :: rest -> (
              (* check to see if there are enough things in tuple to get nth *)
                match nth t n with 
                | None     -> Error(InvalidIndex(Get(c), List.length t, n))
                | Some (v) -> Ok(v :: stk, log, loc, glob)
              )
              (* grab nth value of the tuple and add it to top of stack *)
              | v :: rest -> Error(WrongType(Get(c), v :: rest)))
  | _      -> Error(WrongType(Get(c), []))

let pop : state -> (state, eval_err) result =
  with_stack @@
    function
    | []    -> Error(InvalidArity(Pop, 0))
    | _::tl -> Ok(tl)

let add : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x + y) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Add, List.length stk))
    | x :: y :: _               -> Error(WrongType(Add, [x; y]))

let sub : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x - y) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Sub, List.length stk))
    | x :: y :: _               -> Error(WrongType(Sub, [x; y]))

let mul : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x * y) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Mul, List.length stk))
    | x :: y :: _               -> Error(WrongType(Mul, [x; y]))

let div : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(0) :: _   -> Error(DivByZero(x, 0))
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x / y) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Div, List.length stk))
    | x :: y :: _               -> Error(WrongType(Div, [x; y]))

let swap : state -> (state, eval_err) result =
  with_stack @@
    function
    | x :: y :: rst       -> Ok(y :: x :: rst)
    | _ :: [] | [] as stk -> Error(InvalidArity(Swap, List.length stk))

let neg : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: rst -> Ok(VInt(-1 * x) :: rst)
    | []             -> Error(InvalidArity(Neg, 0))
    | x :: _         -> Error(WrongType(Neg, [x]))

let concat : state -> (state, eval_err) result =
  with_stack @@
    function
    | VString(x) :: VString(y) :: rst -> Ok(VString(x ^ y) :: rst)
    | _ :: [] | [] as stk             -> Error(InvalidArity(Concat, List.length stk))
    | x :: y :: _                     -> Error(WrongType(Concat, [x; y]))

let and_ : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst when is_bool x && is_bool y
                          -> Ok(VInt(int_of_bool (x = y)) :: rst)
    | _ :: [] | [] as stk -> Error(InvalidArity(And, List.length stk))
    | x :: y :: _         -> Error(WrongType(And, [x; y]))

let or_ : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst when is_bool x && is_bool y
                          -> Ok(VInt(int_of_bool (x = 1 || y = 1)) :: rst)
    | _ :: [] | [] as stk -> Error(InvalidArity(Or, List.length stk))
    | x :: y :: _         -> Error(WrongType(Or, [x; y]))

let not_ : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: rst when is_bool x
             -> Ok(VInt(abs(x - 1)) :: rst)
    | []     -> Error(InvalidArity(Not, 0))
    | x :: _ -> Error(WrongType(Not, [x]))

let equal : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(int_of_bool (x = y)) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Equal, List.length stk))
    | x :: y :: _               -> Error(WrongType(Equal, [x; y]))

let lte : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(int_of_bool (x <= y)) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Lte, List.length stk))
    | x :: y :: _               -> Error(WrongType(Lte, [x; y]))

let injl : state -> (state, eval_err) result =
  with_stack @@
    function
    | v :: rst -> Ok(VLeft v :: rst)
    | [] as stk       -> Error(InvalidArity(InjL, List.length stk))

let injr : state -> (state, eval_err) result =
  with_stack @@
    function
    | v :: rst -> Ok(VRight v :: rst)
    | [] as stk       -> Error(InvalidArity(InjR, List.length stk))

let local (s, l, loc, glob : state) (x : var) : (state, eval_err) result =
  match s with
  | v::rst -> Ok((rst, l, add_bind x v loc, glob))
  | []     -> Error(InvalidArity(Local(x), 0))

let global (s, l, loc, glob : state) (x : var) : (state, eval_err) result =
  match s with
  | v::rst -> Ok((rst, l, loc, add_bind x v glob))
  | []     -> Error(InvalidArity(Global(x), 0))

let rec zfun (s, l, loc, glob : state) (fname : var) (param : var) (blk : prog) (*(funs : var * var * prog list)*) : (state, eval_err) result =
  Ok(s,
  l,
  (fname, 
  VClo((fname, param, blk), 
    List.append loc glob,
    [(fname, param, blk)])) :: loc,
  glob)
  (* match funs with
  | (fname0, fname0, blk0) :: [] -> 
    Ok(s, l, add_bind fname (VClo(fname0, fname0, blk0, List.append loc glob)) loc, glob)
  | (fname0, param0, blk0) :: (fname1, param1, blk1) :: [] -> 
    Ok(s,
      l,
      add bind VClo([fname0, param0, blk0 ; fname1, param1, blk1] List.append loc glob), glob)
  Ok(s,
    l,
    add_bind fname (VClo(fname, parameter, blk, List.append loc glob)) loc,
    glob) *)

let rec add_mut_recursions (original_recursions : (var * var * prog) list) (mutual_recursions : (var * var * prog) list) (vclo_env : env) (final_env : env) : env =
  match mutual_recursions with
  | [] -> final_env
  | (fname, param, blk) :: tl -> 
    let new_vclo = VClo((fname, param, blk), vclo_env, original_recursions) in 
    let new_env = add_bind fname new_vclo final_env in 
    add_mut_recursions original_recursions tl vclo_env new_env

let rec call (stk, l, loc, glob : state) : (state, eval_err) result =
  match stk with
  | VClo((fname, param, blk), vclo_env, mut_recs) :: arg_val :: rst -> 
    let stack = [] in
    let locals = add_bind param arg_val vclo_env in
    let locals = add_mut_recursions mut_recs mut_recs vclo_env locals in
    eval blk (stack, l, locals, glob) |> and_then @@ 
    (* change when Return is handled corrcetly, check for an actual return value
       rather than assuming that the top of the stack is a return value *)
    fun (quitting, (returned_stk, l, locals', glob')) -> (
    match returned_stk with
    | [] -> Error(NoValue(Call))
    | return_val :: _ -> 
      Ok((return_val :: rst, l, loc, glob')))
  | []     -> Error(InvalidArity(Call, 0))
  | x :: _y :: _ -> Error(WrongType(Call, [x]))
  | x :: _      -> Error(InvalidArity(Call, 1))

and begin_end (s, l, loc, glob : state) (blk : prog): (bool * state, eval_err) result =
  eval blk ([], l, loc, glob) |> and_then @@ fun (quitting, (s', l, _, glob)) ->
  match s' with
  | v::rst -> Ok((quitting, (v::s, l, loc, glob)))
  | []     -> Error(NoValue(BeginEnd(blk)))

and ifthen_else (s, l, loc, glob : state) (then_blk : prog) (else_blk : prog): (bool * state, eval_err) result =
  match s with
  | VInt(v)::rst when v = 1
    -> eval then_blk (rst, l, loc, glob)
  | VInt(v)::rst when v = 0
    -> eval else_blk (rst, l, loc, glob)
  | []     -> Error(InvalidArity(IfThenElse(then_blk, else_blk), 0))
  | x::rst -> Error(WrongType(IfThenElse(then_blk, else_blk), [x]))

and case_left_right (stack, log, loc, glob : state) (left_blk : prog) (right_blk : prog): (bool * state, eval_err) result =
  match stack with
  | VLeft(v)::rst
    -> eval left_blk (v :: rst, log, loc, glob)
  | VRight(v)::rst
    -> eval right_blk (v :: rst, log, loc, glob)
  | []     -> Error(InvalidArity(CaseLeftRight(left_blk, right_blk), 0))
  | x::rst -> Error(WrongType(CaseLeftRight(left_blk, right_blk), [x]))


and eval (prog : prog) (st : state) : (bool * state, eval_err) result =
  match prog with
  | []                -> Ok(false, st)
  | Quit      :: _    -> Ok(true, quit st)
  | Push(c)   :: prog -> push   st c |> and_then (eval prog)
  | Tuple(c)  :: prog -> tuple  st c |> and_then (eval prog)
  | Get(c)    :: prog -> get    st c |> and_then (eval prog)
  | Pop       :: prog -> pop    st   |> and_then (eval prog)
  | Add       :: prog -> add    st   |> and_then (eval prog)
  | Sub       :: prog -> sub    st   |> and_then (eval prog)
  | Mul       :: prog -> mul    st   |> and_then (eval prog)
  | Div       :: prog -> div    st   |> and_then (eval prog)
  | Swap      :: prog -> swap   st   |> and_then (eval prog)
  | Neg       :: prog -> neg    st   |> and_then (eval prog)
  | And       :: prog -> and_   st   |> and_then (eval prog)
  | Or        :: prog -> or_    st   |> and_then (eval prog)
  | Not       :: prog -> not_   st   |> and_then (eval prog)
  | Lte       :: prog -> lte    st   |> and_then (eval prog)
  | InjL      :: prog -> injl   st   |> and_then (eval prog)
  | InjR      :: prog -> injr   st   |> and_then (eval prog)
  | Concat    :: prog -> concat st   |> and_then (eval prog)
  | Equal     :: prog -> equal  st   |> and_then (eval prog)
  | Local(x)  :: prog -> local  st x |> and_then (eval prog)
  | Global(x) :: prog -> global st x |> and_then (eval prog)
  | Call      :: prog -> call   st   |> and_then (eval prog)

  | Fun(fname, param, blk) :: prog -> zfun st fname param blk |> and_then (eval prog)
  (* | Fun _ :: prog -> failwith "TODO: eval Funs with 1 or 3+ mutual recursive defs" *)
  | BeginEnd(p) :: prog -> begin_end st p
                    |> and_then @@ fun (quitting, st) ->
                    if not quitting then eval prog st else Ok(quitting, st)
  | IfThenElse(t, e) :: prog -> ifthen_else st t e
                    |> and_then @@ fun (quitting, st) ->
                    if not quitting then eval prog st else Ok(quitting, st)
  | CaseLeftRight(l, r) :: prog -> case_left_right st l r
  |> and_then @@ fun (quitting, st) ->
  if not quitting then eval prog st else Ok(quitting, st)


let write_file_with_log (file_path: string) (log: log) : unit =
  let fp = open_out file_path in
    let _ =
      List.fold_left (
        fun items_left elem ->
          match items_left with
          | 1 -> Printf.fprintf fp "%s" elem; items_left - 1
          | _ -> Printf.fprintf fp "%s\n" elem; items_left - 1
      ) (List.length log) log
    in close_out fp

type interp_err =
  | LexerErr of lexer_err
  | ParseErr of parse_err
  | EvalErr  of eval_err

let lexer_err e = LexerErr(e)
let parse_err e = ParseErr(e)
let eval_err  e = EvalErr(e)

let string_of_interp_err e =
  match e with
  | LexerErr(e) -> string_of_lexer_err e
  | ParseErr(e) -> string_of_parse_err e
  | EvalErr(e)  -> string_of_eval_err  e

let run src =
  tokenize_source src        |> Result.map_error lexer_err |> and_then @@ fun tokens ->
  parse_program tokens       |> Result.map_error parse_err |> and_then @@ fun prog ->
  eval prog ([], [], [], []) |> Result.map_error eval_err

let interpreter (src : string) (output_file_path: string): unit =
  (match run src with
   | Ok(_, (_, log, _, _)) -> log
   | Error(e)      -> print_endline (string_of_interp_err e); ["\"Error\""])
  |> write_file_with_log output_file_path
