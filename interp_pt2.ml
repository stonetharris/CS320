(* INTERPRETER PART 1 SOLUTION *)
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
type name = 
  NName of string

type const =
  | Int of int
  | String of string
  | Name of string

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
  | Local of const
  | Global of const
  | Begin of prog

and prog = com list

and value =
  | VInt of int
  | VString of string
  | VName of string

let value_of_const (c : const) =
  match c with
  | Int(i) -> VInt(i)
  | String(s) -> VString(s)
  | Name(s) -> VName(s)

type binding = (name * value)
type env = binding list

let rec lookup_in_env (e : env) (n : name) : value option = 
  match e with
  | (binding_name, binding_value) :: bindings -> 
    if binding_name = n then
      Some(binding_value)
    else
      lookup_in_env bindings n
  | [] -> None

let rec lookup_in_envs (es: env list) (n : name) : value option =
  match es with
  | [] -> None
  | e :: es -> match lookup_in_env e n with 
              | Some(v) -> Some(v)
              | None -> lookup_in_envs es n

type stack = value list
type log = string list

type state = stack * env list * env * log

let com_arity (c : com): int =
  match c with
  | Quit | Push(_) | Begin(_) -> 0
  | Pop  | Neg | Not | Local(_) | Global(_) -> 1
  | Add  | Sub | Mul | Div | Swap | Concat | And | Or | Equal | Lte -> 2

(*PRINTERS*)
let string_of_const c =
  match c with
  | Int(i)    -> string_of_int i
  | String(s) -> "\"" ^ s ^ "\""
  | Name(s) -> s

let string_of_value v =
  match v with
  | VInt(i)    -> string_of_int i
  | VString(s) -> "\"" ^ s ^ "\""
  | VName(s) -> s

let string_of_com c =
  match c with
  | Quit    -> "Quit"
  | Push(c) -> Printf.sprintf "Push(%s)" (string_of_const c)
  | Local(c) -> Printf.sprintf "Local(%s)" (string_of_const c)
  | Global(c) -> Printf.sprintf "Global(%s)" (string_of_const c)
  | Begin(_) -> "Begin"
  | Pop     -> "Pop"
  | Add     -> "Add"
  | Sub     -> "Sub"
  | Mul     -> "Mul"
  | Div     -> "Div"
  | Swap    -> "Swap"
  | Neg     -> "Neg"
  | Concat  -> "Concat"
  | And -> "And"
  | Not -> "Not"
  | Or -> "Or"
  | Equal -> "Equal"
  | Lte -> "Lte"

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
  | NAME of string
  | SYMBOL of string

let string_of_token tok =
  match tok with
  | NEWLINE   -> "\\n"
  | STRING(s) -> "\"" ^ s ^ "\""
  | NUMBER(n) -> string_of_int n
  | NAME(s) -> s
  | SYMBOL(s) -> s

let rec string_of_tokens (toks : token list) : string = 
  match toks with
  | [] -> ""
  | [token] -> string_of_token token
  | t :: token1 :: toks' -> string_of_token t ^ " " ^ string_of_tokens (token1 :: toks')

let is_space (c : char): bool =
  c = ' ' || c = '\t'

let is_digit (c : char): bool = '0' <= c && c <= '9'

let is_lletter (c : char): bool = 'a' <= c && c <= 'z'

let is_uletter (c : char): bool = 'A' <= c && c <= 'Z'

let is_alpha (c : char): bool = is_lletter c || is_uletter c

let int_of_char (c : char): int = int_of_string @@ Char.escaped c

let is_name_char (c : char): bool = is_alpha c || c = '_'

type lexer_err =
  | UnclosedString of (* line number *) int
  | InvalidChar    of (* line number *) int * char
  | UnknownChar    of (* line number *) int * char

let string_of_lexer_err e =
  match e with
  | UnclosedString(i) -> Printf.sprintf "Unclosed string at line %i" i
  | InvalidChar(i, c) -> Printf.sprintf "Invalid char '%c' at line %i" c i
  | UnknownChar(i, c) -> Printf.sprintf "Unknown char '%c' at line %i" c i

let tokenize_source (src : string): (token list, lexer_err) result =
  let rec helper line_num ls acc =
    let rec tokenize_string ls acc: (string * char list, lexer_err) result =
      match ls with
      | [] -> Error(UnclosedString(line_num))
      | ch::rst ->
        if ch = '\"' then
          Ok(acc, rst)
        else if is_alpha ch then
          tokenize_string rst (acc ^ Char.escaped ch)
        else
          Error(InvalidChar(line_num, ch))
    in
    let rec tokenize_number ls acc: (int * char list, lexer_err) result =
      match ls with
      | [] -> Ok(acc, [])
      | ch::rst ->
        if ch = '\n' || is_space ch then
          Ok(acc, ls)
        else if is_digit ch then
          tokenize_number rst @@ acc * 10 + int_of_char ch
        else Error(InvalidChar(line_num, ch))
    in
    let rec tokenize_symbol ls acc: (string * char list, lexer_err) result =
      match ls with
      | [] -> Ok(acc, [])
      | ch::rst ->
        if ch = '\n' || is_space ch then
          Ok(acc, ls)
        else if is_alpha ch then
          tokenize_symbol rst (acc ^ Char.escaped ch)
        else
          Error(InvalidChar(line_num, ch))
    in
    let tokenize_name ls acc: (string * char list, lexer_err) result =
      let rec tokenize_rest_of_name ls acc : (string * char list, lexer_err) result = 
        match ls with 
        | [] -> Ok(acc, [])
        | ch :: rst  -> if ch = '\n' || is_space ch then 
                          Ok(acc, ls)
                        else if is_name_char ch then
                          tokenize_rest_of_name rst (acc ^ Char.escaped ch)
                        else
                          Error(InvalidChar(line_num, ch))
        in
        match ls with
        (* Return error if [] is found *)
        | [] -> Ok(acc, [])
        | ch::rst ->
          if is_lletter ch then
            tokenize_rest_of_name rst (Char.escaped ch)
          else
            Error(InvalidChar(line_num, ch))
    in
    match ls with
    | [] -> Ok(List.rev acc)
    | ch::rst ->
      match ch with
      | '\n' -> helper (line_num + 1) rst (NEWLINE :: acc)
      | '\"' -> tokenize_string rst ""
                |> and_then @@ fun (s, rst) -> helper line_num rst (STRING(s) :: acc)
      | '-' -> tokenize_number rst 0
               |> and_then @@ fun (n, rst) -> helper line_num rst (NUMBER(-1 * n) :: acc)
      | ch -> if is_space ch then
          helper line_num rst acc
        else if is_lletter ch then
          tokenize_name  ls ""
          |> and_then @@ fun (s, rst) -> helper line_num rst (NAME(s) :: acc)
        else if is_alpha ch then
          tokenize_symbol  ls ""
          |> and_then @@ fun (s, rst) -> helper line_num rst (SYMBOL(s) :: acc)
        else if is_digit ch then
          tokenize_number ls 0
          |> and_then @@ fun (n, rst) -> helper line_num rst (NUMBER(n) :: acc)
        else Error(UnknownChar(line_num, ch))
  in helper 1 (char_list_of_string src) []

(*PARSING*)
type parse_err =
  | EmptyProgram
  | MissingArguments of (* line number *) int
  | InvalidCom       of (* line number *) int * token
  | ExpectedConst    of (* line number *) int * token
  | ExpectedName     of (* line number *) int * token
  | TrailingTokens   of (* line number *) int * token list (* End with no matching Begin  *)


let string_of_parse_err e =
  match e with
  | EmptyProgram -> "Cannot interpret an empty program"
  | MissingArguments(i) ->
    Printf.sprintf "Missing arguments to command at line %i" i
  | InvalidCom(i, tok) ->
    Printf.sprintf "Invalid command at line %i, got: \"%s\"" i (string_of_token tok)
  | ExpectedConst(i, tok) ->
    Printf.sprintf "Expected constant at line %i, got: \"%s\"" i (string_of_token tok)
  | ExpectedName(i, tok) ->
    Printf.sprintf "Expected name at line %i, got: \"%s\"" i (string_of_token tok)
  | TrailingTokens(i, tok) ->
    Printf.sprintf "Unexpected tokens at line %i, got: \"%s\"" i (string_of_tokens tok)


let parse_name (line_num : int) tok : (const, parse_err) result =
  match tok with
  | NAME(s) -> Ok(Name(s))
  | tok     -> Error(ExpectedName(line_num, tok))

let parse_const line_num tok =
  match tok with
  | NUMBER(n) -> Ok(Int(n))
  | STRING(s) -> Ok(String(s))
  | NAME(s) -> Ok(Name(s))
  | tok       -> Error(ExpectedConst(line_num, tok))

let eat_newline line_num ls =
  match ls with
  | [] -> Ok(ls)
  | NEWLINE::rst -> Ok(rst)
  | tok::_ -> Error(InvalidCom(line_num, tok))

let rec parse_com line_num ls acc =
  match ls with
  | [] -> Error(EmptyProgram)
  | SYMBOL("Push")  ::fst::rst ->
    parse_const line_num fst |> and_then @@ fun x -> Ok(Push(x)::acc, rst)
  | SYMBOL("Local")  ::fst::rst ->
  parse_name line_num fst |> and_then @@ fun x -> Ok(Local(x)::acc, rst)
  | SYMBOL("Global") ::fst::rst ->
  parse_name line_num fst |> and_then @@ fun x -> Ok(Global(x)::acc, rst)
  | SYMBOL("Begin") ::rst -> eat_newline line_num rst
  |> and_then @@ fun remaining_tokens'' ->
  parse_prog line_num rst [] |> and_then @@ fun (inner_prog, rst, line_num) -> 
    Ok(Begin(inner_prog)::acc, rst)
  | SYMBOL("Quit")  ::rst -> Ok(Quit::acc, rst)
  | SYMBOL("Pop")   ::rst -> Ok(Pop::acc, rst)
  | SYMBOL("Add")   ::rst -> Ok(Add::acc, rst)
  | SYMBOL("Sub")   ::rst -> Ok(Sub::acc, rst)
  | SYMBOL("Mul")   ::rst -> Ok(Mul::acc, rst)
  | SYMBOL("Div")   ::rst -> Ok(Div::acc, rst)
  | SYMBOL("Swap")  ::rst -> Ok(Swap::acc, rst)
  | SYMBOL("Neg")   ::rst -> Ok(Neg::acc, rst)
  | SYMBOL("Concat")::rst -> Ok(Concat::acc, rst)
  | SYMBOL("And")   ::rst -> Ok(And::acc, rst)
  | SYMBOL("Or")    ::rst -> Ok(Or::acc, rst)
  | SYMBOL("Not")   ::rst -> Ok(Not::acc, rst)
  | SYMBOL("Equal") ::rst -> Ok(Equal::acc, rst)
  | SYMBOL("Lte")   ::rst -> Ok(Lte::acc, rst)
  | tok::_ -> Error(InvalidCom(line_num, tok))
and
parse_prog (line_num : int) (token_list : token list) (com_list : prog) : (prog * token list * int, parse_err) result =
  match token_list with
  | []       -> Ok(List.rev com_list, [], line_num)
  | SYMBOL("End") :: remaining_tokens -> Ok(List.rev com_list, remaining_tokens, line_num)
  | _::_     -> parse_com line_num token_list com_list
                |> and_then @@ fun (com_list', remaining_tokens') ->
    eat_newline line_num remaining_tokens'
    |> and_then @@ fun remaining_tokens'' ->
    parse_prog (line_num + 1) remaining_tokens'' com_list'

let parse_program (src : token list): (prog, parse_err) result  =
  match parse_prog 1 src [] with
  | Ok(program, [], line_num) -> Ok(program)
  | Ok(program, trailing_tokens, line_num) -> Error(TrailingTokens(line_num, trailing_tokens))
  | Error(parse_err) -> Error(parse_err)

(*EVALUATION*)

type eval_err =
  | InvalidArity of com * (* # got *) int
  | WrongType    of com * (* args got *) value list
  | DivByZero    of int * int
  | UnboundRef   of name
  | EmptyStack   of com

let string_of_eval_err e =
  match e with
  | InvalidArity(c, i) ->
    Printf.sprintf "Got %i arguments to %s, expected %i" i (string_of_com c) (com_arity c)
  | WrongType(_, ls) ->
    Printf.sprintf "Got arguments of incorrect type: " ^ string_of_list string_of_value ls
  | DivByZero(m, n) ->
    Printf.sprintf "Got arguments to div: %i / %i" m n
  | UnboundRef(NName(n)) -> 
    Printf.sprintf "There is no \"%s\" in locals" n
  | EmptyStack(c) -> 
    Printf.sprintf "Begin ended with empty stack \"%s\"" (string_of_com c)

(*PROGRAM METHODS*)
let quit (stk, locals, global, log : state): state =
  stk, locals, global, (List.fold_right
          (fun elem acc ->
             string_of_value elem :: acc
          ) stk []) @ log

let push (stk, locals, global, log : state) (c : const): (state, eval_err) result =
  match (c, locals) with
  | Name(s), local :: locals' -> (
    match lookup_in_envs locals (NName(s)) with
    | Some(v) -> Ok(v :: stk, locals, global, log)
    | None -> (match lookup_in_env global (NName(s)) with 
                    | Some(v) -> Ok(v :: stk, locals, global, log)
                    | None -> Error(UnboundRef(NName(s)))
    )
  )
  | _ -> Ok(value_of_const c :: stk, locals, global, log)

let pop (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | []     -> Error(InvalidArity(Pop, 0))
  | _::tl -> Ok(tl, locals, global, log)

let add (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x + y) :: rst, locals, global, log)
  | _ :: [] | []  -> Error(InvalidArity(Add, List.length stk))
  | x :: y :: _ -> Error(WrongType(Add, [x; y]))

let sub (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x - y) :: rst, locals, global, log)
  | _ :: [] | []  -> Error(InvalidArity(Sub, List.length stk))
  | x :: y :: _ -> Error(WrongType(Sub, [x; y]))

let mul (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x * y) :: rst, locals, global, log)
  | _ :: [] | []  -> Error(InvalidArity(Mul, List.length stk))
  | x :: y :: _ -> Error(WrongType(Mul, [x; y]))

let div (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | VInt(x) :: VInt(0) :: _ -> Error(DivByZero(x, 0))
  | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x / y) :: rst, locals, global, log)
  | _ :: [] | []  -> Error(InvalidArity(Div, List.length stk))
  | x :: y :: _ -> Error(WrongType(Div, [x; y]))

let swap (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | x :: y :: rst -> Ok(y :: x :: rst, locals, global, log)
  | _ :: [] | []  -> Error(InvalidArity(Swap, List.length stk))

let neg (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | []  -> Error(InvalidArity(Neg, 0))
  | VInt(x) :: rst -> Ok(VInt(-1 * x) :: rst, locals, global, log)
  | x :: _ -> Error(WrongType(Neg, [x]))

let concat (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | VString(x) :: VString(y) :: rst -> Ok(VString(x ^ y) :: rst, locals, global, log)
  | _ :: [] | []  -> Error(InvalidArity(Concat, List.length stk))
  | x :: y :: _ -> Error(WrongType(Concat, [x; y]))

let zand (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | VInt(0) :: VInt(0) :: rst -> Ok(VInt(0) :: rst, locals, global, log)
  | VInt(1) :: VInt(0) :: rst -> Ok(VInt(0) :: rst, locals, global, log)
  | VInt(0) :: VInt(1) :: rst -> Ok(VInt(0) :: rst, locals, global, log)
  | VInt(1) :: VInt(1) :: rst -> Ok(VInt(1) :: rst, locals, global, log)
  | _ :: [] | []  -> Error(InvalidArity(And, List.length stk))
  | x :: y :: _ -> Error(WrongType(And, [x; y]))

let zor (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | VInt(0) :: VInt(0) :: rst -> Ok(VInt(0) :: rst, locals, global, log)
  | VInt(1) :: VInt(0) :: rst -> Ok(VInt(1) :: rst, locals, global, log)
  | VInt(0) :: VInt(1) :: rst -> Ok(VInt(1) :: rst, locals, global, log)
  | VInt(1) :: VInt(1) :: rst -> Ok(VInt(1) :: rst, locals, global, log)
  | _ :: [] | []  -> Error(InvalidArity(Or, List.length stk))
  | x :: y :: _ -> Error(WrongType(Or, [x; y]))

let znot (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | []  -> Error(InvalidArity(Not, 0))
  | VInt(0) :: rst -> Ok(VInt(1) :: rst, locals, global, log)
  | VInt(1) :: rst -> Ok(VInt(0) :: rst, locals, global, log)
  | x :: _ -> Error(WrongType(Not, [x]))

let equal (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | VInt(x) :: VInt(y) :: rst -> if x = y then 
                                  Ok(VInt(1) :: rst, locals, global, log)
                                else
                                  Ok(VInt(0) :: rst, locals, global, log)
  | _ :: [] | []  -> Error(InvalidArity(Equal, List.length stk))
  | x :: y :: _ -> Error(WrongType(Equal, [x; y]))

let lte (stk, locals, global, log : state): (state, eval_err) result =
  match stk with
  | VInt(x) :: VInt(y) :: rst -> if x <= y then 
                                  Ok(VInt(1) :: rst, locals, global, log)
                                else
                                  Ok(VInt(0) :: rst, locals, global, log)
  | _ :: [] | []  -> Error(InvalidArity(Lte, List.length stk))
  | x :: y :: _ -> Error(WrongType(Lte, [x; y]))

let local (stk, locals, global, log : state) (c : const): (state, eval_err) result =
  (* grab a value off of the stack, associate it with n in local environment *)
  match (stk, c, locals) with
  | v :: stk, Name(s), e :: envs -> Ok(stk, (((NName s, v) :: e) :: envs), global, log)
  | _, _, _ -> Error(InvalidArity(Local(c), 0))

let zglobal (stk, locals, global, log : state) (c : const): (state, eval_err) result =
  match (stk, c) with
  | v :: stk, Name(s) -> Ok(stk, locals, ((NName s, v) :: global),log)
  | _, _ -> Error(InvalidArity(Local(c), 0))
  

let rec zbegin (stk, locals, global, log : state) (program : prog): (state, eval_err) result =
  match eval program ([], locals, global, log) with
  | Ok(h :: rst, _locals, global, log) -> Ok(h :: stk, locals, global, log)
  | Ok([],  _locals, global, log) -> Error(EmptyStack(Begin(program)))
  (* if not, there is an error *)
  | e -> e


and eval (prog : prog) (stk, locals, global, log : state) : (state, eval_err) result =
  match prog with
  | [] -> Ok((stk, locals, global, log))
  | Quit    :: _    -> Ok(quit (stk, locals, global, log))
  | Push(c) :: prog -> push (stk, locals, global, log) c  |> and_then (eval prog)
  | Pop     :: prog -> pop  (stk, locals, global, log)    |> and_then (eval prog)
  | Add     :: prog -> add  (stk, locals, global, log)    |> and_then (eval prog)
  | Sub     :: prog -> sub  (stk, locals, global, log)    |> and_then (eval prog)
  | Mul     :: prog -> mul  (stk, locals, global, log)    |> and_then (eval prog)
  | Div     :: prog -> div  (stk, locals, global, log)    |> and_then (eval prog)
  | Swap    :: prog -> swap (stk, locals, global, log)    |> and_then (eval prog)
  | Neg     :: prog -> neg  (stk, locals, global, log)    |> and_then (eval prog)
  | Concat  :: prog -> concat  (stk, locals, global, log) |> and_then (eval prog)
  | And     :: prog -> zand  (stk, locals, global, log)    |> and_then (eval prog)
  | Or      :: prog -> zor  (stk, locals, global, log)    |> and_then (eval prog)
  | Not     :: prog -> znot  (stk, locals, global, log)    |> and_then (eval prog)
  | Equal   :: prog -> equal  (stk, locals, global, log)    |> and_then (eval prog)
  | Lte     :: prog -> lte  (stk, locals, global, log)    |> and_then (eval prog)
  | Local(c) :: prog -> local (stk, locals, global, log) c  |> and_then (eval prog)
  | Global(c) :: prog -> zglobal (stk, locals, global, log) c  |> and_then (eval prog)
  | Begin(program) :: prog -> zbegin (stk, locals, global, log) program |> and_then (eval prog)


let write_file_with_log (file_path: string) (log: log) : unit =
  let fp = open_out file_path in
  let (), _ =
    List.fold_left (
      fun ((), items_left) elem ->
        match items_left with
        | 1 -> (Printf.fprintf fp "%s" elem, items_left - 1)
        | _ -> (Printf.fprintf fp "%s\n" elem, items_left - 1)
    ) ((), List.length log) log
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
  tokenize_source src  |> Result.map_error lexer_err |> and_then @@ fun tokens ->
  parse_program tokens |> Result.map_error parse_err |> and_then @@ fun prog ->
  eval prog ([], [[]], [], [])   |> Result.map_error eval_err

let interpreter (src : string) (output_file_path: string): unit =
  let run src =
    tokenize_source src  |> Result.map_error lexer_err |> and_then @@ fun tokens ->
    parse_program tokens |> Result.map_error parse_err |> and_then @@ fun prog ->
    eval prog ([], [[]], [], [])   |> Result.map_error eval_err
  in
  (match run src with
   | Ok(_, _, _, log) -> log
   | Error(e)   -> print_endline (string_of_interp_err e); ["\"Error\""])
  |> write_file_with_log output_file_path
