(* parsing util functions *)

let is_lower_case c = 'a' <= c && c <= 'z'

let is_upper_case c = 'A' <= c && c <= 'Z'

let is_alpha c = is_lower_case c || is_upper_case c

let is_digit c = '0' <= c && c <= '9'

let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

let is_blank c = String.contains " \012\n\r\t" c

let explode s = List.of_seq (String.to_seq s)

let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)

let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let ( >>= ) = bind

let ( let* ) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then
      Some (x, ls)
    else
      None
  | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let ( >> ) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> (
      match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
  | None -> None

let ( << ) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> Some (x, ls)
  | None -> p2 ls

let ( <|> ) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some ((), ls)
    else
      None
  | _ -> None

let ws : unit parser = many whitespace >| ()

let ws1 : unit parser = many1 whitespace >| ()

let digit : char parser = satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match (cs, ls) with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c then
        loop cs xs
      else
        None
    | _ -> None
  in
  loop cs ls

let keyword (s : string) : unit parser = literal s >> ws >| ()

(* end of parser combinators *)

(* TODO *)


let integerParser : int parser =
  natural
  <|> ( satisfy (fun x -> x = '-') >>= fun c1 ->
        natural >>= fun n -> pure (-1 * n) )

let pushP =
  satisfy (fun x -> x = 'P') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 's') >>= fun c3 ->
  satisfy (fun x -> x = 'h') >>= fun c4 ->
  ws >>= fun c5 ->
  integerParser >>= fun n ->
  ws >>= fun c6 -> pure ("Push " ^ string_of_int n)


let popP =
  satisfy (fun x -> x = 'P') >>= fun c1 ->
  satisfy (fun x -> x = 'o') >>= fun c2 ->
  satisfy (fun x -> x = 'p') >>= fun c3 ->
  ws >>= fun c4 ->
  integerParser >>= fun n ->
  ws >>= fun c5 -> pure ("Pop " ^ string_of_int n)


let traceP =
  satisfy (fun x -> x = 'T') >>= fun c1 ->
  satisfy (fun x -> x = 'r') >>= fun c2 ->
  satisfy (fun x -> x = 'a') >>= fun c3 ->
  satisfy (fun x -> x = 'c') >>= fun c4 ->
  satisfy (fun x -> x = 'e') >>= fun c5 ->
  ws >>= fun c6 ->
  integerParser >>= fun n ->
  ws >>= fun c7 -> pure ("Trace " ^ string_of_int n)


let addP =
  satisfy (fun x -> x = 'A') >>= fun c1 ->
  satisfy (fun x -> x = 'd') >>= fun c2 ->
  satisfy (fun x -> x = 'd') >>= fun c3 ->
  ws >>= fun c4 ->
  integerParser >>= fun n ->
  ws >>= fun c5 -> pure ("Add "^ string_of_int n)


let subP =
  satisfy (fun x -> x = 'S') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 'b') >>= fun c3 ->
  ws >>= fun c4 ->
  integerParser >>= fun n ->
  ws >>= fun c5 -> pure ("Sub "^ string_of_int n)


let mulP =
  satisfy (fun x -> x = 'M') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 'l') >>= fun c3 ->
  ws >>= fun c4 ->
  integerParser >>= fun n ->
  ws >>= fun c5 -> pure ("Mul " ^ string_of_int n)


let divP =
  satisfy (fun x -> x = 'D') >>= fun c1 ->
  satisfy (fun x -> x = 'i') >>= fun c2 ->
  satisfy (fun x -> x = 'v') >>= fun c3 ->
  ws >>= fun c4 ->
  integerParser >>= fun n ->
  ws >>= fun c5 -> pure ("Div " ^ string_of_int n)


let pushBoolT =
  satisfy (fun x -> x = 'P') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 's') >>= fun c3 ->
  satisfy (fun x -> x = 'h') >>= fun c4 ->
  ws >>= fun c5 ->
  satisfy (fun x -> x = 'T') >>= fun c6 ->
  satisfy (fun x -> x = 'r') >>= fun c7 ->
  satisfy (fun x -> x = 'u') >>= fun c8 ->
  satisfy (fun x -> x = 'e') >>= fun c9 ->
  ws >>= fun c10 ->
  pure ("Push True")


let pushBoolF =
  satisfy (fun x -> x = 'P') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 's') >>= fun c3 ->
  satisfy (fun x -> x = 'h') >>= fun c4 ->
  ws >>= fun c5 ->
  satisfy (fun x -> x = 'F') >>= fun c6 ->
  satisfy (fun x -> x = 'a') >>= fun c7 ->
  satisfy (fun x -> x = 'l') >>= fun c8 ->
  satisfy (fun x -> x = 's') >>= fun c9 ->
  satisfy (fun x -> x = 'e') >>= fun c10 ->
  ws >>= fun c11 ->
  pure ("Push False")


let pushParen =
  satisfy (fun x -> x = 'P') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 's') >>= fun c3 ->
  satisfy (fun x -> x = 'h') >>= fun c4 ->
  ws >>= fun c5 ->
  satisfy (fun x -> x = '(') >>= fun c6 ->
  satisfy (fun x -> x = ')') >>= fun c7 ->
  ws >>= fun c8 -> pure ("Push ()")


let pushEnd =
  satisfy (fun x -> x = 'P') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 's') >>= fun c3 ->
  satisfy (fun x -> x = 'h') >>= fun c4 ->
  ws >>= fun c5 ->
  integerParser >>= fun n ->
  pure ("Push "^ string_of_int n)


let popEnd =
  satisfy (fun x -> x = 'P') >>= fun c1 ->
  satisfy (fun x -> x = 'o') >>= fun c2 ->
  satisfy (fun x -> x = 'p') >>= fun c3 ->
  ws >>= fun c4 ->
  integerParser >>= fun n ->
  pure ("Pop " ^ string_of_int n)


let traceEnd =
  satisfy (fun x -> x = 'T') >>= fun c1 ->
  satisfy (fun x -> x = 'r') >>= fun c2 ->
  satisfy (fun x -> x = 'a') >>= fun c3 ->
  satisfy (fun x -> x = 'c') >>= fun c4 ->
  satisfy (fun x -> x = 'e') >>= fun c5 ->
  ws >>= fun c6 ->
  integerParser >>= fun n ->
  pure ("Trace "^ string_of_int n)


let addEnd =
  satisfy (fun x -> x = 'A') >>= fun c1 ->
  satisfy (fun x -> x = 'd') >>= fun c2 ->
  satisfy (fun x -> x = 'd') >>= fun c3 ->
  ws >>= fun c4 ->
  integerParser >>= fun n ->
  pure ("Add "^ string_of_int n)


let subEnd =
  satisfy (fun x -> x = 'S') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 'b') >>= fun c3 ->
  ws >>= fun c4 ->
  integerParser >>= fun n ->
  pure ("Sub " ^ string_of_int n)


let mulEnd =
  satisfy (fun x -> x = 'M') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 'l') >>= fun c3 ->
  ws >>= fun c4 ->
  integerParser >>= fun n ->
  pure ("Mul " ^ string_of_int n)


let divEnd =
  satisfy (fun x -> x = 'D') >>= fun c1 ->
  satisfy (fun x -> x = 'i') >>= fun c2 ->
  satisfy (fun x -> x = 'v') >>= fun c3 ->
  ws >>= fun c4 ->
  integerParser >>= fun n ->
  pure ("Div " ^ string_of_int n)


let pushBoolTend =
  satisfy (fun x -> x = 'P') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 's') >>= fun c3 ->
  satisfy (fun x -> x = 'h') >>= fun c4 ->
  ws >>= fun c5 ->
  satisfy (fun x -> x = 'T') >>= fun c6 ->
  satisfy (fun x -> x = 'r') >>= fun c7 ->
  satisfy (fun x -> x = 'u') >>= fun c8 ->
  satisfy (fun x -> x = 'e') >>= fun c9 ->
  pure ("Push True")


let pushBoolFend =
  satisfy (fun x -> x = 'P') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 's') >>= fun c3 ->
  satisfy (fun x -> x = 'h') >>= fun c4 ->
  ws >>= fun c5 ->
  satisfy (fun x -> x = 'F') >>= fun c6 ->
  satisfy (fun x -> x = 'a') >>= fun c7 ->
  satisfy (fun x -> x = 'l') >>= fun c8 ->
  satisfy (fun x -> x = 's') >>= fun c9 ->
  satisfy (fun x -> x = 'e') >>= fun c10 ->
  pure ("Push False")


let pushParenEnd =
  satisfy (fun x -> x = 'P') >>= fun c1 ->
  satisfy (fun x -> x = 'u') >>= fun c2 ->
  satisfy (fun x -> x = 's') >>= fun c3 ->
  satisfy (fun x -> x = 'h') >>= fun c4 ->
  ws >>= fun c5 ->
  satisfy (fun x -> x = '(') >>= fun c6 ->
  satisfy (fun x -> x = ')') >>= fun c7 ->
  pure ("Push ()")

let parseOverall (src : string) = 
  parse(many( pushP <|> popP <|> traceP <|> addP
  <|> mulP <|> subP <|> divP <|> pushBoolF
  <|> pushBoolT <|> pushBoolFend <|> pushBoolTend <|> pushParen
  <|> pushParenEnd <|> pushEnd <|> popEnd
  <|> traceEnd <|> addEnd <|> mulEnd
  <|> subEnd <|> divEnd)) src 


let rev ls = 
  let rec aux summ ls = 
    match ls with
    | [] -> summ
    |h::t -> aux(h::summ) t

  in aux [] ls

let push st src =
  st :: src

let pop num lst =
  let rec aux n x =
    if n < 0 then 
      ["FAIL"]
    else if n == List.length x then
      []
    else if n == 0 then
      x
    else
      match x with 
      | h::t-> aux (n-1) t
      | _-> x

  in aux (int_of_string num) lst


let add num lst =
  let rec aux n x acc =
    if n == 0 && acc == 0 then
      "0"::x
    else if n == 0 then
      (string_of_int acc)::x
    else if ((List.length x) - n) < 0 then
      ["FAIL"]
    else
      match x with
      | h::t -> 
                if compare h "True" == 0 || compare h "False" == 0 || compare h "()" == 0 then
                  ["FAIL"]
                else
                  aux (n-1) t (acc + int_of_string h)
      | _ -> x

  in aux (int_of_string num) lst 0

let rec first lst = 
  match lst with
  | [] -> ""
  | h::t -> h

let rec rest lst = 
  match lst with 
  | [] -> []
  | h::t -> t

let sub num lst =
  if num == 0 then
    "0"::lst
  else
  
  let rec aux n x acc = 
    if n > List.length x then
      ["FAIL"]
    else if n == 1 then
      string_of_int acc :: x
    
    else
      match x with 
      |h::t -> 
              if compare h "True" == 0 || compare h "False" == 0 || compare h "()" == 0 then
                ["FAIL"]
              else aux (n - 1) t (acc - int_of_string h)
      |_ -> x


  in aux num (rest lst) (int_of_string (first lst))

let mul num lst =
  if int_of_string num == 0 then
    "1"::lst
  else 
  let rec aux n x acc =
     if n == 0 then
      (string_of_int acc)::x
    else if ((List.length x) - n) < 0 then
      ["FAIL"]
    else
      match x with
      | h::t -> 
                if compare h "True" == 0 || compare h "False" == 0 || compare h "()" == 0 then
                  ["FAIL"]
                else
                  aux (n-1) t (acc * (int_of_string h))
      | _ -> x

  in aux (int_of_string num) lst 1

let div num lst =
  if num == 0 then
    "1"::lst
  else
  
  let rec aux n x acc = 
    if n > List.length x then
      ["FAIL"]
    else if n == 1 then
      string_of_int acc :: x
    
    else
      match x with 
      |h::t -> 
              if compare h "True" == 0 || compare h "False" == 0 || compare h "()" == 0 || int_of_string h == 0 then
                ["FAIL"]
              
              else 
              
              if int_of_string h == 0 then
                ["FAIL"]
              else
              aux (n - 1) t (acc / int_of_string h)
      |_ -> x


  in aux num (rest lst) (int_of_string (first lst))

let rec tracerAdd counter inList outList =
  if counter <= 0 then
    outList
  else
    match inList with 
    | [] -> outList
    | h::t -> tracerAdd (counter - 1) t (h::outList)
  
                                             

let rec tracerRem counter inList outList =
  if counter <= 0 then
    inList
  else 
    match inList with
    | [] -> inList
    | h::t -> tracerRem (counter - 1) t outList

                        
let tupleBrkr s = 
  match s with
    | None -> []
    | Some x -> 
          match snd x with
            | [] -> (match fst x with
                       _ -> fst x)
            | _ -> ["Error"]

            
let rec checker lst =
  match lst with
  |h::t -> 
          if compare h "True" == 0 || compare h "False" == 0 || compare h "()" == 0 then
             ["FAIL"]
          else
            checker (t)
  | [] -> lst


  let rec divChecker lst =
    match lst with
    |h::t -> 
            if compare h "True" == 0 || compare h "False" == 0 || compare h "()" == 0 then
               ["FAIL"]
            else
              divChecker (t)
    | [] -> lst

let subHelp num lst = 
  if checker lst == ["FAIL"] then
    ["FAIL"]
  else
    [string_of_int(int_of_string (first lst) - int_of_string (first (add (string_of_int (List.length (rest lst))) (rest lst))))]


let divHelp num lst = 
  if divChecker lst == ["FAIL"] then
    ["FAIL"]
  else if int_of_string (first (mul (string_of_int (List.length (rest lst))) (rest lst))) == 0 then
    ["FAIL"]
  else
    [string_of_int(int_of_string (first lst) / int_of_string (first (mul (string_of_int (List.length (rest lst))) (rest lst))))]


let interp (src : string) : string list =
  let rec aux lst inStack outStack =
    if inStack == ["FAIL"] then
      ["Error"]
    else
      match lst with
        |[] -> outStack
        |h::t ->  
                match (implode [h.[0]; h.[1]]) with
                | "Pu" -> 
                          aux  t ((String.sub h 5 (String.length h - 5))::inStack ) outStack
                | "Po" -> 
                        if int_of_string (String.sub h 4 (String.length h - 4)) > (List.length inStack) then
                          ["Error"]

                        else if int_of_string (String.sub h 4 (String.length h - 4)) == 0 then
                          aux t inStack outStack
                        
                        else
                        aux t (pop ((String.sub h 4 (String.length h - 4))) inStack ) outStack
                | "Ad" -> 
                          aux t (add ((String.sub h 4 (String.length h - 4))) inStack ) outStack
                | "Su" -> 
                        
                          if (int_of_string (String.sub h 4 (String.length h - 4))) == 0 then
                              aux t ("0"::inStack) outStack
                         
                          else if compare (first inStack) "True" == 0 || compare (first inStack) "False" == 0 || compare (first inStack) "()" == 0 then
                              ["Error"]

                          else if (int_of_string (String.sub h 4 (String.length h - 4))) > List.length inStack then
                            ["Error"]
                          
                            

                          else if (int_of_string (String.sub h 4 (String.length h - 4))) == List.length inStack then
                            aux t (subHelp (int_of_string (String.sub h 4 (String.length h - 4))) inStack) outStack                      
                          else
                            aux t (sub (int_of_string (String.sub h 4 (String.length h - 4))) inStack  ) outStack
                            
                | "Tr" ->
                        if inStack == [] && (int_of_string (String.sub h 6 (String.length h - 6))) != 0 then
                          ["Error"]


                        else if (int_of_string (String.sub h 6 (String.length h - 6))) == 0 then
                          aux t inStack outStack

                        else if List.length inStack < (int_of_string (String.sub h 6 (String.length h - 6))) then
                          ["Error"]

                        else
                          aux t  (tracerRem (int_of_string (String.sub h 6 (String.length h - 6))) inStack outStack) (tracerAdd  (int_of_string (String.sub h 6 (String.length h - 6)))  inStack outStack)

                | "Mu" ->
                          if int_of_string(String.sub h 4 (String.length h - 4)) > List.length inStack then
                            ["Error"] 

                          else if int_of_string(String.sub h 4 (String.length h - 4)) == 0 then
                            aux t ("1"::inStack) outStack
                          
                          else if compare (first inStack) "True" == 0 || compare (first inStack) "False" == 0 || compare (first inStack) "()" == 0 then
                            ["Error"]

                          else if (String.sub h 4 (String.length h - 4)) = "1" then
                            aux t inStack outStack
                          
                          else
                          aux t (mul (String.sub h 4 (String.length h - 4)) inStack   ) outStack
                | "Di" -> 
                  if (int_of_string (String.sub h 4 (String.length h - 4))) == 0 then
                              aux t ("1"::inStack) outStack
                         
                          else if compare (first inStack) "True" == 0 || compare (first inStack) "False" == 0 || compare (first inStack) "()" == 0 then
                              ["Error"]

                          else if (int_of_string (String.sub h 4 (String.length h - 4))) > List.length inStack then
                            ["Error"]
                          

                          else if (int_of_string (String.sub h 4 (String.length h - 4))) == List.length inStack && (divChecker inStack == ["FAIL"]) then
                            ["Error"]

                          else if (int_of_string (String.sub h 4 (String.length h - 4))) == List.length inStack && ((first inStack) = "0") then
                            aux t ["0"] outStack



                          
                          else if (int_of_string (String.sub h 4 (String.length h - 4))) == List.length inStack then
                            aux t (divHelp (int_of_string (String.sub h 4 (String.length h - 4))) inStack) outStack 
                
                          else
                             aux t (div (int_of_string (String.sub h 4 (String.length h - 4))) inStack  ) outStack
                          
                | _ -> ["FAIL"]

  in aux (tupleBrkr (parseOverall src)) [] []



(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src
