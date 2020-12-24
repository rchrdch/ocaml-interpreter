open List
open String

type const = I of int | B of bool | Error | S of string | N of string | Unit | Closure of (const * commands * (const * const) list)
and stack = const list

and command = Push of const | Pop | Add | Sub | Mul | Div | Rem | Neg | Swap | Cat | And | Or | Not | Eq | Lte | Lt | Gte | Gt | Bnd | Begin_End of commands | IfThen of (commands * commands * commands) | Func of (string * const * commands) | TryWith of (commands * commands) | Call | Ret | Quit | Exit
and commands = command list

(* PARSER DEFINITION *)
type 'a parser = Parser of (string -> ('a * string) list)

let (memory: ('a * 'b) list) = []

(* -------------------------------------------------------------------------------------------------------------------- *)
(* HELPER FUNCTIONS *)
let explode (s: string) : char list = 
  let rec expl i l = 
    if i < 0 then l 
    else expl (i - 1) (String.get s i :: l) in 
  expl (String.length s - 1) []

let implode (cl: char list) : string = 
  String.concat "" (List.map (String.make 1) cl)

let isAlphabet = 
  function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let isDigit =
  function '0' .. '9' -> true | _ -> false

let isUnderscore =
  function '_' -> true | _ -> false

let runParser parser input =
  let Parser parse_func = parser in parse_func input

let return (a: 'a) : 'a parser =
  Parser (fun input -> [a, input])

let fail : 'a parser =
  Parser (fun input -> [])

(* BIND OPERATOR *)
let (>>=) (parser: 'a parser) (func: 'a -> 'b parser) : 'b parser = 
  Parser (fun input -> 
    match (runParser parser input) with 
      | [] -> []
      | (x, y)::_ -> runParser (func x) y 
  )

let (let*) = (>>=)

(* CHOICE OPERATOR *)
let (<|>) (parser1: 'a parser) (parser2: 'a parser) : 'a parser = 
  Parser (fun input -> 
      match runParser parser1 input with 
        | [] -> runParser parser2 input (* return the result of the second parser if the first parser fails *)
        | res -> res  (* return the result of the first parser *) 
  )

(* READ A SINGLE CHAR *)
let read : char parser =
  Parser (fun str ->
    match explode str with
    | h::str -> [(h, implode str)]
    | [] -> [] 
  )

(* READ N CHARS *)
let rec readn (n:int) : string parser =
  if n > 0 then
    let* x = read in 
    let* y = readn (n-1) in
    return (String.make 1 x^y)
  else
    return ""

let convert func =
  read >>= (fun x -> if func x then return x else fail)

(* -------------------------------------------------------------------------------------------------------------------- *)
(* PARSERS *)
let digit_parser = convert isDigit
let alphabet_parser = convert isAlphabet 
let char_parser x = convert (fun y -> y = x) 
let whitespace_parser = char_parser ' ' <|> char_parser '\n' <|> char_parser '\t' <|> char_parser '\r'

let any_char_parser = char_parser '`' <|> char_parser '~' <|> char_parser '!' <|> char_parser '@' <|> char_parser '#' <|> char_parser '$' <|> char_parser '%' <|> char_parser '^' <|> char_parser '&' <|> char_parser '*' <|> char_parser '(' <|> char_parser ')' <|> char_parser '-' <|> char_parser '_' <|> char_parser '+' <|> char_parser '=' <|> char_parser '[' <|> char_parser ']' <|> char_parser '{' <|> char_parser '}' <|> char_parser '|' <|> char_parser ':' <|> char_parser ';' <|> char_parser '<' <|> char_parser '>' <|> char_parser ',' <|> char_parser '.' <|> char_parser '?' <|> char_parser '/'

let rec many_parser (parser: 'a parser) : 'a list parser =
  (let* output = parser in
  let* lst = many_parser parser in
  return(output::lst))
  <|> return []

let rec many1_parser (p: 'a parser): 'a list parser =
  (let* a = p in
   let* ls = many_parser p in
   return(a :: ls))

let natural_num_parser =
  many1_parser digit_parser >>= fun char_list -> return (int_of_string (implode char_list))

let int_parser =
  (char_parser '-' >>= fun _ -> natural_num_parser >>= fun n -> return (-n)) <|> natural_num_parser

(* TAKES IN A STRING AS ITS INPUT AND CREATES A STRING PARSER *)
let string_parser (str: string) : string parser =
  let length = String.length str in
  (readn length) >>= fun x ->
  if x = str then return x
  else fail

let string_p = 
  many_parser (any_char_parser <|> digit_parser <|> alphabet_parser <|> char_parser ' ') >>= fun x -> return (implode x)

let bool_parser =
  (string_parser "<true>" >>= fun x -> return (String.sub x 1 4))
  <|> 
  (string_parser "<false>" >>= fun y -> return (String.sub y 1 5))

let error_parser =
  string_parser "<error>"

let unit_parser = 
  string_parser "<unit>"

let name_parser =
  many1_parser (alphabet_parser <|> digit_parser <|> char_parser '_') >>= fun x -> return (implode x)

(* -------------------------------------------------------------------------------------------------------------------- *)
(* COMMAND PARSERS *)
let push_generic : command parser =
  string_parser "Push" >>= fun _ -> 
  whitespace_parser >>= fun _ ->
  (int_parser >>= fun i -> 
  string_parser "\n" >>= fun _ ->
  return (Push (I i)))
  <|>
  (string_parser "\"" >>= fun _ ->
  string_p >>= fun s -> 
  string_parser "\"" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Push (S s)))
  <|>
  (bool_parser >>= fun b -> 
  string_parser "\n" >>= fun _ ->
  return (Push (B (bool_of_string b))))
  <|>
  (error_parser >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Push (Error)))
  <|>
  (unit_parser >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Push (Unit)))
  <|>
  (name_parser >>= fun n ->
  string_parser "\n" >>= fun _ ->
  return (Push (N n)))

let pop : command parser =
  string_parser "Pop" >>= fun _ ->
  string_parser "\n" >>= fun _ -> 
  return (Pop)

let add : command parser =
  string_parser "Add" >>= fun _ -> 
  string_parser "\n" >>= fun _ ->
  return (Add)

let sub : command parser =
  string_parser "Sub" >>= fun _ -> 
  string_parser "\n" >>= fun _ ->
  return (Sub)

let mult : command parser =
  string_parser "Mul" >>= fun _ -> 
  string_parser "\n" >>= fun _ ->
  return (Mul)

let div : command parser =
  string_parser "Div" >>= fun _ -> 
  string_parser "\n" >>= fun _ ->
  return (Div)

let rem : command parser =
  string_parser "Rem" >>= fun _ -> 
  string_parser "\n" >>= fun _ ->
  return (Rem)

let neg : command parser =
  string_parser "Neg" >>= fun _ -> 
  string_parser "\n" >>= fun _ ->
  return (Neg)

let swap : command parser =
  string_parser "Swap" >>= fun _ ->
  string_parser "\n" >>= fun _ -> 
  return (Swap)

let cat : command parser =
  string_parser "Cat" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Cat)

let and_command : command parser =
  string_parser "And" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (And)

let or_command : command parser =
  string_parser "Or" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Or)

let not_command : command parser =
  string_parser "Not" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Not)

let eq : command parser =
  string_parser "Eq" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Eq)

let lte : command parser =
  string_parser "Lte" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Lte)

let lt : command parser =
  string_parser "Lt" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Lt)

let gte : command parser =
  string_parser "Gte" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Gte)

let gt : command parser =
  string_parser "Gt" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Gt)

let bind : command parser = 
  string_parser "Bnd" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Bnd)

let quit : command parser =
  string_parser "Quit" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Quit) 

let call : command parser =
  string_parser "Call" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Call)

let ret : command parser =
  string_parser "Return" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Ret)

let rec if_command () : command parser = 
  string_parser "If" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  many1_parser (call <|> ret <|> func () <|> trywith () <|> begin_end_command () <|> if_command () <|> push_generic <|> pop <|> add <|> sub <|> mult <|> div <|> rem <|> neg <|> swap <|> cat <|> and_command <|> or_command <|> not_command <|> eq <|> lte <|> lt <|> gte <|> gt <|> bind <|> quit)  
  >>= fun parse_test -> 
  string_parser "Then" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  many1_parser (call <|> ret <|> func () <|> trywith () <|> begin_end_command () <|> if_command () <|> push_generic <|> pop <|> add <|> sub <|> mult <|> div <|> rem <|> neg <|> swap <|> cat <|> and_command <|> or_command <|> not_command <|> eq <|> lte <|> lt <|> gte <|> gt <|> bind <|> quit)  
  >>= fun parse_true ->
  string_parser "Else" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  many1_parser (call <|> ret <|> func () <|> trywith () <|> begin_end_command () <|> if_command () <|> push_generic <|> pop <|> add <|> sub <|> mult <|> div <|> rem <|> neg <|> swap <|> cat <|> and_command <|> or_command <|> not_command <|> eq <|> lte <|> lt <|> gte <|> gt <|> bind <|> quit)  
  >>= fun parse_false -> 
  string_parser "EndIf" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (IfThen (parse_test, parse_true, parse_false))

and func () : command parser =
  string_parser "Fun" >>= fun _ ->
  whitespace_parser >>= fun _ ->
  name_parser >>= fun fname ->
  print_newline();
  whitespace_parser >>= fun _ ->
  name_parser >>= fun input ->
  print_newline();
  string_parser "\n" >>= fun _ ->

  many1_parser (call <|> ret <|> trywith () <|> begin_end_command () <|> ret <|> func () <|> if_command () <|> push_generic <|> pop <|> add <|> sub <|> mult <|> div <|> rem <|> neg <|> swap <|> cat <|> and_command <|> or_command <|> not_command <|> eq <|> lte <|> lt <|> gte <|> gt <|> bind <|> quit) 
  >>= fun comms ->
  (* string_parser "Return" >>= fun _ ->
  string_parser "\n" >>= fun _ -> *)
  string_parser "EndFun" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Func (fname, N input, comms))

and trywith () : command parser =
  string_parser "Try" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  many1_parser (call <|> ret <|> begin_end_command () <|> trywith () <|> func () <|> if_command () <|> push_generic <|> pop <|> add <|> sub <|> mult <|> div <|> rem <|> neg <|> swap <|> cat <|> and_command <|> or_command <|> not_command <|> eq <|> lte <|> lt <|> gte <|> gt <|> bind <|> quit) 
  >>= fun try_comms ->

  string_parser "With" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  many1_parser (call <|> ret <|> begin_end_command () <|> trywith () <|> func () <|> if_command () <|> push_generic <|> pop <|> add <|> sub <|> mult <|> div <|> rem <|> neg <|> swap <|> cat <|> and_command <|> or_command <|> not_command <|> eq <|> lte <|> lt <|> gte <|> gt <|> bind <|> quit) 
  >>= fun with_comms ->
  string_parser "EndTry" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (TryWith (try_comms, with_comms))

and begin_end_command () : command parser = 
  string_parser "Begin" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  many1_parser (call <|> ret <|> if_command () <|> trywith () <|> func () <|> call <|> ret <|> push_generic <|> pop <|> add <|> sub <|> mult <|> div <|> rem <|> neg <|> swap <|> cat <|> and_command <|> or_command <|> not_command <|> eq <|> gte <|> lte <|> gt <|> lt <|> bind <|> begin_end_command () <|> quit)  
  >>= fun comm ->
  string_parser "End" >>= fun _ ->
  string_parser "\n" >>= fun _ ->
  return (Begin_End comm)


(* -------------------------------------------------------------------------------------------------------------------- *)
(* PART 1: parse file into string list as tokens and reach each element *)
(* EX: ["Push 5"; "Push 3"; "Pop"] *)
let rec read_helper (channel) (lst) : string list =
  match input_line channel with
  | line -> read_helper channel (line::lst)
  | exception End_of_file -> close_in channel; List.rev lst

let rec token_parser (input: string) : string list =
  (* read lines from file as input *)
  let channel = open_in input in read_helper (channel) ([])

let rec write_helper channel (l: string list) =
  match l with
  | [] -> close_out channel
  | h::t -> Printf.fprintf channel "%s\n" h; write_helper channel t

let rec write_list_str (f_name: string) (content: string list): unit = 
  let channel = open_out f_name in write_helper (channel) (List.rev content) 

(* -------------------------------------------------------------------------------------------------------------------- *)
(* PART 2: convert string list into command list *)
let flip_order func second_arg first_arg = func first_arg second_arg
let ($) func g  = fun x -> func (g x)

let rec parse input = 
  runParser (many1_parser (push_generic <|> pop <|> add <|> sub <|> mult <|> div <|> rem <|> swap <|> neg <|> cat <|> and_command <|> or_command <|> not_command <|> eq <|> gte <|> lte <|> gt <|> lt <|> bind <|> if_command () <|> begin_end_command () <|> func () <|> call <|> ret <|> trywith () <|> quit)) input

let rec string_list2string (l: string list) : string = 
  (* let lst = List.map (flip_order List.nth 0 $ parse) l in
  List.map (fun (command, empty_string) -> command) lst *)
  match l with
  | [] -> ""
  | h::t -> h ^ "\n" ^ (string_list2string t)

let rec command_list_parser (comm_lst: command list) =
  match comm_lst with
  | [] -> []
  | h::t -> [h] @ (command_list_parser t)

let rec command_parser (comm_string: (command list * string) list) : command list =
  match comm_string with
  | [] -> []
  | (tuple_comm, tuple_string)::rest -> (command_list_parser tuple_comm)

let rec stack2string (l: const list) (string_lst: string list) : string list = 
  match l with
  | [] -> string_lst
  | h::t -> stack2string t (match h with
              | I i -> (string_of_int i::string_lst)
              | B b -> ("<" ^ (string_of_bool b) ^ ">")::string_lst
              | S s -> s::string_lst
              | N n -> n::string_lst
              | Error -> "<error>"::string_lst
              | Unit -> "<unit>"::string_lst
              (* | Closure (arg, commands, env) -> "hi"::string_lst *)
            )

(* -------------------------------------------------------------------------------------------------------------------- *)
(* PART 3: evaluate the list of commands *)

(* FOR BIND COMMAND *)
let rec fetch variable memory =
  match memory with
  | [] -> None
  | (key, value)::t -> if key = variable then (Some value) else fetch variable t

let extract_val option_val =
  match option_val with
  | None -> Error
  | Some value -> value

let rec print_bind memory =
  match memory with
  | [] -> None
  | (key, value)::t -> print_string "("; print_string key; print_int value; print_string ")"; print_bind t

(* FOR INT *)
(* FETCH FROM MEMORY *)
let rec fetch_int (variable: const) (memory: (const * const) list) : int option =
  match memory with
  | [] -> None
  | (key, value)::t ->  (match value with
                        | I i -> if key = variable then (Some i) else fetch_int variable t
                        | _ -> fetch_int variable t) 
  (* if key = variable then (Some value) else fetch variable t *)

(* EXTRACT CONST VALUE FROM OUTPUT OF FETCH *)
let extract_int (option_val: int option) =
  match option_val with
  | None -> Error
  | Some value -> I value

let full_extract variable memory =
  extract_int (fetch_int variable memory)

(* FOR BOOL *)
let rec fetch_bool (variable: const) (memory: (const * const) list) : bool option =
  match memory with
  | [] -> None
  | (key, value)::t ->  (match value with
                        | B b -> if key = variable then (Some b) else fetch_bool variable t
                        | _ -> fetch_bool variable t) 

let extract_bool (option_val: bool option) =
  match option_val with
  | None -> Error
  | Some value -> B value

let full_extract_bool variable memory =
  extract_bool (fetch_bool variable memory)

(* FOR STRING *)
let rec fetch_str (variable: const) (memory: (const * const) list) : string option =
  match memory with
  | [] -> None
  | (key, value)::t ->  (match value with
                        | S s -> if key = variable then (Some s) else fetch_str variable t
                        | _ -> fetch_str variable t) 

let extract_str (option_val: string option) =
  match option_val with
  | None -> Error
  | Some value -> S value

let full_extract_str variable memory =
  extract_str (fetch_str variable memory)

(* EXTRACT COMMANDS FROM CLOSURE *)
let commands_extract closure : commands option =
  match closure with
  | Closure c -> (match c with 
                  | (x, comms, y) -> Some comms
                 )
  | _ -> None

let commands_extract_helper (option_closure: commands option) : commands =
  match option_closure with
  | Some value -> value
  | None -> []
 
let rec closure_extract (name: const) (memory: (const * const) list) =
  match memory with 
  | [] -> None
  | (key, closure)::t -> if key = name then (commands_extract closure) else closure_extract name t

let command_closure_extract name memory =
  commands_extract_helper (closure_extract name memory)

(* EXTRACT ARGUMENT FROM CLOSURE *)
let arg_extract closure =
  match closure with
  | Closure c -> (match c with 
                  | (x, comms, y) -> Some x
                 )
  | _ -> None

let arg_extract_helper (option_arg) : const =
  match option_arg with
  | Some value -> value
  | None -> Error

let rec closure_extract_2 name memory =
  match memory with 
  | [] -> None
  | (key, closure)::t -> if key = name then (arg_extract closure) else closure_extract_2 name t

let arg_closure_extract name memory =
  arg_extract_helper (closure_extract_2 name memory)

(* EXTRACT ENV FROM CLOSURE *)
let env_extract closure =
  match closure with
  | Closure c -> (match c with 
                  | (x, comms, y) -> Some y
                 )
  | _ -> None

let env_extract_helper (option_env) : (const * const) list =
  match option_env with
  | Some value -> value
  | None -> []

let rec closure_extract_3 name memory =
  match memory with 
  | [] -> None
  | (key, closure)::t -> if key = name then (env_extract closure) else closure_extract_3 name t

let env_closure_extract name memory =
  env_extract_helper (closure_extract_3 name memory)

(* FOR TRYWITH *)
let rec trywith_helper input =
  match input with
  | h::t -> if h = Error then h else trywith_helper t
  | [] -> S "success"

let rec delete_closure_command commands : commands =
  match commands with
  | [] -> []
  | h::t -> if h = Exit then t else delete_closure_command t

let rec delete_closure_env env : (const * const) list =
  match env with
  | [] -> []
  | (x, y)::t -> if x = Error && y = Error then t else delete_closure_env t

let rec eval (commands: command list) (stack: const list) (memory: (const * const) list) : const list =
  match (commands, stack) with
  | Push x::rest, stack' -> eval rest (x::stack') memory
  
  | Pop::rest, x::stack' -> eval rest stack' memory
  | Pop::rest, [] -> eval rest (Error::[]) memory

  | Sub::rest, x::y::stack' -> (match x, y with
                                | I x, I y -> eval rest (I (x - y)::stack') memory
                                | I x, N y -> (match (full_extract (N y) memory) with
                                                | I i -> eval rest (I (x - i)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, I y -> (match (full_extract (N x) memory) with
                                                | I i -> eval rest (I (i - y)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract (N x) memory), (full_extract (N y) memory) with
                                                | I x, I y -> eval rest (I (x - y)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                               )
  | Sub::rest, x::stack' -> eval rest (Error::stack) memory
  | Sub::rest, [] -> eval rest (Error::stack) memory
         
  | Add::rest, x::y::stack' -> (match x, y with
                                | I x, I y -> eval rest (I (x + y)::stack') memory
                                | I x, N y -> (match (full_extract (N y) memory) with
                                                | I i -> eval rest (I (x + i)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, I y -> (match (full_extract (N x) memory) with
                                                | I i -> eval rest (I (i + y)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract (N x) memory), (full_extract (N y) memory) with
                                                | I x, I y -> eval rest (I (x + y)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )  
                                | _, _ -> eval rest (Error::stack) memory
                               )
  | Add::rest, x::stack' -> eval rest (Error::stack) memory
  | Add::rest, [] -> eval rest (Error::stack) memory

  | Mul::rest, x::y::stack' -> (match x, y with
                                | I x, I y -> eval rest (I (x * y)::stack') memory
                                | I x, N y -> (match (full_extract (N y) memory) with
                                                | I i -> eval rest (I (x * i)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, I y -> (match (full_extract (N x) memory) with
                                                | I i -> eval rest (I (i * y)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract (N x) memory), (full_extract (N y) memory) with
                                                | I x, I y -> eval rest (I (x * y)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                               )
  | Mul::rest, x::stack' -> eval rest (Error::stack) memory
  | Mul::rest, [] -> eval rest (Error::stack) memory

  | Div::rest, x::y::stack' -> (match x, y with
                                | I 0, I y -> eval rest (I (0)::stack') memory
                                | I x, I 0 -> eval rest (Error::stack) memory
                                | I x, I y ->  eval rest (I (x / y)::stack') memory
                                | I x, N y -> (match (full_extract (N y) memory) with
                                                | I i -> if i = 0 then eval rest (Error::stack) memory else eval rest (I (x / i)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, I y -> (match (full_extract (N x) memory) with
                                                | I i -> if i = 0 then eval rest (I (0)::stack') memory else eval rest (I (i / y)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract (N x) memory), (full_extract (N y) memory) with
                                                | I x, I y -> if x = 0 then eval rest (I (0)::stack') memory else if y = 0 then eval rest (Error::stack) memory else eval rest (I (x / y)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                               )
  | Div::rest, x::stack' -> eval rest (Error::stack) memory
  | Div::rest, [] -> eval rest (Error::stack) memory


  | Rem::rest, x::y::stack' -> (match x, y with
                                (* | I 0, I y -> eval rest (Error::stack) *)
                                | I x, I 0 -> eval rest (Error::stack) memory
                                | I x, I y -> eval rest (I (x mod y)::stack') memory
                                | I x, N y -> (match (full_extract (N y) memory) with
                                                | I i -> if i = 0 then eval rest (Error::stack) memory else eval rest (I (x mod i)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, I y -> (match (full_extract (N x) memory) with
                                                | I i -> if y = 0 then eval rest (Error::stack) memory else eval rest (I (i mod y)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract (N x) memory), (full_extract (N y) memory) with
                                                | I x, I y -> if y = 0 then eval rest (Error::stack) memory else eval rest (I (x mod y)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                               )
  | Rem::rest, x::stack' -> eval rest (Error::stack) memory
  | Rem::rest, [] -> eval rest (Error::stack) memory


  | Neg::rest, x::stack' -> (match x with
                              | I 0 -> eval rest (x::stack') memory
                              | I x -> eval rest ((I (x-(2*x)))::stack') memory
                              | N x -> (match (full_extract (N x) memory) with
                                                | I i -> eval rest (I (i-(2*i))::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                              | _ -> eval rest (Error::stack) memory
                              )
  | Neg::rest, [] -> eval rest (Error::stack) memory

  | Swap::rest, x::y::stack' -> (match x, y with
                                  | x, y -> eval rest (y::x::stack') memory
                                  | _, _ -> eval rest (Error::stack) memory
                                )
  | Swap::rest, x::stack' -> eval rest (Error::stack) memory
  | Swap::rest, [] -> eval rest (Error::stack) memory

  | Cat::rest, x::y::stack' -> (match x, y with
                                | S x, S y -> eval rest (S (x^y)::stack') memory
                                | S x, N y -> (match (full_extract_str (N y) memory) with
                                                | S s -> eval rest (S (x^s)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, S y -> (match (full_extract_str (N x) memory) with
                                                | S s -> eval rest (S (s^y)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract_str (N x) memory), (full_extract_str (N y) memory) with
                                                | S x, S y -> eval rest (S (x^y)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                               )
  | Cat::rest, x::stack' -> eval rest (Error::stack) memory
  | Cat::rest, [] -> eval rest (Error::stack) memory

  | And::rest, x::y::stack' -> (match x, y with
                                | B x, B y -> eval rest (B (x && y)::stack') memory
                                | B x, N y -> (match (full_extract_bool (N y) memory) with
                                                | B b -> eval rest (B (x && b)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, B y -> (match (full_extract_bool (N x) memory) with
                                                | B b -> eval rest (B (b && y)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract_bool (N x) memory), (full_extract_bool (N y) memory) with
                                                | B x, B y -> eval rest (B (x && y)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                               )
  | And::rest, x::stack' -> eval rest (Error::stack) memory
  | And::rest, [] -> eval rest (Error::stack) memory

  | Or::rest, x::y::stack' -> (match x, y with
                                | B x, B y -> eval rest (B (x || y)::stack') memory
                                | B x, N y -> (match (full_extract_bool (N y) memory) with
                                                | B b -> eval rest (B (x || b)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, B y -> (match (full_extract_bool (N x) memory) with
                                                | B b -> eval rest (B (b || y)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract_bool (N x) memory), (full_extract_bool (N y) memory) with
                                                | B x, B y -> eval rest (B (x || y)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error:: stack) memory
                                )
  | Or::rest, x::stack' -> eval rest (Error::stack) memory
  | Or::rest, [] -> eval rest (Error::stack) memory

  | Not::rest, x::stack' -> (match x with
                              | B x -> if x = true then eval rest (B (false)::stack') memory else eval rest (B (true)::stack') memory
                              | N x -> (match (full_extract_bool (N x) memory) with
                                                | B x -> if x = true then eval rest (B (false)::stack') memory else eval rest (B (true)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                              | _ -> eval rest (Error::stack) memory
                            )
  | Not::rest, [] -> eval rest (Error::stack) memory

  | Eq::rest, x::y::stack' -> (match x, y with
                                | I x, I y -> if x = y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                | I x, N y -> (match (full_extract (N y) memory) with
                                                | I i -> if x = i then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, I y -> (match (full_extract (N x) memory) with
                                                | I i -> if i = y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract (N x) memory), (full_extract (N y) memory) with
                                                | I x, I y -> if x = y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                              )
  | Eq::rest, x::stack' -> eval rest (Error::stack) memory
  | Eq::rest, [] -> eval rest (Error::stack) memory

  | Lte::rest, x::y::stack' -> (match x, y with
                                | I x, I y -> if x <= y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                | I x, N y -> (match (full_extract (N y) memory) with
                                                | I i -> if x <= i then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, I y -> (match (full_extract (N x) memory) with
                                                | I i -> if i <= y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract (N x) memory), (full_extract (N y) memory) with
                                                | I x, I y -> if x <= y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                               )
  | Lte::rest, x::stack' -> eval rest (Error::stack) memory
  | Lte::rest, [] -> eval rest (Error::stack) memory

  | Lt::rest, x::y::stack' -> (match x, y with
                                | I x, I y -> if x < y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                | I x, N y -> (match (full_extract (N y) memory) with
                                                | I i -> if x < i then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, I y -> (match (full_extract (N x) memory) with
                                                | I i -> if i < y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract (N x) memory), (full_extract (N y) memory) with
                                                | I x, I y -> if x < y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                              )
  | Lt::rest, x::stack' -> eval rest (Error::stack) memory
  | Lt::rest, [] -> eval rest (Error::stack) memory

  | Gte::rest, x::y::stack' -> (match x, y with
                                | I x, I y -> if x >= y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                | I x, N y -> (match (full_extract (N y) memory) with
                                                | I i -> if x >= i then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, I y -> (match (full_extract (N x) memory) with
                                                | I i -> if i >= y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract (N x) memory), (full_extract (N y) memory) with
                                                | I x, I y -> if x >= y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                              )
  | Gte::rest, x::stack' -> eval rest (Error::stack) memory
  | Gte::rest, [] -> eval rest (Error::stack) memory

  | Gt::rest, x::y::stack' -> (match x, y with
                                | I x, I y -> if x > y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                | I x, N y -> (match (full_extract (N y) memory) with
                                                | I i -> if x > i then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, I y -> (match (full_extract (N x) memory) with
                                                | I i -> if i > y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _ -> eval rest (Error::stack) memory
                                              )
                                | N x, N y -> (match (full_extract (N x) memory), (full_extract (N y) memory) with
                                                | I x, I y -> if x > y then eval rest (B (true)::stack') memory else eval rest (B (false)::stack') memory
                                                | _, _ -> eval rest (Error::stack) memory
                                              )
                                | _, _ -> eval rest (Error::stack) memory
                              )
  | Gt::rest, x::stack' -> eval rest (Error::stack) memory
  | Gt::rest, [] -> eval rest (Error::stack) memory

  | Bnd::rest, x::y::stack' -> (match x, y with
                                | N x, I y -> eval rest (Unit::stack') ([(N x, I y)] @ memory)
                                | N x, B y -> eval rest (Unit::stack') ([(N x, B y)] @ memory)
                                | N x, S y -> eval rest (Unit::stack') ([(N x, S y)] @ memory)
                                | N x, Unit -> eval rest (Unit::stack') ([(N x, Unit)] @ memory)                               
                                | N x, N y -> if ((fetch (N y) memory)) = None then eval rest (Error::stack) memory else eval rest (Unit::stack') ([(N x, extract_val (fetch (N y) memory))] @ memory)
                                | _, _ -> eval rest (Error::stack) memory
                               )
  | Bnd::rest, x::stack' -> eval rest (Error::stack) memory
  | Bnd::rest, [] -> eval rest (Error::stack) memory

  | Begin_End commands::rest, stack' -> (match eval commands stack' memory with
                                          | h::t -> eval rest (h::stack') memory
                                          | _ -> eval rest (Error::stack) memory
                                        )    

  | IfThen (test_comm, true_comm, false_comm)::rest, stack' -> (match eval test_comm stack' memory with
                                                                | B h::t -> if h = true then eval (true_comm) stack' memory else eval false_comm stack' memory
                                                                | N h::t -> (match (full_extract_bool (N h) memory) with
                                                                              | B x -> if x = true then eval (true_comm) stack' memory else eval false_comm stack' memory
                                                                              | _ -> eval rest (Error::stack) memory)                                                         
                                                                | _ -> eval rest (Error::stack) memory
                                                              )                                                          
  
  | TryWith (try_comms, with_comms)::rest, stack' -> (match trywith_helper (eval try_comms stack' memory) with
                                                      | Error -> if (eval with_comms stack' memory) = [] then eval rest (stack') memory else
                                                      eval rest (List.hd (eval with_comms stack' memory)::stack') memory
                                                      | _ -> if (eval try_comms stack' memory) = [] then eval rest stack' memory else
                                                      eval rest (List.hd (eval try_comms stack' memory)::stack') memory
                                                    )
                                                              
  | Func (fname, arg, commands)::rest, stack' -> eval rest (Unit::stack') ([N fname, Closure (arg, commands, memory)] @ memory) 

  | Call::rest, x::y::stack' -> (match x, y with
                                  (* | arg_val, N fname -> eval ((command_closure_extract (N fname) memory) @ [Exit] @ rest) stack' 
                                  ((([arg_closure_extract (N fname) memory, arg_val]) @ (env_closure_extract (N fname) memory))@[(Error, Error)]@memory) *)

                                  | arg_val, N fname -> eval rest 
                                  (([List.hd (eval (command_closure_extract (N fname) (memory)) stack' (([arg_closure_extract (N fname) memory, arg_val]) @ (env_closure_extract (N fname) memory) @ memory))]) @ stack') memory

                                  | _, _ -> eval rest (Error::stack) memory
                                ) 

  | Call::rest, x::stack' -> eval rest (Error::stack) memory
  | Call::rest, [] -> eval rest (Error::stack) memory

  | Ret::rest, x::stack' -> (match x with
                          | N x -> if ((fetch (N x) memory)) = None then (N x::stack)
                          else (extract_val (fetch (N x) memory)::stack')

                          | _ -> ([x])                      
                         )
  | Ret::rest, [] -> (Error::stack)

  | Quit::rest, stack' -> stack'
  | _, stack' -> stack'
  
(*************************************************************************************************)
(* INTERPRETER FUNCTION *)
(*************************************************************************************************)

let interpreter (inputFile : string) (outputFile : string) : unit = 
  let string_list = token_parser inputFile in 
  let output = (eval ((command_parser (parse (string_list2string string_list)))) [] []) in
  write_list_str (outputFile) (stack2string output [])