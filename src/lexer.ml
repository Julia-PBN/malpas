type position =
  {
    filename : string;
    line : int;
    offset_line : int;
    offset_pos : int;
  }

let inc_pos i pos = 
  {pos with
     offset_pos = pos.offset_pos + i
  }
 
let inc_line pos =
  {pos with
     line = pos.line + 1;
     offset_line = pos.offset_pos + 1
  } |> inc_pos 1

let dec_pos i pos =
  {pos with 
     offset_pos = pos.offset_pos - i
  }

type token_type =
  | Let
  | Function
  | Begin
  | End
  | Is
  | If
  | Elseif
  | Then
  | Else
  | True
  | False
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Equal 
  | LessEqual
  | MoreEqual
  | Less
  | More
  | NotEqual
  | Ampersand
  | And
  | Or
  | Bang
  | LParen
  | RParen
  | Comma
  | Colon
  | Ident of string
  | Int of int 
  | EOF

type token = 
  {
    keyword : bool;
    begin_pos : position;
    end_pos : position;
    token : token_type
  }

type source =
  | String of string

type lexer = 
  {
    source : source;
    cur_pos : position;
    tokens : token list;
    cur_lexeme : string;
    eof : bool;
    error : bool
  }

let zero_pos = 
  {
    filename = "";
    line = 0;
    offset_line = 0;
    offset_pos = 0;
  }

let of_string ~filename ~source = 
  { 
    source = String source;
    cur_pos = {zero_pos with filename};
    tokens = [];
    cur_lexeme = "";
    eof = false;
    error = false
  }

let cur_char lexer = 
  match lexer.source with
  | String s -> s.[lexer.cur_pos.offset_pos - 1]

let is_newline c = c = '\n'

let add_to_lexeme (c : char) lexer =
  {lexer with
     cur_pos = if is_newline c then inc_line lexer.cur_pos else inc_pos 1 lexer.cur_pos;
     cur_lexeme = lexer.cur_lexeme ^ (String.make 1 c)
  }
 
let add_token token_type is_keyword lexer =
  if lexer.error 
  then lexer
  else
  let token = 
    {
       keyword = is_keyword;
       begin_pos = dec_pos (String.length lexer.cur_lexeme) lexer.cur_pos;
       end_pos = lexer.cur_pos;
       token = token_type;
    } 
  in 
  {lexer with
     tokens = lexer.tokens @ [token];
     cur_lexeme = "";
  }

let is_letter c = 
    (Char.code 'a' <= Char.code c) && (Char.code c <= Char.code 'z')
    ||
    (Char.code 'A' <= Char.code c) && (Char.code c <= Char.code 'Z')
  
let is_digit c = Char.code '0' <= Char.code c && Char.code c <= Char.code '9'
  
let is_alphanum c = is_letter c || is_digit c
  
let is_whitespace c = List.mem c [' ' ; '\t']

let check_eof lexer = 
  let s = 
    match lexer.source with
    | String str -> str
  in
  if lexer.cur_pos.offset_pos = String.length s
  then {lexer with eof = true}
  else lexer

let advance_str lexer =
  let s = 
    match lexer.source with
    | String str -> str
  in
  if lexer.eof 
  then lexer
  else 
    let lexer = add_to_lexeme (s.[lexer.cur_pos.offset_pos]) lexer in
      check_eof lexer

let advance lexer =
  match lexer.source with
  | String _ -> advance_str lexer

let skip lexer = {lexer with cur_pos = inc_pos 1 lexer.cur_pos} |> check_eof

let newline lexer = {lexer with cur_pos = inc_line lexer.cur_pos} |> check_eof

let error lexer = {lexer with error = true}

let is_single_operator c = List.mem c ['+' ; '-' ; '*' ; '/' ; '%' ; '=' ; '<' ; '>' ; '!' ; '&']

let is_operator c = is_single_operator c || List.mem c ['|' ; '&']

let is_punctuation c = List.mem c ['(' ; ')' ; ',' ; ':']

let rec next_token lexer = 
  if lexer.eof
  then lexer
  else
    let next_lexer = advance lexer in
    let next_chr = cur_char next_lexer in
    match next_chr with
    | c when is_newline c -> next_token @@ newline lexer
    | c when is_whitespace c -> next_token @@ skip lexer
    | c when is_letter c -> next_ident_or_keyword next_lexer
    | c when is_digit c ->
        let int = next_int next_lexer in
          add_token (Int (int_of_string int.cur_lexeme)) false int
    | c when is_operator c -> next_full_operator next_lexer
    | c when is_punctuation c -> next_full_punctuation next_lexer
    | _ -> error lexer
and next_ident_or_keyword lexer =
  let ident = next_ident lexer in
  let add_keyword = (fun token -> add_token token true ident) in
  match String.lowercase_ascii ident.cur_lexeme with
  | "let" -> add_keyword Let
  | "function" -> add_keyword Function
  | "begin" -> add_keyword Begin
  | "end" -> add_keyword End
  | "is" -> add_keyword Is
  | "if" -> add_keyword If
  | "elseif" -> add_keyword Elseif
  | "then" -> add_keyword Then
  | "else" -> add_keyword Else
  | "true" -> add_keyword True
  | "false" -> add_keyword False
  | s -> add_token (Ident s) false ident
and next_ident lexer =
  if lexer.eof 
  then lexer
  else 
  let next_lexer = advance lexer in
  let next_chr = cur_char next_lexer in
  match next_chr with
  | c when not (is_alphanum c) -> lexer
  | c when is_alphanum c -> next_ident next_lexer
  | _ -> error lexer
and next_int lexer =
  if lexer.eof 
  then lexer
  else 
  let next_lexer = advance lexer in
  let next_chr = cur_char next_lexer in
  match next_chr with
  | c when not (is_digit c) -> lexer 
  | c when is_digit c -> next_int next_lexer
  | _ -> error lexer
and next_full_operator lexer = 
  let operator = next_operator lexer 1 in
  let add_keyword = (fun token -> add_token token true operator) in
  match operator.cur_lexeme with
  | "+" -> add_keyword Plus
  | "-" -> add_keyword Minus
  | "*" -> add_keyword Mul
  | "/" -> add_keyword Div
  | "%" -> add_keyword Mod
  | "=" -> add_keyword Equal
  | "<=" -> add_keyword LessEqual
  | ">=" -> add_keyword MoreEqual
  | "<" -> add_keyword Less
  | ">" -> add_keyword More
  | "<>" -> add_keyword NotEqual
  | "&" -> add_keyword Ampersand
  | "&&" -> add_keyword And
  | "||" -> add_keyword Or
  | "!" -> add_keyword Bang
  | _ -> error operator
and next_operator lexer op_len =
  if lexer.eof 
  then lexer
  else 
  let chr = cur_char lexer in 
  let next_lexer = advance lexer in
  let next_chr = cur_char next_lexer in
  match next_chr with
  | '>' when chr = '<' && op_len = 1 -> next_operator next_lexer 2
  | '|' when chr = '|' && op_len = 1 -> next_operator next_lexer 2
  | '&' when chr = '&' && op_len = 1 -> next_operator next_lexer 2
  | '=' when (chr = '<' || chr = '>') && op_len = 1 -> next_operator next_lexer 2
  | _ -> lexer
and next_full_punctuation lexer =
  let punctuation = next_punctuation lexer in
  let add_keyword = (fun token -> add_token token true punctuation) in
  match lexer.cur_lexeme with
  | "(" -> add_keyword LParen
  | ")" -> add_keyword RParen
  | "," -> add_keyword Comma
  | ":" -> add_keyword Colon
  | _ -> error punctuation
and next_punctuation lexer =
  if lexer.eof 
  then lexer
  else 
  let next_lexer = advance lexer in
  let next_chr = cur_char next_lexer in
  match next_chr with 
  | _ -> lexer 
