type position = {
  filename : string;
  line : int;
  offset_line : int;
  offset_pos : int;
}

val inc_pos : int -> position -> position
val inc_line : position -> position
val dec_pos : int -> position -> position

type token_type =
    Let
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

type token = {
  keyword : bool;
  begin_pos : position;
  end_pos : position;
  token : token_type;
}

type source = String of string

type lexer = {
  source : source;
  cur_pos : position;
  tokens : token list;
  cur_lexeme : string;
  eof : bool;
  error : bool;
}

val zero_pos : position

val of_string : filename:string -> source:string -> lexer

val cur_char : lexer -> char

val advance : lexer -> lexer
val skip : lexer -> lexer
val error : lexer -> lexer

val next_token : lexer -> lexer
