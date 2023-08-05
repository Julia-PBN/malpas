open Lexer

(*
program = eval*
eval = statement
statement = let-statement
let-statement = LET IDENTIFIER EQUAL expression
expression = or

or = and (BAR_BAR and)*
and = ineq (AND_AND ineq)*
ineq = min ((LT | LT_EQ | GT | GT_EQ) min)*
min = plus (MINUS plus)*
plus = mul ((PLUS | MINUS) mul )*
mul = (bitwise ((STAR|DIV|MODULO))* bitwise
bitwise = fn-call ((BAR|AND) fncall)*
fn-call = unary+
unary = (BANG|MINUS|PLUS)* atom

atom = function | IDENTIFIER | INT | BOOL | "(" expression ")" | BUILT-IN-FUNCTION | if-expr

function: parse_fn = FUNCTION IDENTIFIER LPAREN fn-args RPAREN COLON type IS statement* BEGIN expression END
if-expr: parse_if = IF expression THEN expression (ELIF expression) ELSE expression END

*)

(*
record parse_fn {
  name : String,
  type : type_def
  arity: Int (can be found by the type definition)
  body: expression
}
 *)
(*
record fn_call {
  fn: parse_fn (we'll see later how to store it nicely)
  args: List<expression>
}
*)
(*
record parse_if {
  cond: expression
  if-true: expression
  else: expression
}
*)
type type_atom =
  | Bool
  | Int
  | Unit

type type_fn =
  {
    left: type_type;
    right: type_type
  }
  and type_type = 
    | Atom of type_atom
    | Fn of type_fn

type parse_fn_t =
  {
    t: type_type;
    arity: Int;
    body: expression
  }

  and parse_if_t =
  {
    cond: expression;
    condtrue: expression;
    condfalse: expression
  }

  and fn_call_t =
  {
    fn: expression;
    args: expression list
  }

  and parser_iter_t =
  {
    tokens: token List;
    position: Int
  }

  and let_statement_t =
  {
    name: String;
    value: expression
  }

  and built_in_t =
  | Bool_Or
  | Bool_And
  | EQ
  | NE
  | GT
  | GE
  | LE
  | LT
  | Int_Plus
  | Int_Mul
  | Int_Div
  | Int_Time
  | Int_And
  | Int_Or

  and:w expression =
  | Function of parse_fn_t
  | Identifier of Ident
  | Call of fn_call_t
  | Built_in of built_in_t

type statement =
  | Let of let_statement_t

let program_t = statement list

fun get_tokens (t: Lexer)  = 
  {
    tokens: t.tokens,
    position: 1
  }

fun parse_program = 

fun parse t =
  parse_program (get_tokens t)

