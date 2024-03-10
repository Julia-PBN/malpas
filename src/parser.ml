open Lexer

type identifier = string 

type atom = 
  | Bool 
  | Int 
  | Unit 
and function_type = {
  left : mtype
}
and mtype = 
  | Atom of atom 
  | Function of function_type

type function_declaration = 
  {
    return_type : mtype;
    parameters : (identifier * mtype) list;
    environment : statement list;
    body : expression;
  }
and if_expression = 
  {
    cond : expression;
    body_true : expression;
    body_false : expression;
  }
and function_call = 
  {
    function_qualifier : expression;
    arguments : expression list;
  }
and builtins = 
  | Bool_or | Bool_and 
  | Eq | Neq | Gt | Ge | Lt | Le 
  | Int_plus | Int_minus | Int_mul | Int_div
  | Int_mod | Int_and | Int_or | Int_xor
and atom_builtin = 
  | Unit_element
and expression = 
  | Function_declaration of function_declaration
  | If_expression of if_expression
  | Function_call of function_call 
  | Identifier of string 
  | Builtin of builtins
  | Atom_builtin of atom_builtin
and let_statement = {
  name : string; 
  value : expression
}
and statement = 
  | Let_statement of let_statement

let get_str source token = 
  match source with
  | String str -> String.sub str token.begin_pos.offset_pos token.end_pos.offset_pos

let parse_error message = print_endline message ; exit(-1)

let builtins_of_token_type token_type = 
  match token_type with 
  | Equal -> Eq 
  | NotEqual -> Neq
  | LessEqual -> Le 
  | MoreEqual -> Ge 
  | Less -> Lt 
  | More -> Gt 
  | Or -> Bool_or
  | And -> Bool_and
  | _ -> parse_error "invalid builtin"

(* parsing return the parsed program and the final lexer state *)
let rec parse filename source = 
  let lexer = of_string ~filename ~source in 
  let program, _ = parse_statements lexer in 
  program 
and parse_statements lexer = 
  let rec loop lexer statements =
    match lexer.tokens.(token_count lexer).token with 
    | Let -> 
        let parsed_let, lexer' = parse_let lexer in 
        loop lexer' (Array.append statements [|parsed_let|])
    | _ -> statements, lexer
  in loop (next_token lexer) [||]
and parse_let lexer = 
  let lexer'' = CCFun.(next_token %> next_token) lexer in 
  let tc = token_count lexer'' in 
  match lexer''.tokens.(tc - 2).token, lexer''.tokens.(tc - 1).token, lexer''.tokens.(tc).token with 
  | Let, Ident name, Equal -> 
    let parsed_expr, lexer' = parse_expr lexer in
    Let_statement {name ; value = parsed_expr}, lexer'
  | _ -> parse_error "invalid let expression"
and parse_expr lexer = parse_bool_eq lexer 
and parse_binop op_left op_right check lexer = 
  let left, lexer' = op_left lexer in 
  let lexer'' = next_token lexer' in
  let tc = token_count lexer'' in 
  let token = lexer''.tokens.(tc).token in
  if check(token) then
      let right, lexer''' = op_right lexer'' in
      Function_call {function_qualifier = Builtin (builtins_of_token_type token); arguments = [left ; right]}, lexer'''
  else left, lexer'
and parse_bool_eq lexer = 
  parse_binop parse_bool_or parse_bool_eq
  begin function 
    | Equal | NotEqual -> true 
    | _ -> false
  end
  lexer
and parse_bool_or lexer = 
  parse_binop parse_bool_and parse_bool_or
  begin function 
    | Or -> true 
    | _ -> false
  end 
  lexer
and parse_bool_and lexer = 
  parse_binop parse_ineq parse_bool_and
  begin function 
    | And -> true 
    | _ -> false
  end 
  lexer
and parse_ineq lexer = 
  parse_binop parse_min parse_ineq
  begin function 
    | More | Less | MoreEqual | LessEqual -> true 
    | _ -> false
  end 
  lexer
and parse_min lexer = 
  let left, lexer' = parse_mul lexer in 
  let rec loop left lexer = 
  let lexer'' = next_token lexer in
  let tc = token_count lexer'' in 
    match lexer''.tokens.(tc).token with 
    | (Minus | Plus) as token ->  
      let right, lexer''' = parse_mul lexer'' in
      loop (Function_call {function_qualifier = Builtin (builtins_of_token_type token); arguments = [left ; right]}) lexer'''
    | _ -> left, lexer'' in 
  loop left lexer'
and parse_mul lexer = 
  let left, lexer' = parse_int_or lexer in 
  let rec loop left lexer = 
    let lexer'' = next_token lexer in
    let tc = token_count lexer'' in 
    match lexer''.tokens.(tc).token with 
    | (Mul | Div | Mod) as token ->  
      let right, lexer''' = parse_int_or lexer'' in
      loop (Function_call {function_qualifier = Builtin (builtins_of_token_type token); arguments = [left ; right]}) lexer'''
    | _ -> left, lexer'' in 
  loop left lexer'
and parse_int_or = 
  parse_binop parse_int_xor parse_or
  begin function 
    |  -> true 
    | _ -> false
  end 
  lexer
