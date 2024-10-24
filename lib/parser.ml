type identifier = string 

type atom = 
  | Bool 
  | Int 
  | Unit 
and function_type = {
  left : mtype;
  right : mtype
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

