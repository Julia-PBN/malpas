# suppose that the type of tokens is:
# struct Token
#   keyword::Bool
#   begin_pos::Int
#   end_pos::Int
#   token::TokenType
#end

@enum TypeAtom begin
    BOOL 
    INT 
    UNIT  # the ()
end
  
struct TypeFn # the A -> B
  left::Type # A 
  right::Type # B
end

const Type = Union{TypeAtom, TypeFn} # Union is basically a sum type between what's in the {}s

struct ParseFn  # when you're declaring a function
  t::Type # the return type of the function
  args::Vector{Tuple{Identifier, Type}}
  environment::Vector{Statement} # between the is and begin, to create closures or variables
  body::Expression
end

struct ParseIf
  cond::Expression
  condtrue::Expression
  condfalse::Expression
end

struct FnCall # when you're calling a function with `f a b` syntax
  fn::Expression # because you could do (f a) b and (f a) is an exmpression
  args::Vector{Expression} # Vector = list
end

struct Identifier # variables, kinda
  value::String
end

@enum BuiltIn begin
  BOOL_OR # ||
  BOOL_AND # &&
  EQ # =
  NE # !=
  GT # >
  GE # >= 
  LT # < 
  LE # <=
  INT_PLUS # +
  INT_MINUS # -
  INT_MUL # *
  INT_DIV # /
  INT_MOD # %
  INT_AND # &
  INT_OR # |
  INT_XOR # ^
end

@enum AtomBuiltIn begin
  UNIT_ELEM
end

Expression = Union{ParseFn, ParseIf, FnCall, Identifier, BuiltInt, AtomBuiltIn}

struct LetStatement # let a = b 
  name::String # the 'a'
  value::Expression # the 'b'
end

Statment = Union{LetStatement} # we may add more in the future ?

get_str(source::String, token::Token) = source[token.start_pos:token.end_pos]

  # THE MAIN FUNCTION TO BE CALLED !!!
function parse(source::String, tokens::Vector{Token})
  expr, i = parse_statements(source, tokens)
  @assert tokens[i].token == EOF
  return expr
end

  # I'll stop to do type annotation as I think you know now which types are what
function parse_statements(source, tokens)
  pos = 1
  stmts = Vector{Statement}() # empty vector of statements
  while tokens[i].token == Let 
    st, i = parse_let(source, tokens, i)
    push!(stmts, st) # add the new statement to the list
  end
  return stmts, i
end

function parse_let(source, tokens, i)
  @assert tokens[i].token == Let  # you'd normally address it gracefully with helper functions, but this is clearer for a rewrite imo
  @assert tokens[i+1].token == Ident
  @assert tokens[i+2].token == Equal
  expr, i = parse_expr(source, tokens, i+3)
  let_statement = LetStatement(get_str(source, tokens[i+1]), expr)
  return let_statement, i
end
  
function parse_expr(source, tokens, i)
  return parse_bool_eq(source, tokens, i)
end

function parse_bool_eq(source, tokens, i)
  expr, i = parse_bool_or(source, tokens, i)
  if tokens[i].token in [Equal, NotEqual]
    builtin = if tokens[i].token == Equal
      EQ 
    else # == NotEqual
      NE
    end
    right, i = parse_bool_eq(source, tokens, i+1)
    return FnCall(builtin, [expr, right]), i
  else
    return expr, i
  end
end

  function parse_bool_or(source, tokens, i)
    expr, i = parse_bool_and(source, tokens, i)
    if tokens[i].token == Or
      right, i = parse_bool_or(source, tokens, i+1)
      return FnCall(BOOL_OR, [expr, right]), i
    else
      return expr, i
    end
  end

  function parse_bool_and(source, tokens, i)
    expr, i = parse_ineq(source, tokens, i)
    if  tokens[i].token == And 
      right, i = parse_bool_and(source, tokens, i+1)
      return FnCall(BOOL_AND, [expr, right]), i
    else
      return expr, i
    end
  end

  function parse_ineq(source, tokens, i)
    expr, i = parse_min(source, tokens, i)
    if tokens[i].token in [More, Less, MoreEqual, LessEqual]
      t = tokens[i].token
      builtin = if t == More
        GT
      elseif t == MoreEqual
        GE
      elseif t == Less
        LT
      else # t == LessEqual
        LE
      end
      right, i =  parse_ineq(source, tokens, i+1)
      return FnCall(builtin, [expr, right]), i
    else 
      return expr, i
    end
  end

function parse_min(source, tokens, i) 
  expr, i = parse_plus(source, tokens, i)
  while tokens[i].token == Minus # it REQUIRE to be left associative, so it's not the same form
    exprright, i = parse_plus(source, tokens, i+1)
    expr = FnCall(INT_MINUS, [expr, exprright])
  end
  return expr, i
end

function parse_plus(source, tokens, i)
  expr, i = parse_mul(source, tokens, i)
  if tokens[i].token == Mul 
    right, i = parse_plus(source, tokens, i+1)
    return FnCall(INT_PLUS, [expr, right]), i
  else
    return expr, i
  end
end

function parse_mul(source, tokens, i) # * and / and % have same priority, left associative
  expr, i = parse_int_or(source, tokens, i)
  while tokens[i].token in [Mul, Div, Mod]
    t = tokens[i].token
    builtin = if t == Mul
      INT_MUL
    elseif t == Div
      INT_DIV
    else
      INT_MOD
    end
    right, i = parse_int_or(source, tokens, i+1)
    expr = FnCall(builtin, [expr, right]) 
  end
  return expr, i
end

function parse_int_or(source, tokens, i)
  expr, i = parse_int_xor(source, tokens, i)
  if tokens[i].token == Bar
    right, i = parse_or(source, tokens, i+1)
    return FnCall(INT_OR, [expr, right]), i
  else
    return expr, i
  end
end

function parse_int_xor(source, tokens, i)
  expr, i = parse_int_and(source, tokens, i)
  if tokens[i].token == Caret 
    right, i = parse_xor(source, tokens, i+1)
    return FnCall(INT_XOR, [expr, right]), i
  else
    return expr, i
  end
end

function parse_int_and(source, tokens, i)
  expr, i = parse_fncall(source, tokens, i)
  if tokens[i].token == Ampersand
    right, i = parse_int_and(source, tokens, i+1)
    return FnCall(INT_AND, [expr, right]), i
  else
    return expr, i
  end
end

function parse_fncall(source, tokens, i)
  fn, i = parse_unary(source, tokens, i)
  if !(tokens[i].token in [EOF, Let, RParen, End])
    args = Vector{Expression}()
    while !(tokens[i].token in [EOF, Let, RParen, End])
      expr, i = parse_unary(source, tokens, i)
      push!(args, i)
    end
    return FnCall(fn, args), i
  else
    return fn, i 
  end
end

function parse_unary(source, tokens, i)
  if tokens[i].token in [Minus, Bang]
    builtin = if tokens[i].token == Minus
      INT_MINUS
    else # == Bang
      BOOL_NOT
    end
    value, i = parse_unary(source, tokens, i+1)
    FnCall(builtin, [value]), i
  else
    parse_atom(source, tokens, i)
  end
end

function parse_atom(source, tokens, i)
  t = tokens[i].token
  if t == LParen
    value, i = parse_expr(source, tokens, i+1)
    @assert tokens[i].token == RParen
    return value, i+1 # +1 for the ')' token
  elseif t == Ident
    return Identifier(get_str(source, tokens[i])), i+1
  elseif t == If 
    return parse_if(source, tokens, i)
  elseif t == Function
    return parse_fn(source, tokens, i)
  elseif t == ElemUnit
    return UNIT_ELEM, i+1
  end
  throw("non atom found") 
end

function parse_if(source, tokens, i)
  @assert tokens[i].token in [If, Elseif]
  cond, i = parse_expr(source, tokens, i+1)
  @assert tokens[i].token == Then
  if_true, i = parse_expr(source, tokens, i)
  @assert tokens[i].token in [Else, Elseif]
  if tokens[i].token == Elseif
    if_false, i = parse_if(source, tokens, i+1)
    return ParseIf(cond, if_true, if_false), i
  else
    if_false, i = parse_expr(source, tokens, i+1)
    @assert tokens[i].token == End
    return ParseIf(cond, if_true, if_false), i+1
  end
end

function parse_fn(source, tokens, i)
  @assert tokens[i].token == Function
  i += 1
  @assert tokens[i].token == Function
  i += 1
  args = Vector{Tuple{Identifier, Type}}()
  while tokens[i].token != RParen
    @assert tokens[i].token == Ident
    argname = Identifier(get_str(source, tokens[i]))
    i += 1
    @assert tokens[i].token == Colon
    argtype, i = parse_type(source, tokens, i+1)
    @assert tokens[i].token in [RParen, Comma]
    push!(args, (argname, argtype))
  end
  @assert tokens[i] .type == Colon
  fn_type, i = parse_type(source, tokens, i+1)
  @assert tokens[i].token == Is
  environment, i = parse_statements(source, tokens, i+1)
  @assert tokens[i].token== Begin
  fn_expr, i = parse_expr(source, tokens, i+1)
  @assert tokens[i].token == End
  return ParseFn(fn_type, args, environment, fn_expr), i+1
end

function parse_type(source, tokens, i)
  parse_type_impl(source, tokens, i)
end

function parse_type_impl(source, tokens, i)
  t, i = parse_type_atom(source, tokens, i)
  if tokens[i].token == Arrow
    right, i = parse_type_impl(source, tokens, i+1)
    return TypeFn(t, right), i
  else
    return t, i
  end
end

function parse_type_atom(source, tokens, i)
  t = tokens[].token
  return if t == Bool# this `return` is useless, as most of the ones I've wrote, but I'm hoping that it makes it more readable for you
    BOOL, i+1
  elseif t == Int
    INT, i+1
  elseif t == LParen
    type, i = parse_type(source, tokens, i+1)
    @assert tokens[i].token == RParen
    return type, i+1
  elseif t == TypeUnit
    UNIT, i+1
  end
  throw("type not recognized at position ", tokens[i].position)
end
