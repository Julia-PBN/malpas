type stack = 
  {
    stack_array : int array;
    mutable top_pointer : int
  }

exception Stack_underflow
exception Stack_overflow

let make size = { stack_array = Array.make size (-1) ; top_pointer = 0 }

let push value stack =
  if stack.top_pointer = Array.length stack.stack_array
  then raise Stack_overflow
  else 
  stack.top_pointer <- stack.top_pointer + 1;
  stack.stack_array.(stack.top_pointer) <- value

let read offset stack = stack.stack_array.(stack.top_pointer - offset)

let pop stack = 
  if stack.top_pointer = 0
  then raise Stack_underflow
  else 
  let value = read 0 stack in
  stack.top_pointer <- stack.top_pointer - 1;
  value

let dup stack =
  let value = read 0 stack in
  push value stack

let swap stack =
  let first, second = read 0 stack, read 1 stack in
  stack.stack_array.(stack.top_pointer) <- second;
  stack.stack_array.(stack.top_pointer - 1) <- first;
  