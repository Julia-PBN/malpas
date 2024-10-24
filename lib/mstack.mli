type stack
exception Stack_underflow
exception Stack_overflow

val make : int -> stack
(** [make size] create a stack of size [size] *)

val push : int -> stack -> unit
(** [push value stack] push a value [value] into stack [stack]. *)

val read : int -> stack -> int
(** [read offset stack] read value at position [top - offset]. 
    Negative offset aren't supported, and OOB may arise.*)

val pop : stack -> int
(** [pop stack] pops the topmost value out of the stack and returns it. *)

val dup : stack -> unit
(** [dup stack] push the topmost value again.*)

val swap : stack -> unit
(** [swap stack] swaps the two topmost values.*)