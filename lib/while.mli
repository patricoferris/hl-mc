type 'a t
(* A "while" program *)

type 'a variable
(* Variables *)

type 'a value
(* Values in While *)

val skip : unit t
(** Do nothing *)

val while_ : bool t -> unit t -> unit t
(** [while b e] evaluates [e] whilst [b] is [True] *)

val if_then_else : bool t -> 'a t -> 'a t -> 'a t
(** [if_then_else b e1 e2] if [e1] if [b] else [e2] *)

val i : int -> int t
(** Lifts OCaml integers into While integers *)

val var : 'a variable -> 'a t
(** Dereferences variables *)

val ( <-- ) : 'a variable -> 'a t -> unit t
(** Assignment *)

val ( + ) : int t -> int t -> int t
(** Addition *)

val ( - ) : int t -> int t -> int t
(** Subtraction *)

val ( === ) : int t -> int t -> bool t
(** Equality *)

val ( =/= ) : int t -> int t -> bool t
(** Not equality *)

val ( & ) : unit t -> unit t -> unit t
(* Sequencing operator *)

val ( let* ) : 'a variable * unit t -> ('a variable -> unit t) -> unit t
(* Let-binding, useful with {! new_var} *)

val new_var : string -> 'a t -> 'a variable * unit t
(* Create a new variable with an initial value *)

val run_and_print_store : Format.formatter -> 'a t -> unit
