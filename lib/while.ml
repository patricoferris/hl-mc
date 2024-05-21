(* The WHILE language tessed as a GADT
   in OCaml. With all those [unit]s you can
   see it is pretty imperative.

   We split the pure tessions into their own
   type. *)
type _ value =
  | Int : int -> int value
  | True : bool value
  | False : bool value
  | Unit : unit value

(* Type witnesses for variables *)
module Variables = struct
  type 'a t = { info : string; witness : 'a value Type.Id.t }

  let v info =
    let witness = Type.Id.make () in
    { info; witness }

  let equal : type a b. a t -> b t -> bool * (a value, b value) Type.eq option =
   fun a b -> (a.info = b.info, Type.Id.provably_equal a.witness b.witness)
end

type _ t =
  | Skip : unit t
  | Seq : unit t * unit t -> unit t
  | Assign : 'a variable * 'a t -> unit t
  | If_then_else : bool t * 'a t * 'a t -> 'a t
  | While : bool t * unit t -> unit t
  | Var : 'a variable -> 'a t
  | Add : int t * int t -> int t
  | Sub : int t * int t -> int t
  | Equals : int t * int t -> bool t
  | Not : bool t -> bool t
  | Value : 'a value -> 'a t

and 'a variable = 'a Variables.t

type hidden = H : 'a Variables.t * 'a value -> hidden

let pp_value : type a. Format.formatter -> a value -> unit =
 fun ppf -> function
  | Int i -> Format.pp_print_int ppf i
  | True -> Format.pp_print_string ppf "true"
  | False -> Format.pp_print_string ppf "false"
  | Unit -> Format.pp_print_string ppf "()"

module Store : sig
  val add : 'a Variables.t -> 'a value -> unit
  val get : 'a Variables.t -> 'a value
  val iter : (hidden -> unit) -> unit
end = struct
  let x = ref []

  let rec add' : type a. a Variables.t -> a value -> hidden list -> hidden list
      =
   fun k v -> function
    | [] -> [ H (k, v) ]
    | H (k', v') :: rest -> (
        match Variables.equal k k' with
        | true, Some Type.Equal -> H (k, v) :: rest
        | _ -> H (k', v') :: add' k v rest)

  let add : type a. a Variables.t -> a value -> unit =
   fun k v -> x := add' k v !x

  let rec get' : type a. a Variables.t -> hidden list -> a value =
   fun k -> function
    | [] -> raise Not_found
    | H (k', v) :: rest -> (
        match Variables.equal k k' with
        | true, Some Type.Equal -> v
        | _ -> get' k rest)

  let get k = get' k !x
  let iter f = List.iter f !x
end

let rec eval : type a. a t -> a value = function
  | Skip -> Unit
  | Seq (e1, e2) ->
      let Unit = eval e1 in
      eval e2
  | Assign (key, e1) ->
      Store.add key (eval e1);
      Unit
  | While (b_t, t) as e ->
      if eval b_t = True then
        let Unit = eval t in
        eval e
      else Unit
  | If_then_else (b, e1, e2) -> if eval b = True then eval e1 else eval e2
  | Var var -> Store.get var
  | Add (i1, i2) ->
      let (Int v1) = eval i1 in
      let (Int v2) = eval i2 in
      Int (v1 + v2)
  | Sub (i1, i2) ->
      let (Int v1) = eval i1 in
      let (Int v2) = eval i2 in
      Int (v1 - v2)
  | Equals (i1, i2) ->
      let (Int v1) = eval i1 in
      let (Int v2) = eval i2 in
      if Int.equal v1 v2 then True else False
  | Not b -> if eval b = True then False else True
  | Value v -> v

(* OCaml tessions for specifying a little program *)
let skip = Skip
let while_ b t = While (b, t)
let if_then_else b e1 e2 = If_then_else (b, e1, e2)
let i int = Value (Int int)
let var e = Var e
let ( <-- ) k v = Assign (k, v)
let ( + ) i1 i2 = Add (i1, i2)
let ( - ) i1 i2 = Sub (i1, i2)
let ( === ) a b = Equals (a, b)
let ( =/= ) a b = Not (Equals (a, b))
let ( & ) a b = Seq (a, b)
let ( let* ) (s, e) b = Seq (e, b s)

let new_var s v =
  let s = Variables.v s in
  let t = Assign (s, v) in
  (s, t)

let run_and_print_store ppf prog =
  let v = eval prog in
  Format.fprintf ppf "Program exited with %a\n" pp_value v;
  Format.fprintf ppf "STORE:\n";
  Store.iter (fun (H (k, v)) -> Format.fprintf ppf "%s=%a\n" k.info pp_value v)
