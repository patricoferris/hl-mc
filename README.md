Hoare Logic and Model Checking
------------------------------

Some bits and pieces for the [Hoare Logic and Model Checking](https://www.cl.cam.ac.uk/teaching/2324/HLog+ModC/)
course at University of Cambridge.

An example program to calculate the square of a given number.

```ocaml
# let square n =
  let open While in
  let* x = new_var "x" (i n) in
  let* tmp = new_var "tmp" (var x) in
  let* y = new_var "y" (i 0) in
  while_ (var x =/= i 0) (
    y <-- var y + var tmp & 
    x <-- var x - i 1
  )
val square : int -> unit While.t = <fun>
```

Which can be evaluated.

```ocaml
# While.run_and_print_store Format.str_formatter (square 9);;
- : unit = ()
# print_endline @@ Format.flush_str_formatter ();;
Program exited with ()
STORE:
x=0
tmp=9
y=81

- : unit = ()
```

