let square =
  let open While in
  let* x = new_var "x" (i 14) in
  let* tmp = new_var "tmp" (var x) in
  let* y = new_var "y" (i 0) in
  while_ (var x =/= i 0) (y <-- var y + var tmp & x <-- var x - i 1)

let () = While.run_and_print_store square
