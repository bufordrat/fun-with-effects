type _ eff += Wzap : int -> int eff

open Effect

let example1 () = perform (Wzap 1) + 2

open Effect.Deep

let handle computation =
  match computation () with
  | effect (Wzap n), k -> continue k n
  | other -> other

let resume_twice computation =
  match computation () with
  | effect (Wzap n), k ->
     continue k n + continue k (n + 1)
  | other -> other

let dont_resume computation =
  match computation () with
  | effect (Wzap n), _ -> n
  | other -> other

let example2 () =
  perform (Wzap 3) + perform (Wzap 4)

let example3 () =
  let three = perform (Wzap 3) in
  let four = perform (Wzap 4) in
  three + four

