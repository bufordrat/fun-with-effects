module WhyNotJustDoThis = struct
  type t =
    | Xchg of int

  let handle = function
    | Xchg n -> n + 1
            
  let comp' () = handle (Xchg 0) + handle (Xchg 1)
end

module Simple = struct
  open Effect
  open Effect.Deep

  type _ Effect.t += Xchg : int -> int t
  let comp' () = perform (Xchg 0) + perform (Xchg 1)
  let comp () =
    try comp' () with
    | effect (Xchg n), k -> continue k (n + 1)

  let double_k_comp () =
    try comp' () with
    | effect (Xchg n), k -> continue k (n + 1) + continue k (n - 1)

  let no_k_comp () =
    try comp' () with
    | effect (Xchg n), _ -> n + 1
end

module StringSimple = struct
  open Effect
  open Effect.Deep

  type _ Effect.t += Xchg : string -> string t
  let comp' () = perform (Xchg "hi") ^ perform (Xchg "keith")
  let comp () =
    try comp' () with
    | effect (Xchg n), k ->
       print_endline "AHAOHAOI" ;
       continue k (n ^ " stuff")

  let no_k_comp () =
    try comp' () with
    | effect (Xchg n), _ ->
       print_endline "OIHFOEWHFIEWOHF" ;
       n ^ " things"

  let let_comp' () =
    let x = perform (Xchg "hi") in
    let y = perform (Xchg "keith") in
    x ^ y

  let handler effectful =
    try effectful () with
    | effect (Xchg n), _ ->
       print_endline "OIHFOEWHFIEWOHF" ;
       n ^ " things"
end
