module Xchg = struct
  open Effect
  type _ Effect.t += Xchg : int -> int t

  (* from the Effect module: *)
  (* type 'a t = 'a eff = .. *)
  (* external perform : 'a t -> 'a = "%perform" *)

  (* unit -> int *)
  (* Exception: Stdlib.Effect.Unhandled(Lib.Xchg.Xchg(1)) *)
  let comp' () = perform (Xchg 0) + perform (Xchg 1)
end

module WhyNotJustDoThis = struct
  type t =
    | Xchg of int

  (* t -> int *)
  let handle = function
    | Xchg n -> n + 1

  (* unit -> int *)
  let comp' () = handle (Xchg 0) + handle (Xchg 1)

  (* the answer seems to be that you don't get the k *)
end

module Simple = struct
  include Xchg
  open Effect.Deep

  (* unit -> int *)
  let comp () =
    try comp' () with
    | effect (Xchg n), k -> continue k (n + 1)

  (* unit -> int *)
  (* Exception: Stdlib.Effect.Continuation_already_resumed. *)
  let double_k_comp () =
    try comp' () with
    | effect (Xchg n), k -> continue k (n + 1) + continue k (n - 1)

  (* unit -> int *)
  let no_k_comp () =
    try comp' () with
    | effect (Xchg n), _ -> n + 1
end

module MessagePassing = struct
  include Xchg
  open Effect.Deep

  type 'a status =
    | Complete of 'a
    | Suspended of { msg : int
                   ; cont : (int, 'a status) continuation }
end
