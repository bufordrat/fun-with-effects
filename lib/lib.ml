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
  open Effect
  open Effect.Deep

  type 'a status =
    | Complete of 'a
    | Suspended of { msg : int ;
                     cont : (int, 'a status) continuation }

  let step (f : unit -> 'a) () : 'a status =
    match f () with
    | v -> Complete v
    | effect (Xchg msg), cont ->
       (* print_endline (string_of_int msg) ; *)
       Suspended { msg ; cont }

  let rec run_both a b =
    match a (), b () with
    | Complete va, Complete vb -> (va, vb)
    | Suspended { msg = m1 ;
                  cont = k1 ; },
      Suspended { msg = m2 ;
                  cont = k2 } ->
       run_both
         (fun () -> continue k1 m2)
         (fun () -> continue k2 m1)
    | _ -> failwith "Improper synchronization"

  let comp1 () = perform (Xchg 0) + perform (Xchg 1)
  let comp2 () = perform (Xchg 21) * perform (Xchg 21)

  let comp3 () =
    let zero = perform (Xchg 0) in
    let one = perform (Xchg 1) in
    zero + one
  
  let comp4 () =
    let twenty_one1 = perform (Xchg 21) in
    let twenty_one2 = perform (Xchg 21) in
    twenty_one1 * twenty_one2

  let rec run_both' a b =
    match a (), b () with
    | Complete va, Complete vb ->
       (* Printf.printf "complete 1! %s\ncomplete 2! %s\n" (string_of_int va) (string_of_int vb) ; *)
       (va, vb)
    | Suspended { msg = m1 ;
                  cont = k1 ; },
      Suspended { msg = m2 ;
                  cont = k2 } ->
       (* print_endline (string_of_int m1) ; *)
       (* print_endline (string_of_int m2) ; *)
       run_both'
         (fun () -> continue k1 m2)
         (fun () -> continue k2 m1)
    | _ -> failwith "Improper synchronization"
end

module Examples = struct
  open Effect
  type _ Effect.t += Wzap : int -> int t

  (* let left_wzap () = perform (Wzap 2) + 3 *)
  (* let right_wzap () = 3 + perform (Wzap 2) *)

  (* open Effect.Deep *)

  (* let run computation () = *)
  (*   match computation () with *)
  (*   | effect (Wzap n), k -> continue k n *)
  (*   | other -> other *)
  
  (* let run' computation () = *)
  (*   match computation () with *)
  (*   | effect (Wzap n), _ -> n *)
  (*   | other -> other *)
end

module Scheduler = struct
  open Effect
  open Effect.Deep
  type _ Effect.t +=
     | Xchg : int -> int t
     | Fork : (unit -> unit) -> unit t
     | Yield : unit t

  let fork f = perform (Fork f)
  let yield () = perform Yield
  let xchg v = perform (Xchg v)

  let run main =
    let exchanger
        : (int * (int, unit) continuation) option ref
      = ref None
    in
    let run_q = Queue.create () in
    let enqueue k v =
      let task () = continue k v in
      Queue.push task run_q
    in
    let dequeue () =
      if Queue.is_empty run_q
      then ()
      else 
        let task = Queue.pop run_q in
        task ()
    in
    let rec spawn f =
      match f () with
      | () -> dequeue ()
      | exception e ->
         print_endline (Printexc.to_string e) ;
         dequeue ()
      | effect Yield, k -> enqueue k () ;
                           dequeue ()
      | effect (Fork f), k -> enqueue k () ;
                              spawn f
      | effect (Xchg n), k ->
         begin
           match !exchanger with
           | Some (n', k') -> exchanger := None ;
                              enqueue k' n ;
                              continue k n'
           | None -> exchanger := Some (n, k) ;
                     dequeue ()
         end
    in
    spawn main

  let doit _ =
    run
      (fun _ ->
        fork (fun _ ->
            Printf.printf "[t1] Sending 0\n" ;
            let v = xchg 0 in
            Printf.printf "[t1] received %d\n" v ;) ;
        fork (fun _ ->
            Printf.printf "[t2] Sending 1\n" ;
            let v = xchg 1 in
            Printf.printf "[t2] received %d\n" v ;) ;
      )
end

module Group = Group
module Wzap = Wzap
