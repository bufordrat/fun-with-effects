type _ eff +=
   | Xchg : int -> int eff
   | Yield : unit eff
   | Fork : (unit -> unit) -> unit eff

open Effect

let fork f = perform (Fork f)
let yield () = perform Yield
let xchg n = perform (Xchg n)

open Effect.Deep

let scheduler main =
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
    else let task = Queue.pop run_q in
         task ()
  in
  let rec spawn f =
    match f () with
    | () -> dequeue ()
    | exception e ->
       print_endline (Printexc.to_string e) ;
       dequeue ()
    | effect Yield, k ->
       enqueue k () ;
       dequeue ()
    | effect (Fork f), k ->
       enqueue k () ;
       spawn f
    | effect (Xchg n), k ->
       match !exchanger with
       | Some (n', k') -> exchanger := None ;
                          enqueue k' n ;
                          continue k n'
       | None -> exchanger := Some (n, k) ;
                 dequeue ()
  in
  spawn main

let doit () =
  let print1 _ =
    Printf.printf "[t1] Sending 0\n" ;
    let v = xchg 0 in
    Printf.printf "[t1] received %d\n" v
  in
  let print2 _ =
    Printf.printf "[t2] Sending 1\n" ;
    let v = xchg 1 in
    Printf.printf "[t2] received %d\n" v
  in
  scheduler (fun _ -> fork print1 ; fork print2)
