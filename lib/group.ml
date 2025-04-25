module Swap = struct
  open Effect
  type _ Effect.t +=
     | Xchg : int -> int t

  open Effect.Deep
  type 'a status =
    | Complete of 'a
    | Suspended of { msg : int ;
                     cont : (int, 'a status) continuation ; }

  let step computation () =
    match computation () with
    | effect (Xchg n), k -> Suspended { msg = n ; cont = k }
    | n -> Complete n

  let rec scheduler comp1 comp2 =
    match comp1 (), comp2 () with
    | Complete x, Complete y -> x, y
    | Suspended { msg = msg1 ;
                  cont = cont1 ; },
      Suspended { msg = msg2 ;
                  cont = cont2 ; }
      -> scheduler
           (fun _ -> continue cont1 msg2)
           (fun _ -> continue cont2 msg1)
    | _ -> failwith "synchronization error"

  let comp1 () =
    let zero = perform (Xchg 0) in
    let one = perform (Xchg 1) in
    zero + one

  let comp2 () =
    let x = perform (Xchg 21) in
    let y = perform (Xchg 21) in
    x * y

  let doit () =
    scheduler (step comp1) (step comp2)
end

module Threads = struct
  open Effect
    type _ Effect.t +=
     | Xchg : int -> int t
     | Yield : unit t
     | Fork : (unit -> unit) -> unit t

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
        Unix.sleep 2 ;
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
end
