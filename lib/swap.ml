type _ eff +=
   | Swap : int -> int eff

open Effect.Deep

type 'a status =
  | Complete of 'a
  | Suspended of { number : int ;
                   cont : (int, 'a status) continuation ; }

let freeze_effect computation () =
  match computation () with
  | effect (Swap n), k -> Suspended { number = n ; cont = k }
  | n -> Complete n

let rec scheduler comp1 comp2 =
  match comp1 (), comp2 () with
  | Complete x, Complete y -> x, y
  | Suspended { number = number1 ;
                cont = cont1 ; },
    Suspended { number = number2 ;
                cont = cont2 ; }
    -> scheduler
         (fun _ -> continue cont1 number2)
         (fun _ -> continue cont2 number1)
  | _ -> failwith "synchronization error"

open Effect

let comp1 () =
  perform (Swap 0) + perform (Swap 1)

let comp2 () =
  perform (Swap 21) * perform (Swap 21)

let doit () =
  scheduler (freeze_effect comp1) (freeze_effect comp2)
