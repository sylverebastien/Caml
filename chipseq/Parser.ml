open Core.Std

module Lines : sig

type state
val initial_state : state
val step : string option -> state -> state * string list

end

= struct

type state =
	| Current_line of string
	| Finished

let initial_state = Current_line ""

let step sthg st = match sthg, st with
 | _, Finished -> (Finished, [])
 | None, Current_line l -> (Finished, [l])
 | Some sentence, Current_line l ->
  match String.rsplit2 sentence ~on:'\n' with
   | None -> (Current_line (l ^ sentence), [])
   | Some (b, a) -> 
     match String.split b ~on:'\n' with
     | [] -> assert false (* [split] never returns an empty list *)
     | h :: t -> (Current_line a, (l ^ h) :: t)

end


