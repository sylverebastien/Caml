open Core.Std

module Lines : sig

type state
val initial_state : state
val step : string option -> state -> state * string list

(*
val cut_step_init : string array
val cut_step_aux : string -> string array -> int -> string array
val cut_step : string list -> string array -> string array
val cut_line : string option -> string list
val cut_mline : string option list -> string list list -> string list array
val rplce_object : string option -> int -> string -> unit
*)


end = struct

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

(*

let cut_step_init = Array.create 9 "";;

let rec cut_step_aux line cut_step_init lvl =
  match String.lsplit2 line ~on:'\t' with
    | None -> cut_step_init
    | Some (b, a) ->
      cut_step_init.(lvl) <- b ;
      match String.mem a '\t' with
        | true -> cut_step_aux a cut_step_init (lvl + 1)
        | false -> cut_step_init.(lvl + 1) <- a ;
          cut_step_aux a cut_step_init (lvl + 1)


let rec cut_step mline cut_step_init = match mline with
  | [] -> cut_step_init
  | h :: t ->
    let cut_step_init = cut_step_aux h cut_step_init 0 in
    cut_step t cut_step_init


let test ="This\texpression\thas\ttype\tint\tbut\tan\texpression\twas";;
let test2 ="This2\texpression2\thas2\ttype2\tint2\tbut2\tan2\texpression2\twas2";;


let cut_line line = match line with
  | None -> []
  | Some line ->
    String.split line ~on:'\t'


let rec cut_mline mline res = match mline with
  | [] -> Array.of_list res
  | h :: t ->
    let res = res @ [cut_line h] in
    cut_mline t res


let rplce_object line pos mot =
  let change () = (Array.of_list (cut_line line)).(pos) <- mot in
  change ()

*)

end

module Tsv : sig

  val parse_line : string -> string array

end = struct

let parse_line l =
  String.split l ~on:'\t'
  |> Array.of_list

end



module Gff : sig

  type record = {
    seqid : string ;
    source : string ;
    feature : string ;
    pos_start : int ;
    pos_end : int ;
    score : float option ;
    strand : [ `Plus | `Minus | `Not_relevant | `Unknown ] ;
    phase : int option ;
    attributes : (string * string) list ;
  }
  and item = [
    | `Comment of string
    | `Record of record
  ]

  val parse_item : string -> item
  val unparse_item : item -> string

end
=
struct

  type record = {
    seqid : string ;
    source : string ;
    feature : string ;
    pos_start : int ;
    pos_end : int ;
    score : float option ;
    strand : [ `Plus | `Minus | `Not_relevant | `Unknown ] ;
    phase : int option ;
    attributes : (string * string) list ;
  }
  and item = [
    | `Comment of string
    | `Record of record
  ]

  let parse_strand = function
    | "." -> `Not_relevant
    | "?" -> `Unknown
    | "+" -> `Plus
    | "-" -> `Minus
    | _ -> assert false

  let parse_attributes attributes =
    let parse_att x =
      match String.lsplit2 ~on:'=' x with
      | None -> assert false
      | Some kv -> kv
    in
    String.split ~on:';' attributes
    |> List.map ~f:parse_att

  let parse_item line =
    match line with
    | "" -> assert false
    | _ ->
      if line.[0] = '#' then `Comment line
      else
          match String.split ~on:'\t' line with
          | [ seqid ; source ; feature ; pos_start ; pos_end ;
              score ; strand ; phase ; attributes ] ->
            `Record { seqid ; source ; feature ;
                      pos_start = Int.of_string pos_start ;
                      pos_end = Int.of_string pos_end ;
                      score = (
                        match score with
                          | "." -> None
                          | _ -> Some (Float.of_string score)
                      );
                      strand = parse_strand strand ;
                      phase = (match phase with
                          | "." -> None
                          | _ -> Some (Int.of_string phase)
                        );
                      attributes = parse_attributes attributes ;
                    }
          | _ -> assert false


  let unparse_item = function
    | `Comment c -> sprintf "%s" c
    | `Record r ->
      String.concat ~sep:"\t" [
        r.seqid ; r.source ; r.feature ;
        sprintf "%d" r.pos_start ; sprintf "%d" r.pos_end ;
        Option.value_map ~default:"." ~f:(sprintf "%g") r.score ;
        (match r.strand with
         | `Not_relevant -> "."
         | `Unknown -> "?"
         | `Plus -> "+"
         | `Minus -> "-") ;
        Option.value_map ~default:"." ~f:(sprintf "%d") r.phase ;
        (String.concat ~sep:";"
           (List.map r.attributes (fun (k,v) ->
                sprintf "%s=%s" k v )));
    ]

end

(*
let test = "#Ceci est un commentaire";;
let test2 = "chr1\tgenbank\tintron\t0\t2\t0.1\t+\t.\tID=ENS001;Transcript=TNS0002";;

let res = Gff.parse_item test;;
let res2 = Gff.parse_item test2;;
let res3 = Gff.unparse_item res;;
let res4 = Gff.unparse_item res2;;
*)
