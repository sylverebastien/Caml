(*
Voir version, dependance ...
opam show --raw lablgtz
*)

module List : sig
type 'a t
val empty : 'a t
val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val map :  ('a -> 'b) -> 'a t -> 'b t
val mem : 'a -> 'a t -> bool
val exists : ('a -> bool) -> 'a t -> bool
val insert : 'a -> 'a t -> 'a t
val filter : ('a -> bool) -> 'a t -> 'a t
val length : 'a t -> int
val sort : 'a t -> 'a t
val drop : int -> 'a t -> 'a t
val init : int -> int t
val hd : 'a t -> 'a

end

= struct
type 'a t =
	| Empty
	| Cons of 'a * 'a t
let empty = Empty
let singleton e = Cons (e,Empty)
let rec cons h t = Cons (h,t)

let rec map f = function
	| Empty -> Empty
	| Cons (h,t) -> Cons (f h,map f t)

let rec mem n = function
	| Empty -> false
	| Cons (h,t) ->
		if h = n then true
		else mem n t

let rec exists f = function
	| Empty -> false
	| Cons (h,t) -> f h || exists f t

let rec insert x l = match l with
	| Empty -> Cons (x,Empty)
	| Cons (h,t) ->
		if x <= h then Cons (x,Cons (h,t))
		else Cons (h,insert x t)

let rec filter x l = match l with
	| Empty -> Empty
	| Cons (h,t) ->
		if x h then Cons (h,filter x t)
		else filter x t

let rec length l = match l with
	| Empty -> 0
	| Cons (_,t) -> 1 + length t

let rec sort l = match l with
	| Empty -> Empty
	| Cons (h,t) -> insert h (sort t)

let rec drop n l =
	if n = 0 then l
	else match l with Cons(h,t) -> drop (n - 1) t

let init n =
	if n = 0 then Empty
	else let res = ref Empty in
		for i = 0 to n - 1 do
		res := insert i !res;
		done;
	!res;;

let hd l = match l with
	| Empty -> raise (Invalid_argument "Empty list")
	| Cons (h,t) -> h

end


module Dict : sig
type ('key,'value) t
val empty : ('key,'value) t
val mem_key : ('key,'value) t -> 'key -> bool
val add : ('key,'value) t -> 'key -> 'value -> ('key,'value) t
val add_rplce : ('key,'value) t -> 'key -> 'value -> ('key,'value) t

val keys : ('key,'value) t -> 'key List.t (*map*)
val values : ('key,'value) t -> 'value List.t


val rmv_key : ('key,'value) t -> 'key -> ('key,'value) t
val rmv_value : ('key,'value) t -> 'value -> ('key,'value) t
val rplce_value : ('key,'value) t -> 'key -> 'value -> ('key,'value) t
val rplce_key : ('key,'value) t -> 'key -> 'value -> ('key,'value) t

val find_key : ('key, 'value) t -> 'value -> 'key List.t
val find_value : ('key, 'value) t -> 'key -> 'value List.t
end

= struct
type ('key,'value) t = ('key * 'value) List.t
let empty = List.empty
let mem_key d k = List.exists (fun (w, _) -> k=w) d

let keys g = List.map (fun (x, _) -> x) g
let values g = List.map (fun (_, x) -> x) g

let rmv_key d k = List.filter (fun (x,y) -> x <> k ) d
let rmv_value d v = List.filter (fun (x,y) -> y <> v ) d

let rplce_key d k v = List.cons (k,v) (rmv_value d v)
let rplce_value d k v =	List.cons (k,v) (rmv_key d k)

let find_key d v = 
	let d = List.filter (fun (x,y) -> y = v ) d in keys d
let find_value d k = 
	let d = List.filter (fun (x,y) -> x = k ) d in values d

let add d k v =
	if mem_key d k then raise (Invalid_argument "Key exists")
	else List.cons (k,v) d

let add_rplce d k v =
	if mem_key d k then rplce_value d k v
	else List.cons (k,v) d
end


module Graph : sig
type t
type vertex
val empty : t

val add_vertex : t -> vertex -> t

val add_edge : t -> vertex -> vertex -> t

val create_vertex : unit -> vertex 
val create_vertex2 : unit -> vertex 

end

=struct
type vertex = int
type t = (vertex, vertex List.t) Dict.t

let empty = Dict.empty

let add_vertex : t -> vertex -> t = fun g node -> Dict.add_rplce g node List.empty


let rec create_vertex () =
	let listetot = List.init 50 in
	let n = List.hd listetot in 
	let listetot = List.drop 1 listetot in n


let rec create_vertex2 () =
	let node = read_int () in 
		if node < 0 then create_vertex2 ()
		else node
	
let add_edge g node1 node2 =
	let g = add_vertex g node2 in
		Dict.add_rplce g node1 (List.cons node2 List.empty)

(*
let rec add_edge_aux g node1 node2 = match g with
	| [] -> (node1,[node2]) :: g
	| (u, succ_u) :: g' ->
			if u = node1 then
				if List.mem node2 succ_u then g
				else (u, node2 :: succ_u) :: g'
			else (u, succ_u) :: add_edge_aux g' node1 node2
*)

end

(*
let test = List.cons 1 List.empty
let k = 1
let v = 234
let d = List.empty
let test2 = List.insert k d
let test3 = List.cons (k,v) d
let test4 = Dict.empty
let test7 = Dict.add Dict.empty "un" 34
let test8 = Dict.keys test7
let test9 = Dict.values test7
let test12 = Dict.add test7 "1" 36
let test10 = Dict.add test7 "deux" 36
let test11 = Dict.add test7 "un" 36
let test13 = Dict.rplce_value test7 "un" 36
let test14 = Dict.rmv_key test10 "un"
*)

