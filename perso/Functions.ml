let rec length l = match l with
	| [] -> 0
	| _::t -> 1 + length t


let rec sum l = match l with
	| [] -> 0
	| h::t -> h + sum t


let rec length_inner l n = match l with
	| [] -> n
	| h::t -> 1 + length_inner t (n + 1)


let rec odd_elements l = match l with
	| a::_::t -> a :: odd_elements t
	| _ -> l


let rec append a b = match a with
	| [] -> b
	| h::t -> h:: append t b


let rec rev l = match l with
	| [] -> []
	| h::t -> rev t @ [h]


let rec take n l =
	if n = 0 then []
	else match l with h::t -> h:: take (n - 1) t


let rec drop n l =
  if n = 0 then l
  else match l with h::t -> drop (n - 1) t

let rec take_noerror n l = match l with
	| [] ->
		if n = 0 then []
		else raise (Invalid_argument "take")
	| h::t ->
		if n < 0 then raise (Invalid_argument "take")
		else if n = 0 then []
		else take_noerror (n - 1) t


let rec drop_noerror n l = match l with
	| [] ->
		if n = 0 then []
		else raise (Invalid_argument "drop")
	| h::t ->
		if n < 0 then raise (Invalid_argument "drop")
		else if n= 0 then l
		else drop_noerror (n - 1) t


let rec insert x l = match l with
	| [] -> [x]
	| h::t ->
		if x <= h then x::h::t
		else h::insert x t


let rec sort l = match l with
	| [] -> []
	| h::t -> insert h (sort t)


let rec merge x y = match x, y with
	| [], l -> l
	| l, [] -> l
	| hx::tx, hy::ty ->
		if hx < hy then hx :: merge tx (hy :: ty)
		else hy :: merge (hx :: tx) ty


let rec msort l = match l with
	| [] -> []
	| [x] -> [x]
	| _ ->
		let left = take (length l / 2) l in
		let right = drop (length l / 2) l in
		merge (msort left) (msort right)


let rec double l = match l with
	| [] -> []
	| h::t -> (h * 2) :: double t


let rec evens l = match l with
	| [] -> []
	| h::t -> (h mod 2 = 0) :: evens t


let rec map f l = match l with
	| [] -> []
	| h::t -> f h :: map f t


let rec merge_cmp cmp x y = match x, y with
	| [], l -> l
	| l, [] -> l
	| hx::tx, hy::ty ->
		if cmp hx hy then hx :: merge_cmp cmp tx (hy :: ty)
		else hy :: merge_cmp cmp (hx :: tx) ty


let rec msort_cmp cmp l = match l with
	| [] -> []
	| [x] -> [x]
	| _ ->
		let left = take (length l / 2) l in
		let right = drop (length l / 2) l in
		merge_cmp cmp (msort_cmp cmp left) (msort_cmp cmp right)


let rec last l = match l with
	| [] -> raise Not_found
	| [x] -> x
	| _::t -> last t


let rec filter p l = match l with
	| [] -> []
	| h::t ->
		if p h then h::filter p t
		else filter p t


let rec partition p l = match l with
	| [] -> ([],[])
	| h::t ->
		let (t_pos, t_neg) = partition p t in
			if p h then (h::t_pos, t_neg)
			else (t_pos, h::t_neg)


let rec quicksort l = match l with
	| [] -> []
	| h::t ->
		let (inf, sup) = partition (fun x -> x <= h) t in
		quicksort inf@(h::quicksort sup)

let empty = []

let is_empty = function
	| [] -> true
	| _ -> false


let mem_vertex g v = List.exists (fun (w, _) -> v=w) g

let add_vertex g v =
	if mem_vertex g v then g
	else (v,[])::g


let fst (x, _) = x

let snd (_, y) = y


let rec add_edge_aux g node1 node2 = match g with
	| [] -> (node1,[node2]) :: g
	| (u, succ_u) :: g' ->
			if u = node1 then
				if List.mem node2 succ_u then g
				else (u, node2 :: succ_u) :: g'
			else (u, succ_u) :: add_edge_aux g' node1 node2


let add_edge g node1 node2 =
	let g = add_vertex g node2 in
		add_edge_aux g node1 node2


let rec add_edges g node1 nodes = match nodes with
	| [] -> g
	| h::t ->
		let g = add_edge g node1 h in add_edges g node1 t


let rec remove n l = match l with
	| [] -> l
	| h::t ->
		if h = n then remove n t
		else h :: remove n t


let rec remove2 n l = match l with
	| [] -> l
	| h::t ->
		List.filter (fun x->x<>n) l


let is_null x = match x with
	| 0 -> true
	| _ -> false


let rec rmv_vertex g v = match g with
	| [] -> g
	| (u, succ_u) :: g' ->
		if u = v then rmv_vertex g' v
		else
			if List.mem v succ_u then (u, remove v succ_u) :: rmv_vertex g' v
			else (u, succ_u) :: rmv_vertex g' v


let rec rmv_edge g node1 node2 = match g with
	| [] -> g
	| (u, succ_u) :: g' ->
			if u = node1 then (u, remove2 node2 succ_u) :: rmv_edge g' node1 node2
			else (u, succ_u) :: rmv_edge g' node1 node2


let rec succ g node = match g with
	| [] -> raise Not_found
	| (u, succ_u) :: g' ->
			if u = node then succ_u
			else succ g' node


let rec liste_nbr_succ_aux g n l = match g with
	| [] -> l
	| (u, succ_u) :: g' ->
			if n = 0 then raise (Invalid_argument "No node 0")
			else if length succ_u = n then let l = ((u, succ_u) :: l) in liste_nbr_succ_aux g' n l
				else liste_nbr_succ_aux g' n l


let liste_nbr_succ g n =
	let l = [] in liste_nbr_succ_aux g n l


let nbr_succ g n = match g with
	| [] -> 0
	| h::t -> length (liste_nbr_succ g n)


let rec singleton_aux g l = match g with
	| [] -> l
	| (u, succ_u) :: g' ->
			if succ_u = [] then let l = (u :: l) in singleton_aux g' l
			else singleton_aux g' l


let singleton g =
	let l = [] in singleton_aux g l


let rec nbr_singleton g = match g with
	| [] -> 0
	| h::t -> nbr_succ g 0


let rec prec_aux g n l = match g with
	| [] -> l
	| (u, succ_u) :: g' ->
			if List.mem n succ_u then let l = (u :: l) in prec_aux g' n l
			else prec_aux g' n l


let prec g n =
	let l = [] in prec_aux g n l


let nbr_prec g n = match g with
	| [] -> 0
	| h::t -> length (prec g n)


let nbr_vertices g = List.length g


let nbr_edges g =
	List.map snd g
	|> List.map List.length
	|> List.fold_left ( + ) 0


let rec union_add g g2 = match g2 with
	| [] -> g
	| (u, succ_u) :: g' ->
			let g = insert (u, succ_u) g in union_add g g'


let rec union_rmv g l = match g with
	| [] -> l
	| (u, succ_u) :: g' ->
		let l = add_edges l u succ_u in union_rmv g' l


let union g g2 = let g = union_add g g2 in union_rmv g []


let verif_graph g = union g []

(*
let rec chemin_aux g n =

match g with
	| [] -> l
	| (u, succ_u) :: g' ->
		if u = n then let l = succ g u in let n = last l in chemin_aux g n []
		else chemin_aux g' n l

*)

let test = [1;2;3]
let graph = [(1,[1;2]);(2,[3;4]);(3,[]);(4,[3]);(5,[])]
let graph2 = [(1,[2]);(2,[5;7]);(3,[1]);(6,[3])]
let graph3 = union graph graph2


let rec mem n = function
	| [] -> false
	| h::t ->
		if h = n then true
		else mem n t

let rec vertices_aux g l = match g with
	| [] -> l
	| (u, succ_u) :: g' ->
		let l' = insert u l in vertices_aux g' l'

let vertices2 g = List.map fst g

let rec edges_aux g l = match g with
	| [] -> l
	| (u, succ_u) :: g' ->
		match succ_u with
			| [] -> edges_aux g' l
			| h::t -> let l' = insert (u, h) l in edges_aux ((u, t) :: g') l'

let edges g = let l = [] in edges_aux g l
let vertices g = let l = [] in vertices_aux g l

let verticesg1 = vertices graph3
let edgesg1 = edges graph3

let init n =
	if n = 0 then []
	else let res = ref [] in
		for i = n - 1 downto 0 do
		res := insert i !res;
		done;
	!res;;


let rec init_node_aux n res =
	if n = 0 then res
	else let res = n :: res in init_node_aux (n - 1) res

let init_node n = init_node_aux n []

let rec init_edge_aux n l res =
	if n = 0 then res
	else let node1 = List.nth l (Random.int (List.length l)) in
		let node2 = List.nth l (Random.int (List.length l)) in
		let res = insert (node1, node2) res in
		init_edge_aux (n - 1) l res

let init_edge n l = init_edge_aux n l [];;

(*
let node_create = init_node (Random.int (40))
let edge_create = init_edge (Random.int (100)) node_create;;
*)

#require "dot";;

let make_graph l l2 =
let open Odot in
let node lbl name =
	Stmt_node ((dblq_node_id lbl), [(Simple_id "label", Some (Double_quoted_id name))]) in
let edge id1 id2 =
	Stmt_edge ((Edge_node_id (dblq_node_id id1)), [Edge_node_id (dblq_node_id id2)], []) in
let statements_of_vertices xs =
	List.map (fun vtx -> let id = string_of_int vtx in node id id) xs in
let statements_of_edges xs =
	List.map (fun (node1, node2) -> let node1 = string_of_int node1 in let node2 = string_of_int node2 in edge node1 node2) xs in
let draw_graphdot name =
	let stmt_list = statements_of_vertices l @ statements_of_edges l2
	(*statements_of_vertices node_create @ statements_of_edges edge_create*)
	(*let stmt_list = [node "1" "1"] @ [edge "1" "1"]*)
	in
	{strict=true;kind=Digraph; stmt_list ; id = Some (Double_quoted_id name)} in
let () =
	let graph = draw_graphdot "Graph" in
	Odot.print_file "graph.dot" graph;
	ignore (Sys.command "dot -Tsvg graph.dot -o graph.svg") in ()


let rec enum_slots_aux ledges lnodes res = match lnodes with
	| [] -> res
	| h::t ->
		let n1 = List.filter (fun (x,_) -> x = h) ledges in
		let n2 = List.filter (fun (_,x) -> x = h) ledges in
		let edge_by_key = n1 @ n2 in
		if List.mem (h, (length edge_by_key)) res then enum_slots_aux ledges t res
		else if length edge_by_key = 0 then let res = insert (h, 0) res in enum_slots_aux ledges t res
		else let res = insert (h, (length edge_by_key)) res in enum_slots_aux ledges t res

let enum_slots g =
	let ledges = edges g in
	let lnodes = ((vertices g) @ (List.map snd ledges)) in
	enum_slots_aux ledges lnodes []

let molslotmax = [(1, 4); (2, 4); (3, 1); (4, 1); (5, 1); (6, 1)]
let molgraph = [(1, [2]); (2, [3]); (1, [5]); (4,[]); (6,[])]


let nbr_slots_empty lslotmax lslotused =
	let namenode = List.map fst lslotmax in
	let slotmax = List.map snd lslotmax in
	let slotused = List.map snd lslotused in
	let slotdispo = List.map2 (-) slotmax slotused in
	let res = [] in
	let rec construct_slot_dispo namenode slotdispo res = match namenode with
		| [] -> res
		| h::t ->
			let res = insert (h, (List.hd slotdispo)) res in
			let slotdispo = drop 1 slotdispo in
			construct_slot_dispo t slotdispo res
	in construct_slot_dispo namenode slotdispo res


(*
let rec enum_structure g =
	let slots = enum_slots g in
	List.map slots (fun s -> add_link g s
		|> enum_structure)
	|> List.concat
*)

let rec create_g nodesmol res = match nodesmol with
	| [] -> res
	| h::t ->
		let res = insert (h,[]) res in create_g t res


let rec create_slot_max_aux valnmol res nbr initval = match valnmol with
	| 0 -> res
	| _ ->
		let res = insert (initval, nbr) res in
		let valnmol = valnmol - 1 in
		let initval = initval + 1 in
		 create_slot_max_aux valnmol res nbr initval


let rec create_slot_max lnmol lnbeliaison res initval = match lnmol with
	| [] -> res
	| h::t ->
		let liaisons = List.hd lnbeliaison in
		let res = create_slot_max_aux h res liaisons initval in
		let initval = initval + h in
		let lnbeliaison = drop 1 lnbeliaison in create_slot_max t lnbeliaison res initval


let decompose lnmol lnbeliaison=
	let sumnmol = sum lnmol in
	let nodesmol = init_node sumnmol in
	let res = [] in
	let lslotsmax = create_slot_max lnmol lnbeliaison res 1 in
	let g = create_g nodesmol res in
	let rec calcul_slot lslotsmax g =
		let lslotused = enum_slots g in
		let lslotdispo = nbr_slots_empty lslotsmax lslotused in
		let lslotdispo = List.filter (fun (_, x) -> x > 0) lslotdispo in
		let node = List.filter (fun (x, _) -> x ) lslotdispo in
		let node1 = List.hd node in
		let node = drop 1 node in
		let node2 = List.hd node in
		let g = insert (node1, node2) g in calcul_slot lslotsmax g
	in calcul_slot lslotsmax g

(*
let rec construct_edge_aux edges res = match edges with
	| [] -> res
	| (node1, node2) :: edges' ->
		let res = (Printf.sprintf ("edge " ^^ "%d" ^^ " %d") node1 node2) :: res in construct_edge_aux edges' res

let construct_edge edgesg1 = construct_edge_aux edgesg1 []
;;

let rec construct_node_aux vertices res = match vertices with
	| [] -> open_out "nodes.txt" ; output_string res ; close_out;
	| h::t ->
		let res = (Printf.sprintf ("node " ^^ "%c" ^^ "%d" ^^ "%c" ^^ "%c" ^^ "%c" ^^ "%d" ^^ "%c") '"' h '"' ' ' '"' h '"') :: res in construct_node_aux t res

let construct_node nodes = construct_node_aux nodes []
*)
