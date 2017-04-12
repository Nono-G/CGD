module type State  =
	sig
	  type t
	  val degree:int
	  val value: t -> int
	end

module type Graph =
	sig
		type state
		type t
		type vertex
		val degree: int
		val create: int -> t
		val createVertex: string -> int -> vertex
		val getId: vertex -> string
		val getNeighbour: vertex -> int -> ((string * int) option)
		val getDegree : t -> int
		val addVertex : t -> vertex -> unit
		val addEdge: t -> (string * int) -> (string * int)-> unit
		val getNeighbourhood: t -> string -> int -> t
		val consistent: t -> t -> bool
		val union: t -> t -> unit
		val setState: t -> string -> state -> unit
		val getState: t -> string -> state
		(* ? *
		val getVertex : t -> string -> vertex
		* ? *)
	end

let top="{\"nodes\":["
let middle="],\"links\":["
let bottom="]}"

module Make(Stat:State)	=
	struct
		type state=Stat.t
		type vertex=(string * ( ( ( string * int ) option ) array ) ) 
		(* degree x vertex hash table *)
		type t=(int * ((string , vertex) Hashtbl.t) * ((string , state) Hashtbl.t ) )
		let degree=Stat.degree

		let stateValue=Stat.value

		let create ()=
			(degree,(Hashtbl.create 10:(string , vertex) Hashtbl.t),(Hashtbl.create 10:((string , state) Hashtbl.t )))

		let getDegree (d,hv,hs)=d

		let getId=fst 

		let createVertex id =	
			(id , Array.create degree (None:( string * int ) option))


		let addVertex g v=
			let id=(fst v) and (_,vertices,_)=g in
				Hashtbl.add vertices id v


		let getVertex g id=
			let (_,vertices,_)=g in
				Hashtbl.find vertices id

		let getNeighbour v p=
			(snd v).(p)

		let addEdge g (id1,p1) (id2,p2)=
			let v1=getVertex g id1
			and v2=getVertex g id2 in
				let tab1=snd v1 and tab2=snd v2 in
					tab1.(p1)<-Some(id2,p2);
					tab2.(p2)<-Some(id1,p1)

		exception Distinct_vertices


		let unionVertex v1 v2=
			if (fst v1)<>(fst v2) then raise Distinct_vertices
			else
				let tab1,tab2=(snd v1),(snd v2) in
					for i=0 to (Array.length(tab1))-1  do
						match tab1.(i) with
						None -> tab1.(i)<-tab2.(i)
						|_->()
					done
		let union g1 g2=
			let tmp id vertex g=
				let (_,hv,_)=g in 
					try(
						let v=Hashtbl.find hv id  in
							unionVertex v vertex
					)with Not_found -> addVertex g vertex
			in let  (_,hv2,hs2)=g2 and (_,hv1,hs1)=g1 in 
				Hashtbl.fold (fun idElem elem ()-> tmp idElem elem g1) hv2 ();
				Hashtbl.fold (fun id vstate () -> Hashtbl.add hs1 id vstate)  hs2 ()

		let rec getNeighbourhood g id r=
			let (_,tabNeigh)=getVertex g id 
			and neighboursList=ref []
			and n=getDegree g in
			let newG=create () in
				for i=0 to n-1 do 
					match tabNeigh.(i) with
					|Some(idN,pN) ->
						(neighboursList:= idN::(!neighboursList);
						addVertex g (createVertex idN );
						addEdge g (id,i) (idN,pN))
					|_->()
				done;
				List.fold_right (fun elem ()-> union newG elem) (List.map  (fun neigh -> getNeighbourhood g neigh (r-1)) !neighboursList) ();
				newG


	let consistent g1 g2=true

	let getState g id =
		let (_,_,hs)=g in Hashtbl.find hs id
	let setState g id s=
		let (_,_,hs)=g in Hashtbl.add hs id s



	let vertex_to_string v g=(* Here we use the value function in the state module to change states into int *)
		"{ \"name\":\""^(fst v)^"\",\"state\":"^(string_of_int (stateValue (getState g (fst v))))^"}," 
		
		
	let edge_to_string v p ed g=
		match ed with
		|None -> ""
		|Some e -> "{\"source\":\""^(fst v)^"\",\"target\":\""^(fst e)^"\",\"portIn\":"^(string_of_int (p))^",\"portOut\":"^(string_of_int (snd e))^"},"

	let edges_to_string v g=
		let tab_edges=snd v
		and str=ref "" in
		for i=0 to (Array.length tab_edges) - 1 do
				str:= !str^(edge_to_string v i tab_edges.(i) g);
		done; !str
		
	let export g file_name=		
		let top="{\"nodes\":["
		and middle="],\"links\":["
		and bottom="]}"
		and oc = open_out file_name in
		 	output_string oc top; (*  Header *)
		 		let (_,vertex_table,_)=g in
		 			let vertex_string= (* Fold over the vertices *)
		 				Hashtbl.fold 
		 				(fun name vertex buffer -> (vertex_to_string vertex g)^buffer)
		 				vertex_table
		 				"" in
		 				if(String.length vertex_string > 0)then output_string oc (String.sub vertex_string 0 ((String.length vertex_string)- 1));
		 				
		 				
		 	output_string oc middle;(* Middler *)
		 		let (_,vertex_table,_)=g in (* Fold over the edges *)
		 			let edges_string=
		 				Hashtbl.fold 
		 				(fun name vertex buffer -> (edges_to_string vertex g)^buffer)
		 				vertex_table
		 				"" in 
		 				if(String.length edges_string > 0)then output_string oc (String.sub edges_string 0 ((String.length edges_string)- 1));
		 	
		 	output_string oc bottom;(* Footer *)
		 	
		 	close_out oc
		 
		 
	
	
		(*
		val setState: t -> string -> state -> unit
		val getState: t ->string -> state *)
	end
