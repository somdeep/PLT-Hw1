
{ 
	type token = EOF | Word of string
	module StringMap = Map.Make(String);;
}

rule token = parse
| eof {EOF}
| ['a'-'z' 'A'-'Z']+ as word {Word(word)}
| _ {token lexbuf}


{
	let dowork hd m =
	if (StringMap.mem hd m) then 
	StringMap.add hd ((StringMap.find hd m) + 1) m
	else
	StringMap.add hd 1 m
	;;


	let rec count m l =
	match l with
	|[] -> m
	|hd::tl -> count (dowork hd m) tl
	;;

	let wordcounts l = 
	List.sort (fun (c1,_) (c2,_) ->
		Pervasives.compare c2 c1) l;
	;;

	let rec print_tuple l =
	match l with
	| [] -> print_string "Reached end"
	| (s,i) :: tl -> print_string s 

	;;

	let listfold l m= 
	(*print_endline l;
	print_endline "enters here";*)
	(StringMap.fold (fun s n l -> (n,s) :: l) m l);
	;;

	let print_map k v = 
	let value = string_of_int v in
	print_string(k ^ " " ^ value ^ "\n");;

	let print_list l=
	print_int (fst l);
	print_string (" " ^ snd l);
	print_endline ""
	;;

	let enlist mymap l = 	
	let m = count mymap l in
	(*print_int (StringMap.find "work" m);
	print_endline "";
	print_int (StringMap.find "try" m);
	print_endline "";
	print_int (StringMap.find "TRY" m);*)
	(*StringMap.iter print_map m;*)
	let l1 = [] in
	let l2 = listfold l1 m in
	let l3 = wordcounts l2 in
	(*StringMap.iter print_map m;*)
	List.iter print_list l3;
	(*List.iter (fun p -> print_int (fst p)) l1*)
	(*print_int (fst (List.hd l1));*)
	;;


	(*List.iter (fun a -> ignore(StringMap.add a 1 mymap)) l;
	List.iter (fun a -> print_endline a) l;;*)
	


	let lexbuf = Lexing.from_channel stdin in
	let wordlist = 
	let rec next l = 
	match token lexbuf with 
	EOF -> l
	|	Word(s) -> next (s :: l)
	in next []
	in
	enlist StringMap.empty wordlist;;

}