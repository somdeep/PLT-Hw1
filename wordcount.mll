
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
		Pervasives.compare c2 c1) l
	;;


	let enlist mymap l = 	
	let m = count mymap l in
	print_int (StringMap.find "work" m);
	print_endline "";
	print_int (StringMap.find "try" m);
	print_endline "";
	print_int (StringMap.find "TRY" m);
	(*let l1 = [];*)
	fun l1 -> (StringMap.fold (fun s n l -> (s,n) :: l) m []);
	(*List.iter (fun p -> print_int (fst p)) l1*)
	print_int (fst (List.hd l1))
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