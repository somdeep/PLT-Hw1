
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


	let rec count m ke =
	match ke with
	|[] -> m
	|hd::tl -> count (dowork hd m) tl
	;;


	let enlist mymap l = 
	let m = count mymap l in
	print_int (StringMap.find "work" m);
	print_endline "";
	print_int (StringMap.find "try" m);;

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