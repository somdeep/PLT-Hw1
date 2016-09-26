let oxford1 a =
	List.fold_left (fun s e -> s ^ e ^ ",") "" a;;
	(* List.iter print_string a;; *)

	(* Use list size potentially to stop recursion *)
	(* let work  = 
	List.fold_left (fun s e -> s ^ e) 0 [1;2;3];;
*)

let oxfordlength a =
	List.length (List.tl a)
;;


let rec oxford l =
	match l with 
	| [] -> ""
	| [a] -> a   
	| [a;b] -> a ^ " and " ^b
	| hd :: tl -> hd ^ ", " ^ oxford tl
;;


(* let b = oxfordlength ["somdeep";"work";"try"];;
print_int b;; *)
let c = oxford ["one";"two";"three";"four"];;
print_endline (c);;
let d = oxford ["one"];;
print_endline d;;
let f = oxford [];;
print_endline f;;
let e = oxford ["one";"two"];;
print_endline e;;

(* let b = work;; *)
(* print_int b;; *)