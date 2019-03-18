(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


	     
(* PROBLEM 1 *)
 
(* a. *)	
fun all_except_option(str, slist) =
    let
	fun all_except(soFar, rest, contains) =
	    case rest of
		[] => if contains then SOME soFar else NONE
	      | x::xs' => if same_string(x, str)
			  then all_except(soFar, xs', true)
			  else all_except(x::soFar, xs', contains)
    in
	all_except([], slist, false)
    end
	     
	
(* b *)
fun get_substitutions1(list, s) =
    case list of
	[] => []
      | x::xs' =>
	let
	    val result = all_except_option(s,x)
	in
	    case result of
		NONE => get_substitutions1(xs', s)
	      | SOME z => z @ get_substitutions1(xs', s)
	end

	    
(* c *)
fun get_substitutions2(list, s) =
    let
	fun aux(soFar, rest) =
	    case rest of
		[] => soFar
	      | x::xs' =>
		let
		    val result = all_except_option(s,x)
		in
		    case result of
			NONE => aux(soFar , xs')
		      | SOME z => aux(z @ soFar, xs')
		end
    in
	aux([], list)
    end

	
(* d *)	
fun similar_names(subs, name : {first:string, middle:string, last:string}) =
    let
	val validSubs = get_substitutions2(subs, (#first name))
	fun combine_names(subs_) =
	    case subs_ of
		[] => []
	      | x::xs' => {first=x, last=(#last name), middle=(#middle name)}::combine_names(xs')	  
    in
	combine_names(#first name:: validSubs)
    end
	     


(* PROBLEM 2 *)
	
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw 
exception IllegalMove;


(* a *)
fun card_color(c) =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black			      
      | _ => Red			      

		 
(* b *)
fun card_value(c) =
    case c of
	(_, Num i) => i
      | (_, Ace) => 11
      | _ => 10

		 
(* c *)
fun remove_card(cs, c, e) =
    let
	fun remove(soFar, rest) =
	    case rest of
		[] => raise e
	      | x::xs' => if x = c then soFar @ xs' else remove(x::soFar, xs')
    in
	remove([], cs)
    end

	
(* d *)
fun all_same_color(cs) =
    case cs of
	_::[] => true
      | x::y::z => if card_color(x) = card_color(y)
		   then all_same_color(y::z)
		   else false
      | _ => true 

			    
(* e *)
fun sum_cards(cs) =
    case cs of
	[] => 0
     | x::xs' => card_value(x) + sum_cards(xs')

(* f *)
fun score(cs, goal) =
    let
	val sum = sum_cards(cs)
	val prelimScore = if sum > goal
			  then 3 * (sum - goal)
			  else goal - sum
    in
	if all_same_color(cs)
	then prelimScore div 2
	else prelimScore
    end

(* g *)
	
fun officiate(clist, mlist, goal) =
    let	    
	fun playRound(deck: card list, moves: move list, hand: card list) =
	    if sum_cards(hand) > goal
	    then score(hand, goal)
	    else
		case moves of
		    [] => score(hand, goal)
		  | Discard c::moves' => playRound(deck, moves', remove_card(hand, c, IllegalMove))
		  | _::moves' => case deck of
				     [] => score(hand, goal)
				   | x::xs' => playRound(xs', moves', x::hand)
    in
	playRound(clist, mlist, [])
    end
	    
