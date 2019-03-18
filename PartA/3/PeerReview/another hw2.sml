(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(x, xs) =
    let fun helper(x, xs, bool, result) =
	    case xs of
		[] => if bool then SOME result else NONE
	      | y::xs' => if same_string(x, y)
			  then helper(x, xs', true, result)
			  else helper(x, xs', bool, result@[y])
    in
	helper(x, xs, false, [])
    end

fun get_substitutions1(sub, str) =
    case sub of
	[] => []
      | x::xs => case all_except_option(str, x) of
		     NONE => get_substitutions1(xs, str)
		   | SOME strings => strings@get_substitutions1(xs, str)

fun get_substitutions2(sub, str) =
    let fun helper(sub, str, result) =
	    case sub of
		[] => result
	      | x::xs => case all_except_option(str, x) of
			     NONE => helper(xs, str, result)
			   | SOME strings => helper(xs, str, result@strings)
    in
	helper(sub, str, [])
    end

fun similar_names(sub, {first:string,middle:string,last:string}) =
    let fun helper(firs) =
	    case firs of
		[] => []
	      | x::xs => {first=x, middle = middle, last = last}::helper(xs)
    in
	helper(first::get_substitutions2(sub,first))
    end
							       
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(x, y) =
    case x of
	Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red

fun card_value(x, y) =
    case y of
	Num i => i
      | Ace => 11
      | _ => 10

fun remove_card(cs, c, e) =
    case cs of
	[] => raise e
      | x::xs => if x=c
		 then xs
		 else x::remove_card(xs, c, e)

fun all_same_color(cs) =
    case cs of
	[] => true
      | c::cs' => let fun helper(cards) =
			       case cards of
				   [] => true
				 | c'::xs => if card_color(c) = card_color(c')
						   then helper(xs)
						   else false
		       in
			   helper(cs')
		       end

fun sum_cards(cs) =
    let fun helper(cs, x) =
	    case cs of
		[] => x
	      | c::cs' => helper(cs', x + card_value(c))
    in
	helper(cs, 0)
    end
	
fun score(cs, goal) =
    let val sum = sum_cards(cs)
	val is_same = all_same_color(cs)
	val preliminary = if sum > goal
		      then 3 * (sum - goal)
		      else goal - sum
    in
	if is_same
	then preliminary div 2
	else preliminary
    end

fun officiate(cs, ms, goal) =
    let fun helper(cs, ms, hs) =
	    case ms of
		[] => hs
	      | m::ms' =>
		case m of
		    Discard c =>
		        helper(cs, ms', remove_card(hs, c, IllegalMove))
		  | Draw =>
			case cs of
			  [] => hs
			| c::cs' => if(sum_cards(c::hs) < goal)
				    then helper(cs', ms', c::hs)
				    else c::hs
    in
	score(helper(cs, ms, []), goal)
    end
