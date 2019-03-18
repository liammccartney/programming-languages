(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s1 : string, sl : string list) =
    let
        fun iterate(xs : string list, acc : string list, flag : bool) =
            case xs of
                [] => (acc, flag)
              | x::xs' => if same_string(x, s1)
                          then iterate(xs', acc, true)
                          else iterate(xs', acc @ [x], flag)
    in
        case (iterate(sl, [], false)) of
            (result, true) => SOME result
          | (_, false) => NONE
    end;

fun get_substitutions1(sll : string list list, s : string) =
    case sll of
        [] => []
      | x::xs => case all_except_option(s, x) of
                       NONE => get_substitutions1(xs,s)
                     | SOME v => v @ get_substitutions1(xs, s)

fun get_substitutions2(sll : string list list, s : string) =
    let
        fun iterate(xs : string list list, acc : string list) =
            case xs of
                [] => acc
              | x::xs' => case all_except_option(s, x) of
                              NONE => iterate(xs', acc)
                            | SOME v => iterate(xs', acc @ v)
    in
        iterate(sll, [])
    end;

type person = {first:string, middle:string, last:string}

fun similar_names (sll : string list list, f : person) =
    let
        val {first=first_name, middle=middle_name, last=last_name} = f
        val subs = get_substitutions2(sll, first_name)
        fun map(xs : string list) =
            case xs of
                [] => []
              | x::xs' => {first=x, middle=middle_name, last=last_name} :: map(xs')
    in
        f :: map(subs)
    end;



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(c : card) =
    case c of
        (Spades, _) => Black
      | (Clubs, _) => Black
      | _  => Red

fun card_value(c : card) =
    case c of
        (_, Num v) => v
      | (_, Ace) => 11
      | _ => 10

fun remove_card(cl : card list, c : card, e : exn) =
    let
        fun filter_once(cl' : card list, flag : bool) =
            case cl' of
                [] => if flag = false then raise e else []
              | x::xs => if x = c andalso flag = false
                         then filter_once(xs, true)
                         else x :: filter_once(xs, flag)
    in
        filter_once(cl, false)
    end;

fun all_same_color(cl : card list) =
    case cl of
        [] => true
      | x::[] => true
      | x::y::xs => if card_color(x) = card_color(y) then all_same_color(y::xs) else false

fun sum_cards(cl : card list) =
    let
        fun reduce(al : card list, acc : int) =
            case al of
                [] => acc
              | x::xs => reduce(xs, acc + card_value(x))
    in
        reduce(cl, 0)
    end;

fun score(cl : card list, goal : int) =
    let
        val cardsum = sum_cards(cl)
        val pre_score = if cardsum > goal
                        then (3 * (cardsum - goal))
                        else goal - cardsum
    in
        if all_same_color(cl)
        then (pre_score div 2)
        else pre_score
    end;

fun officiate(cl : card list, ml : move list, goal : int) =
    let
        fun play(cards : card list, moves : move list, held : card list) =
            case moves of
                [] => score(held, goal)
              | m::ms => case m of
                             Discard c => play(cards, ms, remove_card(held, c, IllegalMove))
                           | Draw => case cards of
                                         [] => score(held, goal)
                                       | ca::cas => if sum_cards(ca::held) > goal
                                                    then score(ca::held, goal)
                                                    else play(cas, ms, ca::held)
    in
        play(cl, ml, [])
    end;
