(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


type fullname = {first: string, middle: string, last: string}

(* 1.a *)
fun all_except_option (str, strs) =
  case strs of
    [] => NONE
  | s::strs' =>
      if same_string(s, str)
      then SOME strs'
      else
        case all_except_option(str, strs') of
          NONE => NONE
        | SOME strs'' => SOME(s::strs'')


(* 1.b *)
fun get_substitutions1 (subs, s) =
  case subs of
    [] => []
  | names::subs' =>
      case all_except_option(s, names) of
        NONE => get_substitutions1(subs', s)
      | SOME names' => names' @ get_substitutions1(subs', s)

(* 1.c *)
fun get_substitutions2 (subs, s) =
  let
    fun aux (subs, acc) =
      case subs of
        [] => acc
      | names::subs' =>
          case all_except_option(s, names) of
            NONE => aux(subs', acc)
          | SOME names' => aux(subs', acc @ names')
  in
    aux(subs, [])
  end


(* 1.d *)
fun similar_names (subs, {first=first, middle=middle, last=last}) =
  let
    fun replace_first_name (first_names) =
    case first_names of
      [] => []
    | first_name::first_names' =>
        {first=first_name, middle=middle, last=last}::replace_first_name(first_names')
  in
    {first=first, middle=middle, last=last}::replace_first_name(get_substitutions2(subs, first))
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

(* 2.a *)
fun card_color (card) =
  case card of
    (Clubs, _) => Black
  | (Spades, _) => Black
  | (Hearts, _) => Red
  | (Diamonds, _) => Red

(* 2.b *)
fun card_value (card) =
  case card of
    (_, Num i) => i
  | (_, Jack) => 10
  | (_, Queen) => 10
  | (_, King) => 10
  | (_, Ace) => 11

(* 2.c *)
fun remove_card (cards, card_to_remove, ex) =
  let
    fun aux(cards, stop) =
      case cards of
        [] =>
          if stop
          then []
          else raise ex
      | card::cards' =>
          if card = card_to_remove andalso stop
          then card::aux(cards', stop)
          else if card = card_to_remove
            then aux(cards', true)
            else card::aux(cards', stop)
  in
    aux(cards, false)
  end


(* 2.d *)
fun all_same_color (cards) =
  case cards of
    [] => true
  | _::[] => true
  | head::(neck::rest) =>
      card_color(head) = card_color(neck) andalso all_same_color(neck::rest)


(* 2.e *)
fun sum_cards (cards) =
  let fun aux (cards, acc) =
    case cards of
      [] => acc
    | card::cards' =>
        aux(cards', card_value(card) + acc)
  in
    aux(cards, 0)
  end


(* 2.f *)
fun score (cards, goal) =
  let
    val sum = sum_cards (cards)
    val same_color = all_same_color (cards)
    val prelim_score =
      if sum > goal
      then 3 * (sum - goal)
      else goal - sum
  in
    if same_color
    then prelim_score div 2
    else prelim_score
  end


(* 2.g *)
fun officiate (deck, moves, goal) =
  let
    fun play (hand, deck, moves) =
      case (moves, deck) of
        ([], _) =>
          score(hand, goal)
      | ((Discard card)::moves', _) =>
          play(remove_card(hand, card, IllegalMove), deck, moves')
      | (Draw::moves', []) =>
            raise IllegalMove
      | (Draw::moves', card::deck') =>
        if sum_cards(card::hand) > goal
        then score(card::hand, goal)
        else play(card::hand, deck', moves')
  in
    play([], deck, moves)
  end
