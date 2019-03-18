(* Coursera Programming Languages, Homework 3, Provided Code *)
(* Liam McCartney *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
      val r = g f1 f2
    in
      case p of
          Wildcard          => f1 ()
        | Variable x        => f2 x
        | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
        | ConstructorP(_,p) => r p
        | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals (strings) =
  List.filter (fn str => Char.isUpper(String.sub(str, 0))) strings


(* 2 *)
fun longest_string1 (strings) =
  let
    fun longer_string (str, longest) =
        if String.size(str) > String.size(longest)
        then str
        else longest
  in
    List.foldl longer_string "" strings
  end

(* 3 *)
fun longest_string2 (strings) =
  let
    fun longer_string (str, longest) =
        if String.size(str) < String.size(longest)
        then longest
        else str
  in
    List.foldl longer_string "" strings
  end

(* 4 *)
fun longest_string_helper f strings =
  List.foldl (fn (x,y) => if f (String.size(x), String.size(y)) then x else y) "" strings 

val longest_string3 =
  longest_string_helper (fn (x,y) => x > y) 

val longest_string4 =
  longest_string_helper (fn (x,y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer func lst =
  case lst of
    [] => raise NoAnswer
  | l::lst' => case func(l) of
                 NONE => first_answer func lst'
               | SOME x => x

(* 8 *)
fun all_answers func lst =
  let
    fun aux (x, acc) =
      case (func x, acc) of
        (SOME y, SOME acc') => SOME (acc' @ y)
      | _ => NONE
  in
    List.foldl aux (SOME []) lst
  end

(* 9.a *)
val count_wildcards = g (fn () => 1) (fn x => 0)

(* 9.b *)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size(x))

(* 9.c *)
fun count_some_var (var, pattern) =
  g (fn () => 0) (fn x => if x = var then 1 else 0) pattern

(* 10 *)
fun check_pat pat =
  let
    fun no_repeats lst =
      case lst of
        [] => true
      | l::lst' =>
          not (List.exists (fn x => x = l) lst') andalso no_repeats(lst')

    fun collect_vars (pat, vars) =
      case pat of
        Variable x => vars @ [x]
      | ConstructorP (_, p) => collect_vars(p, vars)
      | TupleP pats => List.foldl collect_vars vars pats
      | _ => vars
  in
    no_repeats (collect_vars(pat, []))
  end

(* 11 *)
fun match (v, p) =
  case (v, p) of
    (_, Wildcard) => SOME []
  | (_, Variable p') => SOME [(p', v)]
  | (Unit, UnitP) => SOME []
  | (Const v', ConstP p') => if v' = p' then SOME [] else NONE
  | (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps)
                             then all_answers match (ListPair.zip(vs, ps)) 
                             else NONE
  | (Constructor (s, v'), ConstructorP (sp, p')) => if s = sp then match(v', p') else NONE
  | _ => NONE 



(* 11 *)
fun first_match v ps =
  let 
    fun curry f x y = f(x,y)
  in
  SOME (first_answer (curry match v) ps)
    handle NoAnswer => NONE
  end
