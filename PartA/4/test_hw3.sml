(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";
(*
val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 ["A","bc","C", "lol"] = "lol"
val test2b = longest_string1 ["thisisolong", "A","bc","C", "lol"] = "thisisolong"
val test2c = longest_string1 ["A","bc","C", "lol", "baz"] = "lol"


val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 ["A","bc","C", "lol"] = "lol"
val test3b = longest_string2 ["thisisolong", "A","bc","C", "lol"] = "thisisolong"
val test3c = longest_string2 ["A","bc","C", "lol", "baz"] = "baz"
*)


val test4a1 = longest_string3 ["A","bc","C"] = "bc"
val test4a2 = longest_string3 ["A","bc","C"] = "bc"
val test4a3 = longest_string3 ["A","bc","C", "lol"] = "lol"
val test4a4 = longest_string3 ["thisisolong", "A","bc","C", "lol"] = "thisisolong"
val test4a5 = longest_string3 ["A","bc","C", "lol", "baz"] = "lol"


val test4b   = longest_string4 ["A","B","C"] = "C"
val test4b1  = longest_string4 ["A","bc","C"] = "bc"
val test4b2  = longest_string4 ["A","bc","C", "lol"] = "lol"
val test4b3  = longest_string4 ["thisisolong", "A","bc","C", "lol"] = "thisisolong"
val test4b4  = longest_string4 ["A","bc","C", "lol", "baz"] = "baz"


val test5   = longest_capitalized  ["A","bc","C"] = "A"
val test5a  = longest_capitalized  ["A","bc", "HW", "C"] = "HW"
val test5b  = longest_capitalized  ["A","bc","C", "lol"] = "A"
val test5c  = longest_capitalized  ["thisisolong", "A","bc","C", "lol", "Vaa"] = "Vaa"
val test5d  = longest_capitalized  ["A", "bc", "Car", "lol", "baz"] = "Car"
val test5e  = longest_capitalized  ["a", "bc", "car", "lol", "baz"] = ""
(*


val test6 = rev_string "abc" = "cba"


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7a = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,3,5] = 5
(* TODO: Test raises error *)



val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,1,4,5,6,7] = NONE
val test8b = all_answers (fn x => if x >= 1 then SOME [x] else NONE) [2,3,1,4,1,5,16,7] = SOME [2,3,1,4,1,5,16,7]
val test8c = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []


val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (TupleP[Wildcard, TupleP[Wildcard,Wildcard]]) = 3
val test9a2 = count_wildcards (TupleP[Wildcard, ConstP 5]) = 1
val test9a3 = count_wildcards (TupleP[TupleP[Wildcard, ConstP 5], TupleP[Wildcard, ConstP 5]]) = 2


val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b1 = count_wild_and_variable_lengths (Variable("abc")) = 3
val test9b2 = count_wild_and_variable_lengths (TupleP([Variable("abc"), Wildcard])) = 4
val test9b3 = count_wild_and_variable_lengths (TupleP[Wildcard, TupleP[Variable "hello",Variable "world"]]) = 11
val test9b4 = count_wild_and_variable_lengths (TupleP[Wildcard, ConstP 5]) = 1
val test9b5 = count_wild_and_variable_lengths (TupleP[TupleP[Wildcard, Variable "lol"], TupleP[Wildcard, ConstP 5]]) = 5
val test9b6 = count_wild_and_variable_lengths (TupleP[Variable"s",TupleP[Variable"t",Wildcard]]) = 3
val test9b7 = count_wild_and_variable_lengths (ConstructorP("SOME",TupleP[Variable"x",ConstP 3])) = 1

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c1 = count_some_var ("x", (TupleP([Variable("x"), Variable("x")]))) = 2
val test9c2 = count_some_var ("x", (ConstructorP("SOME",TupleP[Variable"x",ConstP 3]))) = 1
val test9c01 = count_some_var ("x", (TupleP [Wildcard, ConstP 12, Wildcard])) = 0
val test9c02 = count_some_var ("x", (TupleP [Wildcard, Variable "str", Wildcard])) = 0
val test9c03 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Wildcard])) = 1
val test9c04 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Wildcard, Variable "x"])) = 2
val test9c05 = count_some_var ("x", (ConstructorP("pattern", (TupleP [Wildcard, Variable "x", Wildcard])))) = 1
val test9c06 = count_some_var ("x", (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard])))) = 1


val test10 = check_pat (Variable("x")) = true
val test1001 = check_pat (TupleP [Wildcard, Variable "x", Wildcard]) = true
val test1002 = check_pat (TupleP [Wildcard, Variable "x", Variable "y"]) = true
val test1003 = check_pat (TupleP [Wildcard, Variable "x", Variable "x"]) = false
val test1004 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard]))) = true
val test1005 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "y")]))) = true
val test1006 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "x")]))) = false
val test1007 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "y"])]))) = true
val test1008 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "z"])]))) = true
val test1009 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "x"])]))) = false
val test1010 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "y"])))) = true
val test1011 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "x"])))) = false
val test1012 = check_pat (TupleP [Wildcard, Variable "x", TupleP [Variable "y"]]) = true

val test11 = match (Const(1), UnitP) = NONE
val test1101 = match (Const(1), ConstP 1) = SOME []
val test1102 = match (Const(1), Variable "s") = SOME [("s", Const(1))]
val test1103 = match (Const(1), TupleP [Wildcard]) = NONE
val test1104 = match (Const(1), TupleP [ConstP 1]) = NONE
val test1105 = match (Tuple [Unit], TupleP [UnitP]) = SOME []
val test1106 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP]]) = SOME []
val test1107 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP, Variable "x"]]) = NONE
val test1108 = match (Tuple [Const(1), Tuple [Unit]], TupleP [ConstP 1, TupleP[UnitP]]) = SOME []
val test1109 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s")]]) = SOME [("s", Const(2))]
val test1110 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 2, TupleP[UnitP, Variable("s")]]) = NONE
val test1111 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s"), Wildcard]]) = NONE
val test1112 = match (Tuple [Const 3, Unit, Constructor ("c0", Const 3), Constructor ("c1", Const 3)],
                      TupleP [Variable "a", Wildcard, Variable "c0", ConstructorP ("c1", Variable "c1")]) = 
                      SOME [("a",Const 3),("c0",Constructor ("c0",Const 3)),("c1",Const 3)]


val test12 = first_match Unit [UnitP] = SOME []
val test1201 = first_match Unit [Variable ("s")] = SOME [("s", Unit)]
val test1202 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, Variable("s")]])] = SOME [("s", Const(2))]
val test1203 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, ConstP 3]])] = NONE
*)
