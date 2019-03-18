(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

OS.FileSys.chDir("sml-testing");
use "testing.sml";
OS.FileSys.chDir("..");

open SmlTests;

use "hw2.sml";

test("returns empty list on match single element list",
     assert_true(
      all_except_option ("string", ["string"]) = SOME []
    ));

test("removes one matching instance",
     assert_true(
      all_except_option ("string", ["string", "foo"]) = SOME ["foo"]
    ));

test("returns NONE for no match",
     assert_true(
      all_except_option ("string", ["g", "foo"]) = NONE
    ));

test("returns NONE for empty list",
     assert_true(
      all_except_option ("string", []) = NONE
    ));

test("removes only the first instance of a match",
     assert_true(
      all_except_option ("string", ["string", "foo", "string"]) = SOME ["foo", "string"]
   ));

test("get_substitutions1",
   assert_true(
    get_substitutions1 ([["foo"],["there"]], "foo") = []
  ));

test("get_substitutions1",
   assert_equals_string_list(
    ["Fredrick", "Freddie", "F"],
    get_substitutions1 (
      [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred"
    )
  ));

test("get_substitutions1",
   assert_equals_string_list(
    ["Jeffrey", "Geoff", "Jeffrey"],
    get_substitutions1 (
      [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff"
    )
  ));

test("get_substitutions2",
   assert_true(
    get_substitutions2 ([["foo"],["there"]], "foo") = []
  ));

test("get_substitutions2",
   assert_equals_string_list(
    ["Fredrick", "Freddie", "F"],
    get_substitutions2 (
      [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred"
    )
  ));

test("get_substitutions2",
   assert_equals_string_list(
    ["Jeffrey", "Geoff", "Jeffrey"],
    get_substitutions2 (
      [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff"
    )
  ));

test("similar_names",
  assert_true(
    similar_names ([["Fred","Fredrick"],
                    ["Elizabeth","Betty"],
                    ["Freddie","Fred","F"]],
                   {first="Fred", middle="W", last="Smith"}) =
            [{first="Fred", last="Smith", middle="W"},
             {first="Fredrick", last="Smith", middle="W"},
             {first="Freddie", last="Smith", middle="W"},
             {first="F", last="Smith", middle="W"}]
));

test("determines correct card color",
  assert_true(
    card_color (Clubs, Num 2) = Black
  ));

test("determines correct card color 2",
  assert_true(
    card_color (Hearts, Queen) = Red
  ));

test("determines correct card value",
  assert_true(
    card_value (Clubs, Num 2) = 2
  ));

test("determines correct card value 2",
  assert_true(
    card_value (Clubs, Ace) = 11
  ));

test("removes correct card from a list of cards",
  assert_true(
    remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
  ));

test("removes correct card from a list of cards 2",
  assert_true(
    remove_card ([(Hearts, Ace), (Spades, Ace)], (Hearts, Ace), IllegalMove) = [(Spades, Ace)]
  ));

test("removes only one instance of matching card from the list",
  assert_true(
    remove_card ([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace)]
  ));

test("raises an error if the card is not present in the list",
  assert_raises(
    remove_card, ([(Spades, Ace), (Spades, Queen)], (Hearts, Ace), IllegalMove), IllegalMove
  ));

test("returns true for all cards having the same color",
  assert_true(
    all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
  ));

test("returns false if any cards differ in color",
  assert_true(
    all_same_color [(Hearts, Ace), (Spades, Ace)] = false
  ));

test("sums up all cards in a list",
  assert_true(
    sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
  ));

test("sums up all cards in a list 2",
  assert_true(
    sum_cards [(Clubs, Num 2),
               (Clubs, Num 2),
               (Clubs, Num 3)
              ] = 7
  ));

test("sums up all cards in a list 3",
  assert_true(
    sum_cards [(Clubs, Ace),
               (Clubs, Queen),
               (Clubs, Num 3)
              ] = 24
  ));

test("sums up all cards in a list 3",
  assert_true(
    sum_cards [] = 0
  ));

test("computes the score",
  assert_true(
    score ([(Hearts, Num 2),(Clubs, Num 4)], 10) = 4
  ));

test("computes the score if the sum exceeds teh goal",
  assert_true(
    score ([(Hearts, Num 2), (Clubs, Num 4), (Diamonds, Num 9)], 10) = 15
  ));

test("computes the score if all cards are the same color",
  assert_true(
    score ([(Hearts, Num 2), (Hearts, Num 4), (Diamonds, Num 9)], 10) = 7
  ));

test("play the game",
  assert_true (
    officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
  ));

test("play the game",
  assert_true (
    officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42) = 3
  ));

test("play the game",
  assert_raises(
    officiate,
    ([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)], 42),
    IllegalMove
  ));

run();
