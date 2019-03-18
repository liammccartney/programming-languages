use "hw1.sml";
(* homework1 simple test *)
(* these are basic test cases. passing these tests does not guarantee that your code will pass the actual homework grader *)
(* to run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* all the tests should evaluate to true. for example, the repl should say: val test1 = true : bool *)

val is_older_test1 = is_older ((1,2,3),(2,3,4)) = true;
val is_older_test2 = is_older ((1,2,3),(1,2,3)) = false;
val is_older_test3 = is_older ((1,2,4),(1,2,3)) = false;
val is_older_test4 = is_older ((1,2,4),(1,3,4)) = true;
val is_older_test5 = is_older ((1,2,4),(1,1,4)) = false;

val number_in_month_test1 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1;
val number_in_month_test2 = number_in_month ([(2012,2,28),(2013,2,1)],2) = 2;
val number_in_month_test3 = number_in_month ([(2012,2,28),(2013,2,1), (2013,2,1)],2) = 3;
val number_in_month_test4 = number_in_month ([],2) = 0;
val number_in_month_test5 = number_in_month ([(2012,3,28), (2012,2,28),(2013,2,1), (2013,2,1)],2) = 3;
val number_in_month_test6 = number_in_month ([(2012,3,28), (2012,4,28),(2013,5,1), (2013,12,1)],2) = 0;

val number_in_months_test1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3;
val number_in_months_test2 = number_in_months ([(2012,2,28),(2013,3,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 4;
val number_in_months_test3 = number_in_months ([(2012,5,28),(2013,1,1),(2011,11,31),(2011,10,28)],[2,3,4]) = 0;

val dates_in_month_test1 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)];
val dates_in_month_test2 = dates_in_month ([(2012,2,28),(2013,2,1)],2) = [(2012,2,28), (2013, 2, 1)];

val dates_in_months_test1 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)];
val dates_in_months_test2 = dates_in_months ([(2012,2,28),(2013,4,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2013,4,1),(2011,4,28)];

val get_nth_test1 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there";
val get_nth_test2 = get_nth (["hi", "there", "how", "are", "you"], 3) = "how";
val get_nth_test2 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi";

val date_to_string_test1 = date_to_string (2013, 6, 1) = "June 1, 2013";
val date_to_string_test2 = date_to_string (2013, 12, 1) = "December 1, 2013";

val number_before_reaching_sum_test1 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3;
val number_before_reaching_sum_test2 = number_before_reaching_sum (11, [1,2,3,4,5]) = 4;
val number_before_reaching_sum_test3 = number_before_reaching_sum (11, [1,2,3,4,5]) = 4;
val number_before_reaching_sum_test4 = number_before_reaching_sum (5, [8]) = 0;

val what_month_test1 = what_month 70 = 3;
val what_month_test2 = what_month 365 = 12;
val what_month_test3 = what_month 1 = 1;
val what_month_test4 = what_month 100 = 4;

val test10 = month_range (31, 34) = [1,2,2,2];

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31);
val test12 = oldest([(2011,3,31), (2012,2,28), (2011,4,28)]) = SOME (2011,3,31);
val test13 = oldest([(2012,2,28),(2011,3,31), (2011,4,28), (2011, 1, 1)]) = SOME (2011, 1, 1);
val test15 = oldest([(2012,2,28),(2011, 1, 1), (2011,3,31),(2011,4,28)]) = SOME (2011, 1, 1);
val test14 = oldest([]) = NONE;
val test14 = oldest([(3000, 1, 24)]) = SOME(3000, 1, 24);
