fun no_repeats lst =
  case lst of
    [] => true
  | l::lst' =>
      not (List.exists (fn x => x = l) lst') andalso no_repeats(lst')

val t = no_repeats [1,2,3,4]
val t1 = no_repeats [1,1,2,3,4]
val t2 = no_repeats [1,2,3,4, 1]
