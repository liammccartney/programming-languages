fun foo (l) =
  List.foldl (fn (x, y) => y) 0 l

val t = foo [1,2,3,4];
