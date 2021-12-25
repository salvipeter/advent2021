(* vim: set nowrap: -*- truncate-lines: t -*- *)

val data = [(13,3,false),(11,12,false),(15,9,false),(~6,12,true),(15,2,false),(~8,1,true),(~4,1,true),(15,13,false),(10,1,false),(11,6,false),(~11,2,true),(0,11,true),(~8,10,true),(~7,3,true)]

fun validate xs = let
    fun f (w, (a, b, m), z) = let
        val z' = if m then Int.quot (z, 26) else z
    in (print (" " ^ Int.toString (z mod 26 + a));
        if z mod 26 + a = w then (print "!"; z')
        else 26 * z' + w + b
       )
    end
in ListPair.foldl f 0 (xs, data) end

(*
  So w + b + a' = w' should be true at least 7 times.
  Looking at the series of b+a' values (3+11, 12+15, 9-6, 12+15, 2-8, ...),
  there are exactly 7 where this can be achieved:
    _ _ 3 _ -6 -3 _ _ _ -5 2 3 3
  Since 0 is not a valid digit, only one of -6 -3 can be used.
*)
