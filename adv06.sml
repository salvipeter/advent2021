(* vim: set nowrap: -*- truncate-lines: t -*- *)

val fish = [3,4,3,1,2,1,5,1,1,1,1,4,1,2,1,1,2,1,1,1,3,4,4,4,1,3,2,1,3,4,1,1,3,4,2,5,5,3,3,3,5,1,4,1,2,3,1,1,1,4,1,4,1,5,3,3,1,4,1,5,1,2,2,1,1,5,5,2,5,1,1,1,1,3,1,4,1,1,1,4,1,1,1,5,2,3,5,3,4,1,1,1,1,1,2,2,1,1,1,1,1,1,5,5,1,3,3,1,2,1,3,1,5,1,1,4,1,1,2,4,1,5,1,1,3,3,3,4,2,4,1,1,5,1,1,1,1,4,4,1,1,1,3,1,1,2,1,3,1,1,1,1,5,3,3,2,2,1,4,3,3,2,1,3,3,1,2,5,1,3,5,2,2,1,1,1,1,5,1,2,1,1,3,5,4,2,3,1,1,1,4,1,3,2,1,5,4,5,1,4,5,1,3,3,5,1,2,1,1,3,3,1,5,3,1,1,1,3,2,5,5,1,1,4,2,1,2,1,1,5,5,1,4,1,1,3,1,5,2,5,3,1,5,2,2,1,1,5,1,5,1,2,1,3,1,1,1,2,3,2,1,4,1,1,1,1,5,4,1,4,5,1,4,3,4,1,1,1,1,2,5,4,1,1,3,1,2,1,1,2,1,1,1,2,1,1,1,1,1,4]

fun births k n =
    let fun f k = if k <= 0 then 1
                  else f (k-7) + f (k-9)
    in f (k-n) end

val adv06 = foldl op + 0 (map (births 80) fish)

(* Same, but memoized *)
local val cache = Array.array (256, NONE)
in fun births' k n =
       let fun f k =
               if k <= 0 then 1
               else case Array.sub (cache, k) of
                        NONE   => let val b = f (k-7) + f (k-9)
                                  in Array.update (cache, k, SOME b); b end
                      | SOME b => b
       in f (k-n) end
end

(* Solution using tail-recursion *)
fun births'' k n =
    let fun f 0 (x::_) = x
          | f k xs = let val next = List.nth (xs,6) + List.nth (xs,8)
                     in f (k-1) (List.take (next::xs,9)) end
    in f (k-n) [1,1,1,1,1,1,1,1,1] end

val adv06b = foldl op + 0 (map (births'' 256) fish)
