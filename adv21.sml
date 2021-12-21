val start = (10, 3)

fun move n k = (n + k - 1) mod 10 + 1

val adv21 = let
    fun die n = 9 * (n mod 100) + 6
    fun f (a, b) (pa, pb) n =
        if pa >= 1000 then pb * 3 * n else
        if pb >= 1000 then pa * 3 * n else
        if n mod 2 = 0
        then let val a' = move a (die n)
             in f (a', b) (pa + a', pb) (n+1) end
        else let val b' = move b (die n)
             in f (a, b') (pa, pb + b') (n+1) end
in f start (0, 0) 0 end

val dirac = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]

fun wins fst snd = let
    fun f (a, b) (pa, pb) n =
        if fst (pa, pb) >= 21 then 1 else
        if snd (pa, pb) >= 21 then 0 else
        let fun die (d, k) =
                if n mod 2 = 0
                then let val a' = move a d
                     in k * f (a', b) (pa + a', pb) (n+1) end
                else let val b' = move b d
                     in k * f (a, b') (pa, pb + b') (n+1) end
        in foldl op + 0 (map die dirac) end
in f start (0, 0) 0 end

val adv21 = Int.max (wins #1 #2, wins #2 #1)
