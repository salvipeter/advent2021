(* vim: set nowrap: -*- truncate-lines: t -*- *)

val octopi = [[7,3,1,3,5,1,1,5,5,1],[3,7,2,4,8,5,5,8,6,7],[2,3,7,4,3,3,1,5,7,1],[4,4,3,8,2,1,3,4,3,7],[6,5,1,1,5,6,6,2,8,7],[6,7,2,7,2,4,5,5,3,2],[3,7,3,6,8,6,8,6,6,2],[2,3,4,8,1,3,8,2,6,3],[2,4,1,7,4,8,3,1,2,1],[8,8,1,2,6,1,7,1,1,2]]

fun getValue f (i,j)   = SOME (Array2.sub (f,i,j)) handle Subscript => NONE
fun setValue f (i,j) x = Array2.update (f,i,j,x)

fun increase (x, flashed) = (x + 1, flashed)

fun clear (_, true)  = (0, false)
  | clear (x, false) = (x, false)

fun neighbors (i,j) = [(i-1,j-1),(i-1, j ),(i-1,j+1),
                       ( i ,j-1),          ( i ,j+1),
                       (i+1,j-1),(i+1, j ),(i+1,j+1)]

fun flash a (i, j, (x, flashed), n) =
    if flashed orelse x <= 9 then n
    else let val adjacent = neighbors (i, j)
         in setValue a (i, j) (x, true)
          ; foldl op + (n+1) (map (update a) adjacent)
         end
and update a (i, j) =
    case getValue a (i, j) of
        NONE   => 0
      | SOME v => let val v' = increase v
                  in setValue a (i, j) v'
                   ; flash a (i, j, v', 0)
                  end                     

fun flashes _ 0 = 0
  | flashes a n =
    let val region = { base = a, row = 0, col = 0, nrows = NONE, ncols = NONE }
    in Array2.modify Array2.RowMajor increase a
     ; let val f = Array2.foldi Array2.RowMajor (flash a) 0 region
       in Array2.modify Array2.RowMajor clear a
        ; f + flashes a (n-1)
       end
    end

val adv11 =
    let val l = map (fn row => map (fn x => (x, false)) row) octopi
    in flashes (Array2.fromList l) 100 end

fun flashes' a n =
    let val region = { base = a, row = 0, col = 0, nrows = NONE, ncols = NONE }
    in Array2.modify Array2.RowMajor increase a
     ; let val f = Array2.foldi Array2.RowMajor (flash a) 0 region
       in Array2.modify Array2.RowMajor clear a
        ; if f = 100 then n     (* only this part changed *)
          else flashes' a (n+1)
       end
    end

val adv11b =
    let val l = map (fn row => map (fn x => (x, false)) row) octopi
    in flashes' (Array2.fromList l) 1 end
