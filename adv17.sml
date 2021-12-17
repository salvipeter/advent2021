val minx = 240 and maxx = 292 and miny = ~90 and maxy = ~57

fun pos (dx,dy) t =
    (if t <= dx
     then dx * t - (t - 1) * t div 2
     else dx * dx - (dx - 1) * dx div 2,
     dy * t - (t - 1) * t div 2)

(* Time to reach y with a given starting dy speed *)
fun findTime dy y =
    let val dy = Real.fromInt dy
        val y  = Real.fromInt y
    in 0.5 + dy + Math.sqrt (0.25 + dy * (dy + 1.0) - 2.0 * y) end

(* Starting dx speed to reach x in time t *)
fun findDx t x =
    let val t' = Real.fromInt t
        val x' = Real.fromInt x
        val dx = (t' * (t' - 1.0) / 2.0 + x') / t'
    in if t' <= dx then dx
       else findDx (t-1) x end

(* Highest position *)
fun top d = pos d (#2 d)

val adv17 =
    let val result = ref NONE
        val dy = ref (abs miny)
    in while !result = NONE do let
           val mint = ceil  (findTime (!dy) maxy)
           val maxt = floor (findTime (!dy) miny)
           val t = ref mint
       in while !t <= maxt andalso !result = NONE do let
              val dxmin = ceil  (findDx (!t) minx)
              val dxmax = floor (findDx (!t) maxx)
          in if dxmin <= dxmax then result := SOME (dxmin,!dy)
             else t := !t + 1
          end
        ; dy := !dy - 1
       end
     ; (#2 o top o valOf o !) result
    end

fun unique ([]: (int * int) list) = []
  | unique (x::xs) = if List.exists (fn y => x = y) xs
                     then uniq xs
                     else x :: uniq xs

val adv17b =
    let val speeds = ref []
        val dy = ref miny
    in while !dy < (abs miny) do let
           val mint = ceil  (findTime (!dy) maxy)
           val maxt = floor (findTime (!dy) miny)
           val t = ref mint
       in while !t <= maxt do let
              val dxmin = ceil  (findDx (!t) minx)
              val dxmax = floor (findDx (!t) maxx)
              val xs = List.tabulate (dxmax - dxmin + 1, fn dx => (dxmin + dx, !dy))
          in speeds := xs @ !speeds
           ; t := !t + 1
          end
        ; dy := !dy + 1
       end
     ; (length o unique o !) speeds
    end
