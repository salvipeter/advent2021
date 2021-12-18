(* vim: set nowrap: -*- truncate-lines: t -*- *)

datatype snailnum = Pair of snailnum * snailnum | Value of int

val numbers = [Pair(Pair(Pair(Pair(Value 0,Value 6),Pair(Value 8,Value 7)),Pair(Value 2,Value 3)),Pair(Value 3,Pair(Pair(Value 6,Value 5),Pair(Value 0,Value 0)))),Pair(Pair(Value 2,Pair(Value 9,Pair(Value 4,Value 9))),Pair(Pair(Pair(Value 3,Value 0),Value 4),Pair(Value 2,Pair(Value 4,Value 7)))),Pair(Pair(Pair(Value 4,Pair(Value 5,Value 2)),Pair(Value 1,Pair(Value 9,Value 7))),Pair(Pair(Value 2,Value 4),Pair(Pair(Value 4,Value 5),Value 4))),Pair(Pair(Pair(Pair(Value 6,Value 7),Value 8),Pair(Pair(Value 6,Value 6),Value 3)),Pair(Value 6,Pair(Value 7,Pair(Value 4,Value 9)))),Pair(Pair(Pair(Pair(Value 5,Value 4),Pair(Value 7,Value 4)),Pair(Value 8,Pair(Value 2,Value 3))),Pair(Pair(Pair(Value 9,Value 7),Value 6),Pair(Value 2,Pair(Value 4,Value 7)))),Pair(Pair(Pair(Pair(Value 2,Value 4),Pair(Value 0,Value 9)),Value 6),Pair(Pair(Pair(Value 7,Value 3),Pair(Value 5,Value 9)),Pair(Value 0,Pair(Value 2,Value 3)))),Pair(Pair(Value 6,Pair(Value 9,Pair(Value 2,Value 4))),Pair(Pair(Value 3,Value 0),Pair(Value 1,Pair(Value 0,Value 0)))),Pair(Pair(Value 3,Value 7),Pair(Value 3,Pair(Value 2,Value 0))),Pair(Pair(Value 0,Pair(Value 2,Pair(Value 4,Value 6))),Pair(Value 8,Value 9)),Pair(Pair(Pair(Value 7,Value 5),Pair(Pair(Value 3,Value 8),Value 5)),Pair(Pair(Pair(Value 9,Value 8),Pair(Value 4,Value 3)),Pair(Value 5,Pair(Value 1,Value 4)))),Pair(Pair(Value 9,Pair(Pair(Value 4,Value 7),Pair(Value 7,Value 1))),Pair(Pair(Value 7,Pair(Value 8,Value 7)),Pair(Value 4,Pair(Value 2,Value 6)))),Pair(Pair(Pair(Value 0,Value 4),Value 9),Pair(Pair(Pair(Value 5,Value 1),Pair(Value 3,Value 2)),Value 4)),Pair(Pair(Pair(Value 5,Value 9),Value 2),Pair(Value 8,Pair(Value 3,Pair(Value 2,Value 4)))),Pair(Pair(Pair(Pair(Value 2,Value 2),Pair(Value 9,Value 2)),Pair(Value 0,Value 7)),Pair(Pair(Pair(Value 3,Value 7),Value 3),Value 0)),Pair(Value 5,Pair(Pair(Value 9,Value 1),Pair(Pair(Value 6,Value 6),Value 9))),Pair(Pair(Value 5,Pair(Value 1,Pair(Value 1,Value 7))),Pair(Pair(Pair(Value 2,Value 2),Pair(Value 5,Value 2)),Pair(Value 2,Value 0))),Pair(Pair(Pair(Pair(Value 2,Value 0),Value 5),Pair(Pair(Value 6,Value 1),Pair(Value 3,Value 1))),Value 7),Pair(Value 4,Pair(Value 2,Pair(Value 3,Value 3))),Pair(Pair(Pair(Pair(Value 8,Value 3),Pair(Value 3,Value 2)),Pair(Pair(Value 4,Value 0),Value 3)),Pair(Pair(Pair(Value 2,Value 5),Value 9),Value 4)),Pair(Pair(Pair(Value 6,Value 4),Pair(Pair(Value 0,Value 8),Pair(Value 4,Value 9))),Pair(Pair(Pair(Value 7,Value 9),Value 7),Pair(Pair(Value 5,Value 5),Pair(Value 7,Value 8)))),Pair(Value 3,Value 7),Pair(Pair(Value 1,Value 5),Pair(Pair(Pair(Value 3,Value 7),Pair(Value 7,Value 1)),Pair(Pair(Value 7,Value 4),Pair(Value 9,Value 3)))),Pair(Pair(Value 3,Pair(Value 0,Pair(Value 4,Value 4))),Pair(Pair(Value 3,Value 4),Pair(Pair(Value 3,Value 1),Value 0))),Pair(Pair(Value 1,Pair(Pair(Value 1,Value 1),Pair(Value 5,Value 1))),Pair(Pair(Pair(Value 8,Value 0),Value 5),Value 7)),Pair(Pair(Pair(Pair(Value 9,Value 2),Value 0),Value 2),Pair(Value 8,Value 5)),Pair(Value 4,Pair(Pair(Value 0,Pair(Value 0,Value 9)),Pair(Value 2,Value 2))),Pair(Pair(Pair(Value 4,Pair(Value 2,Value 0)),Pair(Pair(Value 5,Value 5),Pair(Value 8,Value 2))),Pair(Pair(Pair(Value 5,Value 1),Pair(Value 7,Value 7)),Pair(Value 0,Value 9))),Pair(Pair(Value 5,Pair(Pair(Value 0,Value 1),Pair(Value 5,Value 9))),Pair(Value 3,Pair(Value 8,Pair(Value 8,Value 4)))),Pair(Pair(Value 1,Value 9),Pair(Pair(Value 3,Pair(Value 1,Value 0)),Pair(Value 4,Value 3))),Pair(Pair(Pair(Value 1,Value 6),Pair(Value 2,Value 8)),Pair(Value 8,Pair(Value 9,Value 3))),Pair(Pair(Pair(Value 3,Value 4),Value 0),Pair(Value 4,Pair(Value 8,Pair(Value 5,Value 8)))),Pair(Pair(Value 8,Pair(Value 9,Value 0)),Pair(Pair(Pair(Value 6,Value 4),Pair(Value 5,Value 5)),Pair(Value 8,Value 3))),Pair(Pair(Pair(Pair(Value 9,Value 1),Pair(Value 3,Value 9)),Pair(Value 1,Pair(Value 8,Value 0))),Pair(Pair(Value 8,Pair(Value 8,Value 5)),Pair(Pair(Value 2,Value 2),Value 0))),Pair(Value 1,Pair(Value 6,Pair(Value 6,Value 7))),Pair(Pair(Pair(Value 5,Pair(Value 5,Value 8)),Pair(Pair(Value 0,Value 8),Value 7)),Pair(Pair(Value 7,Value 6),Pair(Pair(Value 7,Value 6),Pair(Value 3,Value 8)))),Pair(Pair(Value 8,Pair(Value 1,Pair(Value 8,Value 6))),Pair(Pair(Value 8,Value 4),Pair(Pair(Value 3,Value 3),Value 1))),Pair(Value 7,Pair(Value 9,Pair(Value 5,Value 7))),Pair(Pair(Pair(Value 8,Value 9),Pair(Value 9,Value 6)),Pair(Pair(Pair(Value 6,Value 7),Pair(Value 7,Value 4)),Pair(Value 2,Pair(Value 2,Value 6)))),Pair(Pair(Pair(Value 2,Value 0),Value 4),Pair(Value 1,Pair(Value 6,Pair(Value 6,Value 0)))),Pair(Pair(Value 8,Pair(Value 8,Pair(Value 6,Value 1))),Pair(Pair(Value 6,Value 1),Pair(Pair(Value 6,Value 5),Pair(Value 2,Value 3)))),Pair(Value 4,Pair(Value 2,Pair(Pair(Value 9,Value 6),Pair(Value 3,Value 5)))),Pair(Value 6,Pair(Pair(Value 3,Value 7),Pair(Value 6,Value 9))),Pair(Pair(Pair(Pair(Value 8,Value 6),Value 9),Value 4),Pair(Value 8,Pair(Value 5,Value 0))),Pair(Pair(Pair(Pair(Value 6,Value 6),Value 3),Pair(Value 7,Pair(Value 3,Value 9))),Value 1),Pair(Pair(Value 1,Pair(Value 7,Value 5)),Pair(Pair(Value 6,Value 1),Pair(Value 0,Pair(Value 9,Value 3)))),Pair(Pair(Pair(Value 3,Pair(Value 6,Value 0)),Pair(Value 2,Value 5)),Pair(Pair(Value 4,Value 3),Value 0)),Pair(Pair(Pair(Pair(Value 9,Value 2),Value 7),Pair(Pair(Value 3,Value 7),Value 6)),Pair(Pair(Value 1,Pair(Value 9,Value 1)),Pair(Pair(Value 7,Value 1),Pair(Value 7,Value 7)))),Pair(Pair(Pair(Pair(Value 0,Value 7),Value 4),Value 2),Pair(Value 5,Pair(Pair(Value 2,Value 1),Value 3))),Pair(Pair(Pair(Value 1,Value 2),Pair(Pair(Value 6,Value 4),Pair(Value 8,Value 6))),Pair(Pair(Pair(Value 7,Value 3),Value 7),Pair(Pair(Value 6,Value 1),Pair(Value 2,Value 1)))),Pair(Pair(Pair(Pair(Value 7,Value 6),Value 8),Value 5),Pair(Pair(Value 3,Value 3),Pair(Pair(Value 7,Value 3),Value 9))),Pair(Pair(Value 5,Pair(Pair(Value 6,Value 8),Value 0)),Pair(Pair(Value 6,Pair(Value 7,Value 1)),Value 2)),Pair(Pair(Pair(Value 4,Value 8),Pair(Pair(Value 8,Value 2),Pair(Value 6,Value 5))),Pair(Value 5,Pair(Value 5,Pair(Value 8,Value 7)))),Pair(Pair(Value 6,Pair(Pair(Value 4,Value 8),Pair(Value 5,Value 4))),Pair(Pair(Pair(Value 1,Value 7),Value 6),Pair(Value 6,Value 9))),Pair(Value 8,Pair(Value 8,Pair(Value 3,Value 1))),Pair(Pair(Value 8,Value 3),Pair(Value 1,Pair(Value 5,Pair(Value 0,Value 9)))),Pair(Pair(Value 2,Pair(Pair(Value 8,Value 3),Pair(Value 5,Value 1))),Pair(Pair(Value 2,Pair(Value 6,Value 1)),Pair(Pair(Value 4,Value 0),Pair(Value 9,Value 3)))),Pair(Value 5,Value 3),Pair(Pair(Pair(Value 5,Value 3),Pair(Pair(Value 1,Value 2),Pair(Value 4,Value 6))),Pair(Pair(Value 7,Value 6),Pair(Pair(Value 0,Value 3),Value 0))),Pair(Pair(Pair(Value 6,Value 5),Value 8),Pair(Value 2,Pair(Value 8,Value 3))),Pair(Pair(Pair(Value 8,Value 6),Pair(Value 0,Value 5)),Pair(Pair(Value 2,Value 4),Value 5)),Pair(Pair(Pair(Value 1,Pair(Value 4,Value 1)),Pair(Pair(Value 9,Value 4),Value 1)),Value 1),Pair(Pair(Pair(Value 8,Value 6),Pair(Pair(Value 1,Value 4),Pair(Value 9,Value 3))),Pair(Value 4,Pair(Pair(Value 4,Value 4),Value 1))),Pair(Pair(Pair(Value 2,Pair(Value 4,Value 2)),Pair(Value 1,Value 0)),Value 3),Pair(Pair(Value 0,Value 2),Pair(Value 7,Pair(Value 7,Pair(Value 8,Value 5)))),Pair(Pair(Pair(Value 9,Pair(Value 9,Value 5)),Pair(Value 0,Pair(Value 4,Value 8))),Pair(Pair(Value 6,Pair(Value 6,Value 7)),Pair(Pair(Value 3,Value 3),Value 1))),Pair(Value 1,Pair(Pair(Pair(Value 3,Value 7),Pair(Value 3,Value 2)),Value 3)),Pair(Pair(Value 0,Pair(Pair(Value 1,Value 6),Value 4)),Pair(Pair(Pair(Value 2,Value 2),Pair(Value 5,Value 9)),Value 2)),Pair(Pair(Value 5,Value 8),Pair(Value 0,Value 9)),Pair(Pair(Pair(Pair(Value 9,Value 4),Pair(Value 8,Value 8)),Pair(Pair(Value 7,Value 3),Pair(Value 8,Value 1))),Pair(Value 1,Pair(Value 7,Pair(Value 7,Value 6)))),Pair(Pair(Pair(Pair(Value 7,Value 6),Pair(Value 4,Value 2)),Value 7),Pair(Value 3,Pair(Pair(Value 7,Value 5),Pair(Value 0,Value 9)))),Pair(Pair(Pair(Value 5,Value 6),Pair(Value 6,Value 2)),Pair(Pair(Value 8,Value 6),Pair(Value 9,Value 6))),Pair(Pair(Value 4,Value 7),Pair(Value 6,Value 9)),Pair(Value 6,Pair(Pair(Value 0,Pair(Value 7,Value 7)),Pair(Value 1,Value 4))),Pair(Pair(Pair(Pair(Value 2,Value 7),Value 2),Value 4),Pair(Pair(Pair(Value 1,Value 8),Pair(Value 0,Value 3)),Value 3)),Pair(Pair(Value 7,Pair(Pair(Value 1,Value 8),Pair(Value 0,Value 1))),Pair(Pair(Value 3,Value 0),Pair(Pair(Value 5,Value 0),Value 9))),Pair(Pair(Pair(Pair(Value 1,Value 8),Pair(Value 0,Value 3)),Value 2),Pair(Pair(Value 9,Value 5),Value 1)),Pair(Pair(Pair(Pair(Value 1,Value 2),Value 3),Value 6),Pair(Value 3,Pair(Pair(Value 8,Value 3),Pair(Value 8,Value 8)))),Pair(Value 9,Pair(Pair(Value 4,Value 0),Value 2)),Pair(Pair(Pair(Pair(Value 8,Value 5),Value 6),Value 9),Pair(Value 7,Pair(Value 9,Pair(Value 3,Value 4)))),Pair(Pair(Pair(Pair(Value 5,Value 8),Pair(Value 8,Value 5)),Value 0),Value 6),Pair(Pair(Pair(Pair(Value 0,Value 8),Pair(Value 9,Value 3)),Value 3),Pair(Pair(Pair(Value 6,Value 4),Value 9),Pair(Pair(Value 6,Value 8),Value 5))),Pair(Pair(Pair(Pair(Value 2,Value 9),Value 2),Value 0),Pair(Pair(Pair(Value 9,Value 0),Pair(Value 0,Value 7)),Pair(Pair(Value 6,Value 3),Pair(Value 9,Value 8)))),Pair(Pair(Pair(Value 0,Pair(Value 0,Value 5)),Value 1),Value 6),Pair(Pair(Pair(Value 1,Pair(Value 0,Value 5)),Value 9),Pair(Pair(Pair(Value 6,Value 8),Pair(Value 7,Value 4)),Pair(Value 1,Pair(Value 1,Value 1)))),Pair(Pair(Pair(Value 6,Value 1),Pair(Value 8,Value 6)),Pair(Pair(Value 1,Pair(Value 0,Value 8)),Pair(Pair(Value 6,Value 7),Pair(Value 1,Value 8)))),Pair(Pair(Value 5,Pair(Pair(Value 9,Value 9),Value 6)),Pair(Pair(Value 0,Value 7),Pair(Pair(Value 8,Value 2),Pair(Value 4,Value 5)))),Pair(Pair(Value 5,Value 4),Pair(Value 5,Pair(Pair(Value 0,Value 7),Pair(Value 5,Value 7)))),Pair(Pair(Value 5,Pair(Value 4,Value 8)),Pair(Pair(Value 5,Pair(Value 0,Value 7)),Pair(Value 8,Value 6))),Pair(Pair(Pair(Pair(Value 9,Value 5),Value 2),Pair(Value 3,Pair(Value 9,Value 6))),Pair(Pair(Value 6,Value 8),Pair(Value 3,Value 8))),Pair(Pair(Pair(Pair(Value 1,Value 4),Pair(Value 2,Value 9)),Pair(Value 2,Value 4)),Pair(Pair(Value 1,Value 3),Pair(Pair(Value 0,Value 4),Pair(Value 9,Value 9)))),Pair(Pair(Value 0,Value 4),Pair(Pair(Value 7,Pair(Value 1,Value 4)),Value 2)),Pair(Pair(Value 6,Value 4),Pair(Pair(Pair(Value 2,Value 7),Value 9),Value 2)),Pair(Pair(Pair(Pair(Value 9,Value 6),Value 6),Pair(Pair(Value 4,Value 7),Pair(Value 3,Value 7))),Pair(Pair(Pair(Value 4,Value 8),Value 4),Pair(Pair(Value 5,Value 2),Pair(Value 4,Value 8)))),Pair(Pair(Pair(Pair(Value 8,Value 8),Value 0),Pair(Value 6,Value 7)),Pair(Value 3,Pair(Value 0,Pair(Value 7,Value 1)))),Pair(Pair(Pair(Value 0,Pair(Value 0,Value 3)),Value 7),Pair(Pair(Value 2,Value 0),Pair(Value 6,Pair(Value 4,Value 5)))),Pair(Pair(Pair(Pair(Value 0,Value 4),Value 5),Pair(Value 4,Pair(Value 2,Value 6))),Pair(Pair(Value 9,Value 9),Value 7)),Pair(Value 1,Pair(Value 8,Value 8)),Pair(Pair(Pair(Value 4,Value 2),Pair(Value 2,Pair(Value 6,Value 6))),Value 7),Pair(Value 7,Pair(Value 3,Pair(Value 4,Pair(Value 2,Value 3)))),Pair(Value 0,Value 9)]

(* Updates the leftmost  value to the value of the second argument, when it is not NONE. *)
fun updateLeft     x           NONE   = x
  | updateLeft (Value x)     (SOME v) = Value (x + v)
  | updateLeft (Pair (x,y))     v     = Pair (updateLeft x v, y)

(* Updates the rightmost value to the value of the second argument, when it is not NONE. *)
fun updateRight     x          NONE   = x
  | updateRight (Value x)    (SOME v) = Value (x + v)
  | updateRight (Pair (x,y))    v     = Pair (x, updateRight y v)

(* Returns (SOME x') when there was an explosion. *)
fun explode x = let
    datatype explosion = Dud of snailnum
                       | Exploded of (int option * snailnum * int option)
    (*
       f l x r n :
       - (l,r) are optional snailnums containing the left/right neighbors
       -   x   is the current snailnum
       -   n   is the current depth
       Returns (Dud exp) when nothing happened;
         and   (Exploded (a,x',b)) when there was an explosion.
               Here a and b are the optional values to be added on the left/right.
      *)
    fun f _ (Value x) _ _ = Dud (Value x)
      | f _ (Pair (Value a, Value b)) _ 4 = Exploded (SOME a, Value 0, SOME b)
      | f l (Pair (x, y)) r n =
        case f l x (SOME y) (n+1) of
            Exploded (a,x',b) => Exploded (a, Pair (x', updateLeft y b), NONE)
          | Dud x =>
            case f (SOME x) y r (n+1) of
                Exploded (a,y',b) => Exploded (NONE, Pair (updateRight x a, y'), b)
              | Dud y => Dud (Pair (x,y))
in
    case f NONE x NONE 0 of
        Dud         _     => NONE
      | Exploded (_,x',_) => SOME x'
end

(* Returns (SOME x') when a value was split. *)
fun split x = let
    val changed = ref false
    fun f (Pair (x,y)) = if !changed then Pair (x,y) else Pair (f x, f y)
      | f (Value x) =
        if not (!changed) andalso x > 9
        then (
            changed := true;
            Pair (Value (x div 2),
                  Value (x div 2 + x mod 2))
        )
        else Value x
    val x' = f x
in if !changed then SOME x' else NONE end

fun reduce x =
    case explode x of
        SOME x' => reduce x'
      | NONE    =>
        case split x of
            SOME x' => reduce x'
          | NONE    => x

fun add (x,y) = (reduce o Pair) (y,x)

fun magnitude (Pair (x,y)) = 3 * magnitude x + 2 * magnitude y
  | magnitude (Value x) = x

fun magnitudeSum xs = magnitude (foldl add (hd xs) (tl xs))

val adv18 = magnitudeSum numbers

fun select 0   _     = [[]]
  | select _   []    = []
  | select n (x::xs) = let
      val a = select   n   xs
      val b = select (n-1) xs
  in a @ (map (fn ys => (x::ys)) b) end

val adv18b = let
    val pairs   = select 2 numbers
    val mags    = map magnitudeSum pairs
    val revmags = map (magnitudeSum o rev) pairs
in foldl Int.max 0 (mags @ revmags) end
