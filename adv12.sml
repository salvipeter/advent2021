(* vim: set nowrap: -*- truncate-lines: t -*- *)

datatype cave = Start | End | Small of string | Large of string

val caves = [(Small "pg",Large "ch"),(Small "pg",Small "yd"),(Small "yd",Start),(Small "fe",Small "hv"),(Small "bi",Large "ch"),(Large "ch",Small "yd"),(End,Small "bi"),(Small "fe",Large "ry"),(Small "ng",Large "ch"),(Small "fe",Large "ch"),(Small "ng",Small "pg"),(Small "hv",Large "fl"),(Large "fl",Small "fe"),(Small "hv",Small "pg"),(Small "bi",Small "hv"),(Large "ch",End),(Small "hv",Small "ng"),(Small "yd",Small "ng"),(Small "pg",Small "fe"),(Start,Small "ng"),(End,Large "fl"),(Small "fe",Small "bi"),(Large "fl",Small "ks"),(Small "pg",Start)]

fun adjacent (pos: cave) caves =
    let fun f ((a,b), ys) =
            if a = pos then (b :: ys) else
            if b = pos then (a :: ys) else
            ys
    in foldl f [] caves end

fun notStart x = x <> Start

fun visitable visited (s: string) =
    not (List.exists (fn x => s = x) visited)

fun path xs =
    let val count = ref 0
        fun f pos visits =
            let val next = adjacent pos xs
            in app (recurse visits) (List.filter notStart next) end
        and recurse visits p =
            case p of
                Start   => ()
              | End     => count := !count + 1
              | Large _ => f p visits
              | Small s => if visitable visits s
                           then f p (s::visits)
                           else ()
    in f Start [] ; !count end

val adv12 = path caves

fun path' xs =
    let val count = ref 0
        fun f pos visits =
            let val next = adjacent pos xs
            in app (recurse visits) (List.filter notStart next) end
        and recurse visits p =
            case p of
                Start   => ()
              | End     => count := !count + 1
              | Large _ => f p visits
              | Small s => trySmall s visits
        and trySmall s (once, twice) =
            case twice of
                NONE    => if visitable once s
                           then f (Small s) (s::once, NONE)
                           else f (Small s) (once, SOME s)
              | SOME s' => if visitable once s
                           then f (Small s) (s::once, twice)
                           else ()
    in f Start ([], NONE) ; !count end

val adv12b = path' caves
