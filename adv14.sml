(* vim: set nowrap: -*- truncate-lines: t -*- *)

val template = "BNBBNCFHHKOSCHBKKSHN"
val rules = [("CH",#"S"),("KK",#"V"),("FS",#"V"),("CN",#"P"),("VC",#"N"),("CB",#"V"),("VK",#"H"),("CF",#"N"),("PO",#"O"),("KC",#"S"),("HC",#"P"),("PP",#"B"),("KO",#"B"),("BK",#"P"),("BH",#"N"),("CC",#"N"),("PC",#"O"),("FK",#"N"),("KF",#"F"),("FH",#"S"),("SS",#"V"),("ON",#"K"),("OV",#"K"),("NK",#"H"),("BO",#"C"),("VP",#"O"),("CS",#"V"),("KS",#"K"),("SK",#"B"),("OP",#"S"),("PK",#"S"),("HF",#"P"),("SV",#"P"),("SB",#"C"),("BC",#"C"),("FP",#"H"),("FC",#"P"),("PB",#"N"),("NV",#"F"),("VO",#"F"),("VH",#"P"),("BB",#"N"),("SF",#"F"),("NB",#"K"),("KB",#"S"),("VV",#"S"),("NP",#"N"),("SO",#"O"),("PN",#"B"),("BP",#"H"),("BV",#"V"),("OB",#"C"),("HV",#"N"),("PF",#"B"),("SP",#"N"),("HN",#"N"),("CV",#"H"),("BN",#"V"),("PS",#"V"),("CO",#"S"),("BS",#"N"),("VB",#"H"),("PV",#"P"),("NN",#"P"),("HS",#"C"),("OS",#"P"),("FB",#"S"),("HO",#"C"),("KH",#"H"),("HB",#"K"),("VF",#"S"),("CK",#"K"),("FF",#"H"),("FN",#"P"),("OK",#"F"),("SC",#"B"),("HH",#"N"),("OH",#"O"),("VS",#"N"),("FO",#"N"),("OC",#"H"),("NF",#"F"),("PH",#"S"),("HK",#"K"),("NH",#"H"),("FV",#"S"),("OF",#"V"),("NC",#"O"),("HP",#"O"),("KP",#"B"),("BF",#"N"),("NO",#"S"),("CP",#"C"),("NS",#"N"),("VN",#"K"),("KV",#"N"),("OO",#"V"),("SN",#"O"),("KN",#"C"),("SH",#"F")]

(* ORD_KEY, ORD_MAP & ListMapFn are in the SML/NJ Util library *)
structure StringOrd: ORD_KEY =
  struct
    type ord_key = string
    val compare = String.compare
  end
structure StringMap: ORD_MAP = ListMapFn (StringOrd)

(* Convenience function - takes (map, key, value) *)
val add = StringMap.insertWith (op +)

(* Initializes a (letter pair -> count) map *)
fun initCounts str =
    let val n = size str
        fun f m i =
            if i = n - 1 then m
            else let val s = substring (str, i, 2)
                 in f (add (m, s, 1)) (i+1) end
    in f StringMap.empty 0 end

(* Executes one step with the given rule- and count-maps *)
fun step rmap cmap =
    let fun f (str, n, m) =
            let val c     = StringMap.lookup (rmap, str)
                val left  = implode [String.sub (str,0), c]
                val right = implode [c, String.sub (str,1)]
                val m = add (m, left,  n)
                val m = add (m, str,  ~n)
                val m = add (m, right, n)
            in m end
    in StringMap.foldli f cmap cmap end

(*
  The start- and end-characters of the original template are counted only once,
  all other characters in the pairs are counted twice.
*)
fun score original cmap =
    let val c0 = String.sub (original, 0)
        val cn = String.sub (original, size original - 1)
        val s  = StringMap.empty
        val s  = add (s, str c0, 1)
        val s  = add (s, str cn, 1)
        fun f (str, n, m) =
            let val m = add (m, substring (str,0,1), n)
                val m = add (m, substring (str,1,1), n)
            in m end
        val m   = StringMap.foldli f s cmap
        val max = StringMap.foldl Int.max  0  m
        val min = StringMap.foldl Int.min max m
    in (max - min) div 2 end

(* Call with 10 for part 1, and with 40 for part 2 *)
fun adv14 iterations =
    let fun f ((str,c), m) = StringMap.insert (m, str, c)
        val rmap = foldl f StringMap.empty rules
        val cmap = initCounts template
        fun repeat 0 _ x = x
          | repeat n f x = repeat (n-1) f (f x)
    in score template (repeat iterations (step rmap) cmap) end
