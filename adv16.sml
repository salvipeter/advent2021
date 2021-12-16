(* vim: set nowrap: -*- truncate-lines: t -*- *)

val bits = "E20D4100AA9C0199CA6A3D9D6352294D47B3AC6A4335FBE3FDD251003873657600B46F8DC600AE80273CCD2D5028B6600AF802B2959524B727D8A8CC3CCEEF3497188C017A005466DAA6FDB3A96D5944C014C006865D5A7255D79926F5E69200A164C1A65E26C867DDE7D7E4794FE72F3100C0159A42952A7008A6A5C189BCD456442E4A0A46008580273ADB3AD1224E600ACD37E802200084C1083F1540010E8D105A371802D3B845A0090E4BD59DE0E52FFC659A5EBE99AC2B7004A3ECC7E58814492C4E2918023379DA96006EC0008545B84B1B00010F8E915E1E20087D3D0E577B1C9A4C93DD233E2ECF65265D800031D97C8ACCCDDE74A64BD4CC284E401444B05F802B3711695C65BCC010A004067D2E7C4208A803F23B139B9470D7333B71240050A20042236C6A834600C4568F5048801098B90B626B00155271573008A4C7A71662848821001093CB4A009C77874200FCE6E7391049EB509FE3E910421924D3006C40198BB11E2A8803B1AE2A4431007A15C6E8F26009E002A725A5292D294FED5500C7170038C00E602A8CC00D60259D008B140201DC00C401B05400E201608804D45003C00393600B94400970020C00F6002127128C0129CDC7B4F46C91A0084E7C6648DC000DC89D341B23B8D95C802D09453A0069263D8219DF680E339003032A6F30F126780002CC333005E8035400042635C578A8200DC198890AA46F394B29C4016A4960C70017D99D7E8AF309CC014FCFDFB0FE0DA490A6F9D490010567A3780549539ED49167BA47338FAAC1F3005255AEC01200043A3E46C84E200CC4E895114C011C0054A522592912C9C8FDE10005D8164026C70066C200C4618BD074401E8C90E23ACDFE5642700A6672D73F285644B237E8CCCCB77738A0801A3CFED364B823334C46303496C940"

(* Get bits i..i+n-1 as an integer *)
local val data = valOf (StringCvt.scanString (IntInf.scan StringCvt.HEX) bits)
      val data_size = size bits * 4
in fun getBits (i, n) =
       let val shift = data_size - (n + i)
           val shifted = IntInf.~>> (data, Word.fromInt shift)
           val mask = IntInf.<< (IntInf.fromInt 1, Word.fromInt n) - 1
           val masked = IntInf.andb (shifted, mask)
       in IntInf.toInt masked end
end

datatype packet   = Packet of int * content
     and operator = Sum | Product | Minimum | Maximum | Greater | Less | Equal
     and content  = Literal of int | Operator of operator * packet list

fun opType 0 = Sum
  | opType 1 = Product
  | opType 2 = Minimum
  | opType 3 = Maximum
  | opType 5 = Greater
  | opType 6 = Less
  | opType 7 = Equal
  | opType _ = raise Fail "invalid operator"

(* Returns a (value, next_index) pair *)
fun readLiteral i =
    let fun f j acc =
            if getBits (j, 1) = 1
            then let val acc' = acc * 16 + getBits (j+1, 4)
                 in f (j+5) acc' end
            else (acc * 16 + getBits (j+1, 4), j + 5)
    in f i 0 end

(* Returns a (packet, next_index) pair *)
fun readPacket i =
    let val version = getBits (i, 3)
    in case getBits (i+3, 3) of
           4 => let val (lit, next) = readLiteral (i+6)
                in (Packet (version, Literal lit), next) end
         | t => let val (packets, next) = readSubPackets (i+6)
                    val operator = Operator (opType t, packets)
                in (Packet (version, operator), next) end
    end
and readSubPackets i =
    if getBits (i, 1) = 0
    then let val len = getBits (i+1, 15)
             val packets = readUntil (i+16) (i+16+len)
         in (packets, i+16+len) end
    else let val num = getBits (i+1, 11)
         in readTimes (i+12) num end
and readUntil i next =
    let val (packet, next') = readPacket i
    in if next = next' then [packet]
       else packet :: readUntil next' next
    end
and readTimes i n =
    let fun f i 0 acc = (rev acc, i)
          | f i n acc = let val (packet, next) = readPacket i
                        in f next (n-1) (packet::acc) end
    in f i n [] end

fun versionSum (Packet p) =
    case p of
        (n, Literal     _  ) => n
      | (n, Operator (_,xs)) => foldl op + n (map versionSum xs)

val adv16 = (versionSum o #1 o readPacket) 0

fun evaluate (Packet p) =
    case p of
        (_, Literal n) => n
      | (_, Operator (operator,xs)) => evalOp operator xs
and evalOp Sum     xs = foldl op + 0 (map evaluate xs)
  | evalOp Product xs = foldl op * 1 (map evaluate xs)
  | evalOp Minimum xs = let val xs' = map evaluate xs
                        in foldl Int.min (hd xs') (tl xs') end
  | evalOp Maximum xs = let val xs' = map evaluate xs
                        in foldl Int.max (hd xs') (tl xs') end
  | evalOp Greater xs = let val (x,y) = evalPair xs
                        in if x > y then 1 else 0 end
  | evalOp Less    xs = let val (x,y) = evalPair xs
                        in if x < y then 1 else 0 end
  | evalOp Equal   xs = let val (x,y) = evalPair xs
                        in if x = y then 1 else 0 end
and evalPair [x,y] = (evaluate x, evaluate y)
  | evalPair   _   = raise Fail "invalid operands"

val adv16b = (evaluate o #1 o readPacket) 0
