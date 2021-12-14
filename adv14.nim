# vim: set nowrap: -*- truncate-lines: t -*-

import tables

const
  polymer = "BNBBNCFHHKOSCHBKKSHN"
  rules = {"CH":'S',"KK":'V',"FS":'V',"CN":'P',"VC":'N',"CB":'V',"VK":'H',"CF":'N',"PO":'O',"KC":'S',"HC":'P',"PP":'B',"KO":'B',"BK":'P',"BH":'N',"CC":'N',"PC":'O',"FK":'N',"KF":'F',"FH":'S',"SS":'V',"ON":'K',"OV":'K',"NK":'H',"BO":'C',"VP":'O',"CS":'V',"KS":'K',"SK":'B',"OP":'S',"PK":'S',"HF":'P',"SV":'P',"SB":'C',"BC":'C',"FP":'H',"FC":'P',"PB":'N',"NV":'F',"VO":'F',"VH":'P',"BB":'N',"SF":'F',"NB":'K',"KB":'S',"VV":'S',"NP":'N',"SO":'O',"PN":'B',"BP":'H',"BV":'V',"OB":'C',"HV":'N',"PF":'B',"SP":'N',"HN":'N',"CV":'H',"BN":'V',"PS":'V',"CO":'S',"BS":'N',"VB":'H',"PV":'P',"NN":'P',"HS":'C',"OS":'P',"FB":'S',"HO":'C',"KH":'H',"HB":'K',"VF":'S',"CK":'K',"FF":'H',"FN":'P',"OK":'F',"SC":'B',"HH":'N',"OH":'O',"VS":'N',"FO":'N',"OC":'H',"NF":'F',"PH":'S',"HK":'K',"NH":'H',"FV":'S',"OF":'V',"NC":'O',"HP":'O',"KP":'B',"BF":'N',"NO":'S',"CP":'C',"NS":'N',"VN":'K',"KV":'N',"OO":'V',"SN":'O',"KN":'C',"SH":'F'}

func init(str: string): CountTable[string] =
  for i in 1..<str.len:
    result.inc str[i-1..i]

func step(rmap: Table[string, char],
          cmap: CountTable[string]): CountTable[string] =
  result = cmap
  for str, n in cmap.pairs:
    let
      c     = rmap[str]
      left  = str[0..0] & $c
      right = $c & str[1..1]
    result.inc left,  n
    result.inc str,  -n
    result.inc right, n

func score(str: string, cmap: CountTable[string]): int =
  var counts = newCountTable[char]()
  counts.inc str[0]
  counts.inc str[str.len-1]
  for str, n in cmap.pairs:
    counts.inc str[0], n
    counts.inc str[1], n
  (counts.largest[1] - counts.smallest[1]) div 2

for iterations in [10, 40]:
  let rmap = rules.toTable
  var cmap = init polymer
  for i in 1..iterations:
    cmap = step(rmap, cmap)
  echo $score(polymer, cmap)
