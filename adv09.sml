(* vim: set nowrap: -*- truncate-lines: t -*- *)

val height = [[5,4,5,6,7,8,9,3,4,9,8,8,6,4,5,6,8,9,0,1,2,3,9,8,5,4,3,5,5,7,8,9,9,6,5,4,3,2,1,3,4,5,6,7,8,9,6,5,6,8,9,9,9,9,6,4,6,7,7,8,9,2,3,4,9,8,9,7,6,5,4,4,2,3,4,5,7,8,9,7,7,8,9,9,9,9,8,9,6,5,2,3,4,9,8,7,9,8,9,9],[4,3,4,9,8,9,1,2,9,8,7,6,5,3,4,8,7,8,9,3,3,9,8,7,5,3,2,3,4,5,6,7,8,9,6,6,5,4,3,4,5,6,8,9,9,6,5,4,5,6,9,8,8,7,4,3,5,6,6,7,9,9,5,9,8,7,9,8,9,8,3,2,1,4,5,7,8,9,3,5,6,9,9,9,8,8,7,9,9,3,1,9,9,8,7,6,5,6,6,8],[1,2,9,8,9,1,0,9,8,9,8,7,3,2,3,4,5,9,5,4,9,8,7,6,4,3,1,2,3,4,5,6,7,8,9,7,6,7,4,6,8,9,9,9,8,9,6,5,6,9,8,7,5,6,3,2,3,4,5,6,7,8,9,9,7,6,7,9,8,7,4,4,2,5,7,8,9,5,4,6,7,8,9,8,7,9,6,8,8,9,9,8,9,7,6,5,4,4,5,7],[2,9,8,7,9,3,9,8,7,5,4,3,2,1,2,3,4,8,9,9,9,9,9,5,3,2,0,1,2,3,4,5,9,9,6,9,8,6,5,7,9,7,9,9,7,9,9,9,7,9,6,5,4,3,2,0,2,3,4,7,9,9,9,8,9,5,9,8,7,6,5,5,3,6,8,9,9,6,5,7,8,9,8,7,6,8,5,6,7,8,9,7,8,9,5,4,3,3,4,5],[9,8,9,6,8,9,9,9,8,4,3,2,1,0,1,2,6,6,8,8,9,9,8,6,5,3,1,3,5,4,6,7,8,9,5,6,9,7,9,8,9,6,5,4,6,9,8,7,9,8,7,6,5,5,3,1,3,5,5,6,8,9,8,7,6,4,3,9,8,8,7,6,7,9,9,7,8,9,6,8,9,8,7,6,5,9,4,5,6,9,7,6,9,8,7,2,2,2,5,6],[8,7,6,5,7,8,9,9,6,5,4,4,2,1,4,3,4,5,6,7,8,9,9,9,6,5,7,9,6,5,8,8,9,5,4,3,4,9,9,9,8,7,6,3,9,8,7,6,7,9,8,7,8,6,4,5,8,9,6,7,9,8,7,6,5,4,2,0,9,9,8,9,8,9,6,6,7,8,9,9,9,9,8,3,3,1,2,3,5,9,8,5,8,9,8,1,0,1,2,3],[9,9,5,4,6,2,9,8,7,6,5,5,3,2,3,4,6,6,7,8,9,9,9,8,7,9,8,8,7,6,7,9,3,2,1,2,9,8,9,9,9,8,9,2,1,9,7,5,5,3,9,8,8,7,8,6,7,8,9,8,9,9,8,7,9,4,3,9,8,9,9,5,9,3,5,5,6,7,8,9,9,8,9,2,1,0,5,7,9,8,7,6,7,8,9,2,4,3,4,5],[6,5,4,3,2,1,2,9,8,9,6,5,4,3,4,5,7,8,8,9,9,9,8,9,8,9,9,9,9,9,8,9,4,1,0,9,8,7,8,9,9,9,8,9,9,8,5,4,3,2,2,9,9,8,9,7,8,9,7,9,7,9,9,9,8,9,9,8,7,6,5,4,3,1,3,4,9,8,9,9,9,7,6,3,2,3,4,5,6,9,8,7,8,9,5,4,5,4,5,6],[8,6,5,4,5,4,3,4,9,8,7,6,5,4,7,6,8,9,9,3,9,8,7,5,9,1,2,6,6,7,9,9,5,3,9,9,7,6,7,8,9,8,7,9,8,7,6,3,2,1,0,1,2,9,7,9,9,7,6,5,6,7,9,8,6,7,8,9,6,5,4,3,2,0,1,5,6,9,9,8,7,6,5,4,6,5,5,9,9,8,9,8,9,8,7,5,6,5,6,7],[8,7,6,7,6,5,4,5,7,9,8,7,9,9,8,9,9,4,3,2,9,7,6,4,2,0,1,4,5,6,7,8,9,9,8,8,9,5,6,7,8,9,6,5,9,7,5,4,3,2,2,3,4,5,6,7,8,9,3,4,8,9,7,6,5,7,8,9,7,6,5,4,3,1,2,6,9,2,1,9,9,8,7,5,8,6,9,8,9,7,9,9,9,9,7,6,8,9,7,9],[9,9,8,8,7,6,5,6,7,8,9,9,8,9,9,9,7,6,5,9,6,5,4,3,2,1,2,3,7,8,9,9,9,8,7,6,5,4,5,6,9,9,7,4,9,8,6,5,4,3,4,5,7,8,9,8,9,3,2,1,9,8,6,5,4,5,8,8,9,7,6,5,4,4,3,4,8,9,0,1,3,9,7,6,9,9,8,7,8,6,7,8,9,6,9,7,9,9,8,9],[9,8,9,9,8,7,6,9,8,9,9,8,7,8,9,9,8,9,9,8,7,6,8,4,3,2,3,4,8,9,1,2,4,9,6,5,4,3,6,9,8,8,9,3,2,9,7,6,5,5,6,7,8,9,9,9,9,4,3,2,5,9,7,6,3,4,6,7,8,9,8,8,7,5,6,5,7,8,9,3,4,9,8,9,8,7,6,5,7,5,6,9,7,5,3,9,5,6,9,9],[8,7,6,7,9,8,7,8,9,9,8,7,6,9,9,9,9,9,7,9,8,7,7,5,5,3,4,6,9,1,0,1,9,8,7,7,3,2,3,5,6,7,9,9,3,9,8,7,6,6,8,8,9,8,9,8,7,5,6,3,4,9,8,7,4,9,7,8,9,3,9,9,9,6,7,9,8,9,9,9,5,9,9,9,9,6,5,4,2,4,8,9,5,4,2,3,4,7,9,9],[9,6,5,3,2,9,9,9,9,9,8,7,5,7,8,9,9,8,6,5,9,9,8,9,6,4,6,7,8,9,9,9,8,7,6,5,2,1,3,4,5,6,7,8,9,9,9,9,9,7,8,9,8,7,9,8,7,6,7,8,6,7,9,8,9,9,8,9,3,2,3,9,8,7,8,9,9,9,9,8,9,8,9,9,8,6,4,3,1,3,5,9,3,2,1,2,9,9,7,8],[8,6,3,2,1,0,2,3,9,8,7,6,4,5,7,8,9,6,5,4,3,4,9,8,7,5,7,8,9,7,8,6,9,8,7,4,3,0,2,8,9,7,9,9,8,9,9,9,8,9,9,8,8,6,6,9,8,7,8,9,9,8,9,9,9,8,9,5,1,0,1,2,9,9,9,9,9,8,7,7,9,7,9,8,7,6,5,9,2,3,9,8,9,1,0,9,8,7,6,9],[6,5,4,3,2,2,3,9,8,7,6,4,3,5,8,9,8,7,4,3,2,3,4,9,8,6,7,8,9,6,5,4,6,9,7,6,4,1,6,7,8,9,9,8,7,8,9,8,7,8,9,7,6,5,4,3,9,8,9,9,9,9,8,9,8,7,6,4,3,1,2,3,6,7,8,9,8,7,6,5,7,6,8,9,8,7,9,8,9,9,8,7,8,9,9,8,9,7,5,6],[7,6,5,4,5,6,4,5,9,7,5,3,2,5,9,9,9,9,3,2,1,4,9,9,8,7,8,9,9,7,5,3,7,9,8,9,4,3,5,6,8,9,9,8,6,5,4,7,6,7,8,9,7,6,2,1,2,9,7,8,9,8,7,9,8,7,6,5,4,3,3,4,5,6,9,9,7,6,5,4,5,5,7,8,9,9,8,7,6,8,4,5,6,9,8,7,7,5,4,6],[8,7,9,9,7,9,6,7,9,8,5,4,3,4,8,9,8,7,4,1,0,9,8,9,9,8,9,9,8,7,6,1,2,3,9,8,9,4,5,6,9,8,9,8,7,4,3,4,5,6,7,8,9,9,5,3,4,5,6,9,8,7,6,5,9,8,7,8,7,6,5,9,6,9,9,8,9,5,4,3,1,4,5,9,7,9,9,8,5,9,3,4,9,8,7,6,6,4,3,4],[9,9,8,9,9,8,7,9,8,7,6,5,4,6,7,8,9,6,5,9,2,9,7,9,9,9,9,9,9,9,3,2,3,9,9,7,9,5,6,9,8,7,6,5,4,3,2,3,4,5,8,9,9,8,6,7,9,7,9,8,7,6,5,3,2,9,8,9,8,9,9,8,9,8,9,7,8,9,5,4,2,3,4,5,6,9,8,7,4,3,2,9,8,7,6,5,5,3,2,5],[9,8,7,6,8,9,8,9,9,8,7,7,9,8,9,9,8,9,9,8,9,8,5,8,9,9,9,8,7,5,4,9,9,8,7,6,8,9,9,8,7,6,5,4,3,2,1,0,7,6,7,8,9,9,9,9,8,9,2,9,8,9,4,2,1,3,9,5,9,9,8,7,9,7,6,6,8,8,9,4,3,5,6,6,8,9,8,6,5,4,3,4,9,8,9,4,3,2,1,2],[9,7,8,5,7,9,9,6,5,9,9,8,9,9,5,6,7,9,8,7,6,7,4,6,7,8,9,9,7,6,9,8,7,8,6,5,7,8,8,9,8,7,7,5,4,3,2,3,4,7,8,9,9,9,8,9,7,9,1,0,9,8,9,3,0,1,3,4,9,8,7,6,5,4,5,5,6,7,9,5,4,6,8,9,9,5,9,8,6,6,5,5,6,9,9,5,4,3,2,3],[9,6,5,4,5,8,8,9,4,9,8,9,3,2,4,5,6,9,7,6,5,4,3,5,6,9,9,9,8,9,8,7,6,7,3,4,5,6,7,8,9,8,7,6,9,8,6,4,5,6,7,9,8,7,6,7,6,7,9,9,8,7,8,2,1,2,9,9,8,7,6,5,4,3,4,3,5,6,8,9,5,8,9,1,2,4,5,9,9,8,8,7,9,8,7,6,6,5,3,4],[7,5,4,3,5,6,7,9,9,8,7,3,2,1,9,6,8,9,9,7,9,3,2,3,4,7,8,9,9,9,9,5,4,3,2,3,6,7,8,9,6,9,9,7,9,8,7,5,9,7,9,8,7,6,5,6,5,6,8,9,7,6,4,3,2,9,8,7,9,9,9,7,3,2,1,2,3,8,9,9,7,9,1,0,1,9,6,7,9,9,9,8,9,9,8,8,7,6,4,6],[6,4,3,2,4,5,7,8,9,8,6,5,3,9,8,9,9,3,2,9,8,9,3,4,5,6,9,9,9,8,7,6,8,4,5,4,5,8,9,3,5,6,8,9,9,9,8,6,8,9,7,9,8,7,4,5,4,5,9,8,9,6,5,4,3,4,9,6,9,8,7,6,4,3,2,4,5,6,7,8,9,9,9,9,9,8,9,9,7,6,7,9,9,9,9,9,8,7,8,7],[7,4,2,1,3,4,8,9,9,9,7,6,9,8,7,8,9,4,9,8,7,8,9,9,6,7,8,9,9,9,9,8,7,6,6,5,7,9,3,2,3,7,8,9,9,9,9,7,9,6,5,7,9,5,3,4,3,5,6,7,9,8,7,6,5,9,8,5,3,9,8,7,5,6,3,4,6,8,9,9,7,9,8,7,6,7,9,8,6,5,7,7,8,9,4,2,9,8,9,8],[3,2,1,0,1,2,8,7,9,9,8,9,8,7,6,7,9,5,6,9,6,5,9,8,9,8,9,9,9,8,7,9,8,8,8,6,7,8,9,1,2,3,4,9,8,9,9,8,9,3,4,5,9,4,2,1,2,4,7,8,9,9,9,8,9,8,7,6,2,1,9,8,6,8,7,5,6,7,8,9,6,7,9,6,5,9,8,7,5,4,5,6,7,9,9,4,5,9,2,9],[4,3,2,3,2,3,5,6,7,8,9,2,3,4,5,6,8,9,9,8,5,4,5,6,7,9,7,8,9,7,6,7,9,9,9,7,8,9,1,0,3,4,9,8,7,8,9,9,1,2,4,9,8,7,3,6,7,5,6,7,8,9,8,9,7,9,8,7,1,0,1,9,7,9,8,7,9,8,9,4,5,9,8,9,3,2,9,6,5,3,4,5,6,7,8,9,6,9,1,2],[5,4,5,4,3,4,8,7,8,9,6,3,6,5,6,7,9,8,9,7,6,5,8,7,8,9,6,7,9,6,5,4,5,6,9,8,9,3,2,1,3,9,8,7,6,7,8,9,0,9,5,6,9,5,4,5,7,8,7,8,9,9,7,7,6,7,9,8,2,1,2,9,8,9,9,8,9,9,4,2,9,9,7,9,4,9,8,7,3,2,1,4,5,7,9,9,9,8,9,9],[7,6,8,9,6,5,6,8,9,6,5,4,7,6,7,8,9,7,9,8,9,6,9,8,9,5,4,9,8,7,6,5,6,7,8,9,5,4,3,2,9,9,9,6,5,6,7,9,9,8,9,7,9,6,7,7,8,9,8,9,7,8,5,5,5,6,9,9,3,2,4,5,9,0,1,9,8,6,5,9,8,7,6,8,9,8,7,6,4,3,2,3,4,6,8,9,9,7,6,8],[9,7,9,8,7,9,8,9,9,8,6,5,8,9,9,9,8,6,7,9,9,8,9,9,6,4,3,2,9,8,7,6,7,8,9,9,9,5,4,9,8,9,9,2,4,5,6,9,8,6,8,9,8,7,9,8,9,2,9,9,6,7,4,3,4,9,8,6,4,3,5,6,7,9,9,9,9,8,9,8,7,6,5,3,0,9,9,8,5,4,6,4,5,6,7,9,6,5,4,7],[9,8,9,9,8,9,9,1,3,9,7,6,9,6,4,3,4,5,6,9,9,9,8,7,6,5,4,5,6,9,8,7,8,9,5,9,8,9,9,8,7,7,8,9,6,7,7,8,9,5,4,2,9,8,9,9,5,4,9,8,5,4,3,2,1,2,9,7,5,4,9,7,9,7,8,9,9,9,7,9,8,7,4,2,1,9,8,7,6,9,8,5,7,8,8,9,9,6,5,6],[3,9,5,6,9,2,1,0,1,9,8,9,8,7,5,2,1,4,7,8,9,8,9,8,7,6,7,6,7,8,9,9,9,3,4,9,7,9,8,7,6,6,8,9,8,9,8,9,9,9,0,1,2,9,9,8,7,5,9,7,6,5,9,3,9,3,9,8,7,8,9,9,8,6,6,8,9,4,6,7,9,9,5,3,2,3,9,8,7,9,9,9,8,9,9,9,8,9,9,7],[2,3,4,5,8,9,2,3,4,5,9,9,9,8,6,4,2,3,4,9,8,7,6,9,8,7,8,7,8,9,6,4,3,2,9,8,6,5,4,3,4,4,5,8,9,9,9,9,9,8,9,2,9,8,9,9,8,9,9,8,7,9,8,9,8,9,9,9,9,9,9,7,6,5,5,7,8,9,9,9,8,7,6,4,3,4,6,9,8,9,6,7,9,9,9,9,7,9,8,9],[1,0,1,6,7,9,5,4,6,9,8,9,9,9,7,5,6,9,5,9,9,5,4,2,9,8,9,8,9,9,9,9,9,9,9,9,5,4,3,2,0,3,6,7,8,9,9,8,7,7,9,9,7,6,7,8,9,8,9,9,9,8,7,9,7,8,9,9,9,8,7,6,4,3,4,5,9,9,8,9,8,7,6,5,4,5,6,7,9,6,5,6,7,8,9,8,6,8,7,9],[2,5,2,5,6,7,9,5,7,9,7,8,9,9,8,6,9,8,9,8,7,6,2,1,0,9,8,9,8,7,8,8,7,8,9,8,6,5,4,4,1,2,5,6,7,9,9,8,6,6,7,8,9,5,6,9,6,7,9,9,9,7,6,5,6,7,8,9,9,9,8,5,5,2,3,5,6,8,7,8,9,9,7,6,8,7,8,9,6,5,4,3,7,9,8,7,5,6,6,8],[3,4,3,4,5,6,7,9,8,9,6,7,8,9,9,9,8,7,9,9,9,8,3,2,9,8,7,6,7,5,6,5,6,9,8,7,6,5,4,3,2,3,4,5,8,9,7,6,5,4,5,9,5,4,8,4,5,6,8,9,8,7,6,4,5,6,7,8,9,9,9,3,2,1,2,3,4,5,6,9,6,9,8,9,9,9,9,7,5,4,3,2,6,8,9,9,4,3,4,5],[7,5,6,6,7,9,8,9,9,8,9,8,9,2,3,9,8,6,8,9,6,5,4,9,8,9,9,5,4,3,5,4,5,7,9,8,7,7,6,4,5,4,7,6,7,8,9,5,5,3,4,7,9,3,2,3,4,5,6,7,9,8,7,6,6,8,9,9,9,9,8,9,9,0,1,5,8,9,7,9,5,4,9,6,6,9,8,9,8,7,5,3,4,6,7,8,9,6,5,6],[9,7,8,9,8,9,9,8,6,7,9,9,2,1,9,8,9,5,7,8,9,6,5,9,7,9,5,4,3,2,1,3,4,9,9,9,9,9,7,5,6,5,6,8,8,9,5,4,3,1,3,6,7,9,3,6,5,6,7,8,9,9,9,7,7,9,9,9,7,5,7,6,8,9,7,6,7,8,8,9,6,3,2,4,5,9,7,6,9,9,6,6,5,7,8,9,8,7,6,9],[9,8,9,9,9,9,7,6,5,6,8,9,3,9,8,7,6,4,5,7,8,9,9,8,6,8,9,6,4,4,0,4,9,7,8,9,8,8,9,6,7,8,7,8,9,7,6,6,5,9,4,5,6,7,9,7,7,8,9,9,7,5,9,8,8,9,9,8,5,4,3,5,6,8,9,8,8,9,9,9,9,5,1,3,9,8,9,5,9,8,7,7,6,7,8,9,9,8,7,8],[8,9,8,9,8,7,6,5,4,5,6,7,9,9,9,8,4,3,5,6,9,9,8,7,5,7,8,9,5,5,9,9,7,6,7,8,6,7,9,7,8,9,8,9,9,8,7,7,9,8,9,7,8,9,9,8,9,9,9,9,5,4,3,9,9,9,8,7,6,5,4,5,6,9,9,9,9,9,9,9,8,9,2,9,8,7,8,4,5,9,8,9,9,8,9,9,6,9,9,9],[7,8,7,9,9,8,7,9,3,4,5,6,7,9,5,4,3,2,4,9,8,7,6,5,4,6,6,8,9,9,8,7,6,5,3,4,5,6,8,9,9,5,9,6,8,9,8,9,8,7,9,9,9,1,0,9,9,9,9,8,9,3,2,3,6,8,9,8,7,8,7,6,7,8,9,2,1,9,8,7,7,8,9,9,7,6,5,3,2,3,9,5,6,9,9,8,5,3,2,1],[6,5,6,7,8,9,9,8,9,5,6,7,8,9,6,5,4,3,9,8,7,6,5,2,3,4,5,6,9,8,7,6,4,3,2,8,6,7,8,9,2,4,4,5,6,7,9,9,9,6,6,7,8,9,9,9,8,9,8,7,8,9,3,4,5,6,9,9,8,9,9,7,8,9,9,9,9,8,7,6,6,7,9,9,8,7,3,2,1,2,3,4,8,9,8,7,6,4,3,2],[5,4,3,4,9,9,8,7,9,9,7,8,9,8,7,7,5,9,8,9,6,5,4,1,2,3,5,5,6,9,8,3,2,1,0,9,8,9,9,2,1,5,3,4,5,6,7,9,6,5,4,3,4,9,8,8,6,8,7,6,7,9,5,7,8,7,8,9,9,9,9,9,9,3,9,8,7,9,9,4,5,6,7,8,9,5,4,1,0,2,5,6,7,8,9,9,7,6,4,3],[3,1,2,9,8,9,9,6,7,8,9,9,5,9,9,8,9,9,7,8,9,4,3,0,1,2,3,4,8,9,9,5,3,2,1,3,6,7,8,9,0,1,2,5,6,8,9,8,6,4,3,2,9,8,7,6,5,9,7,5,6,8,9,9,9,8,9,9,9,9,8,9,3,2,9,8,6,5,4,3,4,5,8,9,7,6,3,2,1,2,5,9,8,9,8,9,8,8,5,6],[4,5,9,8,7,9,7,5,8,7,8,9,4,3,5,9,8,7,6,8,9,4,2,1,3,5,6,8,9,9,8,7,5,4,2,4,5,9,8,9,1,9,3,4,6,7,9,6,5,4,2,1,0,9,8,6,4,3,2,3,7,9,2,3,4,9,9,9,8,9,7,8,9,1,3,9,8,8,5,4,5,7,9,9,8,7,8,4,5,3,4,8,9,8,7,5,9,8,7,8],[5,9,9,9,6,7,5,4,6,6,7,8,9,2,9,9,8,7,5,6,8,9,4,2,8,6,8,9,5,4,9,8,9,5,3,4,5,6,7,8,9,8,9,9,7,9,8,7,6,5,3,3,4,9,8,7,5,4,3,4,6,9,3,5,9,9,9,7,6,7,6,7,9,0,9,9,9,7,6,8,6,8,9,5,9,9,8,7,6,5,6,7,8,9,9,4,3,9,8,9],[9,8,7,6,5,4,3,2,4,5,6,9,9,9,8,7,6,5,4,5,7,8,9,3,8,7,9,5,4,3,0,9,7,6,4,6,7,9,8,9,6,6,7,8,9,9,9,9,8,7,5,4,5,7,9,8,6,5,4,6,7,8,9,9,8,9,8,6,5,4,5,6,8,9,8,9,8,9,9,9,7,9,2,4,9,8,9,8,7,6,7,8,9,9,8,9,9,9,9,3],[9,9,8,5,4,3,2,1,2,5,7,8,9,9,9,8,7,4,3,5,6,7,8,9,9,8,9,4,3,2,1,9,8,7,5,9,8,9,9,2,5,5,6,9,9,8,8,7,9,8,6,7,6,7,8,9,8,7,5,6,8,9,9,8,7,8,9,8,4,3,4,9,9,8,7,6,7,8,8,9,8,9,1,9,8,7,7,9,8,9,8,9,8,9,7,9,8,9,2,1],[7,8,9,8,7,4,3,0,1,4,5,6,9,8,7,9,8,5,2,4,5,6,7,8,9,9,6,5,9,3,9,7,9,8,6,7,9,3,2,1,3,4,9,8,7,6,7,6,5,9,7,8,7,8,9,9,9,8,9,7,9,9,8,7,6,7,9,9,2,1,9,8,9,7,9,5,6,6,7,8,9,1,0,9,8,6,5,5,9,9,9,8,7,6,5,6,7,8,9,3],[6,9,8,7,6,5,3,1,2,3,6,9,8,7,6,5,9,2,0,3,7,8,8,9,8,9,7,9,8,9,6,5,4,9,7,8,9,2,1,0,1,9,9,7,6,5,6,5,4,9,8,9,8,9,9,9,7,9,9,8,9,8,7,5,5,6,8,9,9,9,8,7,6,6,5,4,4,5,6,7,8,9,2,9,7,6,4,3,4,9,8,7,6,5,4,5,6,9,8,9],[9,9,9,8,7,5,4,5,3,4,8,9,9,9,9,4,3,2,1,2,3,4,5,6,7,8,9,8,7,8,9,3,2,9,8,9,5,3,2,1,9,8,7,6,5,4,3,2,3,4,9,9,9,9,8,7,6,5,6,9,8,7,8,4,4,5,9,6,7,9,7,6,5,4,4,3,2,3,4,6,7,8,9,8,9,9,3,1,0,1,9,8,7,4,3,5,9,8,7,8],[8,9,7,9,9,8,6,7,8,5,6,7,8,9,8,7,4,3,5,3,4,5,6,7,9,9,8,7,6,7,9,4,3,4,9,7,6,4,3,3,4,9,9,8,7,6,2,1,2,5,7,8,9,8,7,6,4,4,5,9,7,6,4,3,2,7,4,5,9,8,7,5,4,3,2,0,1,2,3,7,9,9,7,6,7,8,9,2,3,9,8,7,6,5,6,9,9,9,6,7],[7,7,6,8,9,9,7,8,9,6,7,8,9,8,7,6,5,8,7,4,8,6,8,8,9,8,9,8,5,6,9,5,5,5,9,8,6,5,4,5,6,8,9,9,7,4,3,2,3,4,5,9,9,5,4,3,2,3,9,9,8,9,3,2,1,2,3,4,5,9,8,6,6,5,4,1,2,3,4,5,9,8,7,5,6,7,8,9,4,6,9,8,8,6,9,8,7,8,5,6],[6,4,5,6,7,9,8,9,8,7,8,9,3,9,8,7,6,9,7,5,7,8,9,9,8,7,6,6,4,7,8,9,7,6,7,9,7,9,6,9,7,8,9,8,6,5,4,6,4,5,6,7,8,9,3,2,1,9,8,7,6,5,4,3,2,3,5,5,7,9,8,7,7,5,3,2,3,4,6,6,8,9,3,4,7,8,9,8,5,7,9,9,9,8,9,8,6,5,4,3],[4,3,4,7,8,9,9,9,9,9,9,3,2,3,9,8,7,9,8,6,7,8,9,9,8,7,5,4,3,6,7,8,9,8,8,9,9,8,9,8,9,9,3,9,7,6,7,8,9,6,7,9,9,8,7,6,2,3,9,9,8,9,9,5,4,6,8,6,7,8,9,9,8,5,4,6,4,5,6,7,8,9,4,5,6,9,8,7,6,7,8,9,7,9,8,9,7,4,2,1],[5,4,5,6,9,8,7,8,9,1,2,9,1,9,9,9,8,9,9,9,8,9,6,5,9,7,6,5,4,5,6,7,8,9,9,8,9,6,9,7,8,9,4,9,8,7,8,9,8,7,9,9,8,7,6,5,3,4,5,6,9,9,8,9,5,7,9,7,8,9,9,7,9,7,9,8,7,6,7,8,9,7,5,6,7,8,9,8,9,8,9,4,6,9,6,5,3,2,1,0],[7,5,6,7,8,9,6,7,9,0,9,8,9,8,9,9,9,5,6,7,9,6,5,3,9,8,7,8,6,7,7,8,9,9,9,7,8,5,7,6,9,8,9,9,9,8,9,9,9,8,9,3,9,8,9,5,4,5,6,9,8,8,7,8,9,9,8,9,9,6,8,6,9,8,9,9,8,7,8,9,9,8,6,7,8,9,9,9,9,9,3,2,9,8,9,6,4,5,2,3],[8,8,7,9,9,3,5,7,8,9,8,7,6,7,8,8,9,3,2,9,8,6,4,2,4,9,8,9,7,8,9,9,9,9,8,6,7,4,8,5,6,7,8,8,9,9,9,8,7,9,3,2,3,9,5,9,8,7,9,8,7,6,5,9,4,9,7,8,9,5,4,5,6,9,8,9,9,8,9,6,5,9,7,9,9,8,9,9,9,8,9,9,8,7,8,9,5,6,3,4],[9,9,8,9,3,2,3,4,9,8,7,6,5,5,6,7,8,9,1,0,9,3,2,1,2,3,9,9,8,9,9,9,9,8,7,5,4,3,3,4,7,9,9,7,8,9,8,7,6,4,2,1,0,3,4,5,9,9,9,9,7,5,4,5,3,7,6,9,9,4,3,4,5,6,7,9,8,9,6,5,4,3,9,8,6,7,8,9,8,7,6,7,6,6,9,8,9,7,4,5],[6,6,9,5,2,1,4,9,9,8,8,5,4,4,5,8,9,8,9,9,8,9,9,2,3,5,6,9,9,7,9,8,7,6,5,4,3,2,1,2,3,4,5,6,9,8,7,6,5,4,3,2,1,2,3,4,9,8,8,9,8,4,3,4,2,6,5,6,7,9,2,6,6,9,8,9,6,8,9,9,5,9,8,7,5,6,9,8,7,6,5,4,5,4,8,7,8,9,5,7],[4,5,9,4,3,4,9,8,7,6,5,4,3,2,3,4,5,6,8,9,7,9,8,9,4,6,9,8,6,6,7,9,8,7,6,5,6,1,0,1,3,5,6,7,8,9,8,7,6,7,4,3,6,4,9,9,8,7,6,5,4,3,2,0,1,2,4,5,8,9,3,5,6,7,8,9,5,6,7,8,9,8,7,6,4,3,2,9,9,5,4,3,4,3,4,5,9,9,9,8],[2,9,8,7,6,5,6,9,8,7,4,3,5,1,5,6,6,7,9,9,6,5,7,8,9,9,8,7,5,4,6,4,9,9,7,9,7,2,1,9,4,5,9,8,9,9,9,9,8,6,5,4,9,9,7,6,9,8,9,6,5,4,3,4,2,4,5,6,7,8,9,6,7,8,9,2,4,5,9,9,2,9,8,7,3,2,1,9,5,4,3,2,1,2,3,6,8,9,9,9],[1,0,9,9,8,7,8,9,9,4,3,2,1,0,6,7,7,8,9,8,7,4,6,8,9,8,7,6,4,3,2,3,4,5,9,8,9,4,9,8,9,6,8,9,7,8,9,9,8,7,8,9,8,7,6,5,6,9,8,7,6,5,5,4,3,5,6,7,8,9,9,8,8,9,2,1,3,7,8,9,1,0,9,8,9,3,9,8,6,5,4,5,6,3,5,6,7,8,9,7],[2,1,7,8,9,8,9,9,6,5,4,4,2,1,2,8,9,9,9,9,4,3,5,9,9,9,8,4,3,2,1,2,5,9,8,7,8,9,8,7,8,9,9,5,6,7,8,8,9,8,9,8,7,6,5,4,5,9,9,8,9,8,6,6,4,5,7,8,9,1,2,9,9,5,4,3,4,6,7,8,9,9,8,9,8,9,8,9,7,6,5,6,8,7,6,7,9,9,5,6],[4,5,6,9,3,9,9,8,7,9,6,7,4,2,3,6,7,8,9,7,3,2,3,9,8,7,4,3,2,1,0,1,9,8,7,6,7,4,6,6,7,8,9,3,3,5,6,7,8,9,9,9,6,5,4,3,7,8,9,9,9,8,7,8,9,6,8,9,5,4,3,9,7,6,6,4,5,7,8,9,9,8,7,8,7,7,7,9,8,7,6,9,9,8,7,8,9,6,3,2],[6,8,9,1,2,3,9,9,9,8,7,8,7,6,4,5,8,9,7,6,5,3,9,8,7,6,5,4,3,2,3,9,8,7,6,5,4,3,4,5,6,7,9,1,2,6,7,9,9,1,9,8,7,9,6,4,5,9,9,9,8,9,8,9,8,7,9,7,6,5,9,8,9,8,7,6,7,8,9,9,9,9,6,7,6,5,6,7,9,8,7,8,9,9,9,9,6,5,4,3],[7,9,2,0,1,9,8,7,6,9,8,9,8,6,5,7,9,9,8,7,6,4,5,9,8,7,6,5,4,5,9,8,7,6,5,4,3,2,3,4,6,7,8,9,3,8,8,9,4,3,4,9,9,8,6,5,6,7,8,9,7,6,9,2,9,9,9,9,7,9,9,7,5,9,8,7,9,9,8,9,8,7,5,4,3,4,5,6,7,9,8,9,9,8,9,8,7,6,5,5],[8,9,4,1,9,8,7,6,5,4,9,9,8,7,8,9,7,2,9,9,7,8,6,8,9,8,7,6,5,9,9,9,8,7,5,3,2,1,4,5,6,7,8,9,5,6,9,7,6,4,9,9,9,8,7,7,7,8,9,7,6,5,4,3,9,7,8,8,9,8,9,4,3,2,9,9,9,8,7,9,7,6,5,3,2,1,0,2,6,7,9,9,8,7,6,9,8,7,8,6],[9,6,5,9,9,9,8,5,4,3,2,3,9,8,9,7,5,3,9,9,8,9,7,9,8,9,8,7,9,8,9,9,7,6,5,4,1,0,1,3,7,9,9,7,6,7,8,9,8,9,8,9,8,9,8,9,8,9,9,8,7,9,9,9,8,6,5,7,6,7,8,9,2,1,0,9,8,7,6,5,9,8,7,9,9,9,9,3,5,6,8,9,7,6,5,3,9,9,8,7],[9,9,9,8,9,8,7,6,5,4,3,5,7,9,9,9,9,9,8,5,9,9,8,9,7,8,9,9,8,7,9,9,9,7,6,6,3,1,2,3,4,7,8,9,8,8,9,9,9,8,7,5,6,7,9,2,9,7,5,9,9,7,8,9,7,5,4,4,5,6,7,9,3,2,9,8,9,9,7,3,2,9,9,8,7,8,8,9,9,7,9,7,6,5,4,2,3,4,9,8],[7,7,6,7,8,9,9,7,6,5,4,6,8,9,9,8,8,9,7,4,3,2,9,8,6,7,9,8,7,6,8,9,8,9,8,9,3,2,3,4,5,6,7,8,9,9,6,9,8,7,6,4,7,9,9,1,9,8,9,8,8,6,7,8,9,6,3,3,4,5,9,8,9,9,8,7,7,8,9,4,1,9,8,7,6,7,7,6,8,9,8,9,7,6,4,3,4,5,7,9],[6,7,5,6,7,8,9,9,7,6,9,7,9,7,6,7,7,9,6,5,2,1,0,9,5,6,7,9,9,4,6,6,7,9,9,6,4,5,4,5,6,7,8,9,2,4,5,6,9,9,5,3,8,9,8,9,9,9,8,7,6,5,6,7,8,9,2,2,5,9,8,6,9,8,7,6,6,7,8,9,2,3,9,9,5,6,6,5,9,8,7,8,9,7,5,4,5,6,7,8],[5,3,4,5,9,9,9,8,9,8,9,8,9,7,4,5,6,8,9,4,3,2,1,9,4,5,9,9,8,3,4,5,6,9,8,7,5,6,5,7,8,9,9,5,3,5,6,7,9,8,3,1,9,8,7,8,9,4,3,2,4,4,5,6,8,9,1,0,9,8,7,5,4,3,2,4,5,6,7,8,9,9,8,7,4,3,5,4,5,5,6,7,8,9,6,9,6,9,8,9],[3,2,3,4,9,9,8,7,8,9,4,9,7,6,5,9,9,9,6,5,4,3,9,8,6,9,8,7,6,1,2,3,4,6,9,8,9,7,9,8,9,9,9,9,4,7,7,9,9,7,6,2,9,8,6,5,4,3,2,1,2,3,6,7,9,5,2,1,6,9,7,6,5,9,1,3,4,7,8,9,9,8,7,6,5,2,1,2,3,4,5,6,7,8,9,8,7,8,9,1],[0,1,9,9,8,7,9,6,9,5,3,9,8,7,9,8,7,8,9,6,7,4,9,8,7,9,6,5,4,0,1,6,5,6,9,9,9,8,9,9,5,8,7,8,9,9,9,8,7,6,5,3,9,8,7,6,5,5,4,2,3,4,7,8,9,4,3,4,5,9,8,9,9,8,9,4,5,8,9,9,9,8,7,5,4,3,0,3,4,5,6,7,8,9,6,9,8,9,1,0],[1,9,8,7,6,5,4,5,6,9,2,1,9,8,9,9,6,7,8,9,9,5,6,9,9,8,7,6,2,1,3,4,5,7,8,9,9,9,4,5,4,5,6,9,8,9,9,9,9,7,6,8,9,9,8,7,6,6,5,4,5,6,7,8,9,8,6,5,6,7,9,9,8,7,8,9,6,7,8,9,9,9,8,6,5,4,1,2,5,6,7,8,9,6,5,9,9,9,3,2],[2,3,9,8,5,4,3,4,5,8,9,0,1,9,8,7,5,6,7,9,8,9,7,8,9,9,8,4,3,4,5,5,6,9,9,8,9,6,3,1,3,4,5,6,7,8,9,9,9,8,9,9,4,6,9,8,9,8,7,5,8,7,8,9,9,9,7,6,7,9,9,8,9,6,9,8,9,9,9,9,8,9,9,8,6,5,4,3,4,5,6,7,8,9,3,8,7,8,9,3],[3,9,8,7,6,5,4,5,6,7,8,9,9,8,9,5,4,5,6,7,7,8,9,9,9,9,9,5,6,6,8,9,7,9,8,7,6,5,2,0,2,3,4,5,6,7,8,9,6,9,5,2,3,9,9,9,6,9,9,6,9,8,9,8,8,9,8,7,9,8,7,7,8,4,5,6,7,8,9,9,7,6,5,9,7,6,5,4,5,6,8,8,9,5,4,5,6,7,9,4],[4,5,9,8,8,9,7,6,7,8,9,9,8,7,5,4,2,3,4,5,6,7,8,9,9,8,7,6,7,7,9,9,8,9,9,8,7,4,3,1,3,4,5,7,7,8,9,9,5,4,3,1,9,8,9,4,5,9,8,7,8,9,7,6,7,8,9,9,8,9,6,5,6,3,4,5,4,6,7,8,9,4,3,9,8,7,6,7,6,7,8,9,9,8,7,6,8,9,8,9],[7,6,7,9,9,9,8,7,8,9,3,2,9,8,9,5,3,4,5,9,7,8,9,5,3,9,8,7,8,8,9,2,9,6,7,9,6,5,3,2,4,5,9,8,8,9,8,8,9,5,4,9,8,7,9,3,2,1,9,8,9,3,4,5,8,9,9,8,7,6,5,4,3,2,3,4,3,4,9,9,4,3,2,0,9,8,7,9,7,8,9,8,9,9,9,7,9,9,7,8],[9,7,9,6,9,9,9,8,9,9,9,0,2,9,7,6,5,6,7,8,8,9,9,9,2,1,9,8,9,9,9,3,4,5,9,8,6,5,4,5,6,6,9,9,9,4,6,7,9,6,9,8,7,6,7,9,9,0,9,9,3,2,7,6,7,9,9,9,8,9,8,5,4,1,0,1,2,4,7,8,9,5,4,9,9,9,8,9,8,9,6,7,8,9,8,9,8,7,6,5],[9,8,9,5,7,8,9,9,9,9,8,9,3,5,9,7,6,7,8,9,9,9,9,8,9,2,3,9,9,9,8,4,5,6,7,9,7,6,7,6,7,7,8,9,3,3,4,6,8,9,8,7,6,5,7,7,8,9,8,9,4,3,8,7,9,8,9,9,9,8,7,6,8,2,1,2,3,5,6,9,7,6,9,8,9,9,9,8,9,6,5,8,9,9,7,6,5,7,5,4],[3,9,3,4,5,9,8,9,9,8,7,8,9,6,9,8,7,8,9,9,9,9,8,7,8,9,5,6,9,8,7,5,6,8,9,9,8,9,8,7,8,8,9,0,1,2,4,5,7,8,9,6,5,4,5,6,9,8,7,6,5,4,9,8,9,7,8,9,9,9,9,7,8,3,2,6,9,7,9,8,9,9,6,7,8,9,8,7,6,5,4,9,8,7,6,5,4,3,2,3],[2,1,2,3,5,6,7,9,9,7,6,8,8,9,9,9,8,9,9,8,9,8,7,6,7,9,9,9,9,9,8,6,7,8,9,7,9,9,9,8,9,9,3,2,3,3,4,6,8,9,9,3,2,3,4,5,6,9,9,7,6,9,9,9,8,6,9,8,9,9,9,8,9,5,4,5,9,9,8,7,8,7,5,6,7,8,9,9,5,4,3,4,9,9,8,4,3,2,1,2],[3,2,3,4,9,7,9,7,8,3,4,6,7,8,9,9,9,9,8,7,8,9,9,5,6,6,7,8,9,9,9,9,8,9,5,6,8,9,8,9,6,5,4,3,5,4,5,6,9,6,8,9,1,5,6,7,8,9,9,8,9,8,9,7,4,5,6,7,8,9,9,8,7,6,5,9,8,9,8,6,4,5,4,5,6,7,8,9,5,3,2,3,9,8,7,6,5,3,0,1],[4,5,6,9,8,9,8,6,7,2,4,5,6,7,8,9,9,8,7,6,9,5,4,3,4,5,9,9,7,8,9,9,9,5,4,7,9,6,7,9,7,6,5,4,5,7,6,8,9,5,8,9,2,3,5,6,7,9,9,9,8,7,5,6,3,4,5,6,7,9,9,9,8,7,9,8,7,8,6,5,3,4,3,4,5,7,8,9,6,4,3,9,9,9,8,7,6,4,1,2],[5,6,9,8,7,6,5,4,2,1,3,4,5,6,9,8,7,6,8,4,5,9,9,9,6,7,8,9,5,8,9,9,5,4,2,3,4,5,6,9,8,7,9,8,7,8,7,9,7,6,7,8,9,4,6,8,9,9,9,8,7,6,4,3,2,3,4,5,6,8,9,7,9,8,9,7,6,5,4,3,2,3,2,4,5,6,8,9,6,5,9,8,7,6,9,8,6,4,3,3],[8,7,8,9,8,7,6,5,1,0,1,2,4,5,6,9,6,5,4,3,4,7,7,8,9,9,9,3,4,6,7,8,9,3,1,9,9,9,7,9,9,8,9,9,8,9,9,9,8,7,8,9,7,5,7,9,9,8,9,9,8,7,4,3,1,2,3,4,5,8,9,5,4,9,9,9,8,4,3,2,1,0,1,5,6,7,9,9,9,9,8,7,6,5,4,9,8,7,6,4],[9,9,9,8,7,6,5,4,2,1,2,6,7,8,9,8,5,4,2,1,2,5,6,7,8,9,7,5,6,7,8,9,9,9,9,8,7,8,9,8,7,9,3,2,9,6,7,8,9,8,9,9,8,6,7,9,8,7,6,8,9,8,5,4,2,3,4,5,6,7,8,9,3,5,9,8,7,6,4,3,2,1,2,7,7,8,9,9,8,9,9,6,5,4,3,5,9,8,7,5],[9,3,2,9,8,7,8,6,3,2,3,5,8,9,8,7,6,5,1,0,1,4,5,6,7,8,9,6,9,8,9,9,9,8,9,7,6,7,9,9,5,3,2,1,3,5,6,7,8,9,9,9,9,7,8,9,7,6,5,3,4,9,6,5,4,4,6,7,8,9,9,3,2,3,4,9,8,7,5,4,3,2,3,4,8,9,9,8,7,9,8,7,6,5,6,6,7,9,8,6],[8,9,1,2,9,9,8,7,9,3,4,7,9,1,9,9,4,3,2,1,2,3,6,7,8,9,9,8,9,9,9,8,7,6,5,6,5,9,8,7,6,7,3,2,3,4,5,6,7,9,9,8,9,8,9,9,8,6,5,4,9,8,7,6,7,6,7,8,9,9,9,5,3,4,5,6,9,8,6,5,4,6,5,5,6,7,9,5,6,5,9,9,7,6,8,8,9,9,9,7],[7,8,9,4,9,8,9,8,9,5,5,8,9,2,9,8,7,5,3,2,3,4,8,9,9,7,6,9,2,3,5,9,6,5,4,3,4,5,9,8,7,6,5,3,4,5,6,8,8,9,9,7,8,9,9,9,8,7,6,5,6,9,9,8,8,9,8,9,9,8,7,6,4,5,6,7,8,9,8,6,5,7,8,6,7,9,5,4,3,4,5,9,8,9,9,9,8,9,9,8],[6,7,8,9,9,7,6,9,8,6,6,7,8,9,9,9,9,5,4,7,4,5,6,7,8,9,5,2,1,3,9,8,7,6,5,1,2,3,4,9,9,9,8,6,5,6,8,9,9,8,7,6,5,6,9,8,9,8,9,6,7,9,8,9,9,8,9,6,8,9,8,7,7,6,9,8,9,4,9,7,6,8,9,7,8,9,8,5,4,6,6,9,9,6,5,6,7,8,9,9],[5,6,8,9,8,9,5,4,9,9,8,9,9,8,8,8,8,9,5,6,7,7,7,8,9,5,4,3,0,2,5,9,8,7,8,2,9,4,6,7,8,9,9,7,6,7,9,9,8,7,7,5,4,6,6,7,8,9,9,8,9,8,7,9,9,7,6,5,7,8,9,9,9,7,8,9,4,2,9,8,9,9,9,8,9,8,7,6,8,7,7,8,9,5,4,5,6,7,8,9],[4,5,7,8,7,8,9,3,5,7,9,9,8,7,6,7,7,9,7,7,9,9,9,9,8,7,6,5,4,3,4,5,9,8,9,9,8,9,9,8,9,9,9,8,7,8,9,8,9,6,5,4,3,4,5,6,7,8,9,9,9,9,6,7,8,9,6,4,6,9,9,8,9,9,9,3,2,1,0,9,6,7,9,9,9,9,8,7,9,9,8,9,5,4,3,4,5,6,7,8],[3,4,3,5,6,8,9,4,6,7,9,9,7,6,5,5,6,8,9,8,9,9,8,9,9,8,7,9,5,4,5,6,7,9,9,9,7,8,8,9,9,9,7,9,9,9,6,7,8,9,2,1,2,3,4,5,8,9,9,9,7,6,5,6,7,8,9,3,5,9,8,7,9,8,7,9,9,2,1,2,5,6,7,8,9,7,9,8,9,9,9,5,4,3,2,5,7,7,8,9],[2,3,2,4,5,7,9,5,6,9,8,8,9,8,3,4,5,7,8,9,6,5,7,8,9,9,9,8,9,7,6,8,9,9,8,7,6,6,7,9,8,7,6,5,4,4,5,6,7,8,9,2,3,4,6,8,9,9,9,8,6,5,4,5,9,9,2,1,9,8,7,6,5,7,6,7,8,9,3,3,4,5,9,9,7,5,6,9,9,8,7,6,5,4,5,6,8,9,9,9],[1,0,1,2,4,5,9,6,9,8,7,7,9,3,2,3,5,6,9,9,5,4,6,7,8,9,6,7,9,9,7,9,8,7,7,6,4,5,8,9,9,9,7,4,3,2,3,7,8,9,6,5,4,6,7,9,9,9,9,8,7,9,5,6,8,9,3,9,8,7,6,5,4,5,5,6,7,8,9,4,5,7,8,9,6,4,5,6,9,9,9,7,6,5,6,7,8,9,6,7],[2,1,2,3,4,6,8,9,8,7,6,5,6,3,1,3,4,6,7,8,9,3,2,1,2,3,5,6,7,8,9,9,9,6,5,4,3,6,7,8,9,8,6,5,4,3,5,6,7,8,9,9,5,7,8,9,9,8,8,9,8,7,6,8,9,5,4,9,9,8,6,4,3,5,4,4,5,7,9,9,6,8,9,9,8,3,4,5,8,9,9,9,7,6,8,9,9,4,5,6],[3,6,5,4,5,6,7,8,9,6,5,4,3,2,0,2,3,4,8,9,3,2,1,0,4,5,6,7,8,9,9,9,8,7,6,5,4,5,6,9,9,9,9,8,6,4,5,6,8,9,9,8,6,7,9,9,9,7,6,4,9,8,7,9,8,6,9,8,7,6,5,4,2,1,2,3,5,6,7,8,9,9,7,6,5,4,5,6,7,8,9,9,8,7,8,9,3,2,3,7]]

fun getValue f (i,j)   = SOME (Array2.sub (f,i,j)) handle Subscript => NONE
fun setValue f (i,j) x = Array2.update (f,i,j,x)

fun neighbors (i,j) = [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]

fun localMinimum f p x =
    let fun check  NONE    = true
          | check (SOME y) = x < y
    in List.all check (map (getValue f) (neighbors p)) end

fun localMinima f =
    let val region = { base = f, row = 0, col = 0, nrows = NONE, ncols = NONE }
        fun add (i,j,x,xs) = if localMinimum f (i,j) x then x::xs else xs
    in Array2.foldi Array2.RowMajor add [] region end

val adv09 =
    let val minima = localMinima (Array2.fromList height)
    in length minima + foldl op + 0 minima end

(* Destructively changes each already counted cell to 9 *)
fun basinSize f p =
    case getValue f p of
        NONE   => 0
      | SOME 9 => 0
      | SOME x => ( setValue f p 9
                  ; foldl op + 1 (map (basinSize f) (neighbors p))
                  )

fun findBasins f =
    let val region = { base = f, row = 0, col = 0, nrows = NONE, ncols = NONE }
        fun add (i,j,_,xs) = let val basin = basinSize f (i,j)
                             in if basin > 0 then basin :: xs else xs end
    in Array2.foldi Array2.RowMajor add [] region end

(* ListMergeSort is in the SML/NJ Utils library *)
val adv09b =
    let val basins = findBasins (Array2.fromList height)
        val top3 = List.take (ListMergeSort.sort op < basins, 3)
    in foldl op * 1 top3 end
