
(*************************************************************************************
 * A structure containing definitions for the game of life                           *
 * C.Reade 5/1/87 (Modified Oct 1987)                                                *
 *************************************************************************************
 * type generation
 * val mkgen         : (int * int) list -> generation
 * val alive         : generation -> (int * int) list
 * val mk_nextgen_fn : ((int * int) -> (int * int) list) -> (generation -> generation)
 *------------------------------------------------------------------------------------
 * generation is an abstract type with operations mkgen and alive and mk_nextgen_fn.
 * mkgen produces a generation from an arbitrary list of coordinates (integer pairs)
 *   for live squares.
 * alive produces the list of coordinates of live squares of a generation
 *   in lexical order (with no repetitions).
 * mk_nextgen_fn can be used to produce a nextgeneration function of type
 *   generation -> generation. It should be supplied with an argument function
 * which calculates the neighbours of a coordinate. For example, if you first define
 *                            neighbours:(int*int)->(int*int)list 
 * Then you can define        val nextgen = mk_nextgen_fn (neighbours)
 * This allows you to experiment with different neighbour functions (usually 8 neigh-
 * bours possibly modified with wraparound or cutoff at some upper and lower limits).
 ************************************************************************************)
structure Life =
struct
   local (*********************** AUXILIARY DEFINITIONS *****************************)
    val filter  = Chrisprelude.filter;
    val length = Chrisprelude.length;
    val member = Chrisprelude.member;
    val revonto = Chrisprelude.revonto;
    fun lexordset [] = []
      | lexordset (a::x) = lexordset(filter (lexless a) x) @ [a] @
                           lexordset(filter (lexgreater a) x)
    and lexless (a1:int,b1:int) (a2,b2) = a2<a1 orelse (a2=a1 andalso b2<b1)
    and lexgreater pr1 pr2 = lexless pr2 pr1;
    fun collect f list 
            = let fun accumf sofar [] = sofar
                    | accumf sofar (a::x) = accumf (revonto sofar (f a)) x
              in accumf [] list end;
    fun occurs3 x   (* finds coords which occur exactly 3 times in coordlist x *)
        = let fun f xover x3 x2 x1 [] = diff x3 xover
                | f xover x3 x2 x1 (a::x)
                   = if member xover a then f xover x3 x2 x1 x else
                     if member x3 a then f (a::xover) x3 x2 x1 x else
                     if member x2 a then f xover (a::x3) x2 x1 x else
                     if member x1 a then f xover x3 (a::x2) x1 x else
                                         f xover x3 x2 (a::x1) x
              and diff x y = filter (not o member y) x
          in f [] [] [] [] x end
  in (*********************** MAIN DEFINITIONS FOLLOW *******************************)

abstype generation = GEN of (int*int)list
   with
     fun alive (GEN livecoords) = livecoords
     and mkgen coordlist = GEN(lexordset coordlist)
     and mk_nextgen_fn neighbours gen
          = let val living = alive gen
                val isalive = member living
                val liveneighbours = length o filter isalive o neighbours
                fun twoorthree n = n=2 orelse n=3
                val survivors = filter (twoorthree o liveneighbours) living
                val newnbrlist = collect (filter (not o isalive) o neighbours) living
                val newborn = occurs3 newnbrlist
            in mkgen(survivors @ newborn) end
   end

end (* of local *)
end (* of Life *);

