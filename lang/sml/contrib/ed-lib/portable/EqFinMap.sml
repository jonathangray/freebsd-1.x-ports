(*$EqFinMap: EQ_FIN_MAP List GeneralTypes *)

loadSig "EQ_FIN_MAP";

structure EqFinMap: EQ_FIN_MAP =
struct

(* FINITE MAPS

Created by:     Nick Rothwell, LFCS, University of Edinburgh
		nick@lfcs.ed.ac.uk
		Date:           10 Oct 1990

		Maintenance:    Author

RCS LOG

$Log: EqFinMap.sml,v $
Revision 1.1  1994/02/08 00:23:19  jkh
Initial revision

Revision 1.2  91/09/13  16:47:43  16:47:43  db (Dave Berry)
Fixed definitions of dom and range to work with Poplog ML.
(The problem was different ways of handling overloading.)

Revision 1.1  91/03/08  16:59:21  16:59:21  db (Dave Berry)
Initial revision


*)

(* TYPES *)

  abstype (''a, 'b) Map = Map of (''a *  'b) list
  with

(* CONSTANTS *)

    val empty = Map []


(* CREATORS *)

    fun singleton p = Map [p]


(* OBSERVERS *)

    fun isEmpty (Map nil) = true
    |   isEmpty (Map _) = false

    local
      fun lookup' [] x  = None
        | lookup' ((x,y)::rest) x' =
            if x=x' then Some(y) else lookup' rest x'
    in
      fun lookup (Map l) x = lookup' l x
    end


(* MANIPULATORS *)

    local
      fun add' (x, y, nil) = [(x, y)]
        | add' (x', y', (x, y) :: rest) = 
            if x=x' then (x', y') :: rest
            else (x, y) :: add'(x', y', rest)
    in
      fun add (x, y) (Map l) = Map (add' (x, y, l))
    end

    local
      fun plus' (l, []) = l
        | plus' (l, (x, y) :: tl) = plus'(add(x, y) l, tl)
    in
      fun plus l (Map l') = plus' (l, l')
    end

    fun mergeMap folder (Map l1) (Map l2) =
      let
        fun insert(x', y', nil) = [(x', y')]
	  | insert(x', y', (x, y) :: rest) =
	      if x = x' then (x, folder(y, y')) :: rest
	      else (x, y) :: insert(x', y', rest)
      in
        Map (List.foldL (fn (x, y) => fn m => insert(x, y, m)) l1 l2)
      end

    local
      fun fst (x, y) = x
      fun snd (x, y) = y
    in
      fun dom (Map l) = List.map fst l
      fun range (Map l) = List.map snd l
    end

    fun composeMap (f:'b -> 'c) (Map m: (''a, 'b) Map): (''a, 'c) Map = 
        Map (List.map (fn(x,y)=>(x, f y)) m)
  
    fun fold (f : ('a * 'b) -> 'b) (x : 'b) (Map m : (''d,'a) Map) : 'b = 
        List.foldL (fn (a, b) => fn c => f(b, c)) x m
  
    fun fold' (f : ((''a * 'b) * 'c) -> 'c) (x : 'c)
	      (Map m : (''a,'b) Map) : 'c =
        List.foldL (fn (a, b) => fn c => f((a, b), c)) x m

  end
end;
