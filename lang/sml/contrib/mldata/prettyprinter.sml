(*
	This functor may be used in the construction of folding
	prettyprinters. It's based on the notion of ``item'' --
	a generalisation of strings which contain folding
	information.

	The structure-traversal component of the prettyprinter should
	yield an ``item'' which is then shown with ``showitem''.

	Showitem is simple: if its argument will fit in the current
	space left on the line, then it prints it ``horizontally''
	otherwise it prints it vertically. The various forms of item
	define various useful horizontal / vertical forms.

	One day someone should rewrite mlkernel.pretty.sml so that
	it uses this kit.

*)

import "lib/list" "lib/streamio";

functor prettyprinter()=
struct
   structure list = list();
   open list;

   structure streamio = streamio();
   open streamio;

   val SUM = (0 /> op +);
   fun MAX ns = op max // ns;
   fun MIN ns = op min // ns;

   fun transpose [] = []
   |   transpose nss =
       (map hd nss :: transpose (map tl nss)) handle Hd => [];

   fun MAXIMA nss = map MAX (transpose nss);

   fun interleave f g []        = ()
   |   interleave f g [i]       = g i
   |   interleave f g (i :: is) = (g i; f (); interleave f g is);

   infix 1 withlast;

   fun [] withlast y        = []
   |   [x] withlast y       = [y]
   |   (x :: xs) withlast y = x :: (xs withlast y);

   (* Pretty-printing infrastructure  *)
   val margins = ref [0];
   val rightmargin = ref 30;
   val here = ref 0;

   fun reset ()     = (margins := [0]; here := 0);
   fun pushmargin n = margins := n :: ! margins;
   fun pushhere ()  = pushmargin (! here);
   fun topmargin () = hd (! margins);
   fun pushindent n = pushmargin (topmargin () + n);
   fun pop () = margins := tl (! margins);

   fun und () =
   case !margins of current :: last :: rest => last | _ => 0;

   fun st s = (writestring s; here := ! here + size s);
   fun sp n = if n > 0 then (st " "; sp (n - 1)) else ();
   fun nl () = (writestring "\n"; here := 0; sp (topmargin ()));

   fun roomforwidth n = n + ! here <= ! rightmargin;

   (* Generic infrastructure for prettyprinting  *)
   type punct = int * string;

   fun punct folding nlbefore (n, string) =
   case (folding, nlbefore) of
      (true, true)   => (nl(); st string; sp n)
   |  (true, false)  => (st string; nl())
   |  (false, _)     => (st string)

   fun punctsize (n, string) = (n + size string);

   datatype item =
       S    of string           (* elementary item  *)
     | SEQ  of item list        (* sequential composition  *)
     | L    of punct * item list(* foldable list  *)
     | TAB  of punct * item list(* aligned table (of SEQuences of the same arity)  *)
     | PRE  of punct * item list(* vertical punctuated list with newlines BEFORE punctuation *)
     | POST of punct * item list(* vertical punctuated list with newlines AFTER punctuation *)
     | I    of int              (* push indented margin  *)
     | H                        (* push here as margin  *)
     | X                        (* pop a margin  *)
     | F                        (* fold  *)
     | FF                       (* non-space fold  *)
     | U                        (* push previous margin  *)
     | D    of string * item    (* debugger  *);

   fun itemsize item =
   case item of
      S s         => size s
   |  L p         => sizeofpunctuated p
   |  SEQ items   => SUM (map itemsize items)
   |  TAB p       => sizeofpunctuated p
   |  PRE p       => sizeofpunctuated p
   |  POST p      => sizeofpunctuated p
   |  I _         => 0
   |  H           => 0
   |  X           => 0
   |  F           => 0
   |  FF          => 0
   |  U           => 0
   |  D (_, item) => itemsize item

   and sizeofpunctuated ((_, string), items) =
   let val l = length items
       and s = size string
   in
       s * (l - 1) + SUM (map itemsize items)
   end;

   fun columnsizes (SEQ items)   = map itemsize items
   |   columnsizes (D (_, item)) = columnsizes item;

   fun roomforitem item = roomforwidth (itemsize item);

   fun roomforitemafter n item = roomforwidth (n + itemsize item);

   exception Arity  ;

   fun arity (SEQ items)   = length items
   |   arity (D (_, item)) = arity item
   |   arity _             = raise Arity;

   fun show folding item =
   let val v = not (roomforitem item)
   in
       case item of
	  S s             => st s
       |  L (p, items)    => showfoldlist v p items
       |  SEQ items       => app (show v) items
       |  TAB (p, items)  => showtable v p items
       |  PRE (p, items)  => showlist v true p items
       |  POST (p, items) => showlist v false p items
       |  I 0             => pushhere ()
       |  H               => pushhere ()
       |  I n             => pushindent n
       |  X               => pop ()
       |  U               => pushmargin (und ())
       |  F               => if folding then nl() else sp 1
       |  FF              => if folding then nl() else ()
       |  D (label, item) => show folding item
   end

   and showfoldlist folding comma items =
   case items of
      [] => ()
   |  item :: items =>
      ( if roomforitem item then () else nl ();
	show false item;
	if null items then ()
	else
	(punct false false comma; showfoldlist folding comma items)
      )

   and showlist folding nlbefore comma items =
   case items of
      []     => ()
   |  [item] => show folding item
   |  item :: items =>
      ( show folding item;
	punct folding nlbefore comma;
	showlist folding nlbefore comma items
      )

   and showtable folding comma items =
   if not folding then showlist folding true comma items
   else
      let val arities = map arity items;
	  val maxarity = MAX arities;
	  val minarity = MIN arities;
	  val _ = if maxarity = minarity then () else raise Arity;
	  (* calculate column sizes from the objects which fit  *)
	  val fits = roomforitemafter (punctsize comma) <| items;
	  val _ = if not (null fits) then () else raise Arity;
	  val sizes = map columnsizes fits;
	  val colwidths = MAXIMA sizes;
	  val tabwidth = SUM colwidths;
	  val _ = if roomforwidth tabwidth then () else raise Arity;
	  fun showtabitem (item as SEQ items) =
	      let val folding = not (roomforitem item)
	      in
		  app (showinwidth folding)
		  (items ||| (colwidths withlast 0))
	      end
	  |   showtabitem (D (_, item)) = showtabitem item
      in
	  interleave (fn () => punct folding true comma) showtabitem
	  items
      end
      handle
	 Arity => showlist folding true comma items

   and showinwidth folding (item : item, width : int) =
   let val h = ! here + width
   in
       show folding item;
       if folding then () else sp (h - ! here)
   end

   fun pretty item =
   (   here := 0;
       show false item
   )
end;

