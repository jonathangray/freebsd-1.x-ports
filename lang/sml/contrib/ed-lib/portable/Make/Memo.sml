loadEntry "Busy";

(* MEMO.SML

Created by:     Nick Rothwell, LFCS, University of Edinburgh
                nick@lfcs.ed.ac.uk
Date:           30 Oct 1990

                Modified to fit the library structure by Dave Berry,
                24 Jan 1991.

Maintenance:    Author


DESCRIPTION

   This is part of the portable make system.


RCS LOG

$Log: Memo.sml,v $
Revision 1.1  1994/02/08 00:23:22  jkh
Initial revision

Revision 1.3  91/01/30  18:59:33  18:59:33  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.2  91/01/25  20:13:30  20:13:30  db (Dave Berry)
Prefixed local signature and functor names with MAKE_ or Make respectively.

Revision 1.1  91/01/25  11:41:49  11:41:49  db (Dave Berry)
Initial revision


*)

signature MAKE_MEMO =
   sig
      type Arg

      val memo_fn: ((Arg * Arg) -> bool) -> ((Arg -> '_a) -> (Arg -> '_a))

      exception Enum_memo_fn

      val enum_memo_fn: ((Arg -> int) * int) -> ((Arg -> '_a) -> (Arg -> '_a))

      exception Catalog

      val catalog: {tag: Arg -> 'tag,
                    ordOfTag: 'tag -> int,
                    items: Arg list
                   } -> ('tag -> Arg list)
   end;

functor MakeMemo(structure Global: MAKE_GLOBAL
	     structure Busy: MAKE_BUSY
	     type Arg
	    ): MAKE_MEMO =
   struct
      open Global Global.CoreUtils
      open CoreArray
      infix before sub

      type Arg = Arg
      type 'a relation = 'a * 'a -> bool

      fun memo_fn (Eq: Arg relation) (F: Arg -> '_a) =
         let val MemoSet = (ref nil): (Arg * '_a) list ref
             fun MemoCall(Arg, (X, Y) :: XYRest, Eq) =
                    if Eq(Arg, X) then Y else MemoCall(Arg, XYRest, Eq)   |
                 MemoCall(Arg, nil, _) =
                    let val Y = F(Arg)
                    in  (MemoSet := (Arg, Y) :: !MemoSet; Y)
                    end

         in  fn X => MemoCall(X, !MemoSet, Eq)
         end

      exception Enum_memo_fn
      fun enum_memo_fn (Enum: Arg -> int, Max: int) (F: Arg -> '_a) =
         let val MemoArray = array(Max, NONE): '_a option array
         in  fn x =>
               let val n = Enum(x)
               in  case MemoArray sub n of
                      NONE => let val y = F(x)
                              in  update(MemoArray, n, SOME(y)); y
                              end   |
                      SOME(y) => y
               end
               handle Subscript => raise Enum_memo_fn
         end

	(* catalog: given a function Tag for getting the selector tag
	   from any object, bundle the objects to give an efficient selector
	   function *)

      exception Catalog
      fun catalog{tag, ordOfTag, items}: ('tag -> Arg list) =
        let val OrdOfThing = ordOfTag o tag

             fun MaxOrdOfTag(Item :: IRest, Result): int =
                    (Busy.dot();
                     let val ThisOrd = OrdOfThing(Item)
                     in  if ThisOrd > Result then MaxOrdOfTag(IRest, ThisOrd)
                                             else MaxOrdOfTag(IRest, Result)
                     end
                    )   |
                 MaxOrdOfTag(nil, Result) = Result

             val MaxOrdOfTags =
                (Busy.print "MaxOrdOfTags"; MaxOrdOfTag(items, 0))

             val TheCatalog =
                array(MaxOrdOfTags + 1, NONE): Arg list option array

             fun InsertItem(Item) =
                let val ord = OrdOfThing(Item)
                in  (case TheCatalog sub ord of
                        SOME(L) =>
                           update(TheCatalog, ord, SOME(Item :: L))   |

                        NONE =>
                           update(TheCatalog, ord, SOME [Item])
                    ) before Busy.dot()
                end

             val _ = (Busy.print "catalog"; map InsertItem items)

         in  fn Tag => (case TheCatalog sub ordOfTag(Tag) of
                           SOME(L) => L   |
                           NONE => raise Catalog
                       ) handle Subscript => raise Catalog
         end
   end;
