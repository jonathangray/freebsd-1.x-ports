(*$MEMO: GeneralTypes *)

signature MEMO =
sig

(* MEMO FUNCTION PACKAGE

Created by:	K. Mitchell, LFCS, University of Edinburgh
Date:		April 1989.

Maintenance:	Author


DESCRIPTION

   If inj[xy] are injective functions, f contains no side-effects, and
   expfn is an increasing function, then

   fun memo (expfn: nat -> nat)  <-- determines how the memo table expands
            (inj: 'a -> nat)     <-- converts argument into an array index
            (f: ('a -> '_b) -> 'a -> '_b)      <-- function to be memoised
            : ('a -> '_b) * ('a -> '_b)
            = let fun f' n = f f' n in (f', f') end;

   memo2 is defined using memo, and memo functions of higher arity may be
   constructed in a similar way.
 
   The first component of the result is a memoised version of the supplied
   function.  The second component is a memoised version that retains the
   table between calls.  See the examples for further details of its use.


EXAMPLES

   fun I x = x;
   fun expfn n = n;

   local 
      fun mf fib 0 = 1
        | mf fib 1 = 1
        | mf fib n = fib(n-1) + fib(n-2);
   in val (_,fib) = memo expfn I mf end;


   local
      fun mf ack 0 y = y + 1
        | mf ack x 0 = ack (x-1) 1
        | mf ack x y = ack (x-1) (ack x (y-1));
   in val (_,ack) = memo2 expfn I I mf end;


   fun length [] = 0 | length (_::t) = 1 + length t;

   (* length is injective for the values encountered in the cc function  *)

   local
     fun mf cc 0 _ = 1
       | mf cc _ [] = 0
       | mf cc amount  (kinds as (h::t)) =
              if amount < 0 then 0
              else if amount - h < 0 then cc amount t
              else cc (amount-h) kinds + cc amount t;
   in val (_,cc) = memo2 expfn I length mf end;
  

RCS LOG

$Log: MEMO.sml,v $
Revision 1.1  1994/02/08 00:23:24  jkh
Initial revision

Revision 1.5  91/04/11  10:18:21  10:18:21  db (Dave Berry)
Fixed minor bug in comment to memo3.

Revision 1.4  91/01/25  19:14:48  19:14:48  db (Dave Berry)
Added dependence on GeneralTypes and/or InStreamType.

Revision 1.3  91/01/25  16:56:53  16:56:53  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:06:31  17:06:31  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:50:18  16:50:18  db (Dave Berry)
Initial revision


*)


(* MEMO FUNCTIONS *)

  val memo: (Nat -> Nat) -> ('a -> Nat) -> (('a -> '_b) -> 'a -> '_b) ->
            ('a -> '_b) * ('a -> '_b)
   (* memo expfn inj f; memoises a unary function.  See DESCRIPTION. *)

  val memo2: (Nat -> Nat) -> ('a -> Nat) -> ('_b -> Nat) ->
             (('a -> '_b -> '_c) -> 'a -> '_b -> '_c) ->
             ('a -> '_b -> '_c) * ('a -> '_b -> '_c)
   (* memo2 expfn inj1 inj2 f; memoises a curried binary function.  Can be
      defined in terms of memo. *)

  val memo3: (Nat -> Nat) -> ('a -> Nat) -> ('_b -> Nat) -> ('_c -> Nat) ->
             (('a -> '_b -> '_c -> '_d) -> 'a -> '_b -> '_c -> '_d) ->
             ('a -> '_b -> '_c -> '_d) * ('a -> '_b -> '_c -> '_d)
   (* memo3 expfn inj1 inj2 inj3 f; memoises a curried trinary function.
      Can be defined in terms of memo. *)

end
