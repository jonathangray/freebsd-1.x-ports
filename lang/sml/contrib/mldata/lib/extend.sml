
functor extend() :extend=
struct
   structure integer =
   struct
      fun lfold f r (m, n) = if m > n then r else lfold f (f (r, m)) (m + 1, n);
      fun rfold f r (m, n) = if m > n then r else f (m, rfold f r (m + 1, n));
      fun iter f           = lfold (fn ((), n) => (f n; ())) ()
   end;

   structure array =
   struct
      open Array
      fun lfold f r a =
      integer.lfold (fn (r, i) => f (r, Array.sub (a, i))) r (0, Array.length a - 1);

      fun rfold f r a =
      integer.rfold (fn (i, r) => f (Array.sub (a, i), r)) r (0, Array.length a - 1);

      fun iter f a =
      integer.iter (fn i => f (Array.sub (a, i))) (0, Array.length a - 1);

      fun arrayof (n, f) =
      let val a = array (n, f 0)
      in
	  integer.iter (fn i => update (a, i, f i)) (1, n - 1);
	  a
      end
   end;

   structure ref =
   struct
      infix 0 +:= -:= ::=;

      val op +:= = fn (r as ref n, i : int) => r := n + i;

      val op -:= = fn (r as ref n, i : int) => r := n - i;

      val op ::= = fn (r as ref n, i) => r := i :: n;

      val ++ = fn (r as ref n) => (inc r; n);

      val -- = fn (r as ref n) => (dec r; n)
   end
end;

