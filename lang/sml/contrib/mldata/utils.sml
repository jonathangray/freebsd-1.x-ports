(*
	@(#)utils.sml	2.1 92/09/10 15:39:08
*)


functor utils()=
struct
   open Array;

   infix 9 sub;

   fun rfoldn f n xs =
   let fun F i [] = n | F i (x :: xs) = f ((i, x), F (i + 1) xs) in F 0 xs end;

   fun rfoldn1 f n xs =
   let fun F i [] = n | F i (x :: xs) = f ((i, x), F (i + 1) xs) in F 1 xs end

   and enumerate ts = rfoldn op :: nil ts

   and enumerate1 ts = rfoldn1 op :: nil ts;

   fun rfolds f n string =
   let val s = size string;
       fun F i = if i = s then n else f (substring (string, i, 1), F (i + 1))
   in
       F 0
   end;

   fun lfolds f n string =
   let val s = size string;
       fun F i n =
       if i = s then n else F (i + 1) (f (substring (string, i, 1), n))
   in
       F 0 n
   end;

   fun rfolda f n a =
   let val s = Array.length a;
       fun F i = if i = s then n else f (a sub i, F (i + 1))
   in
       F 0
   end;

   fun lfolda f n a =
   let val s = Array.length a;
       fun F i n = if i = s then n else F (i + 1) (f (a sub i, n))
   in
       F 0 n
   end;

   fun rfoldint f n min max =
   let fun F i = if i = max then n else f (i, F (i + 1)) in F min end;

   fun lfoldint f n min max =
   let fun F i n = if i = max then n else F (i + 1) (f (i, n)) in F min n end;

   fun arrayof (n, f) =
   if n = 0 then arrayoflist []
   else lfoldint (fn (i, a) => (update (a, i, f i); a)) (array (n, f 0)) 1 n;

   local
       fun d n = chr (n + ord "0");
       fun esc n = d (n div 100) ^ d (n mod 100 div 10) ^ d (n mod 10);
       fun character c =
       case c of
	  10 => "\\n"
       |  9  => "\\t"
       |  34 => "\\\""
       |  92 => "\\\\"
       |  _  => if 32 <= c andalso c <= 127 then chr c else ("\\" ^ esc c);
       val character = arrayof (256, character)
   in
       fun quoted string =
       implode
       ("\"" :: rfolds (fn (c, l) => (character sub ord c) :: l) ["\""] string);

       val character = fn c => character sub (ord c)
   end
end;

