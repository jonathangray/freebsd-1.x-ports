
functor list() :list=
struct
   nonfix All Some;
   infixr 7 <|;
   infixr 6 MAP;
   infix  5 </ //;
   infixr 5 \\ />;
   infix  4 ||| \ INTER;
   infixr 4 UNION;
   infix  0 nonmember member subset;

   exception First
   and       Zip
   and       Reduce  ;

   fun P <| []        = []
   |   P <| (x :: xs) = if P x then x :: (P <| xs) else P <| xs;

   fun f MAP []        = []
   |   f MAP (x :: xs) = f x :: (f MAP xs);

   fun [] ||| []               = []
   |   (x :: xs) ||| (y :: ys) = (x, y) :: (xs ||| ys)
   |   _ ||| _                 = raise Zip;

   fun f \\ P =
   let fun F []        = []
       |   F (x :: xs) = if P x then f x :: F xs else F xs
   in
       F
   end;

   fun e /> ++ =
   let infix ++;
       fun F r []        = r
       |   F r (x :: xs) = F (r ++ x) xs
   in
       F e
   end;

   fun ++ </ e =
   let infix ++; fun F [] = e | F (x :: xs) = x ++ F xs in F end;

   fun ++ // (x :: xs) = (x /> ++) xs
   |   ++ // []        = raise Reduce;

   fun first P []        = raise First
   |   first P (x :: xs) = if P x then x else first P xs;

   fun FIRST P nil       = NONE
   |   FIRST P (x :: xs) = if P x then SOME x else FIRST P xs;

   fun Some P xs = case FIRST P xs of SOME _ => true | NONE => false;

   fun All P xs =
   case FIRST (fn x => not (P x)) xs of SOME _ => false | NONE => true;

   fun x member S = Some (fn x' => x = x') S;

   fun x nonmember S = not (x member S);

   fun S INTER T = (fn x => x member S) <| T;

   fun S \ T = (fn x => x nonmember T) <| S;

   fun S UNION T = S @ T;

   val set = [] /> (fn (s, e) => if e member s then s else e :: s)

   fun xs subset ys = All (fn x => x member ys) xs
end;
