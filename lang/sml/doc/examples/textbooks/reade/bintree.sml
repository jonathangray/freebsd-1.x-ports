
(*---------------------------*
 | BINARY TREES AND PICTURES |
 *---------------------------*)

local
   val copy = Chrisprelude.copy;
   val stringofint = Chrisprelude.stringofint;
   open Charpics
in

(*-----------------*
 | type definition |
 *-----------------*)
datatype 'a bintree =   Lf   of 'a 
                    |   ^^   of 'a bintree * 'a bintree ;
infix ^^;

(*-------------------------------------------*
 | some higher order operations for bintrees |
 *-------------------------------------------*)
fun btreeop f g = let fun btfg (Lf x)  = f x 
                        | btfg (t1^^t2)= g(btfg t1, btfg t2)
                  in btfg end;

fun btreemap f = btreeop (Lf o f) (op ^^);

(*-----------------------------------------*
 | a function for making pictures of trees |
 *-----------------------------------------*)
fun btreepic leafpicfun
    = let 
      fun joinpics((p,n1),(p',n1'))
         = let val n  = width p  ;
               val dashn = (n - n1 + n1');
               val dashn2 = dashn div 2; 
               val dashn1 = dashn - dashn2 - 1; 
               val newn1 = n1 + dashn1 +1;
               val line1 = implode(copy newn1 " " @ ["|"]) ;
               val line2 = implode(copy n1 " " @ ["."] @
                                   copy dashn1 "-" @ ["^"] @ 
                                   copy dashn2 "-" @ ["."]);
               val arms = mkpic [line1,line2];
               val newpic = column [arms,rowwith(""," ","")[p,p']]

           in (newpic,newn1) end;

      fun doleaf x 
              = let val p = leafpicfun x;
                    val n = width p;
                    val n1 = n - n div 2 - 1;
                    val arm = mkpic[implode(copy n1 " " @ ["|"])]
                in (column [arm,p],  n1) end;

      fun picof t = let val (p,n) = btreeop doleaf joinpics t
                    in p end
      in 
        picof
      end;


(* intpic makes pictures of integers *)

fun intpic (n:int) = mkpic[implode["(", stringofint n, ")"]];

(* tree1 and tree2 are example trees. Pictures are formed with e.g.
    btreepic intpic tree1
*)

val tree1 = ((((Lf 3^^Lf 4)^^(Lf 5^^(Lf 6^^Lf 7)))^^Lf 8)^^Lf 9);
val tree2 = tree1^^tree1

end (* of local *);
