
(***********************************************************************
 | Structure Charpics containing ABSTRACT TYPE PICTURE                 |
 |                                                                     |
 | (Uses Chrisprelude)                                                 |
 |                                                                     |
 | C.M.P.Reade      Oct 1987                                           |
 |                                                                     |
 | (Comments at End)                                                   |
 ***********************************************************************)

structure Charpics = struct


local open Chrisprelude; infix upto in

abstype picture = Pic of int * int * string list
with
     fun mkpic linelist 
               = let val d = length linelist;
                     val shape = map size linelist;
                     val w = maxposlist shape;
                     fun addspaces line = let val a = size line in
                                           if a<w then line^spaces(w-a) 
                                                  else line
                                          end;
                     val checkedlines = map addspaces linelist
                 in Pic(d,w,checkedlines) end;

     fun depth (Pic(d,_,_)) = d;
     fun width (Pic(_,w,_)) = w;
     fun linesof (Pic(_,_,sl)) = sl;
     val nullpic = Pic(0,0,[]);
     fun padside n (pic as Pic(d,w,sl))
          = if n <= w then pic
                      else Pic(d,n,map (fn s=>s^spaces(n-w)) sl);
     fun padbottom n (pic as Pic(d,w,sl))
          = if n <= d then pic
                      else Pic(n,w,sl @ copy (n-d) (spaces w));
    fun rowwith fsb piclist 
          = let val d' = maxposlist(map depth piclist);
                val blocks = map (linesof o padbottom d') piclist;
                fun mkline n = stringwith fsb (map (select n) blocks);
                val sl' = map mkline (1 upto d');
                val w' = if null sl' then 0 else size(hd sl')
            in Pic(d',w',sl') end;

    val row = rowwith ("","","");

    fun colwith (f,s,b) piclist
          = let val w' = maxposlist(map width piclist);
                val flines = map (implode o (copy w')) (explode f);
                val slines = map (implode o (copy w')) (explode s);
                val blines = map (implode o (copy w')) (explode b);
                val sl' = linkwith(flines,slines,blines)
                                  (map (linesof o padside w') piclist);
                val d' = length sl'
             in Pic(d',w',sl') end;

    val column = colwith ("","","");

    fun indent n (pic as Pic(d,w,sl))
               = if n<1 then pic
                        else Pic(d,w+n,map (concat(spaces n)) sl);

    fun lower n (pic as Pic(d,w,sl))
               = if n<1 then pic
                        else Pic(d+n,w,copy n (spaces w) @ sl);

   fun table [] = nullpic |
       table piclistlist 
                = let fun mkrect piclistlist  (* makes sure each list has same length *)
                  = let val sizerows = map length piclistlist;
                        val maxrow = maxposlist sizerows;
                        fun addnulls len piclist
                            = if len<maxrow 
                              then piclist @ (copy (maxrow-len) nullpic)
                              else piclist
                    in zip addnulls sizerows piclistlist end;
               val newpics = mkrect piclistlist;
               val picwidths = map(map width) newpics
               val colwidths = map maxposlist (transpose picwidths);
               val picrowlists = map (zip padside colwidths) newpics;
               val tablerows = map (rowwith ("|","|","|")) picrowlists;
               fun dashes n = implode (copy n "-");
               val sep = stringwith ("+","+","+") (map dashes colwidths);
               val sl' = linkwith ([sep],[sep],[sep]) (map linesof tablerows);
               val d' = length sl';
               val w' = size(hd sl')
           in Pic(d',w',sl') end;

    fun frame picture = table [[picture]];
           
    fun header s pic = colwith ("","~","") [mkpic[s],pic];

    fun showpic picture
            = (show(stringwith ("","\n","\n") (linesof picture));picture);

    fun paste n m pic1 pic2   (* n,m may be negative, pic2 goes over *)
                              (* pic1 at n rows down and m chars in  *)
            = if n<0 then paste 0 m (lower (~n) pic1) pic2 else
              if m<0 then paste n 0 (indent(~m) pic1) pic2 else
              let val pic1' = padbottom (n+depth pic2) (padside (m+width pic2) pic1);
                  fun spliceat n f x y = if   n<1 
                                         then splice f x y
                                         else hd x::spliceat (n-1) f (tl x) y;
                  fun overlay a b = b;
                  fun stringop line line' = implode(spliceat m overlay 
                                                             (explode line)
                                                             (explode line'));
                  val sl' = spliceat n stringop (linesof pic1') (linesof pic2);
                  val w' = if null sl' then 0 else size(hd sl');
                  val d' = length sl'
              in Pic(d',w',sl') end;

   fun cutfrom pic n m a b  (* n,m,a,b may be negative, a picture of size a deep and b wide *)
                            (* is cut from pic starting at n rows down and m chars in       *)
            = if n<0 then cutfrom (lower (~n) pic) 0 m a b else
              if m<0 then cutfrom (indent(~m) pic) n 0 a b else
              if a<0 then cutfrom pic (n+a) m (~a) b       else
              if b<0 then cutfrom pic n (m+b) a (~b)       else
              let val pic' = padbottom (n+a) (padside (m+b) pic);
                  fun edit str = implode(sublist (m+1) b (explode str));
                  val newsl = map edit (sublist (n+1) a (linesof pic'))
              in Pic(a,b,newsl) end

end (* of abstract type picture *)

end (* of local *)
end (* of Charpics *);




(********************* COMMENTS **********************************


The following are available for constructing character pictures
 
abstype picture

** BASIC OPERATIONS ****************

 mkpic       : (string list) -> picture
             This can be used to form very simple atomic pictures.
             The argument should be a list of the picture lines.
 showpic     : picture -> picture
             This can be used to look at a picture. (It is actually
             the identity operation with a side effect printing the
             picture. Consequently it should only be used at the top level.
 depth       : picture -> int
             Returns the number of lines
 width       : picture -> int
             Returns the length of the longest line
 linesof     : picture -> (string list)
             Returns the lines themselves (used in defining some other ops.)
 nullpic     : picture
             An empty picture. (Equivalent to mkpic[]).

** MAIN PICTURE OPERATIONS *******

 frame       : picture -> picture
             This outlines a picture using "+---+" and "|"
 table       : ((picture list) list) -> picture
             This forms a table when supplied with a list of the rows of the
             table. Each row should be a list of pictures.
 paste       : int -> (int -> (picture -> (picture -> picture)))
             paste n m p1 p2 places p2 ontop of p1 at the point after
             n characters down and m characters along. It is robust in that
             it works for negative n and m and when p1 is too small.
 cutfrom     : picture -> (int -> (int -> (int -> (int -> picture))))
             cutfrom p n m d w produces a picture of depth d and width w
             cut from p starting at the point after n characters down and
             m characters along. (None of the integers are required to be 
             positive.

** SOME OTHER PICTURE OPERATIONS *******

 row         : (picture list) -> picture
             This forms a picture by lining up a list of pictures as a row
 column      : (picture list) -> picture
             This forms a picture by lining up a list of pictures as a column
 rowwith     : (string * string * string) -> ((picture list) -> picture)
             Similar to row, but a triple of strings must be supplied
             to be duplicated on the left, between pictures and on the
             right respectively.
 colwith     : (string * string * string) -> ((picture list) -> picture)
             Similar to column, but a triple of strings must be supplied
             (characters) to be duplicated along the top, between pictures
             and along the bottom, respectively.
 indent      : int -> (picture -> picture)
             indent n p adds spaces to the left of p
 lower       : int -> (picture -> picture)
             lower n p adds spaces at the top of p
 padside     : int -> (picture -> picture)
             padside n p forms a picture of AT LEAST width n using
             spaces to pad when necessary.
 padbottom   : int -> (picture -> picture)
             padbottom n p forms a picture of AT LEAST depth n using
             spaces to pad when necessary.
 header    : string -> (picture -> picture)
             The string is supplied as a heading to be placed above the picture.

***********************************************************************)
