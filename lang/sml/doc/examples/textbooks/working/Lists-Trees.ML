(**** ML Programs from the book

  ML for the Working Programmer
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1991)

Copyright (C) 1991 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.
****)


(**** Chapter 3.  LISTS ****)

(*** Fundamental List Functions ***)

fun null   []   = true
  | null (_::_) = false;

exception Hd;   (*version from Chapter 4*)
fun hd (x::_) = x
  | hd []     = raise Hd;

exception Tl;   (*version from Chapter 4*)
fun tl (_::xs) = xs
  | tl []     = raise Tl;

(*Length of a list*)
local fun length1 (n, [ ])  = n 
        | length1 (n, x::l) = length1 (n+1, l)   
in  fun length l = length1 (0,l) end;

fun take (n, []) = []
  | take (n, x::xs) =  if n>0 then x::take(n-1,xs)  
                       else  [];

fun drop (_, [])    = []
  | drop (n, x::xs) = if n>0 then drop (n-1, xs) 
                             else x::xs;

(*reversal of a list*)
fun revto ([], ys)    = ys
  | revto (x::xs, ys) = revto (xs, x::ys);

fun rev xs = revto(xs,[]);

fun flat [] = []
  | flat(l::ls) = l @ flat ls;

fun combine ([], [])     = []
  | combine(x::xs,y::ys) = (x,y) :: combine(xs,ys);

fun split [] = ([],[])
  | split((x,y)::pairs) =
        let val (xs,ys) = split pairs
        in  (x::xs, y::ys)  end;

(*searching in a list*)
fun assoc ([], a) = []
  | assoc ((x,y)::pairs, a) =
      if a=x then  [y]  else  assoc(pairs, a);

(*membership in a list*)
infix mem;
fun x mem []  =  false
  | x mem (y::l)  =  (x=y) orelse (x mem l);

fun inter([],ys) = []
  | inter(x::xs, ys) = 
        if x mem ys then x::inter(xs, ys)
                    else    inter(xs, ys);


(*** Applications of lists ***)

(** Binary Arithmetic **)

fun bincarry (0, ps) = ps
  | bincarry (1, []) = [1]
  | bincarry (1, p::ps) = (1-p) :: bincarry(p, ps);

(*sum of two binary numbers with carry*)
fun binsum (c, [], qs) = bincarry (c,qs)
  | binsum (c, ps, []) = bincarry (c,ps)
  | binsum (c, p::ps, q::qs) =
      ((c+p+q) mod 2)  ::  binsum((c+p+q) div 2, ps, qs);

(*product of two binary numbers*)
fun binprod ([], _) = []
  | binprod (0::ps, qs) = 0::binprod(ps,qs)
  | binprod (1::ps, qs) = binsum(0, qs, 0::binprod(ps,qs));


(*** MATRIX OPERATIONS ***)

(** Matrix transpose **)

fun headcol []    = []
  | headcol ((x::_) :: rows) = x :: headcol rows;

fun tailcols []    = []
  | tailcols ((_::xs) :: rows) = xs :: tailcols rows;

fun transp ([]::rows) = []
  | transp rows = headcol rows :: transp (tailcols rows);

(** Matrix product **)

fun dotprod([], []) = 0.0
  | dotprod(x::xs,y::ys) = x*y + dotprod(xs,ys);

fun rowprod(row, []) = []
  | rowprod(row, col::cols) =
        dotprod(row,col) :: rowprod(row,cols);

fun rowlistprod([], cols) = []
  | rowlistprod(row::rows, cols) =
        rowprod(row,cols) :: rowlistprod(rows,cols);

fun matprod(Arows,Brows) = rowlistprod(Arows, transp Brows);


(** Gaussian Elimination (Sedgewick, Chapter 5) **)

(*the first row with absolutely greatest head*)
fun pivotrow [row] = row : real list
  | pivotrow (row1::row2::rows) =
      if abs(hd row1) >= abs(hd row2)
      then pivotrow(row1::rows)
      else pivotrow(row2::rows);

(*the matrix excluding the first row with head p*)
fun delrow (p, []) = []
  | delrow (p, row::rows) =
      if p = hd row then rows
      else row :: delrow(p, rows);

fun scalarprod(k, []) = [] : real list
  | scalarprod(k, x::xs) = k*x :: scalarprod(k,xs);

fun vectorsum ([], []) = [] : real list
  | vectorsum (x::xs,y::ys) = x+y :: vectorsum(xs,ys);

fun gausselim [row] = [row]
  | gausselim rows =
      let val p::prow = pivotrow rows
          fun elimcol [] = []
            | elimcol ((x::xs)::rows) =
                  vectorsum(xs, scalarprod(~x/p, prow))
                  :: elimcol rows
      in  (p::prow) :: gausselim(elimcol(delrow(p,rows)))
      end;

fun solutions [] = [~1.0]
  | solutions((x::xs)::rows) =
      let val solns = solutions rows
      in ~(dotprod(solns,xs)/x) :: solns  end;


(** Dijkstra's problems **)

(*Writing a number as the sum of two squares*)
fun squares r =
  let fun between (x,y) =  (*all pairs between x and y*)
        let val diff = r - x*x
            fun above y =  (*all pairs above y*)
                if y>x then []
                else if y*y<diff then above (y+1)
                else if y*y=diff then (x,y)::between(x-1,y+1)
                else (* y*y>diff *)  between(x-1,y)
        in above y  end;
      val firstx = floor(sqrt(real r))
  in between (firstx, 0) end;

(*The problem of the next permutation*)
fun next(xlist, y::ys) : int list =
    if hd xlist <= y then  next(y::xlist, ys)  (*still increasing*)
    else  (*swap y with greatest xk such that x>=xk>y *)
      let fun swap [x] = y::x::ys
            | swap (x::xk::xs) =          (*x > xk *)
                if xk>y then x::swap(xk::xs)
                else (y::xk::xs)@(x::ys)
                         (* x > y >= xk >= xs *)
      in swap(xlist) end;

fun nextperm (y::ys) = next([y], ys);


(** Toplogical sorting **)

fun nexts (a, []) = []
  | nexts (a, (x,y)::pairs) =
      if a=x then  y :: nexts(a,pairs)
             else       nexts(a,pairs);

fun newvisit (x, (visited,cys)) = (x::visited, cys);

fun cyclesort graph =
  let fun sort ([], path, (visited,cys)) = (visited, cys)
        | sort (x::xs, path, (visited,cys)) =
            sort(xs, path, 
               if x mem path   then  (visited, x::cys)
               else if x mem visited then (visited, cys)
               else newvisit(x, sort(nexts(x,graph),
                                     x::path, (visited,cys))))
      val (xs,_) = split graph
  in sort(xs, [], ([],[])) end;


(*** Merge sorting ***)

fun merge([],ys) = ys : real list
  | merge(xs,[]) = xs
  | merge(x::xs, y::ys) =
      if x<=y then x::merge(xs,  y::ys)
              else y::merge(x::xs,  ys);

(*Top-down merge sort*)
fun tmergesort [] = []
  | tmergesort [x] = [x]
  | tmergesort xs =
      let val k = length xs div 2
      in  merge(tmergesort (take(k,xs)),
                tmergesort (drop(k,xs)))
      end;


(** Richard O'Keefe, A Smooth Applicative Merge Sort, 1982 **)

fun mergepairs([l], k) = [l]
  | mergepairs(l1::l2::ls, k) =
      if k mod 2 = 1 then l1::l2::ls
      else mergepairs(merge(l1,l2)::ls, k div 2);

(*Result must be non-empty*)
fun sorting([], ls, r) = hd(mergepairs(ls,0))
  | sorting(x::xs, ls, r) = sorting(xs, mergepairs([x]::ls, r+1), r+1);

fun sort [] = []
  | sort xs = sorting(xs, [], 0);

fun nextrun(run, []) =       (rev run, []: real list)
  | nextrun(run, x::xs) =
        if  x < hd run then  (rev run, x::xs)
                       else  nextrun(x::run, xs);

fun samsorting([], ls, k) = hd(mergepairs(ls,0))
  | samsorting(x::xs, ls, k) = 
      let val (run, tail) = nextrun([x], xs)
      in  samsorting(tail, mergepairs(run::ls, k+1), k+1)
      end;

fun samsort [] = []
  | samsort xs = samsorting(xs, [], 0);


(**** Chapter 4.  TREES AND CONCRETE DATA ****)

(*** Binary trees ***)

datatype 'a tree = Lf
                 | Br of 'a * 'a tree * 'a tree;

(** Converting trees to lists **)

fun preord (Lf, vs) = vs
  | preord (Br(v,t1,t2), vs) =
        v :: preord (t1, preord (t2, vs));

fun inord (Lf, vs) = vs
  | inord (Br(v,t1,t2), vs) =
        inord (t1, v::inord (t2, vs));

fun postord (Lf, vs) = vs
  | postord (Br(v,t1,t2), vs) =
        postord (t1, postord (t2, v::vs));

(** Converting lists to trees **)

fun balpreorder  []    = Lf
  | balpreorder(x::xs) =
      let val k = length xs div 2
      in  Br(x, balpreorder(take(k,xs)),  balpreorder(drop(k,xs)))
      end;

fun balinorder [] = Lf
  | balinorder xs =
      let val k = length xs div 2
          val y::ys = drop(k, xs)
      in  Br(y, balinorder (take(k,xs)),
                    balinorder ys)
      end;


(** Binary search trees, functional arrays -- see versions using modules **)


(*** Propositional logic -- tautology checker ***)

datatype prop = 
    Atom of string
  | Neg  of prop
  | Conj of prop * prop
  | Disj of prop * prop;

fun implies(p,q) = Disj(Neg p, q);

fun show (Atom a) = a
  | show (Neg p) = "(~ " ^ show p ^ ")"
  | show (Conj(p,q)) = "(" ^ show p ^ " & " ^ show q ^ ")"
  | show (Disj(p,q)) = "(" ^ show p ^ " | " ^ show q ^ ")";

(*naive version*)
fun nnf (Atom a) = Atom a
  | nnf (Neg (Atom a)) = Neg (Atom a)
  | nnf (Neg (Neg p)) = nnf p
  | nnf (Neg (Conj(p,q))) = nnf (Disj(Neg p, Neg q))
  | nnf (Neg (Disj(p,q))) = nnf (Conj(Neg p, Neg q))
  | nnf (Conj(p,q)) = Conj(nnf p, nnf q)
  | nnf (Disj(p,q)) = Disj(nnf p, nnf q);

fun nnfpos (Atom a) = Atom a
  | nnfpos (Neg p) = nnfneg p
  | nnfpos (Conj(p,q)) = Conj(nnfpos p, nnfpos q)
  | nnfpos (Disj(p,q)) = Disj(nnfpos p, nnfpos q)
and nnfneg (Atom a) = Neg (Atom a)
  | nnfneg (Neg p) = nnfpos p
  | nnfneg (Conj(p,q)) = Disj(nnfneg p, nnfneg q)
  | nnfneg (Disj(p,q)) = Conj(nnfneg p, nnfneg q);

fun distrib (p, Conj(q,r)) = Conj(distrib(p,q), distrib(p,r))
  | distrib (Conj(q,r), p) = Conj(distrib(q,p), distrib(r,p))
  | distrib (p, q) = Disj(p,q)   (*no conjunctions*) ;

fun cnf (Conj(p,q)) = Conj (cnf p, cnf q)
  | cnf (Disj(p,q)) = distrib (cnf p, cnf q)
  | cnf p = p    (*a literal*) ;

exception NonCNF;

fun positives (Atom a)      = [a]
  | positives (Neg(Atom _)) = []
  | positives (Disj(p,q))   = positives p @ positives q
  | positives  _            = raise NonCNF;

fun negatives (Atom _)      = []
  | negatives (Neg(Atom a)) = [a]
  | negatives (Disj(p,q))   = negatives p @ negatives q
  | negatives  _            = raise NonCNF;

fun taut (Conj(p,q)) = taut p andalso taut q
  | taut p = ([] <> inter(positives p, negatives p));


(******** SHORT DEMONSTRATIONS ********)

(** Binary Arithmetic **)
binsum(0, [1,1,0,1], [0,1,1,1,1]);
binprod([1,1,0,1], [0,1,1,1,1]);

(** Matrix transpose **)
val matrix =
    [ ["a","b","c"],
      ["d","e","f"] ];

headcol matrix;
tailcols matrix;
transp matrix;

(** Matrix product **)
val Arows = [ [2.0, 0.0], 
              [3.0, ~1.0],
              [0.0, 1.0],
              [1.0, 1.0] ];

val Brows = [ [1.0,  0.0, 2.0],
              [4.0, ~1.0, 0.0] ];

matprod(Arows,Brows);

(** Gaussian Elimination (Sedgewick Chapter 5) **)
gausselim [[ 1.0,  3.0, ~4.0,  8.0], 
           [ 0.0,  2.0, ~2.0,  6.0], 
           [~1.0, ~2.0,  5.0, ~1.0]];
solutions it;
(*should be [1,5,2] with ~1 at end*)

gausselim [[ 0.0,  1.0,  2.0,  7.0,  7.0],
           [~4.0,  0.0,  0.0, ~5.0, ~17.0],
           [ 6.0, ~1.0,  0.0,  0.0,  28.0],
           [~2.0,  0.0,  2.0,  8.0,  12.0]];
solutions it;
(*should be [3,~10,5,1] with ~1 at end*)

(** writing a number as the sum of two squares **)
squares 50;
squares 1105;
squares 27625;
squares 1185665;
squares 48612265;

(** the next permutation **)
nextperm [1,2,4,3];
nextperm it;
nextperm [3,2,2,1];
nextperm it;


(*** Tautology checker ***)

val rich = Atom "rich"
and landed = Atom "landed"
and saintly = Atom "saintly";
val premise1 = implies(landed, rich)
and premise2 = Neg(Conj(saintly,rich));
val concl = implies(landed, Neg saintly);
val goal = implies(Conj(premise1,premise2), concl);

show goal;

nnf premise2;
show it;

nnf goal;
show it;

distrib (Conj(rich,saintly), Conj(landed, Neg rich));
show it;

val cgoal = cnf (nnf goal);
show cgoal;

taut cgoal;
(*  " taut goal; "  would raise an exception! *)

