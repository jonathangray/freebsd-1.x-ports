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


(**** Chapter 5.  FUNCTIONS AND INFINITE DATA ****)

(*Sections*)
fun secl x f y = f(x,y);
fun secr f y x = f(x,y);

(*** List functionals ***)

fun filter pred [] = []
  | filter pred (x::xs) =
      if pred(x) then x :: filter pred xs  
      else  filter pred xs;

fun takewhile pred [] = []
  | takewhile pred (x::xs) = 
        if  pred x  then  x :: takewhile pred xs  
        else  [];

fun dropwhile pred [] = []
  | dropwhile pred (x::xs) = 
        if  pred x  then  dropwhile pred xs  
        else  x::xs;

fun foldleft f (e, [])    = e
  | foldleft f (e, x::xs) = foldleft f (f(e,x), xs);

fun foldright f ([],    e) = e
  | foldright f (x::xs, e) = f(x, foldright f (xs,e));


(**** SEQUENCES, OR  LAZY LISTS ***)

datatype 'a seq = Nil
                | Cons of 'a * (unit -> 'a seq);

fun head(Cons(x,_)) = x;
fun tail(Cons(_,xf)) = xf();

(*eager -- evaluates xq -- only for "putting back" a sequence*)
fun consq(x,xq) = Cons(x, fn()=>xq);

fun from k = Cons(k, fn()=> from(k+1));

fun takeq (0, xq) = []
  | takeq (n, Nil) = []
  | takeq (n, Cons(x,xf)) = x :: takeq (n-1, xf());

(** functionals for sequences **)
fun mapq f Nil  = Nil
  | mapq f (Cons(x,xf)) = Cons(f x, fn()=> mapq f (xf()));

fun filterq pred Nil = Nil
  | filterq pred (Cons(x,xf)) =
        if pred x then Cons(x, fn()=> filterq pred (xf()))
                  else filterq pred (xf());

fun iterates f x = Cons(x, fn()=> iterates f (f x));

(*Random numbers: real version for systems with 46-bit mantissas
  Generates sequence of random numbers between 0 and 1 from integer seed *)
local val a = 16807.0  and  m = 2147483647.0 in
  fun nextrandom seed =
        let val t = a*seed
        in  t - m * real(floor(t/m))  end
  fun randseq s = mapq (secr op/ m) (iterates nextrandom (real s))
end;

(** prime numbers **)
fun sift p = filterq (fn n => n mod p <> 0);
fun sieve (Cons(p,nf)) = Cons(p, fn()=> sieve (sift p (nf())));
val primes = sieve (from 2);

(** Square Roots **)

fun nextapprox a x = (a/x + x) / 2.0;

fun within (eps:real) (Cons(x,xf)) =
      let val Cons(y,yf) = xf() 
      in  if abs(x-y) <= eps then y
	  else within eps (Cons(y,yf))
      end;

fun qroot a = within 1E~6 (iterates (nextapprox a) 1.0);


(*** Interleaving and sequences of sequences ***)

fun pair x y = (x,y);
fun makeqq (xq,yq) = mapq (fn x=> mapq (pair x) yq) xq;
fun takeqq ((m,n), xqq) = map (secl n takeq) (takeq (m,xqq));

fun interleave (Nil,    yq) = yq
  | interleave (Cons(x,xf), yq) = 
        Cons(x, fn()=> interleave(yq, xf()));

fun enumerate Nil  = Nil
  | enumerate (Cons(Nil, xqf)) = enumerate (xqf())
  | enumerate (Cons(Cons(x,xf), xqf)) =
        Cons(x, fn()=> interleave(enumerate (xqf()), xf()));

val pairqq = makeqq (from 1, from 1);

fun powof2 n = if n=0 then 1 else 2 * powof2(n-1);
fun pack(i,j) = powof2(i-1) * (2*j - 1);

val nqq = mapq (mapq pack) pairqq;


(*** Searching ***)

fun depthfirst (next,pred) x =
  let fun dfs [] = Nil
        | dfs(y::ys) = 
            if pred y then Cons(y, fn()=> dfs(next y @ ys))
                      else dfs(next y @ ys)
  in  dfs [x]  end;

fun breadthfirst (next,pred) x =
  let fun bfs [] = Nil
        | bfs(y::ys) = 
            if pred y then Cons(y, fn()=> bfs(ys @ next y))
                      else bfs(ys @ next y)
  in  bfs [x]  end;

(** 8 Queens Problem **)

fun upto (m,n) = 
    if m>n then []  else  m :: upto(m+1,n);

infix mem;
fun x mem []  =  false
  | x mem (y::l)  =  (x=y) orelse (x mem l);

local fun length1 (n, [ ])  = n 
        | length1 (n, x::l) = length1 (n+1, l)   
in  fun length l = length1 (0,l) end;

fun safequeen oldqs newq =
    let fun nodiag (i, []) = true
          | nodiag (i, q::qs) =
              abs(newq-q)<>i andalso nodiag(i+1,qs)
    in  not (newq mem oldqs) andalso nodiag (1,oldqs)  end;

fun nextqueen n qs =
   map (secr op:: qs) 
       (filter (safequeen qs) (upto(1,n)));

fun isfull n qs = (length qs=n);

(** Depth-first iterative deepening **)

fun depthiter (next,pred) x =
 let fun dfs k (y, sf) = 
          if k=0 then
              if pred y then fn()=> Cons(y,sf)
                        else sf
          else foldright (dfs (k-1)) (next y, sf)
    fun deepen k = dfs k (x, fn()=> deepen (k+1)) ()
 in  deepen 0  end;


(******** SHORT DEMONSTRATIONS ********)

(*random numbers*)
takeq (15, mapq (floor o secl(10.0) op* ) (randseq 1));

takeq(25,primes);

qroot 9.0;

(*sequences of sequences*)
takeqq ((4,6), nqq);
takeq(15, enumerate nqq);

(*8 Queens Problem*)
takeq(100, depthfirst (nextqueen 8, isfull 8) []);
depthfirst (nextqueen 8, isfull 8) [];
depthiter (nextqueen 8, isfull 8) [];
