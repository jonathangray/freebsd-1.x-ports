(*********************************************************
 | Structure Chrisprelude (general purpose functions)    |
 |                                                       |
 | Chris Reade      Oct 1987                             |
 |                                                       |
 | stringofint added Sept 1991                           |
 | error changed to work for other implementations       |
 | (and ex_undefined renamed as ex_error) July 92        |
 | (Comments at end)                                     |
 *********************************************************)

structure Chrisprelude = struct

exception ex_error of string;
fun error s = (output(std_out,"ERROR REPORT:\n"^s^"\n"); raise ex_error s);

fun equal a b = a=b;

structure Combinator = struct
fun I x = x;
fun K x y = x;
fun C f x y = f y x;
fun W f x = f x x;
fun Y f = let fun fixf x = f (fixf) x in fixf end;
fun curry f a b = f(a,b);
fun uncurry f (a,b) = f a b;
fun pair a b = (a,b);
fun fst(x,y) = x;
fun snd(x,y) = y;
fun couple f g x = (f x,g x);
fun repeat f = let fun rptf n x = if n=0 then x else rptf (n-1) (f x);
                   fun check n = if n<0 then error "repeat <0" else n
               in rptf o check end
end; (* of Combinator *)
open Combinator;

structure Int = struct
fun plus (a:int) b = a+b;
fun times (a:int) b = a*b;
fun less (a:int) b = b<a;
fun lesseq (a:int) b = b<=a;
fun greater (a:int) b = b>a;
fun greatereq (a:int) b = b>=a;
fun max (a:int) b = if a<b then b else a;
fun min (a:int) b = if a<b then a else b
end; (* of Int *)
open Int;

structure Bool = struct
fun non p = not o p;
fun ou p q x = p x orelse q x;
fun et p q x = p x andalso q x
end; (* of Bool *)
open Bool;

structure List = struct
fun assoc pairlist default arg
             = let fun search []         = default arg 
                   |   search ((a,b)::x) = if arg = a then b else search x
               in search pairlist end;



fun fold f = let fun foldf a []     = a 
                 |   foldf a (b::x) = foldf (f a b) x 
             in foldf end;
val accumulate = fold;
fun foldright f a x = accumulate (C f) a (rev x);
fun zip f = let fun zf (a::x) (b::y) = f a b :: zf x y 
                |   zf [] []  = []
                |   zf _  _   = error "zip with different lengthed lists"
            in zf end;
fun splice f = let fun sf (a::x) (b::y) = f a b :: sf x y 
                   |   sf x [] = x 
                   |   sf [] x = x
               in sf end;
fun filter p = let fun consifp x a = if p a then a::x else x
               in rev o accumulate consifp [] end;
fun exists p = let fun existsp [] = false 
                   |   existsp (a::x) = if p a then true
                                               else existsp x
               in existsp end;
val forall =  non o exists o non;
fun member x a = exists (equal a) x ;
fun contains x y = forall (member y) x;
fun null [] = true
|   null _  = false;
fun hd(a::x) = a 
|   hd []    = error "hd of [] is undefined" 
and tl(a::x) = x 
|   tl []    = error "tl of [] is undefined";
fun cons a x = a::x;
fun append x y = x @ y;

   infix 5 upto ;
fun n upto m = if n>m then [] else n::(n+1 upto m);
val revonto = accumulate (C cons);
val reverse = revonto [];
fun link llist = rev (accumulate revonto [] llist) ;
fun linkwith (front,sep,back) l
               = let fun f []  = [back] 
                     |   f [a] = [a,back] 
                     |   f (a::x) = a::sep::f x
                 in link (front::f l)  end;
val pairlists = zip pair;
fun copy n x = repeat (cons x) n [];
val sumlist = accumulate plus 0;
val mullist = accumulate times 1;

fun maxlist (a::x) = accumulate max a x 
|   maxlist []     = error "maxlist of [] is undefined";
fun maxposlist x   = accumulate max 0 x;
fun transpose [] = [] 
|   transpose x  = if exists null x 
                   then [] 
                   else (map hd x):: transpose (map tl x);
val length = let fun count n a = n+1
             in accumulate count 0 end;
val drop = repeat tl;
fun split n = if n<0 
              then error "negative subscript error(split failed)"
              else let fun shunt 0 x1 x2      = (rev x1,x2)
                       |   shunt n x1 (a::x2) = shunt (n-1) (a::x1) x2
                       |   shunt _  _  _      = error "list subscript error(split failed)"
              in shunt n [] end;
fun front n x = fst(split n x);
fun back n x = drop (length x - n) x;
fun select n = hd o (drop (n-1));
fun sublist n m x =  front m (drop (n-1) x)
end; (* List *)
open List;

structure String = struct
fun concat s1 s2 = s1 ^ s2;
fun show x = output(std_out,x);
fun stringwith (front,sep,back) sl 
               = let fun f []  = [back] 
                     |   f [a] = [a,back] 
                     |   f (a::x) = a::sep::f x
                 in implode (front::f sl)  end;
fun spaces n = implode (copy n " ");
fun newlines n = implode (copy n "\n");
local
  fun cofdig n = chr (n+48);
  fun stringofnat n = if n<10 then cofdig n
                              else stringofnat (n div 10) ^ cofdig (n mod 10)
in
  fun stringofint n = if n<0 then "~" ^ stringofnat(~n)
                            else stringofnat n
end;
fun sless (s:string) s' = s' < s;
fun slesseq (s:string) s' = s' <= s
end; (* String *)
open String

end; (* of struct Chrisprelude *)


(************* COMMENTS ************************************


Structure Chrisprelude contains the functions
listed in the following signature (and described below):-

structure Chrisprelude = struct
   exception ex_error : string
   val error = fn : string -> 'a
   structure Bool =
      struct
         val ou = fn : ('a -> bool) -> ('a -> bool) -> 'a -> bool
         val non = fn : ('a -> bool) -> 'a -> bool
         val et = fn : ('a -> bool) -> ('a -> bool) -> 'a -> bool
         end
   structure List =
      struct
         val exists = fn : ('a -> bool) -> 'a list -> bool
         val maxlist = fn : int list -> int
         val zip = fn : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
         val accumulate = fn : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
         val foldright = fn : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
         val cons = fn : 'a -> 'a list -> 'a list
         val maxposlist = fn : int list -> int
         val member = fn : ''a list -> ''a -> bool
         val assoc = fn : (''a * 'b) list -> (''a -> 'b) -> ''a -> 'b
         val append = fn : 'a list -> 'a list -> 'a list
         val null = fn : 'a list -> bool
         val hd = fn : 'a list -> 'a
         val revonto = fn : 'a list -> 'a list -> 'a list
         val linkwith = fn :
            'a list * 'a list * 'a list -> 'a list list -> 'a list
         val copy = fn : int -> 'a -> 'a list
         val splice = fn : ('a -> 'a -> 'a) -> 'a list -> 'a list -> 'a list
         val forall = fn : ('a -> bool) -> 'a list -> bool
         val sumlist = fn : int list -> int
         val mullist = fn : int list -> int
         val length = fn : 'a list -> int
         val drop = fn : int -> 'a list -> 'a list
         val filter = fn : ('a -> bool) -> 'a list -> 'a list
         val back = fn : int -> 'a list -> 'a list
         val upto = fn : int * int -> int list
         val front = fn : int -> 'a list -> 'a list
         val select = fn : int -> 'a list -> 'a
         val pairlists = fn : 'a list -> 'b list -> ('a * 'b) list
         val split = fn : int -> 'a list -> 'a list * 'a list
         val sublist = fn : int -> int -> 'a list -> 'a list
         val transpose = fn : 'a list list -> 'a list list
         val tl = fn : 'a list -> 'a list
         val fold = fn : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
         val reverse = fn : 'a list -> 'a list
         val link = fn : 'a list list -> 'a list
         val contains = fn : ''a list -> ''a list -> bool
         end
   structure Combinator =
      struct
         val uncurry = fn : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
         val I = fn : 'a -> 'a
         val couple = fn : ('a -> 'b) -> ('a -> 'c) -> 'a -> 'b * 'c
         val K = fn : 'a -> 'b -> 'a
         val curry = fn : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
         val snd = fn : 'a * 'b -> 'b
         val W = fn : ('a -> 'a -> 'b) -> 'a -> 'b
         val Y = fn : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
         val repeat = fn : ('a -> 'a) -> int -> 'a -> 'a
         val C = fn : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
         val pair = fn : 'a -> 'b -> 'a * 'b
         val fst = fn : 'a * 'b -> 'a
         end
   structure String =
      struct
         val slesseq = fn : string -> string -> bool
         val sless = fn : string -> string -> bool
         val stringofint = fn : int -> string
         val stringwith = fn : string * string * string -> string list -> string
         val spaces = fn : int -> string
         val newlines = fn : int -> string
         val show = fn : string -> unit
         val concat = fn : string -> string -> string
         end
   structure Int =
      struct
         val greatereq = fn : int -> int -> bool
         val min = fn : int -> int -> int
         val greater = fn : int -> int -> bool
         val max = fn : int -> int -> int
         val lesseq = fn : int -> int -> bool
         val times = fn : int -> int -> int
         val less = fn : int -> int -> bool
         val plus = fn : int -> int -> int
         end

DESCRIPTION OF FUNCTIONS

The functions described here are (mostly) of general use in
constructing further functions.
Some are more specialised and are included to illustrate
ways in which functions can be defined and used in ML.
(One or two higher order functions may look obscure because of
the amount of parameterisation, but they are flexible and can
be easily specialised once understood.)
Each function is given with its type, a brief description and
its actual definition.

error : string -> 'a
When error is applied to any string, the result is undefined
(the result type is arbitrary since there is no result).
The purpose of error is to  cause the program to abort with the
error message supplied as its argument when such an application
is evaluated in some larger expression.
(See the definitions of hd and tl for examples of its use.)
Evaluation abortion is achieved using ML's exception mechanism,
but exceptions are not defined or raised anywhere else in these
definitions other than through applications of 'error'.
(ex_error is the name of the (only) exception raised).

Boolean Related Functions *********************
ou : ('a -> bool) -> ('a -> bool) -> 'a -> bool
combines predicates using 'or' so that
ou p q a = p a orelse q a

et : ('a -> bool) -> ('a -> bool) -> 'a -> bool
combines predicates using 'and' so that
et p q a = p a andalso q a

non : ('a -> bool) -> 'a -> bool
negates a predicate so that
non p a = not(p a)

List Operations *******************************
exists : ('a -> bool) -> 'a list -> bool   
exists p x is true if x contains an item satisfying
predicate p and isfalse otherwise.

maxlist : int list -> int   
maxlist x produces the maximum integer in list x (x non-empty)
and raises ex_undefined if x is empty.

zip : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list   
zip extends two place functions to act on lists in a
zip-fastener fashion.
e.g. zip f [a1,a2,...,an] [b1,b2,...,bn] = [f a1 b1, f a2 b2,..., f an bn] 
zip raises ex_undefined if the list lengths are not the same.

accumulate : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a   
accumulate is a synonym for fold.

foldright : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b  
foldright is similar to accumulate/fold but associating the
argument-function applications to the right instead of the left.
E.g. foldright f a [a1,a2,a3,...,an] = f a1 (f a2 (f a3 (...(f an a)...)))
It is defined in terms of accumulate in order to benefit from
tail recursion optimisation.

cons : 'a -> 'a list -> 'a list   
cons is a curried version of the list constructor.

maxposlist : int list -> int   
maxposlist x is equivalent to maxlist(0::x) and produces 0 if
x is empty or if x contains only negative integers.

member : ''a list -> ''a -> bool   
a test for membership of a list. member [2,4,7,9] 7 = true

assoc : (''a -> 'c) -> ((''a * 'c) list) -> ''a -> 'c
This complex looking function essentially builds a function
from a list of pairs (representing a mapping).
It takes an additional parameter, 
namely: a default function to be used if no match is found.
E.g. fun example = assoc I [(2,4),(3,9)]
Then example 2 = 4, example 3 = 9 and example x = x for any
other int x.

append : 'a list -> 'a list -> 'a list   
append is a curried form for the infix operator @

null : 'a list -> bool   
The predicate null asks if a list is empty

hd : 'a list -> 'a   
The selector hd and tl returns the first item
when applied to a non empty list. 
It raises ex_undefined if applied to the empty list.

revonto : 'a list -> 'a list -> 'a list   
revonto x y is the list produced by reversing list y onto the
front of x. e.g. revonto x [a1,a2,...,an] = [an,...a2,a1] @ x

linkwith : 'a list * 'a list * 'a list -> 'a list list -> 'a list
linkwith (front,sep,back) l  links together a list of lists l  
into a single list seperating each list with the list sep and
surrounding the result by  the lists front and back.
e.g. linkwith (front,sep,back) [a1,a2,a3,...an]
      = front @ a1 @ sep @ a2 @ sep @ a3 @ sep ... @ an @ back

copy : int -> 'a -> 'a list   
copy n x produces a list n copies of x (n>=0) and raises abort if n<0.

splice : ('a -> 'a -> 'a) -> 'a list -> 'a list -> 'a list   
splice extends two place functions to act on lists in the same
way as zip. The difference is that splice appends the remainder
of the longer list if given lists of different lengths and it
has a more restrictive type.
e.g. splice f [a1,a2,...,an] [b1,b2,...,bm] 
      = [f a1 b1, f a2 b2, ... , f an bn] @ [bn',...,bm] 
      when n <= m (n'=n+1) and similarly. 
      = [f a1 b1, f a2 b2, ... , f am bm] @ [am',...,an] 
      when m <= n (m'=m+1). 

forall : ('a -> bool) -> 'a list -> bool   
forall p x is true if every item in x satisfies p and is
false otherwise.

sumlist : int list -> int   
sumlist x produces the sum of the integers in list x.

mullist : int list -> int
mullist x produces the product of the integers in list x.

length : 'a list -> int   
length x returns the length of list x.

drop : int -> 'a list -> 'a list
drop n x is the remainder of list x after dropping the
first n items.
It raises ex_undefined when n<0 and when n>length x.

filter : ('a -> bool) -> 'a list -> 'a list   
filter p x returns the sublist of those items in list x which
satisfy the predicate p (in the same order as they appear in x).
back : int -> 'a list -> 'a list
back n x is the last n items of list x.
It raises ex_undefined when n<0 and when n>length x

upto : int * int -> int list   
upto is an infix operator. "n upto m" produces the ascending
list of integers between and including n and m (n<=m)
and is [] if n>m. e.g. 3 upto 7 = [3,4,5,6,7]
YOU MAY NEED to reaffirm 'infix upto;'
before using it this way.

front : int -> 'a list -> 'a list
front n x is the first n items of x.
It raises ex_undefined when n<0 and when n>length x.

select : int -> 'a list -> 'a   
select n x returns the nth item in list x
(starting at 1 for the head).
It raises ex_undefined when n is too large and when n<1.

pairlists : 'a list -> 'b list -> ('a * 'b) list   
pairlists forms a list of pairs by combining two lists
componentwise e.g.
pairlists [a1,a2, ..., an] [b1,b2,...,bn] = [(a1,b1),(a2,b2),...,(an,bn)]
pairlists raises ex_undefined if the list lengths are not the same.

split : int -> 'a list -> ('a list * 'a list)
split n x returns a pair of lists consisting of the front n
items of x and the rest of x, respectively. 
It raises ex_undefined when n<0 and when n>length x.

sublist : int -> int -> 'a list -> 'a list
sublist n m x is the list of m items starting with and
following on from the nth item of x.
It raises ex_undefined when n<1 and when m<0 and when n+m-1>length x.

transpose : 'a list list -> 'a list list   
transpose takes a list of lists thought of as rows of a matrix and   
produces the list of lists representing columns.
Any rows longer than the shortest row will be clipped.   

tl : 'a list -> 'a list
The selector hd and tl returns the list without the first item
when applied to a non empty list. 
It raises ex_undefined if applied to the empty list.

fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a   
fold combines items in a list with a two place function and a
default value to reduce the list.
e.g. fold f a [a1,a2,a3,...,an] = f (f...(f (f a a1) a2) a3) ...an

reverse : 'a list -> 'a list   
reverse just reverses a list and is equivalent to predefined rev.

link : 'a list list -> 'a list   
link llist  links together llist (a list of lists) into a single list    
e.g. link [a1,a2,a3,...an] = a1 @ a2 @ a3 @ ... @ an

contains : ''a list -> ''a list -> bool   
contains is a list version of set containment.
Testing whether list y contains all the items in x is written:
      contains x y
NOTE THE ORDER OF ARGUMENTS (contains x) is a predicate on lists such as y
contains [2,4,7,9] [3,7,9,5,4,2] = true


Some Combinators ********************************
uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c   
If f is a function expecting two successive arguments, then
(uncurry f) is a similar function applicable to argument pairs.

I : 'a -> 'a
I is the identity function

couple : ('a -> 'b) -> ('a -> 'c) -> 'a -> 'b * 'c
couple is used to apply two functions to the same argument
to return a pair of results.
Thus couple f g a = (f a, g a).

K : 'a -> 'b -> 'a   
K forms constant functions, so that K x returns x when applied
to anything

curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c   
If f is a function expecting an argument pair, then (curry f)
is a similar function expecting the arguments one at a time

snd : 'a * 'b -> 'b
snd returns the second item from a pair

W = fn : ('a -> 'a -> 'b) -> 'a -> 'b
W duplicates an argument for a curried function (W f a =  f a a)
Y : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b   
Y is the fixed point combinator. If F is an operation on functions 
returning a function of the same type, then Y F is the (least) 
fixed point of F satisfying F(Y F) = (Y F).

repeat : ('a -> 'a) -> int -> 'a -> 'a   
repeat f n x is equivalent to n applications of f to x, i.e.
f (f (f...(f x)..)).
However, if n<0 then repeat f n x raises ex_undefined.

C : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c   
C swaps the arguments for a curried function so (C f) x y = f y x.

pair : 'a -> 'b -> 'a * 'b   
pair is a curried function constructing pairs.

fst : 'a * 'b -> 'a
fst returns the first item from a pair

Operations on Strings *************************
slesseq : string -> string -> bool
curried <= for strings where slesseq s1 s2 = s2<=s1
NOTE argument order.

sless : string -> string -> bool
curried < for strings where sless s1 s2 = s2<s1
NOTE argument order.

stringofint : int -> string
converts any integer to its decimal representation as a string

stringwith : string * string * string -> string list -> string
stringwith is a string version of linkwith.
E.g. stringwith (front,sep,back) [s1,s2,s3,...,sn]
      = front ^ s1 ^ sep ^ s2 ^ sep ^ ... ^ sn ^ back

spaces : int -> string
spaces n is a string of n space chars (n>=0) and raises
ex_undefined if n<=0.

newlines : int -> string
newlines n is a string of n newline chars (n>=0) raises
ex_undefined if n<=0.

show : string -> unit   
show prints a string at the terminal as a side effect.
For functional programming, it should only be used at top level
for  displaying strings in their crude form
(with control characters interpreted) or for defining other,
similarly restricted, display functions.

concat : string -> string -> string   
concat is a curried version of the string concatenator ^

Integer Arithmetic
greatereq : int -> int -> bool   
greatereq is a curried version of < for integers
BUT NOTE THE ORDER OF ARGUMENTS
(greatereq a) is a predicate which when applied to b returns true if b>=a.

min : int -> int -> int   
min a b returns the smaller of integers a and b

greater : int -> int -> bool   
greater is a curried version of > for integers
BUT NOTE THE ORDER OF ARGUMENTS
(greater a) is a predicate which when applied to b returns true if b>a.

max : int -> int -> int   
max a b returns the larger of integers a and b

lesseq : int -> int -> bool   
lesseq is a curried version of <= for integers
BUT NOTE THE ORDER OF ARGUMENTS
(lesseq a) is a predicate which when applied to b returns true if b<=a.

times : int -> int -> int   
times is a curried version of * for integers

less : int -> int -> bool   
less is a curried version of < for integers
BUT NOTE THE ORDER OF ARGUMENTS
(less a) is a predicate which when applied to b returns true if b<a.

plus : int -> int -> int   
plus is a curried version of + for integers

**************************************************)
