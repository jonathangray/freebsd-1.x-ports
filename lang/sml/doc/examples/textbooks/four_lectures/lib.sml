signature LISTUTIL= 
 sig
  val isIn : ''a * ''a list -> bool
  val union: ''a list * ''a list -> ''a list
  val minus: ''a list * ''a list -> ''a list
  val intersect: ''a list * ''a list -> ''a list
  val map: ('a -> 'b) -> 'a list -> 'b list
  val fold: (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b
  val zip: 'a list * 'b list -> ('a * 'b)list
  exception Lookup
  val lookup: (''a * 'b) list -> ''a -> 'b
  exception Prefix
  val prefix: 'a list * int -> 'a list
 end;

functor List(): LISTUTIL=
struct
  fun isIn(x,[])= false
    | isIn(x,hd::tl) = x = hd orelse isIn(x,tl)
  fun union([],l) =l
    | union(hd::tl,l) = if isIn(hd,l) then union(tl,l)
                        else hd::union(tl,l)
  fun minus([],l)= []
    | minus(hd::tl,l) = if isIn(hd,l) then minus(tl,l)
                        else hd::minus(tl,l)
  fun intersect([],l) = []
    | intersect(hd::tl, l) = if isIn(hd,l) then
                                hd::intersect(tl,l)
                             else intersect(tl,l)
  fun map f [] = []
    | map f (hd::tl) = f hd :: map f tl

  fun fold f b [] = b
    | fold f b (hd::tl) = f(hd,fold f b tl)

  fun zip([],l) = []
    | zip(l,[]) = []
    | zip(hd::tl, hd'::tl')= (hd,hd')::zip(tl,tl')

  exception Lookup
  fun lookup [] x = raise Lookup
    | lookup ((x,y)::tl) x'=
        if x=x' then y else lookup tl x'

  exception Prefix
  fun prefix(l,0) = []
    | prefix([],n) = raise Prefix
    | prefix((hd::tl),n)= hd::prefix(tl,n-1)
end;


signature PRINTUTIL =
sig
  val intToString: int -> string
  val natToString: int -> string
end;

functor Print():PRINTUTIL=
struct
  fun intToString(i:int)=  
          (if i<0 then " -" else "")^ natToString (abs i)
  and natToString(n:int)=
      let val d = n div 10 in
        if d = 0 then chr(ord"0" + n)
        else natToString(d)^ chr(ord"0" + (n mod 10))
      end
end;
