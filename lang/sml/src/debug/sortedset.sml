signature SORTED_SET_ITEM =
sig
  type t
  type k
  val key: t -> k
  val lt : k * k -> bool
end

signature SORTED_SET =
sig
  type s
  type t
  type k
  exception NotFound
  exception DuplicateKey
  val new: unit -> s
  val insert: s * t -> s
  val delete: s * k -> s
  val find: s * k -> t (* exact match *)
  val findp: s * k -> t (* pred match *)
  val finds: s * k -> t (* succ match *)
  val findc: s * (t->bool) -> t (* first to match monotonic condition *)
  val update: s * t -> s (* must already exist *)
  val iterate: s * (t->'a) -> unit (* lowest to highest *)
  val iteratefrom : s * k * (t->'a) -> unit (* lowest to highest from start *)
  val fold: s * ((t * 'a) -> 'a) * 'a -> 'a    (* highest first order *)
  val revfold: s * ((t * 'a) -> 'a) * 'a -> 'a (* lowest first order *)
  val size: s -> int
end

functor SortedSet (I:SORTED_SET_ITEM) : SORTED_SET =
(* RB-tree insertion implementation courtesy A. Appel *)
struct
 open I
 val op< = lt
 datatype color = RED | BLACK
 datatype tree = empty | tree of (t * bool) * color * tree * tree
 type s = tree * int (* valid count *) * int (* invalid count *)
 exception NotFound
 exception DuplicateKey

 fun key' (n,v) = key n

 fun new () = (empty,0,0)

 fun insert ((s,vc,ivc),n) =
  let val reused = ref false
      fun f empty = tree((n,true),RED,empty,empty)
        | f (s as tree(t as (_,v),BLACK,l,r)) =
	    if key' t < key n
	    then case f r
		 of r as tree(rt,RED, rl as tree(rlt,RED,rll,rlr),rr) =>
			(case l
			 of tree(lt,RED,ll,lr) =>
				tree(t,RED,tree(lt,BLACK,ll,lr),
					   tree(rt,BLACK,rl,rr))
			  | _ => tree(rlt,BLACK,tree(t,RED,l,rll),
						tree(rt,RED,rlr,rr)))
		  | r as tree(rt,RED,rl, rr as tree(rrt,RED,rrl,rrr)) =>
			(case l
			 of tree(lt,RED,ll,lr) =>
				tree(t,RED,tree(lt,BLACK,ll,lr),
					   tree(rt,BLACK,rl,rr))
			  | _ => tree(rt,BLACK,tree(t,RED,l,rl),rr))
	          | r => tree(t,BLACK,l,r)
	    else if key n < key' t
	    then case f l
	         of l as tree(lt,RED,ll, lr as tree(lrt,RED,lrl,lrr)) =>
			(case r
			 of tree(rt,RED,rl,rr) =>
				tree(t,RED,tree(lt,BLACK,ll,lr),
					   tree(rt,BLACK,rl,rr))
			  | _ => tree(lrt,BLACK,tree(lt,RED,ll,lrl),
						tree(t,RED,lrr,r)))
		  | l as tree(lt,RED, ll as tree(llt,RED,lll,llr), lr) =>
			(case r
			 of tree(rt,RED,rl,rr) =>
				tree(t,RED,tree(lt,BLACK,ll,lr),
					   tree(rt,BLACK,rl,rr))
			  | _ => tree(lt,BLACK,ll,tree(t,RED,lr,r)))
	          | l => tree(t,BLACK,l,r)
	    else if v then
		   raise DuplicateKey
		 else (reused := true; tree((n,true),BLACK,l,r))
        | f (s as tree(t as (_,v),RED,l,r)) =
	    if key' t < key n then tree(t,RED,l, f r)
	    else if key n < key' t then tree(t,RED, f l, r)
	    else if v then
		   raise DuplicateKey
		 else (reused := true; tree((n,true),RED,l,r))
      val s' =
        case f s of
	  tree(t,RED, l as tree(_,RED,_,_), r) => tree(t,BLACK,l,r)
	| tree(t,RED, l, r as tree(_,RED,_,_)) => tree(t,BLACK,l,r)
	| s => s
  in (s',vc+1,if !reused then ivc - 1 else ivc)
  end

 fun find((s,_,_),k) =
  let fun look empty = raise NotFound
	| look (tree(t as (n,v),_,l,r)) =
		if k < key' t then look l
		else if key' t < k then look r
		else if v then n
		else raise NotFound
   in look s
  end


 fun findp((s,_,_),k) = (* return item or its predecessor *)
  let fun match empty = raise NotFound
        | match (tree(t as(n,v),_,l,r)) =
	    if k < key' t then 
	      match l
	    else if key' t < k then
	      (match r) handle NotFound => 
		           if v then 
			     n 
			   else lookmax l
	    else if v then
	      n 
	    else lookmax l
      and lookmax empty = raise NotFound
	| lookmax (tree(t as (n,v),_,l,r)) =
	    (lookmax r) handle NotFound => 
		            if v then 
			      n 
			    else lookmax l
  in match s
  end		

 fun finds((s,_,_),k) = (* return item or its successor *)
  let fun match empty = raise NotFound
        | match (tree(t as (n,v),_,l,r)) =
	    if k < key' t then 
	      (match l) handle NotFound =>
		           if v then 
			     n
			   else lookmin r
	    else if key' t < k then
	      match r
	    else if v then
	      n
	    else lookmin r
      and lookmin empty = raise NotFound
	| lookmin (tree(t as (n,v),_,l,r)) =
	    (lookmin l) handle NotFound => 
		            if v then 
			      n 
			    else lookmin r
  in match s
  end		


 fun findc((s,_,_),cond) = (* returns first item to match monotonic cond *)
  let fun match empty = raise NotFound
        | match (tree(t as (n,v),_,l,r)) =
	    if v then
	      if cond n then 
	        (match l) handle NotFound => n
	      else match r
	    else (match l) handle NotFound => match r
  in match s
  end		

 fun update((s,vc,ivc),n) =
   let fun look empty = raise NotFound
         | look (tree(t as (_,v),c,l,r)) =
	      if key n < key' t then tree(t,c,look l,r)
	      else if key' t < key n then tree(t,c,l,look r)
	      else if v then tree((n,true),c,l,r)
              else raise NotFound
   in (look s,vc,ivc)
   end 

  fun iteratefrom ((s,_,_),k,f) =
    let fun g empty = ()
          | g (tree(t as (n,v),_,l,r)) =
	      (if k < key' t then g l else ();
	       if not(key' t < k) andalso v then (f n;()) else ();
	       g r)
    in g s 		  
    end		

  fun iterate ((s,_,_),f) =
    let fun g empty = ()
          | g (tree(t as (n,v),_,l,r)) = 
	         (g l; if v then (f n;()) else (); g r)
    in g s
    end

  fun fold ((s,_,_),f,i) =
    let fun g (empty,a) = a 
	  | g (tree(t as (n,v),_,l,r),a) = 
	         if v then g(l,f(n,g(r,a)))
		 else g(l,g(r,a))
    in g (s,i)
    end	    
	    
  fun revfold((s,_,_),f,i) =
    let fun g (empty,a) = a
          | g (tree(t as (n,v),_,l,r),a) =
	         if v then g(r,f(n,g(l,a)))
		 else g(r,g(l,a))
    in g (s,i)
    end

 fun delete(set as (s,vc,ivc),k) = 
  let fun look empty = raise NotFound
        | look (tree(t as (n,v),c,l,r)) =
	     if k < key' t then tree(t,c,look l,r)
	     else if key' t < k then tree(t,c,l,look r)
	     else if v then tree((n,false),c,l,r)
	     else raise NotFound
      val (set' as (s',vc',ivc')) = (look s,vc-1,ivc+1)
  in if vc' > ivc' then 
       set'
     else (* reorganize *)
       (fold(set',fn (n,set'') => insert(set'',n),new()))
  end 

  fun size (_,vc,_) = vc

end (* functor *)
