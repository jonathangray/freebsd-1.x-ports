(* redblack2.sml *)

functor RedBlack(B : sig type key
			 val > : key*key->bool
		     end):
	    sig type tree
		type key
		val empty : tree
		val insert : key * tree -> tree
		val lookup : key * tree -> key
		exception Notfound of key
	    end =
struct
 open B
 datatype tree = empty | RED of key * tree * tree
		       | BLACK of key * tree * tree
 exception Notfound of key

 fun insert (key,t) =
  let fun f empty = RED(key,empty,empty)
        | f (BLACK(k,l,r)) =
	    if key>k
	    then case f r
		 of r as RED(rk, rl as RED(rlk,rll,rlr),rr) =>
			(case l
			 of RED(lk,ll,lr) =>
				RED(k,BLACK(lk,ll,lr),
					   BLACK(rk,rl,rr))
			  | _ => BLACK(rlk,RED(k,l,rll),
						RED(rk,rlr,rr)))
		  | r as RED(rk,rl, rr as RED(rrk,rrl,rrr)) =>
			(case l
			 of RED(lk,ll,lr) =>
				RED(k,BLACK(lk,ll,lr),
					   BLACK(rk,rl,rr))
			  | _ => BLACK(rk,RED(k,l,rl),rr))
	          | r => BLACK(k,l,r)
	    else if k>key
	    then case f l
	         of l as RED(lk,ll, lr as RED(lrk,lrl,lrr)) =>
			(case r
			 of RED(rk,rl,rr) =>
				RED(k,BLACK(lk,ll,lr),
					   BLACK(rk,rl,rr))
			  | _ => BLACK(lrk,RED(lk,ll,lrl),
						RED(k,lrr,r)))
		  | l as RED(lk, ll as RED(llk,lll,llr), lr) =>
			(case r
			 of RED(rk,rl,rr) =>
				RED(k,BLACK(lk,ll,lr),
					   BLACK(rk,rl,rr))
			  | _ => BLACK(lk,ll,RED(k,lr,r)))
	          | l => BLACK(k,l,r)
	    else BLACK(key,l,r)
        | f (RED(k,l,r)) =
	    if key>k then RED(k,l, f r)
	    else if k>key then RED(k, f l, r)
	    else RED(key,l,r)
   in case f t
      of RED(k, l as RED(_,_,_), r) => BLACK(k,l,r)
       | RED(k, l, r as RED(_,_,_)) => BLACK(k,l,r)
       | t => t
  end

 fun lookup (key,t) =
  let fun next(k,l,r) =
		if k>key then look l
		else if key>k then look r
		else k
      and look empty = raise (Notfound key)
	| look (RED(a)) = next(a)
	| look (BLACK(a)) = next(a)
   in look t
  end

end
