signature TYVARSET = 
  sig type tyvarset
      val no_tyvars : tyvarset
      val singleton_tyvar : Types.tyvar -> tyvarset
      val union_tyvars : tyvarset * tyvarset * ErrorMsg.complainer -> tyvarset
      val diff_tyvars : tyvarset * tyvarset * ErrorMsg.complainer -> tyvarset
      val get_tyvars: tyvarset -> Types.tyvar list
  end

abstraction TyvarSet : TYVARSET =
struct
  open Types ErrorMsg
  type tyvarset = tyvar list

  val no_tyvars = nil
  fun singleton_tyvar t = [t]
  fun get_tyvars s = s

  fun mem(a as ref(OPEN{kind=UBOUND name,eq,weakness,depth}), 
	  (b as ref(OPEN{kind=UBOUND n,eq=e,weakness=w,depth=d}))::rest,err) =
      let fun complain difference =
		err COMPLAIN ("type variable named " ^
			      (Symbol.name name) ^
			      " occurs with different " ^
			      difference ^
			      " in the same scope")
		    nullErrorBody
      in
	  if a=b then
	      true
	  else if Symbol.eq(name,n) then (
	     (if weakness<>w then complain "weakness levels" else ());
	     (if eq<>e then complain "equality requirements" else ());
	     (if depth<>d then impossible "TyvarSet.mem: bad depths" else ());
	     a := INSTANTIATED(VARty b);
	     b := OPEN{kind=UBOUND n, eq=e orelse eq,
		       weakness=min(weakness,w), depth=depth};
	     true
	  ) else mem(a,rest,err)
       end
    | mem _ = false

  fun union_tyvars([],s,err) = s
    | union_tyvars(s,[],err) = s
    | union_tyvars(a::r,s,err) = if mem(a,s,err) then union_tyvars(r,s,err)
				else a::union_tyvars(r,s,err)
  fun diff_tyvars(s,[],err) = s
    | diff_tyvars([],_,err) = []
    | diff_tyvars(a::r,s,err) = if mem(a,s,err) then diff_tyvars(r,s,err)
					else a::diff_tyvars(r,s,err)
end
