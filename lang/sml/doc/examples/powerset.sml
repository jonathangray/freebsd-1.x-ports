(* This is an example of the use of higher-order functors *)


signature OrdSig =
    sig
	type ground
	val ground_eq : ground -> ground -> bool
	val ground_leq : ground -> ground -> bool
	val ground_to_string : ground -> string
    end


structure NumOrd : OrdSig =
    struct
	type ground = int
	fun ground_eq x y = (x = y)
	fun ground_leq (x:int) y = x <= y
	fun ground_to_string x = makestring (x:int)
    end


signature SetSig =
    sig
	structure Ord : OrdSig
	type set
	val choose : set -> Ord.ground option
	val contained_in : set -> set -> bool
	val empty_set : set
	val insert : Ord.ground -> set -> set
	val intersect : set -> set -> set
	val make_set : Ord.ground list -> set
	val member : Ord.ground -> set -> bool
	val remove : Ord.ground -> set -> set
	val set_eq : set -> set -> bool
	val set_to_string : set -> string
	val union : set -> set -> set
	val for_every : (Ord.ground -> bool) -> set -> bool
	val there_is : (Ord.ground -> bool) -> set -> bool
	val allsubsets : set -> set list
    end

functor SetFunc (Ord:OrdSig) : SetSig =
    struct
	structure Ord = Ord
	local
	    fun mem nil a = false
	      | mem (hd::tl) a = if Ord.ground_eq hd a
				     then true
				 else mem tl a
	in abstype set = set of Ord.ground list
	    with val empty_set = set nil
		fun insert e (set es) = 
		    if mem es e then set es
		    else set (e::es)
		fun choose (set []) = NONE
		  | choose (set (e::es)) = SOME e
		fun member e (set es) = mem es e
		fun union (set []) (set es) = set es
		  | union (set (e::es1)) (set es2) = 
		    insert e (union (set es1) (set es2))
		fun intersect (set []) (set es) = set []
		  | intersect (set es) (set []) = set []
		  | intersect (set (e::es1)) (set es2) =
		    if mem es2 e then
			case (intersect (set es1) (set es2)) of
			    set es => set (e::es)
		    else intersect (set es1) (set es2)
		fun remove e (set []) = set []
		  | remove e (set (e1::es)) = 
		    if Ord.ground_eq e e1 then set es
		    else set (e1::(case  remove e (set es) of set es1 => es1))
		(* another way to write the prev. line
                    else let val set es1 = remove e (set es)
                          in set (e1 :: es1) end
		 *)
		fun contained_in (set []) (set es) = true
		  | contained_in (set (e::es1)) (set es) =
		    (mem es e) andalso (contained_in (set es1) (set es))
		fun set_to_string (set l) =
		     let fun conv (nil) = ""
			   |  conv (a::nil) = Ord.ground_to_string a
			   |  conv (a::b) =
			        (Ord.ground_to_string a) ^ "," ^ conv b
		     in "{" ^ conv(l) ^ "}" end
		fun for_every P (set []) = true
		  | for_every P (set (x::xs)) =
		     (P x) andalso (for_every P (set xs))

		fun there_is P (set []) = false
		  | there_is P (set (x::xs)) =
		     (P x) orelse (there_is P (set xs))
	    end
	end
	fun set_eq s1 s2 = (contained_in s1 s2) andalso (contained_in s2 s1)
	fun make_set [] = empty_set
	  | make_set (hd::tl) = insert hd (make_set tl)
	fun allsubsets set =
	    (case choose set
	       of NONE => [empty_set]
	        | SOME elt =>
		      let
			  val subsets_without_elt = allsubsets(remove elt set)
		      in
			  subsets_without_elt @
			  (map (insert elt) subsets_without_elt)
		      end)
    end (* functor SetFunc *)


functor SetContOrdFunc (Set : SetSig) : OrdSig =
    struct
	type ground = Set.set
	val ground_eq = Set.set_eq
	val ground_leq = Set.contained_in
	val ground_to_string = Set.set_to_string
    end


functor SetHoareOrdFunc (Set : SetSig) : OrdSig =
    struct
	type ground = Set.set
	val ground_to_string = Set.set_to_string
	fun ground_leq smaller_set bigger_set =
	      Set.for_every
	       (fn big => (Set.there_is
			   (fn small => Set.Ord.ground_leq small big)
			   smaller_set))
	       bigger_set
	fun ground_eq set1 set2 = ground_leq set1 set2 andalso
	                          ground_leq set2 set1

    end


functor SetSmytheOrdFunc (Set : SetSig) : OrdSig =
    struct
	type ground = Set.set
	val ground_to_string = Set.set_to_string
	fun ground_leq smaller_set bigger_set =
	      Set.for_every
	       (fn small => (Set.there_is
			     (fn big => Set.Ord.ground_leq small big)
			    bigger_set))
	       smaller_set
	fun ground_eq set1 set2 = ground_leq set1 set2 andalso
	                          ground_leq set2 set1
    end

signature PowersetSig =
    sig
	structure Set : SetSig
	structure Powerset : SetSig
	sharing type Set.set = Powerset.Ord.ground
	val powerset : Set.set -> Powerset.set
    end

functor PowersetFunc (functor SetOrdFunc(Set : SetSig)
			  : sig 
				type ground
				val ground_eq : ground -> ground -> bool
				val ground_leq : ground -> ground -> bool
				val ground_to_string : ground -> string
				sharing type Set.set = ground
			    end
		      structure Ord : OrdSig) : PowersetSig =
    struct	
	structure Set = SetFunc(Ord)
	structure Powerset = SetFunc(SetOrdFunc(Set))
	fun powerset set = Powerset.make_set (Set.allsubsets set)
    end


structure NumPowerset = PowersetFunc(functor SetOrdFunc = SetContOrdFunc
				     structure Ord = NumOrd)

structure HoarePowerset = PowersetFunc(functor SetOrdFunc = SetHoareOrdFunc
				       structure Ord = NumOrd)

structure SmythePowerset = PowersetFunc(functor SetOrdFunc = SetSmytheOrdFunc
					structure Ord = NumOrd);


val ans1 = NumPowerset.Powerset.set_to_string
            (NumPowerset.powerset (NumPowerset.Set.make_set [1,2,3,4]));

val ans2 = HoarePowerset.Powerset.set_to_string
            (HoarePowerset.powerset (HoarePowerset.Set.make_set [1,2,3,4]));

val ans3 = SmythePowerset.Powerset.set_to_string
            (SmythePowerset.powerset (SmythePowerset.Set.make_set [1,2,3,4]));


