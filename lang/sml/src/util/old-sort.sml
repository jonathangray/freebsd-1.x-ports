(* Copyright 1989,1990,1991 by AT&T Bell Laboratories *)
signature SORT =
  sig
    (* pass the gt predicate as an argument *)
     val sort : ('a * 'a -> bool) -> 'a list -> 'a list  
     val sorted : ('a * 'a -> bool) -> 'a list -> bool  
  end

structure Sort : SORT = struct

(* Industrial-strength quicksort.
   Selects pivot from middle of input list.
   Distributes elements equal to pivot "randomly" in the two output partitions.
   Special-cases lists of 0, 1, or 2 elements.
*)
(* This sort function breaks the compiler!
 * symptoms: cannot load mlyacc. - lg

fun quickSort (op > : ('x * 'x -> bool)) =
  let fun splita(pivot,nil,less,greater)= qsort less @ (pivot :: qsort greater)
        | splita(pivot,a::rest,less,greater) =
	             if a>pivot then splitb(pivot,rest,less,a::greater)
			        else splitb(pivot,rest,a::less,greater)
      and splitb(pivot,nil,less,greater)= qsort less @ (pivot :: qsort greater)
        | splitb(pivot,a::rest,less,greater) =
	             if pivot>a then splita(pivot,rest,a::less,greater)
			        else splita(pivot,rest,less,a::greater)
      and split1a(pivot,0,_::r,less,greater) = splitb(pivot,r,less,greater)
        | split1a(pivot,i,a::rest,less,greater) =
	             if a>pivot then split1b(pivot,i-1,rest,less,a::greater)
			        else split1b(pivot,i-1,rest,a::less,greater)
      and split1b(pivot,0,_::r,less,greater) = splita(pivot,r,less,greater)
        | split1b(pivot,i,a::rest,less,greater) =
	             if pivot>a then split1a(pivot,i-1,rest,a::less,greater)
			        else split1a(pivot,i-1,rest,less,a::greater)
      and qsort (l as [a,b]) = if a>b then [b,a] else l
        | qsort (l as _::_::_) = 
           let fun getpivot (x::xr, _::_::rest, i) = getpivot(xr,rest,i+1)
                 | getpivot (x::_, _,i) = split1a(x,i,l,nil,nil)
            in getpivot(l,l,0)
           end
        | qsort l = l
   in qsort
  end
*)

(* merge sort -- Damien Doligez *)

fun mergeSort (op > : ('x * 'x -> bool)) =
  let fun make2List(a::b::rest,accum) =
            make2List(rest,(if b>a then [a,b] else [b,a]) :: accum)
        | make2List(l,accum) = l :: accum
		 
      fun merge (L1 as h1 :: l1, L2 as h2 :: l2) =
             if h1 > h2
             then h2 :: (merge (L1, l2))
             else h1 :: (merge (l1, L2))
        | merge ([], l2) = l2
        | merge (l1, []) = l1

      fun merge_step (l1 :: l2 :: T,accum) =
	      merge_step(T, merge(l1,l2)::accum)
        | merge_step([h],accum) = h :: accum
        | merge_step(nil,accum) = accum
 
      fun merge_sort [l] = l
        | merge_sort nil = nil
        | merge_sort L = merge_sort (merge_step(L,nil))
  in fn [] => []
      | l => merge_sort (make2List(l,nil))
  end

(* insertion sort -- slow! *)

fun insertionSort (op > : ('x * 'x -> bool)) =
    let fun s (a::b::c) =
	    let val (x,y) = if a>b then (b,a) else (a,b)
		fun insert' [] = [y]
		  | insert' (l as c::d) = if y>c then c::insert' d else y::l
		fun insert [] = [x,y]
		  | insert (l as c::d) = 
		    if x>c then c::insert d else x::insert' l
	    in insert(s c)
	    end
	  | s l = l
    in s
    end

val sort = mergeSort

fun sorted (op >) =
  let fun s (x::(rest as (y::_))) = not(x>y) andalso s rest
        | s l = true
  in s
  end

end
