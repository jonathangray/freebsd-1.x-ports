(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Eric Cooper (eric.cooper@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

structure ListFns :LIST_FNS =
    struct
	exception Zip

	fun zip [] [] = []
	  | zip (a :: x) (b :: y) = (a, b) :: zip x y
	  | zip _ _ = raise Zip

	fun unzip [] = ([], [])
	  | unzip ((a, b) :: z) =
	    let val (x, y) = unzip z
	    in
		(a :: x, b :: y)
	    end

	exception NotFound

	fun find _ [] = raise NotFound
	  | find f (a :: x) = if f a then a else find f x

	fun assoc x alist = #2 (find (fn (a, b) => a = x) alist)

	fun index e list =
	    let fun loop [] _ = raise NotFound
		  | loop (a :: x) n = if a = e then n else loop x (n+1)
	    in
		loop list 0
	    end

	exception Split

	fun split 0 list = ([], list)
	  | split n (a :: x) =
	    let val (y, z) = split (n-1) x
	    in
		(a :: y, z)
	    end
	  | split _ _ = raise Split

	fun list_string init sep final func list =
	    let fun fmt [] = ""
		  | fmt [e] = func e
		  | fmt (e :: rest) = func e ^ sep ^ fmt rest
	    in
		init ^ fmt list ^ final
	    end

	fun filter [] = []
	  | filter (SOME a :: x) = a :: filter x
	  | filter (NONE :: x) = filter x
    end
