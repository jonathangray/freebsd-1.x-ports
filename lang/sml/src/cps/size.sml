(* Copyright 1989 by AT&T Bell Laboratories *)
structure CPSsize =
struct
local open CPS

fun cells_list (l : 'a list) = 2 * length l
fun descriptors_list (l : 'a list) = length l

val rec cells =
  fn RECORD(_,vl,w,ce) => 2 + 3 + cells_list vl + cells ce
   | SELECT(i,v,w,ce) => 2 + 4 + cells ce
   | OFFSET(i,v,w,ce) => 2 + 4 + cells ce
   | APP(v,vl) => 2 + 2 + cells_list vl
   | FIX(l,ce) => 2 + 2 + cells_list l + cells ce
		+ fold (fn((f,vl,e),b) => 3 + cells_list vl + cells e + b) l 0
   | SWITCH(v,c,l) => 2 + 3 + cells_list l + fold (fn(a,b) => cells a + b) l 0
   | LOOKER(_,vl,w,ce) => 2+4+ cells_list vl + cells ce
   | SETTER(_,vl,ce) => 2+3+ cells_list vl + cells ce
   | ARITH(_,vl,w,ce) => 2+4+ cells_list vl + cells ce
   | PURE(_,vl,w,ce) => 2+4+ cells_list vl + cells ce
   | BRANCH(_,vl,c,e1,e2) => 2+5+cells_list vl + cells e1 + cells e2

val rec descriptors =
  fn RECORD(_,vl,w,ce) => 2 + descriptors_list vl + descriptors ce
   | SELECT(i,v,w,ce) => 2 + descriptors ce
   | OFFSET(i,v,w,ce) => 2 + descriptors ce
   | APP(v,vl) => 2 + descriptors_list vl
   | FIX(l,ce) => 2 + descriptors_list l + descriptors ce
		+ fold (fn((f,vl,e),b) => 1 + descriptors_list vl
			+ descriptors e + b) l 0
   | SWITCH(v,c,l) => 2 + descriptors_list l
			+ fold (fn(a,b) => descriptors a + b) l 0
   | LOOKER(_,vl,w,ce) => 2 + descriptors_list vl + descriptors ce
   | SETTER(_,vl,ce) => 2+ descriptors_list vl + descriptors ce
   | ARITH(_,vl,w,ce) => 2+ descriptors_list vl + descriptors ce
   | PURE(_,vl,w,ce) => 2+ descriptors_list vl + descriptors ce
   | BRANCH(_,vl,c,e1,e2) => 2+descriptors_list vl 
                                + descriptors e1 + descriptors e2
in

fun printsize ce =
  let val c = cells ce
      val d = descriptors ce
   in app System.Print.say["CPSsize: #cells = ", makestring c, "; #descriptors = ",
			   makestring d, "; total = ", makestring(c+d), "\n"]
  end

end (* local *)

end (* structure CPSsize *)
