(* intset-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for sets of integers.
 *)

signature INTSET =
  sig

    type intset

    exception NotFound

    val empty : intset
	(* Create a new set
	 *)

    val singleton : int -> intset
	(* Create a singleton set
	 *)

    val add : intset * int -> intset
	(* Insert an int.  
	 *)

    val addList : intset * int list -> intset
	(* Insert a list of ints.  
	 *)

    val isEmpty : intset -> bool
	(* Return true if and only if the set is empty *)

    val equal : (intset * intset) -> bool
	(* Return true if and only if the two sets are equal *)

    val isSubset : (intset * intset) -> bool
	(* Return true if and only if the first set is a subset of the second *)

    val member : intset * int -> bool
	(* Return true if and only if the int is an element in the set *)

    val delete : intset * int -> intset
	(* Remove an int.
         * Raise NotFound if not found
	 *)

    val numItems : intset ->  int
	(* Return the number of ints in the table *)

    val union : intset * intset -> intset
        (* Union *)

    val intersection : intset * intset -> intset
        (* Intersection *)

    val difference : intset * intset -> intset
        (* Difference *)

    val listItems : intset -> int list
	(* Return a list of the ints in the set *)

    val app : (int -> 'b) -> intset -> unit
	(* Apply a function to the entries of the set 
         * in decreasing order
         *)

    val revapp : (int -> 'b) -> intset -> unit
	(* Apply a function to the entries of the set 
         * in increasing order
         *)

    val fold : (int * 'b -> 'b) -> intset -> 'b -> 'b
	(* Apply a folding function to the entries of the set 
         * in decreasing order
         *)

    val revfold : (int * 'b -> 'b) -> intset -> 'b -> 'b
	(* Apply a folding function to the entries of the set 
         * in increasing order
         *)

    val exists : (int -> bool) -> intset -> int option
	(* Return an item satisfying the predicate, if any *)

  end (* INTSET *)
