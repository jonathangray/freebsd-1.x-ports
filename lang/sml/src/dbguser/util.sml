structure UserDebugUtil = struct
(** Generally useful functions. **)

exception DebugUserError of string  (* Indicates a problem in this code. *)

(* Inform if no debuggable code active. *)
fun printNotUnder () = System.Print.say "[Not executing under debugger.]\n" 

(* Find the first element in a list for which a given condition is
 * satisfied. *)
fun select f nil = NONE
  | select f (a :: a') = if (f a) then SOME a else select f a'

(* Construct a list of members of a given list for which a given condition
 * is satisfied *)
fun choose f l =
    fold (fn (m, l) => (if (f m) then [m] else []) @ l) l []

(* look up an element of an association list *)
fun lookup nil l = NONE
  | lookup ((k, v) :: rest) l = if l = k then SOME v else lookup rest l

fun first f l =
(* Find the index of the first element in a list for which a given condition
 * is satisfied. *)
    let fun first' f n nil = NONE
	  | first' f n (a :: b) =
	    if (f a) then SOME n else first' f (n + 1) b
    in first' f 0 l end

val printL = System.Print.say o implode

val foot = hd o rev  (* last element of a list
		      * probably not a terribly efficient implementation *)

fun pairlist (a::ar) (b::br) = (a,b)::(pairlist ar br)
  | pairlist _ nil = nil
  | pairlist nil _ = nil 

(* Some simple option functions *)

fun eqOption (a, SOME b) = (a = b)
  | eqOption _ = false

fun onSome (f, SOME a) = SOME (f a)
  | onSome (_, NONE) = NONE

fun ofSome (f, SOME a) = f a
  | ofSome (_, NONE) = NONE

fun ifSome (f, SOME a) = f a
  | ifSome (_, NONE) = ()

fun isSome (SOME _) = true
  | isSome NONE = false

fun somes (SOME a :: a') = a :: (somes a')
  | somes (NONE :: a') = (somes a')
  | somes nil = nil

fun ensure (SOME x, _) = x
  | ensure (NONE, e) = raise e

fun ensureD (v, s) = ensure (v, DebugUserError s)

infix isIn
fun elem isIn set = isSome (select (fn x => x = elem) set)

(* Strip duplicate elements from a list *)
fun uniq nil = nil
  | uniq (a :: a') =
    let val u = uniq a' in
	if a isIn u then u else (a :: u)
    end

fun string_first s s' =
(* Find the first occurrence of a given character in a string. *)
    first (fn c => (c = s')) (explode s)

fun rightstring (s, n) =
(* Return the substring of s extending from position n to the end *)
    substring (s, n, size s - n)

fun splitString delimiter s =
(* Return a list of elements (strings separated by single or multiple
   delimiters) in a given string. *)
    if s = "" then [] else
	case string_first s delimiter of
	    NONE => [s]
	  | SOME n =>
		let val rest = splitString delimiter (rightstring (s, n + 1))
		in
		    if n = 0 then rest else substring (s, 0, n) :: rest
		end

fun dewords l c =
(* Return a string constructed by prefixing each word in the given list with
 * the given character. *)
    implode (fold (fn (i, s) => c :: i :: s) l [])

fun absolute pathname directory =
(* Convert the given pathname, which is relative to the given directory,
 * to absolute form. *)
    let fun simplify nil = nil
          | simplify (a :: rest) =
	    let val rest' = simplify rest in
		case rest' of
		    nil => [a]
		  | ".." :: r' => if a <> ".." then r' else a :: rest'
		  | _ => a :: rest'
	    end
	val already = (substring(pathname, 0, 1) = "/")
	val path = splitString "/"
	    (if already then pathname else directory ^ "/" ^ pathname)
    in dewords (simplify path) "/"
    end

end
