(* stream.sml *)

(* streams as a structure *)
(* The type is named stream to avoid conflict with the i/o type stream *)

signature STREAM =
sig
  type 'a stream
  val lazyCons: (unit -> '1a * '1a stream) -> '1a stream
  and cons: '1a * '1a stream -> '1a stream
  and head: 'a stream -> 'a
  and tail: 'a stream -> 'a stream
  and prefix : int -> 'a stream -> 'a list
  and lazyMap : ('1a -> '1b) -> '1a stream -> '1b stream
end

structure Stream : STREAM =
struct
  datatype 'a stream = STREAM of 'a str ref
       and 'a str = SOLID of 'a * 'a stream
		  | SUSPENDED of unit -> ('a * 'a stream);
  fun solidify(STREAM(ref(SOLID p))) = p 
    | solidify(STREAM(s as ref(SUSPENDED f))) =
	let val p = f() in (s := SOLID p; p) end
  fun lazyCons(f) = STREAM(ref(SUSPENDED f))
  fun cons(x,ss) = STREAM(ref(SOLID(x,ss)))
  fun head s = let val (hd,_) = solidify s in hd end
  fun tail s = let val (_,tl) = solidify s in tl end
  fun prefix 0 s = nil
    | prefix n s = (head s)::(prefix (n-1) (tail s))
  fun lazyMap f s =
      let fun g (STREAM(ref(SOLID(x, s)))) = lazyCons(fn () => (f x, g s))
	    | g (s) = lazyCons
		       (fn () => let val (x, s') = solidify s in (f x, g s') end)
       in lazyCons(fn () => solidify(g s))
      end
end;

open Stream;

val nats =   (* stream of natural numbers *)
    let fun f n = lazyCons(fn () => (n, f(n+1)));
     in f 0
    end;

fun add(s1: int stream, s2: int stream) =
    lazyCons(fn () => (head s1 + head s2, add(tail s1, tail s2)));

val fibs =   (* stream of fibonaci numbers *)
    let fun f () = cons(1,lazyCons(fn () =>
				   (1,let val s = f() in add(s, tail s) end)))
     in f ()
    end;
