signature INFO = sig
    exception Info
    val info : string list -> unit
    val search : string -> unit
end

structure Info:INFO = struct

exception Info

local 
  open System.Env System.Symbol System.Compile
in

fun printPath [] = ()
  | printPath (h::t) = (print (h:string);print ".";printPath t)

(* return true if s1 is a substring of s2, false otherwise *)
fun substring s1 s2 =
  let val es1 = explode s1 and es2 = explode s2
      exception TooShort
      fun prefix [] l2 = true
	| prefix l1 [] = raise TooShort
	| prefix (x1::l1) (x2::l2) = (x1 = x2) andalso (prefix l1 l2)
      fun substring [] = false
	| substring (l as (h::t)) =
	    (prefix es1 l orelse substring t) handle TooShort => false
  in substring es2 end

(* get the environment associated to a structure *)

fun strToEnv' path env =
  let val save = !System.Print.out
      (* redirections of messages to /dev/null *)
      val _ = System.Print.out := open_out "/dev/null"
      (* function to recover a sane environment (cancels redirections) *)
      fun sane () = (close_out (!System.Print.out);System.Print.out := save)
      exception Unbound of string list
      fun envStr path [] envPair = envPair
        | envStr path (name::endpath) (env,allEnv) = (
            let val full = concatEnv(env,!pervasiveEnvRef)
		val env' =
	          eval_stream(open_string ("open "^ name^";"),full)
	    in 
            envStr (name::path) endpath (env',concatEnv(env',allEnv))
            end
	    handle e as Unbound _ => raise e
		 | _ => raise Unbound(name::path))

  in 
  (envStr [] path (env,env)) before sane ()
     handle Unbound (h::t) =>
       (sane (); printPath (rev t); print h; print " unbound\n"; raise Info)
  end

(* get the environment of a structure *)
fun strToEnv path = 
  let val (dynEnv,dynAllEnv) = 
    strToEnv' path (concatEnv(!topLevelEnvRef,!pervasiveEnvRef))
  in (staticPart dynEnv, staticPart dynAllEnv) end

(* describes bindings associated to a name in an environment. allenv contains
   all the visible bindings at that level and is used by describe but the
   lookup is done in statenv *)

fun infoEnv nom statenv allenv = 
  let val cat = catalogEnv statenv
      fun test sym =
	 if (name sym) = nom then (describe allenv sym; print "\n") else ()
  in app test cat end

(* information on all the bindings of a qualified identifier *)

fun info name =
  case rev name 
  of [] => raise Info
   | name :: path => 
	let val (env,allEnv) = strToEnv (rev path) 
	in infoEnv name env allEnv end

(* search for all bindings containing the string given in `id`  *)
fun search id = 
  let val envTop = (concatEnv(!topLevelEnvRef,!pervasiveEnvRef))
      val obj =
        catalogEnv (staticPart envTop)
      fun search _ [] = ()
	| search (ctx as (path,env)) (symbol::tail) =
	    let val nom = name symbol and typ = kind symbol
	    in
	    if substring id nom andalso nom <> "it" then (
	      print typ; print " "; printPath (rev path);
	      print nom; print "\n")
	    else ();
	    if typ = "structure" then 
              let val (subEnv,_) = strToEnv' [nom] env
	      in search (nom::path,subEnv) (catalogEnv (staticPart subEnv)) end
	    else ();
	    search ctx tail
            end
  in search ([],envTop) obj end

end

end