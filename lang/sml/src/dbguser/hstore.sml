(* hstore.sml *)

structure HistoricalArray (* : ARRAY *) =
struct
  local
    open System.Unsafe System.Unsafe.Weak
    (* plist is shared with DebugStore *)
    datatype ('a,'b) plist = PNIL | PCONS of 'a * 'b * ('a,'b) plist
    val (updatedAList: (object Array.array weak,int) plist ref,_,_,
	 hcreatea: ((int * object) -> (object array)),_) = 
		  cast (!System.Control.Debug.interface 16)
  in
    open Array
    fun array(cnt:int,obj:'1a) : '1a array = cast (hcreatea(cnt,cast obj)) 
	(* will raise Subscript if cnt < 0 *)

    fun arrayoflist (nil:'1a list) : '1a array = 
	  cast(hcreatea(0,cast 0 (* ?? *)))
      | arrayoflist (l as (e::r)) =
	    let val a = array(List.length l, e)
	        fun init ((e::r),n) = (update(a,n,e); init(r,n+1))
  	          | init (nil,_) = ()
  	    in  init(r,1); a
            end

(* We handle update in the instrumenter for efficiency; it could be 
   handled here thus: 
    fun update (arr, offset, value) =
	  (Array.update(arr,offset,value);
	   updatedAList := PCONS(weak(cast arr),offset,!updatedAList))
*)

    fun tabulate (i:int,f:int -> '1a) : '1a array =
      let fun tab j = 
	  if j < i then f j::tab(j+1) else nil
      in if i < 0 then raise Size else arrayoflist (tab 0)
      end
  end (*local*)
end (*struct*)


structure HistoricalRef =
struct
  local
    open System.Unsafe System.Unsafe.Weak
    val (_,updatedRList: object Array.array weak list ref,_,_,_) =
		  cast (!System.Control.Debug.interface 16)
  in
    open Ref
    (* We handle op := directly in the instrumenter for efficiency;
	  it could be handled here as follows:
      val op := = fn (r,value) => 
		  (Ref.:= (r,value);
		   updatedRList := weak (cast r) :: (!updatedRList)) *)
  
    (* We handle inc and dec here for now, because it is difficult to
	  arrange to instrument them. *)
    fun inc r = (Ref.inc r; 
		 updatedRList := weak (cast r) :: (!updatedRList))
    fun dec r = (Ref.dec r;
		 updatedRList := weak (cast r) :: (!updatedRList))
  end (*local*)
end (*struct*)

