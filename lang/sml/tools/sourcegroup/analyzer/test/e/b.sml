
signature A = sig type t val f :unit-> t end;

functor F (AQ:A) = struct val x:AQ.t=AQ.f() end;

funsig FS (AP:A) = sig val x:AP.t end

(* b.sml:6.30-6.33 Error: unbound structure: AP in path AP.t *)

functor AA :FS = F

