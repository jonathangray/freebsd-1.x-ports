
signature A = sig type t val f :unit-> t end

structure Y:A = struct type t = int * int fun f()=(3,4) end

signature B = sig structure Q:A val x:Q.t end

functor F (D:A) :B = struct structure Q:A=D val x:Q.t=Q.f() end

funsig FS (AP:A) = B

functor AA :FS = F

structure AF :B = F (Y)