
signature A = sig type t val f :unit-> t end
structure Y:A = struct type t = int * int fun f()=(3,4) end

signature B = sig structure Q:A val x:Q.t val y:int end

signature C = sig val w:int end
structure CC :C = struct val w = 4 end

functor F (E:C) (D:A) :B = struct structure Q:A=D val x:Q.t=Q.f() val y=E.w end

funsig FS (AP:A) = B

functor AA :FS = F (CC)

structure AF :B = AA (Y)

structure XX = F (CC)
