signature B = sig  exception y  end
signature A = sig  val x:int  structure C:B  end

functor F (structure A:A  structure B:B) = struct end

structure E :B = struct  exception y  end
structure D :A = struct  val x = 4  structure C = E  end

structure G = F (structure A = D  and  B = A.C)
