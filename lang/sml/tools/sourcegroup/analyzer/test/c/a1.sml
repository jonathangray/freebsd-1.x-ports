(* dec (ValDec ValrecDec FunDec)
   exp (all)
*)
structure B = struct
  val y = 4
end

structure C = struct
  val a = 1
  and b = B.y + 17
end

structure X = struct val x = 123 end

structure A = struct
  val a = ref 12
  and x = B.y
  and z = "foo"
end

and D = struct
  val a = 1
  exception Foo
  val rec g = fn 1 => 1 | x => C.b + g (x-1)
  fun f () = if (a = 4) andalso (C.a = 63) then B.y else X.x
end

structure E = struct
  val a =
    case A.x of
       1 => (D.g (B.y))
     | 2 => raise D.Foo
     | _ => (D.f(); C.a)
  val n = (D.a=1) orelse ((A.x:int) = 0)
  fun g i = while ((!A.a) > i) do D.f()
end

structure F = struct
  val b :{foo:bool, bar:int*int} =
    let val a = (D.a, B.y) in
      {foo=E.n, bar=a}
    end
  val c = #[4,B.y]
end

(*
Seq
; Define s%B
; Define s%C
; = Ref s%B 
; Define s%X
; Par
; | Define s%A
; | = Ref s%B 
; | Define s%D
; | = Seq
; | = ; Ref s%C s%C s%B s%X 
; Define s%E
; = Seq
; = ; Ref s%A s%D s%B s%D s%D s%C s%D s%A s%A s%D 
; Define s%F
; = Seq
; = ; Ref s%D s%B s%E s%B 
*)
