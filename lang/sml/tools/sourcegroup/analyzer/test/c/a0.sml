(* types *)

structure B = struct
  type ('a, 'b) t = int * int * 'a * 'b
end

structure C = struct
  type tt = int
end

structure A = struct
  type x = int * C.tt
end

and D = struct
  type q = (bool, C.tt) B.t
end

structure E = struct
  type 'c r = {a:int, b:C.tt, d:bool*A.x*('c list)}
end

(*
Seq
; Define s%B
; Define s%C
; Par
; | Define s%A
; | = Ref s%C 
; | Define s%D
; | = Seq
; | = ; Ref s%B s%C 
; Define s%E
; = Seq
; = ; Ref s%C s%A 
*)
