(* datatypes *)

structure B = struct type b = bool end

structure A = struct
  datatype ('a, 'b) Map =
     Empty
   | Single of 'a * 'b
   | Flagged of 'a * 'b * B.b
  exception R
end

structure C = struct
  val x :('a,'b) A.Map = A.Empty

  exception E of B.b

  val y =
    case x of
       (A.Single (_,_)) => (A.Empty; print "foo\n")
     | _ => ()
end

structure D = struct
  exception F = C.E
end

structure E = struct
  exception X = A.R
end

(*
Seq
; Define s%B
; Define s%A
; = Ref s%B 
; Define s%C
; = Seq
; = ; Ref s%A s%A s%B s%A s%A 
; Define s%D
; = Ref s%C 
; Define s%E
; = Ref s%A 
*)
