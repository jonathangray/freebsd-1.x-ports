(* Copyright 1989 by AT&T Bell Laboratories *)
structure AbsMach =
struct
  type reg = int
  type label = int

  datatype arithop = imul | iadd | isub | idiv 
		  | orb | andb | xorb | rshift | lshift
		   | fadd | fdiv | fmul | fsub |

  datatype comparison = ilt | ieq | igt | ile | igt | ine
	              | flt | feq | fgt | fle | fgt | fne 
		      | inrange | outofrange
  datatype opcode =
      FETCH of {immutable: bool, offset: int, ptr: reg, dst: reg}
		(* dst := M[ptr+offset]
	 	   if immutable then unaffected by any STORE
		   other than through the allocptr *)
    | FETCHB of {offset: int, ptr: reg, dst: reg}
    | STORE of {offset: int, src: reg, ptr: reg}
		(* M[ptr+offset] := src *)
    | STOREB of {offset: int, ptr: reg, dst: reg}
    | GETLAB of {lab: label, dst: reg}
    | GETREAL of {value: real, dst: reg}
    | ARITH of {oper: arithop, src1: reg, src2: reg, dst: reg}
    | ARITHI of {oper: arithop, src1: reg, src2: int, dst: reg}
    | MOVE of {src: reg, dst: reg}
    | BRANCH of {test: comparison, src1: reg, src2: reg, dst: label}
    | JUMP of {dst: reg}
    | LABEL of {lab:label, live: reg list}
    | WORD of {value: int}
    | STRING of {value: string}
    | LABWORD of {lab: label}

end
