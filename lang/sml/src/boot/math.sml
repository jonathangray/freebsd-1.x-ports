(* Copyright 1989 by AT&T Bell Laboratories *)
(* The following functions were adapted from the 4.3BSD math library.
   Eventually, each machine supported should have a hand-coded math
   functor with more efficient versions of these functions.

***************************************************************************
*                                                                         * 
* Copyright (c) 1985 Regents of the University of California.             *
*                                                                         * 
* Use and reproduction of this software are granted  in  accordance  with *
* the terms and conditions specified in  the  Berkeley  Software  License *
* Agreement (in particular, this entails acknowledgement of the programs' *
* source, and inclusion of this notice) with the additional understanding *
* that  all  recipients  should regard themselves as participants  in  an *
* ongoing  research  project and hence should  feel  obligated  to report *
* their  experiences (good or bad) with these elementary function  codes, *
* using "sendbug 4bsd-bugs@BERKELEY", to the authors.                     *
*                                                                         *
* K.C. Ng, with Z-S. Alex Liu, S. McDonald, P. Tang, W. Kahan.            *
* Revised on 5/10/85, 5/13/85, 6/14/85, 8/20/85, 8/27/85, 9/11/85.        *
*                                                                         *
***************************************************************************

*)

signature MATH =	(* must be alphabetical order *)
  sig
    exception Ln and Sqrt
    val arctan : real -> real
    val cos : real -> real
    val exp : real -> real
    val ln : real -> real
    val sin : real -> real
    val sqrt : real -> real
  end

structure Math : MATH = struct

structure Assembly : ASSEMBLY = Core.Assembly

infix 7 * /
infix 6 + -
infix 4 > < >= <=

exception Sqrt
exception Ln
exception Overflow = Assembly.Overflow

val op + : real * real -> real = InLine.fadd
val op - : real * real -> real = InLine.fsub
val op * : real * real -> real = InLine.fmul
val op / : real * real -> real = InLine.fdiv
val op > : real * real -> bool = InLine.fgt
val op < : real * real -> bool = InLine.flt
val op >= : real * real -> bool = InLine.fge
val op <= : real * real -> bool = InLine.fle

val ieql : int * int -> bool = InLine.ieql
val ineq : int * int -> bool = InLine.ineq
val feql : real * real -> bool = InLine.feql
val lessu : int * int -> bool = InLine.lessu

(* This function is IEEE double-precision specific *)
fun scalb(x,k) = if lessu(InLine.+(k,1022),2046) then Assembly.A.scalb(x,k)
                 else let val k1 = InLine.div(k,2)
		          val k2 = InLine.-(k,k1)
		       in scalb(scalb(x,k1),k2)
		      end

val two_to_the_54 = 18014398509481984.0

(* This function is IEEE double-precision specific *)
fun logb(x) = 
    case Assembly.A.logb x
     of ~1023 => (* denormalized number *)
	         InLine.-(Assembly.A.logb(x*two_to_the_54),54)
      | i => i

val negone = ~1.0
val zero = 0.0
val half = 0.5
val one = 1.0
val two = 2.0
val four = 4.0

val ~ : real -> real = fn x => InLine.fsub(zero,x)

fun copysign(a,b) =
      case (a<zero,b<zero) of
	      (true,true) => a
	    | (false,false) => a
	    | _ => ~a

fun abs x = if x < zero then ~x else x
fun op mod(a,b) = InLine.-(a,InLine.*(InLine.div(a,b),b))

val floor = Assembly.A.floor
fun truncate n = if n < 0.0 then InLine.~(floor(~n)) else floor n
fun ceiling n = InLine.~(floor(~n))

local fun loop 0 = zero
        | loop n = let val x = two * loop(InLine.rshift(n,1))
		   in case InLine.andb(n,1) of 0 => x | 1 => x + one
		   end
in fun real n = if InLine.<(n,0) then ~(loop(InLine.~ n)) else loop n
end

val maxint = 4503599627370496.0 (* This is the IEEE double-precision
				   maxint; won't work accurately on VAX *)

fun realround x = if x>=0.0 then x+maxint-maxint else x-maxint+maxint
(* realround(x) returns x rounded to some nearby integer, almost always
   the nearest integer. *)

(* sin/cos *)
local
    val S0 = ~1.6666666666666463126E~1
    val S1 =  8.3333333332992771264E~3
    val S2 = ~1.9841269816180999116E~4
    val S3 =  2.7557309793219876880E~6
    val S4 = ~2.5050225177523807003E~8
    val S5 =  1.5868926979889205164E~10
in  fun sin__S z = (z*(S0+z*(S1+z*(S2+z*(S3+z*(S4+z*S5))))))
end

local
    val C0 =  4.1666666666666504759E~2
    val C1 = ~1.3888888888865301516E~3
    val C2 =  2.4801587269650015769E~5
    val C3 = ~2.7557304623183959811E~7
    val C4 =  2.0873958177697780076E~9
    val C5 = ~1.1250289076471311557E~11
in  fun cos__C z = (z*z*(C0+z*(C1+z*(C2+z*(C3+z*(C4+z*C5))))))
end

val PIo4   =  7.8539816339744827900E~1
val PIo2   =  1.5707963267948965580E0
val PI3o4  =  2.3561944901923448370E0
val PI     =  3.1415926535897931160E0
val PI2    =  6.2831853071795862320E0
val oneOver2Pi = 0.15915494309189533576888376337251486

local
    val thresh =  2.6117239648121182150E~1
in  fun S y = y + y * sin__S(y*y)
    fun C y =
	let val yy = y*y
	    val c = cos__C yy
	    val Y = yy/two
	in  if Y < thresh then one - (Y - c)
	    else half - (Y - half - c)
	end
end
    fun sin x =
	let val xover2pi = x * oneOver2Pi
	    val x = PI2*(xover2pi - realround(xover2pi))
	       (* now, probably,  ~pi <= x <= pi, except on vaxes *)
	    fun lessPIo2 x = if x>PIo4 then C(PIo2-x) else S x
	    fun lessPI x = if x>PIo2 then lessPIo2(PI-x) else lessPIo2 x
	    fun positive x = if x>PI then sin(x-PI2) (* exceedingly rare *)
			             else lessPI x
	 in if x>=0.0 
		then positive x
	        else ~(positive(~x))
	end

    fun cos x = sin(PIo2-x)

local
    val p1 =  1.3887401997267371720E~2
    val p2 =  3.3044019718331897649E~5
    val q1 =  1.1110813732786649355E~1
    val q2 =  9.9176615021572857300E~4
in  fun exp__E(x:real,c:real) =
	let val z = x*x
	    val p = z*(p1+z*p2)
	    val q = z*(q1+z*q2)
	    val xp= x*p 
	    val xh= x*half
	    val w = xh-(q-xp)
	    val c = c+x*((xh*w-(q-(p+p+xp)))/(one-w)+c)
	in  z*half+c
	end
end

(* for exp and ln *)
val ln2hi = 6.9314718036912381649E~1
val ln2lo = 1.9082149292705877000E~10
val sqrt2 = 1.4142135623730951455E0
val lnhuge =  7.1602103751842355450E2
val lntiny = ~7.5137154372698068983E2
val invln2 =  1.4426950408889633870E0

fun exp(x:real) =
    let fun exp_norm x =
	    let (* argument reduction : x --> x - k*ln2 *)
		val k = floor(invln2*x+copysign(half,x)) (* k=NINT(x/ln2) *)
		val K = real k
		(* express x-k*ln2 as z+c *)
		val hi = x-K*ln2hi
		val lo = K*ln2lo
		val z = hi - lo
		val c = (hi-z)-lo
		(* return 2^k*[expm1(x) + 1] *)
		val z = z + exp__E(z,c)
	    in  scalb(z+one,k)
	    end
    in	if x <= lnhuge 
	     then if x >= lntiny
		    then exp_norm x
		    else zero
	     else raise Overflow
    end

local
    val L1 = 6.6666666666667340202E~1
    val L2 = 3.9999999999416702146E~1
    val L3 = 2.8571428742008753154E~1
    val L4 = 2.2222198607186277597E~1
    val L5 = 1.8183562745289935658E~1
    val L6 = 1.5314087275331442206E~1
    val L7 = 1.4795612545334174692E~1
in  fun log__L(z) = z*(L1+z*(L2+z*(L3+z*(L4+z*(L5+z*(L6+z*L7))))))
end

fun ln(x:real) =
      if x <= zero then raise Ln
	else let val k = logb(x)
		 val x = scalb(x,InLine.~ k)
		 val (x,k) = if ieql(k,~1022) (* subnormal no. *)
			 then let val n = logb(x)
			      in  (scalb(x,InLine.~ n),InLine.+(k,n)) end
			 else (x,k)
		 val (k,x) = if x >= sqrt2 then (InLine.+(k,1),x*half) 
					   else (k,x)
		 val K = real k
		 val x = x - one
		 (* compute log(1+x) *)
		 val s = x/(two+x)
		 val t = x*x*half
		 val z = K*ln2lo+s*(t+log__L(s*s))
		 val x = x + (z - t)
	     in  K*ln2hi+x 
	    end

local
    val athfhi =  4.6364760900080611433E~1
    val athflo =  1.0147340032515978826E~18
    val at1hi =   0.78539816339744830676
    val at1lo =   1.11258708870781088040E~18
    val a1     =  3.3333333333333942106E~1
    val a2     = ~1.9999999999979536924E~1
    val a3     =  1.4285714278004377209E~1
    val a4     = ~1.1111110579344973814E~1
    val a5     =  9.0908906105474668324E~2
    val a6     = ~7.6919217767468239799E~2
    val a7     =  6.6614695906082474486E~2
    val a8     = ~5.8358371008508623523E~2
    val a9     =  4.9850617156082015213E~2
    val a10    = ~3.6700606902093604877E~2
    val a11    =  1.6438029044759730479E~2

    fun atn(t,hi,lo) = (* for ~0.4375 <= t <= 0.4375 *)
		   let val z = t*t
		    in hi+(t+(lo-t*(z*(a1+z*(a2+z*(a3+z*(a4+z*(a5+z*(a6+z*(a7+
				z*(a8+z*(a9+z*(a10+z*a11)))))))))))))
		   end

    fun atan(t) = (* 0 <= t <= 1 *)
        if t <= 0.4375 then atn(t,zero,zero)
	 else if t <= 0.6875 then atn((t-half)/(one+half*t), athfhi, athflo)
	 else atn((t-one)/(one+t), at1hi,at1lo)

    fun atanpy y = (* y>=0 *)
	if y>one then PIo2 - atan(one/y) else atan(y)

in  fun arctan y = if y<=0.0 then ~(atanpy(~y)) else atanpy y
end

fun sqrt(x: real) =
      if x<=zero then if x<zero then raise Sqrt else x
      else 
        let val k = 6 (* log base 2 of the precision *)
	    val n = InLine.rshift(logb(x),1)
	    val x = scalb(x, InLine.~(InLine.lshift(n,1)))
	    fun iter(0,g) = g
              | iter(i,g) = iter(InLine.-(i,1), half * (g + x/g))
         in scalb(iter(k,one),n)
        end

end
