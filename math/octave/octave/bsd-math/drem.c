/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)support.c	5.6 (Berkeley) 10/9/90";
#endif /* not lint */

/* 
 * Some IEEE standard 754 recommended functions and remainder and sqrt for 
 * supporting the C elementary functions.
 ******************************************************************************
 * WARNING:
 *      These codes are developed (in double) to support the C elementary
 * functions temporarily. They are not universal, and some of them are very
 * slow (in particular, drem and sqrt is extremely inefficient). Each 
 * computer system should have its implementation of these functions using 
 * its own assembler.
 ******************************************************************************
 *
 * IEEE 754 required operations:
 *     drem(x,p) 
 *              returns  x REM y  =  x - [x/y]*y , where [x/y] is the integer
 *              nearest x/y; in half way case, choose the even one.
 *     sqrt(x) 
 *              returns the square root of x correctly rounded according to 
 *		the rounding mod.
 *
 * IEEE 754 recommended functions:
 * (a) copysign(x,y) 
 *              returns x with the sign of y. 
 * (b) scalb(x,N) 
 *              returns  x * (2**N), for integer values N.
 * (c) logb(x) 
 *              returns the unbiased exponent of x, a signed integer in 
 *              double precision, except that logb(0) is -INF, logb(INF) 
 *              is +INF, and logb(NAN) is that NAN.
 * (d) finite(x) 
 *              returns the value TRUE if -INF < x < +INF and returns 
 *              FALSE otherwise.
 *
 *
 * CODED IN C BY K.C. NG, 11/25/84;
 * REVISED BY K.C. NG on 1/22/85, 2/13/85, 3/24/85.
 */

#include "mathimpl.h"

#if defined(vax)||defined(tahoe)      /* VAX D format */
#include <errno.h>
    static const unsigned short msign=0x7fff , mexp =0x7f80 ;
    static const short  prep1=57, gap=7, bias=129           ;   
    static const double novf=1.7E38, nunf=3.0E-39, zero=0.0 ;
#else	/* defined(vax)||defined(tahoe) */
    static const unsigned short msign=0x7fff, mexp =0x7ff0  ;
    static const short prep1=54, gap=4, bias=1023           ;
    static const double novf=1.7E308, nunf=3.0E-308,zero=0.0;
#endif	/* defined(vax)||defined(tahoe) */

double drem(x,p)
double x,p;
{
        short sign;
        double hp,dp,tmp;
        unsigned short  k; 
#ifdef national
        unsigned short
              *px=(unsigned short *) &x  +3, 
              *pp=(unsigned short *) &p  +3,
              *pd=(unsigned short *) &dp +3,
              *pt=(unsigned short *) &tmp+3;
#else	/* national */
        unsigned short
              *px=(unsigned short *) &x  , 
              *pp=(unsigned short *) &p  ,
              *pd=(unsigned short *) &dp ,
              *pt=(unsigned short *) &tmp;
#endif	/* national */

        *pp &= msign ;

#if defined(vax)||defined(tahoe)
        if( ( *px & mexp ) == ~msign )	/* is x a reserved operand? */
#else	/* defined(vax)||defined(tahoe) */
        if( ( *px & mexp ) == mexp )
#endif	/* defined(vax)||defined(tahoe) */
		return  (x-p)-(x-p);	/* create nan if x is inf */
	if (p == zero) {
#if defined(vax)||defined(tahoe)
		return(infnan(EDOM));
#else	/* defined(vax)||defined(tahoe) */
		return zero/zero;
#endif	/* defined(vax)||defined(tahoe) */
	}

#if defined(vax)||defined(tahoe)
        if( ( *pp & mexp ) == ~msign )	/* is p a reserved operand? */
#else	/* defined(vax)||defined(tahoe) */
        if( ( *pp & mexp ) == mexp )
#endif	/* defined(vax)||defined(tahoe) */
		{ if (p != p) return p; else return x;}

        else  if ( ((*pp & mexp)>>gap) <= 1 ) 
                /* subnormal p, or almost subnormal p */
            { double b; b=scalb(1.0,(int)prep1);
              p *= b; x = drem(x,p); x *= b; return(drem(x,p)/b);}
        else  if ( p >= novf/2)
            { p /= 2 ; x /= 2; return(drem(x,p)*2);}
        else 
            {
                dp=p+p; hp=p/2;
                sign= *px & ~msign ;
                *px &= msign       ;
                while ( x > dp )
                    {
                        k=(*px & mexp) - (*pd & mexp) ;
                        tmp = dp ;
                        *pt += k ;

#if defined(vax)||defined(tahoe)
                        if( x < tmp ) *pt -= 128 ;
#else	/* defined(vax)||defined(tahoe) */
                        if( x < tmp ) *pt -= 16 ;
#endif	/* defined(vax)||defined(tahoe) */

                        x -= tmp ;
                    }
                if ( x > hp )
                    { x -= p ;  if ( x >= hp ) x -= p ; }

#if defined(vax)||defined(tahoe)
		if (x)
#endif	/* defined(vax)||defined(tahoe) */
			*px ^= sign;
                return( x);

            }
}
