/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include <sys/types.h>

unsigned long rhypot(x,y)
unsigned long x,y;
{
    register u_long square,m,step=0x4000;
    register u_long r=0x8000;
    register u_long off=0x10000000;
    register u_long s=0x40000000;
    register u_long flag=0;
    
    if((x|y)& 0xffff8000) {
	x>>=2;
	y>>=2;
	flag=1;
    }
    square=x*x+y*y;

    for(m=0xf;m;m--) {
	if(s<square) {
	    s+=r<<m;
	    r+=step;
	} else {
	    s-=r<<m;
	    r-=step;
	}
	step>>=1;
	s+=off;
	off>>=2;
    }
    
    if(flag) r <<= 2;
    
    return r;
}


