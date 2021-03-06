/*
 *      math.c          logo math functions module              dvb
 *
 *	Copyright (C) 1993 by the Regents of the University of California
 *
 *      This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2 of the License, or
 *      (at your option) any later version.
 *  
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *  
 *      You should have received a copy of the GNU General Public License
 *      along with this program; if not, write to the Free Software
 *      Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include "logo.h"
#include "globals.h"
#include <signal.h>
#include <setjmp.h>
#include <math.h>

#define isdigit(dig)    (dig >= '0' && dig <= '9')

int numberp(NODE *snd)
{
    int dl,dr, pcnt, plen;
    char *p;

    if (is_number(snd)) return(1);

    snd = cnv_node_to_strnode(snd);
    if (snd == UNBOUND) return(0);

    p = getstrptr(snd); plen = getstrlen(snd); pcnt = dl = dr = 0;
    if (plen >= MAX_NUMBER) {
	return(0);
    }

    if (pcnt < plen && *p == '-')
	p++, pcnt++;

    while (pcnt < plen && isdigit(*p))
	p++, pcnt++, dl++;

    if (pcnt < plen && *p == '.') {
	p++, pcnt++;
	while (pcnt < plen && isdigit(*p))
	    p++, pcnt++, dr++;
    }

    if (pcnt < plen && (dl || dr) && (*p == 'E' || *p == 'e')) {
	p++, pcnt++;

	if (pcnt < plen && *p == '+' || *p == '-')
	    p++, pcnt++;

	while (pcnt < plen && isdigit(*p))
	    p++, pcnt++, dr++;
    }

    if ((dl == 0 && dr == 0) || pcnt != plen)
	return (0);
    else
	return (dr + 1);
}

NODE *lrandom(NODE *arg)
{
	NODE *val;
	long r;

	val = pos_int_arg(arg);
	if (NOT_THROWING) {
#ifdef bsd
		r = (getint(val) == 0 ? 0 : random() % getint(val));
#else
		r = (getint(val) == 0 ? 0 : rand() % getint(val));
#endif
		val = newnode(INT);
		setint(val, (FIXNUM)r);
		return(val);
	} else return(UNBOUND);
}

NODE *lrerandom(NODE *arg)
{
	int seed=1;

	if (arg != NIL) {
		seed = int_arg(arg);
	}
	if (NOT_THROWING) {
#ifdef bsd
		srandom((int)seed);
#else
		srand((int)seed);
#endif
	}
	return(UNBOUND);
}

jmp_buf oflo_buf;

#ifdef __ZTC__
#define sig_arg 0
void handle_oflo(int sig) {
#else
#define sig_arg 
void handle_oflo() {
#endif
    longjmp(oflo_buf,1);
}

#ifdef vax
void allow_intov() {
    long dummy;
    register long *p = &dummy;

    p[2] |= 040;    /* Turn on IV enable in saved PSW (I hate the vax) */
}

double infnan() {
    longjmp(oflo_buf,1);
}
#endif

#ifdef sun
int matherr(struct exception *x)
{
    if (x->type == UNDERFLOW) return(1);
    longjmp(oflo_buf,1);
}
#endif

#ifdef mac
FLONUM degrad = 0.017453292520;
#else
FLONUM degrad = 3.141592653589793227020265931059839203954/180.0;
#endif

NODE *binary(NODE *args, char fcn)
{
    NODE *arg, *val;
    BOOLEAN imode;
    FIXNUM iarg, ival, oval, nval;
    FLONUM farg, fval;
    int sign, wantint=0;

    arg = numeric_arg(args);
    args = cdr(args);
    if (stopping_flag == THROWING) return UNBOUND;
    if (nodetype(arg) == INT) {
	imode = TRUE;
	ival = getint(arg);
    } else {
	imode = FALSE;
	fval = getfloat(arg);
    }
    if (args == NIL) {    /* one argument supplied */
      if (imode)
	switch(fcn) {
	  case '-': ival = -ival; break;
	  case '~': ival = ~ival; break;
	  case 's':
	  case 'c':
	  case 't':
	  case 'S':
	  case 'C':
	  case 'T':
	  case 'q':
	  case 'e':
	  case 'g':
	  case 'n':
	  case '/':
	    imode = FALSE;
	    fval = (FLONUM)ival;
	    break;
	}
      if (imode == FALSE) {
       if (!setjmp(oflo_buf)) {
	switch(fcn) {
	  case '-': fval = -fval; break;
	  case '/':
	    if (fval == 0.0)
		err_logo(BAD_DATA_UNREC,arg);
	    else
		fval = 1/fval;
	    break;
	  case '~': err_logo(BAD_DATA_UNREC,arg); break;
	  case 'c':
	    fval = 90.0 - fval;
	  case 's':
	    /* Kahan sez we can't just multiply any old
	     * angle by degrad, but have to get into the
	     * range 0-45 first */
	    sign = (fval < 0.0);
	    if (sign) fval = -fval;
#ifndef unix
	    fval = fmod(fval,360.0);
#else
	    fval = drem(fval,360.0);
#endif
	    if (fval > 180.0) {
		fval -= 180.0;
		sign = !sign;
	    }
	    if (fval > 90.0) fval = 180.0 - fval;
	    if (fval > 45.0)
		fval = cos((90.0-fval)*degrad);
	    else
		fval = sin(fval*degrad);
	    if (sign) fval = -fval;
	    break;
	  case 't': fval = atan(fval)/degrad; break;
	  case 'S': fval = sin(fval); break;
	  case 'C': fval = cos(fval); break;
	  case 'T': fval = atan(fval); break;
	  case 'q': fval = sqrt(fval); break;
	  case 'e': fval = exp(fval); break;
	  case 'g': fval = log10(fval); break;
	  case 'n': fval = log(fval); break;
	  case 'r':
	    fval += (fval < 0 ? -0.5 : 0.5);
	  case 'i':
#ifdef vax
	    allow_intov();
#else
	    if (fval > (FLONUM)MAXINT ||
		    fval < -(FLONUM)MAXINT)
		handle_oflo(sig_arg);
#endif
	    signal(SIGFPE, handle_oflo);
	    ival = (FIXNUM)fval;
	    imode = TRUE;
	    signal(SIGFPE, SIG_DFL);
	    break;
	}
       } else {	/* overflow */
	    if (fcn == 'r' || fcn == 'i') {
	      if (fval < 0.0)
		fval = ceil(fval);
	      else
		fval = floor(fval);
	    } else
		err_logo(BAD_DATA_UNREC,arg);
       }
      }	    /* end float case */
    }	    /* end monadic */
    while (args != NIL && NOT_THROWING) {
	arg = numeric_arg(args);
	args = cdr(args);
	if (stopping_flag == THROWING) return UNBOUND;

	if (nodetype(arg) == INT) {
	    if (imode) iarg = getint(arg);
	    else farg = (FLONUM)getint(arg);
	} else {
	    if (imode) {
		fval = (FLONUM)ival;
		imode = FALSE;
	    }
	    farg = getfloat(arg);
	}

	if (imode) {
	    oval = ival;
#ifdef vax
	    allow_intov();
#endif
	    signal(SIGFPE, handle_oflo);
	    if (setjmp(oflo_buf) == 0) {
	     switch(fcn) {
#ifdef vax
	      case '+': ival += iarg; break;
	      case '-': ival -= iarg; break;
	      case '*': ival *= iarg; break;
#else
	      case '-': iarg = -iarg;
	      case '+':
		if (iarg < 0) {
		    nval = ival + iarg;
		    if (nval >= ival)
			handle_oflo(sig_arg);
		    else ival = nval;
		} else {
		    nval = ival + iarg;
		    if (nval < ival)
			handle_oflo(sig_arg);
		    else ival = nval;
		}
		break;
#endif
	      case '/':
		if (iarg == 0)
		  err_logo(BAD_DATA_UNREC,arg);
		else
		  if (ival % iarg != 0) {
		    imode = FALSE;
		    fval = (FLONUM)ival;
		    farg = (FLONUM)iarg;
		  }
		  else ival /= iarg;
		  break;
	      case '%':
		ival %= iarg;
		if ((ival < 0) != (iarg < 0))
		    ival += iarg;
		break;
	      case '&': ival &= iarg; break;
	      case '|': ival |= iarg; break;
	      case '^': ival ^= iarg; break;
	      case 'a':
	      case 'l':
		if (iarg < 0) {
		  if (fcn == 'a')
		    ival >>= -iarg;
		  else
		    ival = (unsigned)ival
			>> -iarg;
		} else
		  ival <<= iarg;
		break;
#ifndef vax
	      case '*':
		if (ival < SAFEINT && ival > -SAFEINT &&
		    iarg < SAFEINT && iarg > -SAFEINT) {
		    ival *= iarg;
		    break;
		}
		wantint++;
#endif
	      default: /* math library */
		imode = FALSE;
		fval = (FLONUM)ival;
		farg = (FLONUM)iarg;
	     }
	    } else {    /* integer overflow detected */
		imode = FALSE;
		fval = (FLONUM)oval;
		farg = (FLONUM)iarg;
	    }
	    signal(SIGFPE,SIG_DFL);
	}
	if (imode == FALSE) {
	  signal(SIGFPE,handle_oflo);
	  if (setjmp(oflo_buf) == 0) {
	    switch(fcn) {
	      case '+': fval += farg; break;
	      case '-': fval -= farg; break;
	      case '*':
		fval *= farg;
#ifndef vax
		if (wantint) {
		    wantint = 0;
		    if (fval <= MAXINT && fval >= -MAXINT) {
			imode = TRUE;
			ival = fval;
		    }
		}
#endif
		break;
	      case '/': if (farg == 0.0)
		      err_logo(BAD_DATA_UNREC,arg);
		    else
		      fval /= farg;
		    break;
	      case 't':
		fval = atan2(farg,fval)/degrad;
		break;
	      case 'T':
		fval = atan2(farg,fval);
		break;
	      case 'p':
		fval = pow(fval,farg);
		break;
	      default: /* logical op */
		if (nodetype(arg) == INT)
		  err_logo(BAD_DATA_UNREC, make_floatnode(fval));
		else
		  err_logo(BAD_DATA_UNREC,arg);
	    }
	  } else {    /* floating overflow detected */
	    err_logo(BAD_DATA_UNREC,arg);
	  }
	  signal(SIGFPE,SIG_DFL);
	}    /* end floating point */
    }	/* end dyadic */
    if (NOT_THROWING) {
	if (imode) {
	    val = newnode(INT);
	    setint(val, ival);
	} else {
	    val = newnode(FLOAT);
	    setfloat(val, fval);
	}
	return(val);
    }
    return(UNBOUND);
}

NODE *ladd(NODE *args)
{
    if (args == NIL) return make_intnode(0);
    return(binary(args, '+'));
}

NODE *lsub(NODE *args)
{
    return(binary(args, '-'));
}

NODE *lmul(NODE *args)
{
    if (args == NIL) return make_intnode(1);
    return(binary(args, '*'));
}

NODE *ldivide(NODE *args)
{
    return(binary(args, '/'));
}

NODE *lremainder(NODE *args)
{
    return(binary(args, '%'));
}

NODE *lbitand(NODE *args)
{
    if (args == NIL) return make_intnode(-1);
    return(binary(args, '&'));
}

NODE *lbitor(NODE *args)
{
    if (args == NIL) return make_intnode(0);
    return(binary(args, '|'));
}

NODE *lbitxor(NODE *args)
{
    if (args == NIL) return make_intnode(0);
    return(binary(args, '^'));
}

NODE *lashift(NODE *args)
{
    return(binary(args, 'a'));
}

NODE *llshift(NODE *args)
{
    return(binary(args, 'l'));
}

NODE *lbitnot(NODE *args)
{
    return(binary(args, '~'));
}

NODE *lsin(NODE *args)
{
    return(binary(args, 's'));
}

NODE *lcos(NODE *args)
{
    return(binary(args, 'c'));
}

NODE *latan(NODE *args)
{
    return(binary(args, 't'));
}

NODE *lradsin(NODE *args)
{
    return(binary(args, 'S'));
}

NODE *lradcos(NODE *args)
{
    return(binary(args, 'C'));
}

NODE *lradatan(NODE *args)
{
    return(binary(args, 'T'));
}

NODE *lsqrt(NODE *args)
{
    return(binary(args, 'q'));
}

NODE *linteg(NODE *args)
{
    return(binary(args, 'i'));
}

NODE *lround(NODE *args)
{
    return(binary(args, 'r'));
}

NODE *lexp(NODE *args)
{
    return(binary(args, 'e'));
}

NODE *llog10(NODE *args)
{
    return(binary(args, 'g'));
}

NODE *lln(NODE *args)
{
    return(binary(args, 'n'));
}

NODE *lpower(NODE *args)
{
    return(binary(args, 'p'));
}

int compare_numnodes(NODE *n1, NODE *n2)
{
    FLONUM f;
    FIXNUM i;

    if (nodetype(n1) == INT) {
	if (nodetype(n2) == INT) {
	    i = getint(n1) - getint(n2);
	    return (i == 0L ? 0 : (i > 0L ? 1 : -1));
	} else {
	    f = (FLONUM)getint(n1) - getfloat(n2);
	    return(f == 0.0 ? 0 : (f > 0.0 ? 1 : -1));
	}
    }
    else {
	if (nodetype(n2) == INT) {
	    f = getfloat(n1) - (FLONUM)getint(n2);
	    return(f == 0.0 ? 0 : (f > 0.0 ? 1 : -1));
	}
	else {
	    f = getfloat(n1) - getfloat(n2);
	    return(f == 0.0 ? 0 : (f > 0.0 ? 1 : -1));
	}
    }
}

NODE *torf(BOOLEAN tf) {
    return (tf ? True : False);
}

NODE *llessp(NODE *args)
{
    NODE *n1, *n2;

    n1 = numeric_arg(args);
    n2 = numeric_arg(cdr(args));

    if (NOT_THROWING) {
	return torf(compare_numnodes(n1, n2) < 0);
    }
    return(UNBOUND);
}

NODE *lgreaterp(NODE *args)
{
    NODE *n1, *n2;

    n1 = numeric_arg(args);
    n2 = numeric_arg(cdr(args));

    if (NOT_THROWING) {
	return torf(compare_numnodes(n1, n2) > 0);
    }
    return(UNBOUND);
}

int compare_node(NODE *n1, NODE *n2, BOOLEAN ignorecase)
{
    NODE *a1 = NIL, *a2 = NIL, *nn1 = NIL, *nn2 = NIL;
    int icmp, cmp_len;
    NODETYPES nt1, nt2;

    if (n1 == n2) return 0;

    nt1 = nodetype(n1);
    nt2 = nodetype(n2);

    if (!(nt1 & NT_WORD) || !(nt2 & NT_WORD)) return -9999;

    if (nt1 == CASEOBJ && nt2 == CASEOBJ && ignorecase &&
	 (object__caseobj(n1) == object__caseobj(n2))) return 0;

    if ((nt1 & NT_NUMBER) && (nt2 & NT_NUMBER))
	return compare_numnodes(n1, n2);

    if (nt1 & NT_NUMBER) {
	nn2 = cnv_node_to_numnode(n2);
	if (nn2 != UNBOUND) {
	    icmp = compare_numnodes(n1, nn2);
	    gcref(nn2);
	    return icmp;
	}
    }

    if (nt2 & NT_NUMBER) {
	nn1 = cnv_node_to_numnode(n1);
	if (nn1 != UNBOUND) {
	    icmp = compare_numnodes(nn1, n2);
	    gcref(nn1);
	    return icmp;
	}
    }

    a1 = cnv_node_to_strnode(n1);
    a2 = cnv_node_to_strnode(n2);
    nt1 = nodetype(a1);
    nt2 = nodetype(a2);
    if (nt1 == STRING && nt2 == STRING) {
	if ((getstrlen(a1) == getstrlen(a2)) &&
		    (getstrptr(a1) == getstrptr(a2)))
	    icmp = 0;
	else {
	cmp_len = (getstrlen(a1) > getstrlen(a2)) ?
		getstrlen(a2) : getstrlen(a1);

	if (ignorecase)
	    icmp = low_strncmp(getstrptr(a1), getstrptr(a2), cmp_len);
	else
	    icmp = strncmp(getstrptr(a1), getstrptr(a2), cmp_len);
	if ((getstrlen(a1) != getstrlen(a2)) && icmp == 0)
	    icmp = getstrlen(a1) - getstrlen(a2);
	}
    }
    else if (nt1 & NT_BACKSL || nt2 & NT_BACKSL) {
	if ((getstrlen(a1) == getstrlen(a2)) &&
			(getstrptr(a1) == getstrptr(a2)))
	    icmp = 0;
	else {
	cmp_len = (getstrlen(a1) > getstrlen(a2)) ?
		    getstrlen(a2) : getstrlen(a1);

	if (ignorecase)
	    icmp = noparitylow_strncmp(getstrptr(a1), getstrptr(a2), cmp_len);
	else
	    icmp = noparity_strncmp(getstrptr(a1), getstrptr(a2), cmp_len);
	if ((getstrlen(a1) != getstrlen(a2)) && icmp == 0)
	    icmp = getstrlen(a1) - getstrlen(a2);
	}
    }
    else err_logo(FATAL, NIL);
 
    if (a1 != n1) gcref(a1);
    if (a2 != n2) gcref(a2);
    return(icmp);
}

BOOLEAN equalp_help(NODE *arg1, NODE *arg2, BOOLEAN ingc)
{
    if (is_list(arg1)) {
	if (!is_list(arg2)) return FALSE;
	while (arg1 != NIL && arg2 != NIL) {
	    if (!equalp_help(car(arg1), car(arg2), ingc))
		return FALSE;
	    arg1 = cdr(arg1);
	    arg2 = cdr(arg2);
	    if (check_throwing) break;
	}
	return (arg1 == NIL && arg2 == NIL);
    } else if (is_list(arg2))
	return FALSE;
    else if (nodetype(arg1) == ARRAY) {
	if (nodetype(arg2) != ARRAY) return FALSE;
	return (arg1 == arg2);
    } else if (nodetype(arg2) == ARRAY)
	return FALSE;
    else return (!compare_node(arg1, arg2, ingc));
}

NODE *lequalp(NODE *args)
{
    NODE *arg1, *arg2;
    BOOLEAN val;

    arg1 = car(args);
    arg2 = cadr(args);

    if (compare_node(valnode__caseobj(Caseignoredp), True, TRUE) == 0)
	val = equalp_help(arg1, arg2, TRUE);
    else
	val = equalp_help(arg1, arg2, FALSE);

    return(torf(val));
}

NODE *l_eq(NODE *args)
{
    return torf(car(args) == cadr(args));
}

NODE *lbeforep(NODE *args)
{
    NODE *arg1, *arg2;
    int val;

    arg1 = string_arg(args);
    arg2 = string_arg(cdr(args));

    if (compare_node(valnode__caseobj(Caseignoredp), True, TRUE) == 0)
	val = compare_node(arg1, arg2, TRUE);
    else
	val = compare_node(arg1, arg2, FALSE);

    return (val < 0 ? True : False);
}
