/* uni - uniform random number generator - Marsaglia's portable gen.   */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
# include "xmath.h"
# include "xlisp.h"

unsigned long time();

#define STATELENGTH 22
#define MDIG 32
#define M1 18
#define M2 19
#define I  20
#define J  21

/* external variables */
extern LVAL s_random_state, s_true;

/* external functions */
extern LVAL copyvector();

/* seed: seed, m[1] - m[17], m1, m2, i, j */

LVAL newrandomstate()
{
  LVAL state;
  unsigned long seed;
  long m1, m2, k0, k1, j0, j1;
  int i;
  
  xlsave1(state);
  state = newvector(STATELENGTH);
  
  /* set seed from clock */
  seed = time((unsigned long *) NULL);
  
  m1 = floor(pow(2.0, (MDIG - 2.0)) + .1)
     + floor(pow(2.0, (MDIG - 2.0)) + .1) - 1;
  m2 = floor(pow(2.0, (MDIG / 2.0)) + .1);

  setelement(state, 0, cvfixnum((FIXTYPE) seed));

  setelement(state, M1, cvfixnum((FIXTYPE) m1));
  setelement(state, M2, cvfixnum((FIXTYPE) m2));
  setelement(state,  I,  cvfixnum((FIXTYPE) 5));
  setelement(state,  J,  cvfixnum((FIXTYPE) 17));

  if (seed % 2 == 0) seed--;
  k0 = 9069 % m2;
  k1 = 9069 / m2;
  j0 = seed % m2;
  j1 = seed / m2;
  for (i = 1; i <= 17; i++) {
    seed = j0 * k0;
    j1 = ((seed / m2) + j0 * k1 + j1 * k0) % (m2 / 2);
    j0 = seed % m2;
    setelement(state, i, cvfixnum((FIXTYPE) j0 + m2 * j1));
  }
  xlpop();
  return(state);
}

static double uni1()
{
  LVAL state;
  long k, i, j, m1;

  state = getvalue(s_random_state);
  checkstate(state);
  
  m1 = getfixnum(getelement(state, M1));
  i =  getfixnum(getelement(state,  I));
  j =  getfixnum(getelement(state,  J));

  k =  getfixnum(getelement(state, i)) - getfixnum(getelement(state, j));
  if (k < 0) k = k + m1;
  setelement(state, j, cvfixnum((FIXTYPE) k));
  i = i - 1;
  if (i <= 0) i = 17;
  j = j - 1;
  if (j <= 0) j = 17;
  setelement(state, I, cvfixnum((FIXTYPE) i));
  setelement(state, J, cvfixnum((FIXTYPE) j));
  
  return(((double) k) / m1);
}

double uni()
{
  double x;
  do { x = uni1(); } while (x <= 0.0 || x >= 1.0);
  return (x);
}

int osrand(n)
  unsigned n;
{
  int k;
  
  do {
    k = n * uni();
  } while (k < 0 || k >= n);
  return(k);
}

LVAL xsmake_random_state()
{
  LVAL arg = NIL;
  
  if (moreargs()) arg = xlgetarg();
  xllastarg();
  
  if (arg == NIL) return(copyvector(getvalue(s_random_state)));
  else if (arg == s_true) return(newrandomstate());
  else {
    checkstate(arg);
    return(copyvector(arg));
  }
}

LVAL xsrandom_state_p()
{
  LVAL state;
  
  state = xlgetarg();
  return((random_state_p(state)) ? s_true : NIL);
}

static random_state_p(state)
	LVAL state;
{
  long i, j, m1;
  
  if (! vectorp(state) || getsize(state) != STATELENGTH)
    return(FALSE);
    
  m1 = getfixnum(getelement(state, M1));
  i =  getfixnum(getelement(state,  I));
  j =  getfixnum(getelement(state,  J));

  if (m1 <= 0 || i <= 0 || i > 17 || j <= 0 || j > 17)
    return(FALSE);

  return(TRUE);
}

static checkstate(state)
	LVAL state;
{
  if (! random_state_p(state)) xlerror("bad random state", state);
}
