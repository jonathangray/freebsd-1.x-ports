/* Scheme implementation intended for JACAL.
   Copyright (C) 1992, 1993 Aubrey Jaffer.

The set of uniform vector types is:
 Vector of:		 Called:
char			string
boolean			bvect
signed int		ivect
unsigned int		uvect
float			fvect
double			dvect
complex double		cvect
*/

#include "scm.h"

long tc16_array;
SCM aref();
SCM uve_fill();
SCM array_prot();

void uvprin1(exp,port,writing)
     SCM exp;
     SCM port;
     int writing;
{
#ifdef FLOATS
  SCM z;
#endif
  register long i, j, w;
  if (TYP7(exp) == tc7_bvect) {
    lputs("#*",port);
    for(i = 0;i<(LENGTH(exp))/LONG_BIT;i++) {
      w = VELTS(exp)[i];
      for(j = LONG_BIT;j;j--) {
	lputc(w&1?'1':'0',port);
	w >>= 1;
      }
    }
    j = LENGTH(exp)%LONG_BIT;
    if (j) {
      w = VELTS(exp)[LENGTH(exp)/LONG_BIT];
      for(;j;j--) {
	lputc(w&1?'1':'0',port);
	w >>= 1;
      }
    }
    return;
  }
  lputc('#',port);
  switch TYP7(exp) {
  case tc7_uvect:
    lputs("u(",port);
    goto ivect;
  case tc7_ivect:
    lputs("e(",port);
  ivect:
    i = VELTS(exp)[0];
    for (j = 1; j < LENGTH(exp); j++) {
      intprint(i,10,port);
      lputc(' ',port);
      i = VELTS(exp)[j];
    }
    if (j == LENGTH(exp)) intprint(i,10,port);
    break;
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect:
    lputs("s(",port);
/* fix by orre@nada.kth.se was z = aref(exp,INUM0,EOL); */
    z = makdbl(1.0, 0.0);
    FLO(z) = *((float *)VELTS(exp));
    for (j = 1; j < LENGTH(exp); j++) {
      floprint(z,port,writing);
      lputc(' ',port);
      FLO(z) = ((float *)VELTS(exp))[j];
    }
    if (j == LENGTH(exp)) floprint(z,port,writing);
    break;
#endif /*SINGLES*/
  case tc7_dvect:
    lputs("i(",port);
    z = makdbl(1.0/3.0, 0.0);
    REAL(z) = *((double *)VELTS(exp));
    for (j = 1; j < LENGTH(exp); j++) {
      floprint(z,port,writing);
      lputc(' ',port);
      REAL(z) = ((double *)VELTS(exp))[j];
    }
    if (j == LENGTH(exp)) floprint(z,port,writing);
    break;
  case tc7_cvect:
    lputs("c(",port);
    z = makdbl(1.0,1.0);
    REAL(z) = ((double *)VELTS(exp))[0];
    IMAG(z) = ((double *)VELTS(exp))[1];
    for (j = 2; j < 2*LENGTH(exp); j++) {
      floprint(z,port,writing);
      lputc(' ',port);
      REAL(z) = ((double *)VELTS(exp))[j++];
      IMAG(z) = ((double *)VELTS(exp))[j];
    }
    if (j == LENGTH(exp)) floprint(z,port,writing);
    break;
#endif /*FLOATS*/
  }
  lputc(')',port);
}

char s_make_uve[] = "make-uniform-vector";
SCM make_uve(k,prot)
SCM k,prot;
{
  SCM v;
  long i, type;
  ASSERT(INUMP(k),k,ARG1,s_make_uve);
  if (BOOL_T==prot) {
    i = sizeof(long)*((INUM(k)+LONG_BIT-1)/LONG_BIT);
    type = tc7_bvect;
  }
  else if ICHRP(prot) {
    i = sizeof(char)*INUM(k);
    type = tc7_string;
  }
  else if INUMP(prot) {
    i = sizeof(long)*INUM(k);
    if (INUM(prot)>0) type = tc7_uvect;
    else type = tc7_ivect;
  }
  else
#ifdef FLOATS
     if (IMP(prot) || !INEXP(prot))
#endif
       return make_vector(k,UNDEFINED); /* no special vector */
#ifdef FLOATS
# ifdef SINGLES
  else if SINGP(prot) {
    i = sizeof(float)*INUM(k);
    type = tc7_fvect;
  }
# endif
  else if (CPLXP(prot)) {
    i = 2*sizeof(double)*INUM(k);
    type = tc7_cvect;
  }
  else {
    i = sizeof(double)*INUM(k);
    type = tc7_dvect;
  }
#endif
  NEWCELL(v);
  DEFER_INTS;
  SETCHARS(v,must_malloc((i ? i : 1L),s_vector));
  SETLENGTH(v, INUM(k), type);
  ALLOW_INTS;
  return v;
}

SCM uve_equal(u,v)
SCM u;
SCM v;
{
  long len;
  register long k;
  if (TYP7(u) != TYP7(v)) return BOOL_F;
  if ((len = LENGTH(u)) != LENGTH(v)) return BOOL_F;
  switch (TYP7(u)) {
  default: return BOOL_F;
  case tc7_bvect:
    {
      register long *ue = (long *)VELTS(u);
      register long *ve = (long *)VELTS(v);
      unsigned long mask;
      for (k = 0; k < len/LONG_BIT; k++)
	if (ue[k] != ve[k]) return BOOL_F;
      mask = (1L << len%LONG_BIT) - 1;
      if (mask && (mask & ue[k]) !=  (mask & ve[k])) return BOOL_F;
      return BOOL_T;
    }
  case tc7_uvect:
  case tc7_ivect:
    {
      register SCM *ue = VELTS(u);
      register SCM *ve = VELTS(v);
      for (k = 0; k < len; k++)
	if (ue[k] != ve[k]) return BOOL_F;
      return BOOL_T;
    }
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect:
    {
      register float *ue = (float *)VELTS(u);
      register float *ve = (float *)VELTS(v);
      for (k = 0; k < len; k++)
	if (ue[k] != ve[k]) return BOOL_F;
      return BOOL_T;
    }
#endif /*SINGLES*/
  case tc7_cvect:
    len = 2*len;
  case tc7_dvect:
    {
      register double *ue = (double *)VELTS(u);
      register double *ve = (double *)VELTS(v);
      for (k = 0; k < len; k++)
	if (ue[k] != ve[k]) return BOOL_F;
      return BOOL_T;
    }
#endif /*FLOATS*/
  }
}

static char s_uve_len[] = "uniform-vector-length";
SCM uve_len(v)
     SCM v;
{
  ASRTGO(NIMP(v),badarg1);
  switch TYP7(v) {
  default: badarg1: wta(v,(char *)ARG1,s_uve_len);
  case tc7_bvect:
  case tc7_string:
  case tc7_uvect:
  case tc7_ivect:
  case tc7_fvect:
  case tc7_dvect:
  case tc7_cvect:
  case tc7_vector:
    return MAKINUM(LENGTH(v));
  }
}

SCM arrayp(v, prot)
     SCM v, prot;
{
  int nprot = UNBNDP(prot);
  if IMP(v) return BOOL_F;
 loop:
  switch TYP7(v) {
  case tc7_smob: if (!ARRAYP(v)) return BOOL_F;
    if (nprot) return BOOL_T;
    v = ARRAY_V(v);
    goto loop;
  case tc7_bvect: return nprot || prot==BOOL_T ? BOOL_T : BOOL_F;
  case tc7_string: return nprot || ICHRP(prot) ? BOOL_T : BOOL_F;
  case tc7_uvect: 
    return nprot || INUMP(prot) && INUM(prot)>0 ? BOOL_T : BOOL_F;
  case tc7_ivect: 
    return nprot || INUMP(prot) && INUM(prot)<=0 ? BOOL_T : BOOL_F;
# ifdef FLOATS
# ifdef SINGLES
  case tc7_fvect: return nprot || NIMP(prot) && SINGP(prot) ? BOOL_T : BOOL_F;
# endif
  case tc7_dvect: return nprot || NIMP(prot) && REALP(prot) ? BOOL_T : BOOL_F;
  case tc7_cvect: return nprot || NIMP(prot) && CPLXP(prot) ? BOOL_T : BOOL_F;
# endif
  case tc7_vector: return nprot || NULLP(prot) ? BOOL_T : BOOL_F;
  default:;
  }
  return BOOL_F;
}
SCM array_rank(ra)
     SCM ra;
{
  if IMP(ra) return INUM0;
  switch (TYP7(ra)) {
  default: return INUM0;
  case tc7_string: case tc7_vector:
  case tc7_uvect: case tc7_ivect: case tc7_fvect:
  case tc7_cvect: case tc7_dvect:
    return MAKINUM(1L);
  case tc7_smob:
    if ARRAYP(ra) return MAKINUM(ARRAY_NDIM(ra));
    return INUM0;
  }
}
static char s_array_dims[] = "array-dimensions";
SCM array_dims(ra)
     SCM ra;
{
  SCM res=EOL;
  sizet k;
  array_dim *s;
  if IMP(ra) return BOOL_F;
  switch (TYP7(ra)) {
  default: return BOOL_F;
  case tc7_string: case tc7_vector: case tc7_bvect:
  case tc7_uvect: case tc7_ivect: case tc7_fvect:
  case tc7_cvect: case tc7_dvect:
    return cons(MAKINUM(LENGTH(ra)),EOL);
  case tc7_smob:
    if (!ARRAYP(ra)) return BOOL_F;
    k = ARRAY_NDIM(ra);
    s = &(ARRAY_DIMS(ra)[k-1]);
    for (; k--; s--)
      res = cons(s->lbnd ? cons2(MAKINUM(s->lbnd),MAKINUM(s->ubnd),EOL) :
		 MAKINUM(1+(s->ubnd))
		 ,res);
    return res;
  }
}
static char s_bad_ind[] = "Bad array index";
static char s_aind[] = "array-indexer";
SCM aind(ra,ind,args)
     SCM ra, ind, args;
{
  register sizet pos,k;
  register long j;
  array_dim *s;
  ASSERT(NIMP(ra) && ARRAYP(ra), ra, ARG1, s_aind);
  k = ARRAY_NDIM(ra);
  s = ARRAY_DIMS(ra);
  pos = ARRAY_BASE(ra);
  while (!0) {
    ASSERT(INUMP(ind),ind,s_bad_ind,s_aind);
    j = INUM(ind);
    ASSERT(j >= (s->lbnd) && j <= (s->ubnd),
	   ind, OUTOFRANGE,s_aind);
    pos += (j - s->lbnd)*(s->inc);
    if (!(--k && NIMP(args))) break;
    ind = CAR(args); args = CDR(args); s++;
  }
  ASSERT(0==k && NULLP(args), args, WNA, s_aind);
  return MAKINUM(pos);
}

static char s_bad_spec[] = "Bad array dimension";

/* Increments will still need to be set. */
SCM shap2ra(args, what)
     SCM args;
     char *what;
{
  sizet ndim;
  array_dim *s;
  SCM ra, spec, sp;
  ndim = ilength(args);
  ASSERT(0 < ndim, args, s_bad_spec, what);
  NEWCELL(ra);
  DEFER_INTS;
  SETCDR(ra, must_malloc((long)(sizeof(array)+ndim*sizeof(array_dim)),
			 what));
  SETNUMDIGS(ra, ndim, tc16_array);
  ARRAY_BASE(ra) = 0;
  ARRAY_V(ra) = nullvect;
  ALLOW_INTS;
  s = ARRAY_DIMS(ra);
  for (; NIMP(args); s++, args = CDR(args)) {
    spec = CAR(args);
    if IMP(spec) {
      ASSERT(INUMP(spec)&&INUM(spec)>=0, spec, s_bad_spec, what);
      s->lbnd = 0;
      s->ubnd = INUM(spec) - 1;
      s->inc = 1;
    }
    else {
      ASSERT(CONSP(spec) && INUMP(CAR(spec)), spec, s_bad_spec, what);
      s->lbnd = INUM(CAR(spec));
      sp = CDR(spec);
      ASSERT(INUMP(CAR(sp)) && NULLP(CDR(sp)),
	     spec,s_bad_spec,what);
      s->ubnd = INUM(CAR(sp));
      s->inc = 1;
    }
  }
  return ra;
}

static char s_dims2ura[] = "dimensions->uniform-array";
SCM dims2ura(dims,prot,fill)
     SCM dims,prot,fill;
{
  sizet vlen = 1;
  long rlen = 1;
  array_dim *s;
  SCM ra;
  if INUMP(dims) return make_uve(dims,prot);
  ASSERT(NIMP(dims) && CONSP(dims), dims, ARG1, s_dims2ura);
  ra = shap2ra(dims, s_dims2ura);
  s = &(ARRAY_DIMS(ra)[ARRAY_NDIM(ra)-1]);
  for (; s >= ARRAY_DIMS(ra); s--) {
    s->inc = (rlen > 0 ? rlen : 0);
    rlen = (s->ubnd - s->lbnd + 1)*s->inc;
    vlen *= (s->ubnd - s->lbnd + 1);
  }
  ARRAY_V(ra) = make_uve((rlen > 0 ? MAKINUM(vlen) : INUM0), prot);
  if NNULLP(fill) {
    ASSERT(1==ilength(fill),fill,WNA,s_dims2ura);
    uve_fill(ARRAY_V(ra),CAR(fill));
  }
  s++;				/* s = ARRAY_DIMS(ra); */
  if (1==ARRAY_NDIM(ra))
    if (s->ubnd < s->lbnd || 1==s->inc && 0==s->lbnd
	&& LENGTH(ARRAY_V(ra))==1+s->ubnd) return ARRAY_V(ra);
  return ra;
}
  
char s_make_sh_array[] = "make-shared-array";
SCM make_sh_array(oldra, mapfunc, dims)
     SCM oldra;
     SCM mapfunc;
     SCM dims;
{
  SCM ra;
  SCM inds, indptr;
  SCM imap;
  sizet i;
  long old_min, new_min, old_max, new_max;
  array_dim *s;
  ASSERT(BOOL_T==procedurep(mapfunc), mapfunc, ARG2, s_make_sh_array);
  ASSERT(NIMP(oldra) && arrayp(oldra,UNDEFINED), oldra, ARG1, s_make_sh_array);
  ra = shap2ra(dims, s_make_sh_array);
  if (ARRAYP(oldra)) {
    ARRAY_V(ra) = ARRAY_V(oldra);
    old_min = old_max = ARRAY_BASE(oldra);
    for (s=ARRAY_DIMS(oldra); s < ARRAY_DIMS(oldra)+ARRAY_NDIM(oldra); s++) {
      if (s->inc > 0) 
	old_max += (s->ubnd - s->lbnd)*s->inc;
      else
	old_min += (s->ubnd - s->lbnd)*s->inc;
    }
  }
  else {
    ARRAY_V(ra) = oldra;
    old_min = 0;
    old_max = LENGTH(oldra) - 1;
  }
  inds = EOL;
  s = &(ARRAY_DIMS(ra)[ARRAY_NDIM(ra)-1]);
  for (; s >= ARRAY_DIMS(ra); s--) {
    inds = cons(MAKINUM(s->lbnd), inds);
    if (s->ubnd < s->lbnd) {
      if (1==ARRAY_NDIM(ra)) 
	ra = make_uve(INUM0,array_prot(ra));
      else 
	ARRAY_V(ra) = make_uve(INUM0,array_prot(ra));
      return ra;
    }
  }
  imap = apply(mapfunc, append(cons(inds,listofnull)), EOL);
  if ARRAYP(oldra) {
    ASSERT(NIMP(imap), mapfunc, ARG2, s_make_sh_array);
    imap = aind(oldra, CAR(imap), CDR(imap));
  }
  else {
    ASSERT(1==ilength(imap) && INUMP(CAR(imap)),
	   imap,s_bad_ind,s_make_sh_array);
    imap = CAR(imap);
  }
  ARRAY_BASE(ra) = i = new_min = new_max = INUM(imap);
  indptr = inds;
  s++;				/* s = ARRAY_DIMS(ra); */
  for (s=ARRAY_DIMS(ra); s < ARRAY_DIMS(ra)+ARRAY_NDIM(ra); s++) {
    if (s->ubnd > s->lbnd)
      CAR(indptr) = MAKINUM(INUM(CAR(indptr))+1);
    imap = apply(mapfunc, append(cons(inds,listofnull)), EOL);
    if ARRAYP(oldra) {
      ASSERT(NIMP(imap), mapfunc, ARG2, s_make_sh_array);
      imap = aind(oldra, CAR(imap), CDR(imap));
    }
    else {
      ASSERT(1==ilength(imap) && INUMP(CAR(imap)),
	     imap,s_bad_ind,s_make_sh_array);
      imap = CAR(imap);
    }
    s->inc = INUM(imap) - i;
    if (s->inc > 0)
      new_max += (s->ubnd - s->lbnd)*s->inc;
    else
      new_min += (s->ubnd - s->lbnd)*s->inc;
    i = INUM(imap);
    indptr = CDR(indptr);
  }
  ASSERT(old_min <= new_min && old_max >= new_max, UNDEFINED,
	 "mapping out of range", s_make_sh_array);
  --s;
  if (1==ARRAY_NDIM(ra)) {
    if (1==s->inc && 0==s->lbnd
	&& LENGTH(ARRAY_V(ra))==1+s->ubnd) return ARRAY_V(ra);
    if (s->ubnd < s->lbnd) return make_uve(INUM0, array_prot(ra));
  }
  return ra;
}
static char s_array_inbp[] = "array-in-bounds?";
SCM array_inbp(v,ind,args)
  SCM v,ind,args;
{
  long pos = INUM(ind);
  register sizet k;
  register long j;
  array_dim *s;
  ASRTGO(NIMP(v),badarg1);
  ASSERT(INUMP(ind),ind,ARG2,s_array_inbp);
 tail:
  switch TYP7(v) {
  default: badarg1: wta(v,(char *)ARG1,s_array_inbp);
  case tc7_smob:
    k = ARRAY_NDIM(v);
    s = ARRAY_DIMS(v);
    pos = ARRAY_BASE(v);
    while (!0) {
      j = INUM(ind);
      if (!(j >= (s->lbnd) && j <= (s->ubnd))) return BOOL_F;
      pos += (j - s->lbnd)*(s->inc);
      if (!(--k && NIMP(args))) break;
      ind = CAR(args); args = CDR(args); s++;
      ASSERT(INUMP(ind),ind,s_bad_ind,s_array_inbp);
    }
    ASSERT(0==k && NULLP(args), args, WNA, s_aind);
    v = ARRAY_V(v);
    goto tail;
  case tc7_bvect: case tc7_string: case tc7_uvect: case tc7_ivect:
  case tc7_fvect: case tc7_dvect: case tc7_cvect: case tc7_vector:
    return pos >= 0 && pos < LENGTH(v) ? BOOL_T : BOOL_F;
  }
}
static char s_aref[] = "array-ref";
SCM aref(v,ind,args)
  SCM v,ind,args;
{
  long pos = INUM(ind);
  ASRTGO(NIMP(v),badarg1);
  ASSERT(INUMP(ind),ind,ARG2,s_aref);
 tail:
  switch TYP7(v) {
  default: badarg1: wta(v,(char *)ARG1,s_aref);
  outrng: wta(MAKINUM(pos),(char *)OUTOFRANGE,s_aref);
  wna: wta(UNDEFINED,(char *)WNA,s_aref);
  case tc7_smob:
    pos = INUM(aind(v,ind,args));
    args = EOL;
    v = ARRAY_V(v);
    goto tail;
  case tc7_bvect: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    if (VELTS(v)[pos/LONG_BIT]&(1L<<(pos%LONG_BIT)))
      return BOOL_T;
    else return BOOL_F;
  case tc7_string: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    return MAKICHR(CHARS(v)[pos]);
  case tc7_uvect:
  case tc7_ivect: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    return MAKINUM(VELTS(v)[pos]);
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    return makdbl(((float *)CDR(v))[pos],0.0);
#endif
  case tc7_dvect: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    return makdbl(((double *)CDR(v))[pos],0.0);
  case tc7_cvect: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    return makdbl(((double *)CDR(v))[2*pos],
		  ((double *)CDR(v))[2*pos+1]);
#endif
  case tc7_vector: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    return VELTS(v)[((long) pos)];
  }
}
static char s_aset[] = "array-set!";
SCM aset(v,obj,args)
SCM v,obj,args;
{
  SCM ind = args;
  long pos;
  if INUMP(args) args = EOL;
  else {
    ASSERT(NIMP(args) && CONSP(args), args, ARG3, s_aset);
    ind = CAR(args);
    args = CDR(args);
  }
  ASRTGO(NIMP(v),badarg1);
  ASSERT(INUMP(ind),ind,ARG2,s_aset);
 tail:
  pos = INUM(ind);
  switch TYP7(v) {
  default: badarg1: wta(v,(char *)ARG1,s_aset);
  outrng: wta(MAKINUM(pos),(char *)OUTOFRANGE,s_aset);
  wna: wta(UNDEFINED,(char *)WNA,s_aset);
  case tc7_smob:
    ind = aind(v,ind,args);
    args = EOL;
    v = ARRAY_V(v);
    goto tail;
  case tc7_bvect: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    if (BOOL_F==obj)
      VELTS(v)[pos/LONG_BIT] &= ~(1L<<(pos%LONG_BIT));
    else if (BOOL_T==obj)
      VELTS(v)[pos/LONG_BIT] |= (1L<<(pos%LONG_BIT));
    else badarg3: wta(obj,(char *)ARG3,s_aset);
    break;
  case tc7_string: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    ASRTGO(ICHRP(obj),badarg3);
    CHARS(v)[pos] = ICHR(obj); break;
  case tc7_uvect:
    ASRTGO(INUM(obj) >= 0,badarg3);
  case tc7_ivect: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(INUMP(obj),badarg3);
    VELTS(v)[pos] = INUM(obj); break;
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    ASRTGO(NIMP(obj) && REALP(obj),badarg3);
    ((float *)CDR(v))[pos] = REALPART(obj); break;
#endif
  case tc7_dvect: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    ASRTGO(NIMP(obj) && REALP(obj),badarg3);
    ((double *)CDR(v))[pos] = REALPART(obj); break;
  case tc7_cvect: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    ASRTGO(NIMP(obj) && INEXP(obj),badarg3);
    ((double *)CDR(v))[2*pos] = REALPART(obj);
    ((double *)CDR(v))[2*pos+1] = CPLXP(obj)?IMAG(obj):0.0; break;
#endif
  case tc7_vector: ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
    ASRTGO(NULLP(args), wna);
    VELTS(v)[((long) pos)] = obj; break;
  }
  return UNSPECIFIED;
}
static char s_uve_fill[] = "uniform-vector-fill!";
SCM uve_fill(v,obj)
SCM v,obj;
{
  register long k;
  ASRTGO(NIMP(v),badarg1);
  k = LENGTH(v);
  switch TYP7(v) {
  default: badarg1: wta(v,(char *)ARG1,s_uve_fill);
  case tc7_bvect:
    if (BOOL_F==obj)
      for (k = (k+LONG_BIT-1)/LONG_BIT;k--;)
	VELTS(v)[k] = 0L;
    else if (BOOL_T==obj)
      for (k = (k+LONG_BIT-1)/LONG_BIT;k--;)
	VELTS(v)[k] = ~0L;
    else badarg2: wta(obj,(char *)ARG2,s_uve_fill);
    break;
  case tc7_string: ASRTGO(ICHRP(obj),badarg2);
    while (k--) CHARS(v)[k] = ICHR(obj);
    break;
  case tc7_uvect: ASRTGO(INUM(obj) >= 0,badarg2);
  case tc7_ivect: ASRTGO(INUMP(obj),badarg2);
    while (k--) VELTS(v)[k] = INUM(obj);
    break;
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect: ASRTGO(NIMP(obj) && REALP(obj),badarg2);
    while (k--) ((float *)CDR(v))[k] = REALPART(obj);
    break;
#endif
  case tc7_dvect: ASRTGO(NIMP(obj) && REALP(obj),badarg2);
    while (k--) ((double *)CDR(v))[k] = REALPART(obj);
    break;
  case tc7_cvect: ASRTGO(NIMP(obj) && INEXP(obj),badarg2);
    while (k--) {
      ((double *)CDR(v))[2*k] = REALPART(obj);
      ((double *)CDR(v))[2*k+1] = CPLXP(obj)?IMAG(obj):0.0;
      }
    break;
#endif
  case tc7_vector:
    while (k--) VELTS(v)[k] = obj;
    break;
  }
  return UNSPECIFIED;
}
static char s_array_contp[] = "array-contiguous?";
SCM array_contp(ra)
     SCM ra;
{
  long inc=1;
  sizet k;
  if IMP(ra) return BOOL_F;
  switch TYP7(ra) {
  default: 
    return BOOL_F;
  case tc7_vector: case tc7_string: case tc7_bvect: case tc7_uvect:
  case tc7_ivect: case tc7_fvect: case tc7_dvect: case tc7_cvect:
    return BOOL_T;
  case tc7_smob: if (!ARRAYP(ra)) return BOOL_F;
    for (k=ARRAY_NDIM(ra); k--;) {
      if (inc != ARRAY_DIMS(ra)[k].inc) return BOOL_F;
      inc *= (ARRAY_DIMS(ra)[k].ubnd - ARRAY_DIMS(ra)[k].lbnd + 1);
    }
    if (tc7_bvect==TYP7(ARRAY_V(ra))) {	                 /* Ugly, isn't it? */
      if (inc == LENGTH(ARRAY_V(ra))) return BOOL_T;
      return (ARRAY_BASE(ra)%LONG_BIT || inc%LONG_BIT ? BOOL_F : BOOL_T);
      return BOOL_F;
    }
    return BOOL_T;
  }	    
}
static char s_uve_rd[] = "uniform-vector-read!";
SCM uve_read(v,port)
SCM v,port;
{
  long sz, len, ans;
  long start=0;
  char *vptr;
  if UNBNDP(port) port = cur_inp;
  else ASSERT(NIMP(port) && OPINFPORTP(port),port,ARG2,s_uve_rd);
  ASRTGO(NIMP(v),badarg1);
  len = LENGTH(v);
 loop:
  switch TYP7(v) {
  default: badarg1: wta(v,(char *)ARG1,s_uve_rd);
  case tc7_smob: ASRTGO(ARRAYP(v) && BOOL_T==array_contp(v),badarg1);
    start = ARRAY_BASE(v);
    len = ARRAY_DIMS(v)->inc * (ARRAY_DIMS(v)->ubnd - ARRAY_DIMS(v)->lbnd + 1);
    v = ARRAY_V(v);
    goto loop;
  case tc7_string:
    sz = sizeof(char);
    vptr = CHARS(v) + start;
    break;
  case tc7_bvect:
    len = (len+LONG_BIT-1)/LONG_BIT;
  case tc7_uvect:
  case tc7_ivect:
    sz = sizeof(long);
    vptr = (char *)((long *)VELTS(v) + start);
    break;
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect:
    sz = sizeof(float);
    vptr = (char *)((float *)VELTS(v) + start);
    break;
#endif
  case tc7_dvect:
    sz = sizeof(double);
    vptr = (char *)((double *)VELTS(v) + start);
    break;
  case tc7_cvect:
    sz = 2*sizeof(double);
    vptr = (char *)((long *)VELTS(v) + 2*start);
    break;
#endif
  }
  /* An ungetc before an fread will not work on some systems if setbuf(0).
     do #define NOSETBUF in config.h to fix this. */
  if CRDYP(port) {	/* UGGH!!! */
    ungetc(CGETUN(port),STREAM(port));
    CLRDY(port);		/* Clear ungetted char */
  }
  SYSCALL(ans = fread(vptr,(sizet)sz,(sizet)len,STREAM(port)););
  if (TYP7(v)==tc7_bvect) ans *= LONG_BIT;
  return MAKINUM(ans);
}

static char s_uve_wr[] = "uniform-vector-write";
SCM uve_write(v,port)
SCM v,port;
{
  long sz, len, ans;
  long start=0;
  if UNBNDP(port) port = cur_outp;
  else ASSERT(NIMP(port) && OPOUTFPORTP(port),port,ARG2,s_uve_wr);
  ASRTGO(NIMP(v),badarg1);
  len = LENGTH(v);
 loop:
  switch TYP7(v) {
  default: badarg1: wta(v,(char *)ARG1,s_uve_wr);
  case tc7_smob: ASRTGO(ARRAYP(v) && BOOL_T==array_contp(v),badarg1);
    start = ARRAY_BASE(v);
    len = ARRAY_DIMS(v)->inc * (ARRAY_DIMS(v)->ubnd - ARRAY_DIMS(v)->lbnd + 1);
    v = ARRAY_V(v);
    goto loop;
  case tc7_string:
    sz = sizeof(char);
    break;
  case tc7_bvect:
    len = (len+LONG_BIT-1)/LONG_BIT;
  case tc7_uvect:
  case tc7_ivect:
    sz = sizeof(long);
    break;
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect:
    sz = sizeof(float);
    break;
#endif
  case tc7_dvect:
    sz = sizeof(double);
    break;
  case tc7_cvect:
    sz = 2*sizeof(double);
    break;
#endif
  }
  SYSCALL(ans = fwrite(CHARS(v),(sizet)sz,(sizet)len,STREAM(port)););
  if (TYP7(v)==tc7_bvect) ans *= LONG_BIT;
  return MAKINUM(ans);
}

static char cnt_tab[16] = {0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4};
static char s_count[] = "bit-count";
SCM lcount(item, seq)
     SCM item, seq;
{
  long i;
  register unsigned long cnt = 0, w;
  ASSERT(NIMP(seq),seq,ARG2,s_count);
  switch TYP7(seq) {
  default: wta(seq,(char *)ARG2,s_count);
  case tc7_bvect:
    if (0==LENGTH(seq)) return INUM0;
    i = (LENGTH(seq)-1)/LONG_BIT;
    w = VELTS(seq)[i];
    if (BOOL_F==item) w = ~w;
    w <<= LONG_BIT-1-((LENGTH(seq)-1)%LONG_BIT);
    while (!0) {
      for(;w;w >>= 4) cnt += cnt_tab[w & 0x0f];
      if (0==i--) return MAKINUM(cnt);
      w = VELTS(seq)[i];
      if (BOOL_F==item) w = ~w;
    }
  }
}
static char s_uve_pos[] = "bit-position";
SCM position(item,v,k)
SCM item,v,k;
{
  long i, lenw, xbits, pos = INUM(k);
  register unsigned long w;
  ASSERT(NIMP(v),v,ARG2,s_uve_pos);
  ASSERT(INUMP(k),k,ARG3,s_uve_pos);
  ASSERT((pos <= LENGTH(v)) && (pos >= 0),
	 k,OUTOFRANGE,s_uve_pos);
  if (pos==LENGTH(v)) return BOOL_F;
  switch TYP7(v) {
  default: wta(v,(char *)ARG2,s_uve_pos);
  case tc7_bvect:
    if (0==LENGTH(v)) return MAKINUM(-1);
    lenw = (LENGTH(v)-1)/LONG_BIT; /* watch for part words */
    i = pos/LONG_BIT;
    w = VELTS(v)[i];
    if (BOOL_F==item) w = ~w;
    xbits = (pos%LONG_BIT);
    pos -= xbits;
    w = ((w >> xbits) << xbits);
    xbits = LONG_BIT-1-(LENGTH(v)-1)%LONG_BIT;
    while (!0) {
      if (w && (i==lenw))
	w = ((w << xbits) >> xbits);
      if (w) while (w) switch (w & 0x0f)
	{
	default: return MAKINUM(pos);
	case 2: case 6: case 10: case 14: return MAKINUM(pos+1);
	case 4: case 12: return MAKINUM(pos+2);
	case 8: return MAKINUM(pos+3);
	case 0: pos += 4; w >>= 4;
	}
      if (++i > lenw) break;
      pos += LONG_BIT;
      w = VELTS(v)[i];
      if (BOOL_F==item) w = ~w;
    }
    return BOOL_F;
  }
}

static char s_bit_set[] = "bit-set*!";
SCM bit_set(v,kv,obj)
SCM v,kv,obj;
{
  register long i, k, vlen;
  ASRTGO(NIMP(v),badarg1);
  ASRTGO(NIMP(kv),badarg2);
  switch TYP7(kv) {
    default: badarg2: wta(kv,(char *)ARG2,s_bit_set);
    case tc7_uvect:
      switch TYP7(v) {
	default: badarg1: wta(v,(char *)ARG1,s_bit_set);
	case tc7_bvect:
	  vlen = LENGTH(v);
	  if (BOOL_F==obj) for (i = LENGTH(kv);i;) {
	    k = VELTS(kv)[--i];
	    ASSERT((k < vlen), MAKINUM(k),OUTOFRANGE,s_bit_set);
	    VELTS(v)[k/LONG_BIT] &= ~(1L<<(k%LONG_BIT));
	  }
	  else if (BOOL_T==obj) for (i = LENGTH(kv); i;) {
	    k = VELTS(kv)[--i];
	    ASSERT((k < vlen), MAKINUM(k),OUTOFRANGE,s_bit_set);
	    VELTS(v)[k/LONG_BIT] |= (1L<<(k%LONG_BIT));
	  }
	  else
	  badarg3: wta(obj,(char *)ARG3,s_bit_set);
	}
      break;
    case tc7_bvect:
      ASRTGO(TYP7(v)==tc7_bvect && LENGTH(v)==LENGTH(kv),badarg1);
      if (BOOL_F==obj)
	for (k = (LENGTH(v)+LONG_BIT-1)/LONG_BIT;k--;)
	  VELTS(v)[k] &= ~(VELTS(kv)[k]);
      else if (BOOL_T==obj)
	for (k = (LENGTH(v)+LONG_BIT-1)/LONG_BIT;k--;)
	  VELTS(v)[k] |= VELTS(kv)[k];
      else goto badarg3;
      break;
    }
  return UNSPECIFIED;
}

static char s_bit_count[] = "bit-count*";
SCM bit_count(v,kv,obj)
SCM v,kv,obj;
{
  register long i, vlen, count = 0;
  register unsigned long k;
  ASRTGO(NIMP(v),badarg1);
  ASRTGO(NIMP(kv),badarg2);
  switch TYP7(kv) {
    default: badarg2: wta(kv,(char *)ARG2,s_bit_count);
    case tc7_uvect:
      switch TYP7(v) {
	default: badarg1: wta(v,(char *)ARG1,s_bit_count);
	case tc7_bvect:
	  vlen = LENGTH(v);
	  if (BOOL_F==obj) for (i = LENGTH(kv);i;) {
	    k = VELTS(kv)[--i];
	    ASSERT((k < vlen), MAKINUM(k),OUTOFRANGE,s_bit_count);
	    if (!(VELTS(v)[k/LONG_BIT] & (1L<<(k%LONG_BIT)))) count++;
	  }
	  else if (BOOL_T==obj) for (i = LENGTH(kv); i;) {
	    k = VELTS(kv)[--i];
	    ASSERT((k < vlen), MAKINUM(k),OUTOFRANGE,s_bit_count);
	    if (VELTS(v)[k/LONG_BIT] & (1L<<(k%LONG_BIT))) count++;
	  }
	  else
	  badarg3: wta(obj,(char *)ARG3,s_bit_count);
	}
      break;
    case tc7_bvect:
      ASRTGO(TYP7(v)==tc7_bvect && LENGTH(v)==LENGTH(kv),badarg1);
      if (0==LENGTH(v)) return INUM0;
      ASRTGO(BOOL_T==obj || BOOL_F==obj,badarg3);
      obj = (BOOL_T==obj);
      i = (LENGTH(v)-1)/LONG_BIT;
      k = VELTS(kv)[i] & (obj ? VELTS(v)[i] : ~VELTS(v)[i]);
      k <<= LONG_BIT-1-((LENGTH(v)-1)%LONG_BIT);
      while (!0) {
	for(;k;k >>= 4) count += cnt_tab[k & 0x0f];
	if (0==i--) return MAKINUM(count);
	k = VELTS(kv)[i] & (obj ? VELTS(v)[i] : ~VELTS(v)[i]);
      }
      break;
    }
  return MAKINUM(count);
}

static char s_bit_inv[] = "bit-invert!";
SCM bit_inv(v)
SCM v;
{
  register long k;
  ASRTGO(NIMP(v),badarg1);
  k = LENGTH(v);
  switch TYP7(v) {
  default: badarg1: wta(v,(char *)ARG1,s_bit_inv);
  case tc7_bvect:
    for (k = (k+LONG_BIT-1)/LONG_BIT;k--;)
      VELTS(v)[k] = ~VELTS(v)[k];
  }
  return UNSPECIFIED;
}

static char s_strup[] = "string-upcase!";
SCM strup(v)
SCM v;
{
  register long k;
  register unsigned char *cs;
  ASRTGO(NIMP(v),badarg1);
  k = LENGTH(v);
  switch TYP7(v) {
  default: badarg1: wta(v,(char *)ARG1,s_strup);
  case tc7_string:
    cs = UCHARS(v);
    while (k--) cs[k] = upcase[cs[k]];
  }
  return v;
}

static char s_strdown[] = "string-downcase!";
SCM strdown(v)
SCM v;
{
  register long k;
  register unsigned char *cs;
  ASRTGO(NIMP(v),badarg1);
  k = LENGTH(v);
  switch TYP7(v) {
  default: badarg1: wta(v,(char *)ARG1,s_strdown);
  case tc7_string:
    cs = UCHARS(v);
    while (k--) cs[k] = downcase[cs[k]];
  }
  return v;
}

char s_resizuve[] = "vector-set-length!";
SCM resizuve(vect, len)
     SCM vect, len;
{
  long l = INUM(len);
  sizet siz, sz;
  ASRTGO(NIMP(vect),badarg1);
  switch TYP7(vect) {
  default: badarg1: wta(vect,(char *)ARG1,s_resizuve);
  case tc7_string:
    ASRTGO(vect != nullstr,badarg1);
    sz = sizeof(char);
    l++;
    break;
  case tc7_vector:
    ASRTGO(vect != nullvect,badarg1);
    sz = sizeof(SCM);
    break;
  case tc7_bvect:
    l = (l+LONG_BIT-1)/LONG_BIT;
  case tc7_uvect:
  case tc7_ivect:
    sz = sizeof(long);
    break;
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect:
    sz = sizeof(float);
    break;
#endif
  case tc7_dvect:
    sz = sizeof(double);
    break;
  case tc7_cvect:
    sz = 2*sizeof(double);
    break;
#endif
  }
  ASSERT(INUMP(len),len,ARG2,s_resizuve);
  if (!l) l = 1L;
  siz = l * sz;
  if (siz != l * sz) wta((SCM)MAKINUM(l * sz), (char *) NALLOC, s_resizuve);
  DEFER_INTS;
  SETCHARS(vect,(char *)must_realloc((char *)CHARS(vect),
				     (long)LENGTH(vect)*sz,
				     (long)siz, s_resizuve));
  if VECTORP(vect) {
    sz = LENGTH(vect);
    while(l > sz) VELTS(vect)[--l] = UNSPECIFIED;
  }
  else if STRINGP(vect) CHARS(vect)[l-1] = 0;
  SETLENGTH(vect,INUM(len),TYP7(vect));
  ALLOW_INTS;
  return vect;
}

static char s_list2uve[] = "list->uniform-vector";
SCM list2uve(prot, lst)
SCM prot;
SCM lst;
{
  SCM v;
  register SCM obj;
  register SCM *data;
  register long k;
  long len = ilength(lst);
  v = make_uve(MAKINUM(len),prot);
  data = VELTS(v);
  switch TYP7(v) {
  default: return vector(lst);
  case tc7_bvect:
    {
      register unsigned long mask;
      len = sizeof(long)*((len+LONG_BIT-1)/LONG_BIT);
      for (k = 0; k < len; k++) {
	((long *)data)[k] = 0L;
	for (mask = 1L; mask && NNULLP(lst); mask <<= 1, lst = CDR(lst)) {
	  obj = CAR(lst);
	  if (obj == BOOL_T || obj == MAKINUM(1))
	    ((long *)data)[k] |= mask;
	  else if (obj != BOOL_F && obj != INUM0)
	    goto badarg;
	}
      }
    } 
    return v;
  case tc7_string: return string(lst);
  case tc7_uvect:
    for (;NNULLP(lst) && (obj = CAR(lst)); lst = CDR(lst))
      { ASRTGO(INUMP(obj) && INUM(obj) >= 0, badarg);
	 *data++ = INUM(obj);
      }
    return v;
  badarg: wta(lst,(char *)ARG2,s_list2uve);
  case tc7_ivect:
    for (;NNULLP(lst) && (obj = CAR(lst)); lst = CDR(lst))
      { ASRTGO(INUMP(obj), badarg);
	 *data++ = INUM(obj);
      }
    return v;
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect:
    for (k = 0; NIMP(lst) && (obj = CAR(lst)); lst = CDR(lst), k++)
      { ASRTGO(NIMP(obj) && REALP(obj), badarg);
	((float *)(data))[k] = REALPART(obj);
      }
    return v;
#endif /*SINGLES*/
  case tc7_dvect:
    for (k = 0; NIMP(lst) && (obj = CAR(lst)); lst = CDR(lst), k++)
      { ASRTGO(NIMP(obj) && REALP(obj), badarg);
	((double *)(data))[k] = REALPART(obj);
      }
    return v;
  case tc7_cvect:
    for (k = 0; NIMP(lst) && (obj = CAR(lst)); lst = CDR(lst), k++)
      { ASRTGO(NIMP(obj) && INEXP(obj), badarg);
	((double *)(data))[k++] = REALPART(obj);
	((double *)(data))[k] = CPLXP(obj)?IMAG(obj):0.0;
      }
    return v;
#endif /*FLOATS*/
  }
}
SCM istr2bve(str, len)
char *str;
long len;
{
  SCM v = make_uve(MAKINUM(len),BOOL_T);
  long *data = (long *)VELTS(v);
  register unsigned long mask;
  register long k;
  register int j;
  for (k = 0; k < (len+LONG_BIT-1)/LONG_BIT; k++) {
    data[k] = 0L;
    j = len - k*LONG_BIT;
    if (j > LONG_BIT) j = LONG_BIT;
    for (mask = 1L; j--; mask <<= 1)
      switch (*str++) {
      case '0': break;
      case '1': data[k] |= mask; break;
      default:  return BOOL_F;
      }
  }
  return v;
}    
SCM clist2uve(c, lst)
int c;
SCM lst;
{
  switch (c) {
  default: return BOOL_F;
  case 'a': case 'A':
    return list2uve(MAKICHR('a'),lst);
  case 'b': case 'B':
    return list2uve(BOOL_T,lst);
  case 'u': case 'U':
    return list2uve(MAKINUM(1),lst);
  case 'e': case 'E':
    return list2uve(MAKINUM(-1),lst);
#ifdef FLOATS
#ifdef SINGLES
  case 's': case 'S':
    return list2uve(flo0,lst);
#endif /*SINGLES*/
  case 'i': case 'I':
    return list2uve(makdbl(1.0/3.0,0.0),lst);
  case 'c': case 'C':
    return list2uve(makdbl(1.0,1.0),lst);
#endif /*FLOATS*/
  }
}

static SCM ra2l(ra, base, k)
     SCM ra;
     sizet base;
     sizet k;
{
  register SCM res = EOL;
  register long inc = ARRAY_DIMS(ra)[k].inc;
  register sizet i;
  if (ARRAY_DIMS(ra)[k].ubnd < ARRAY_DIMS(ra)[k].lbnd) return EOL;
  i = base + (1 + ARRAY_DIMS(ra)[k].ubnd - ARRAY_DIMS(ra)[k].lbnd)*inc;
  if (k < ARRAY_NDIM(ra) - 1) {
    do {
      i -= inc;
      res = cons(ra2l(ra,i,k+1), res);
    } while (i != base);
  }
  else
    do {
      i -= inc;
      res = cons(aref(ARRAY_V(ra),MAKINUM(i),EOL), res);
    } while (i != base);
  return res;
}

static char s_array2list[] = "array->list";
SCM array2list(v)
SCM v;
{
  SCM res = EOL;
  register long k;
  register unsigned long mask;
  SCM *data;
  ASRTGO(NIMP(v),badarg1);
    switch TYP7(v) {
  default: badarg1: wta(v, (char *)ARG1, s_array2list);
  case tc7_smob: ASRTGO(ARRAYP(v),badarg1);
    return ra2l(v,ARRAY_BASE(v),0);
  case tc7_vector: return vector2list(v);
  case tc7_string: return string2list(v);
  case tc7_bvect:
    data = VELTS(v);
    for (k = (LENGTH(v)-1)/LONG_BIT; k > 0; k--)
      for (mask = 1L<<(LONG_BIT-1); mask; mask >>=1)
	res = cons(((long *)data)[k] & mask ? BOOL_T : BOOL_F, res);
    for (mask = 1L<<((LENGTH(v)%LONG_BIT)-1); mask; mask >>=1)
      res = cons(((long *)data)[k] & mask ? BOOL_T : BOOL_F, res);
    return res;
  case tc7_uvect: 
  case tc7_ivect:
    data = VELTS(v);
    for (k = LENGTH(v) - 1; k >= 0; k--)
      res = cons((SCM)MAKINUM(data[k]), res);
    return res;
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect:
    data = VELTS(v);
    for (k = LENGTH(v) - 1; k >= 0; k--)
      res = cons(makdbl(((float *)data)[k], 0.0), res);
    return res;
#endif /*SINGLES*/
  case tc7_dvect:
    data = VELTS(v);
    for (k = LENGTH(v) - 1; k >= 0; k--)
      res = cons(makdbl(((double *)data)[k], 0.0), res);
    return res;
  case tc7_cvect:
    data = VELTS(v);
    for (k = 2*(LENGTH(v) - 1); k >= 0; k-=2)
      res = cons(makdbl(((double *)data)[k],((double *)data)[k+1]), res);
    return res;
#endif /*FLOATS*/
  }
}

static char s_bad_ralst[] = "Bad array contents list";
static char s_list2ura[] = "list->uniform-array";
static int l2ra();
SCM list2ura(ndim, prot, lst)
     SCM ndim;
     SCM prot;
     SCM lst;
{
  SCM shp=EOL;
  SCM row=lst;
  SCM ra;
  sizet k;
  long n;
  if (MAKINUM(1)==ndim) return list2uve(prot,lst);
  ASSERT(INUMP(ndim), ndim, ARG1, s_list2ura);
  k = INUM(ndim);
  for (; k--; NIMP(row) && (row = CAR(row))) {
    n = ilength(row);
    ASSERT(n>=0, lst, ARG2, s_list2ura);
    shp = cons(MAKINUM(n),shp);
  }
  ra = dims2ura(reverse(shp), prot, EOL);
  if (l2ra(lst, ra, ARRAY_BASE(ra), 0))
    return ra;
  else
    wta(lst,s_bad_ralst,s_list2ura);
  return BOOL_F;
}

static int l2ra(lst, ra, base, k)
     SCM lst;
     SCM ra;
     sizet base;
     sizet k;
{
  register long inc = ARRAY_DIMS(ra)[k].inc;
  register long n = (1 + ARRAY_DIMS(ra)[k].ubnd - ARRAY_DIMS(ra)[k].lbnd);
  int ok = 1;
  if (n <= 0) return (EOL==lst);
  if (k < ARRAY_NDIM(ra) - 1) {
    while (n--) {
      if (IMP(lst) || NCONSP(lst)) return 0;
      ok = ok && l2ra(CAR(lst), ra, base, k+1);
      base += inc;
      lst = CDR(lst);
    }
    if NNULLP(lst) return 0;
  }
  else {
    while (n--) {
      if (IMP(lst) || NCONSP(lst)) return 0;
      ok = ok && aset(ARRAY_V(ra), CAR(lst), MAKINUM(base));
      base += inc;
      lst = CDR(lst);
    } 
    if NNULLP(lst) return 0;
  }
  return ok;
}

static void rapr1(ra, base, k, port, writing)
     SCM ra;
     sizet base;
     sizet k;
     SCM port;
     int writing;
{
  register sizet inc = ARRAY_DIMS(ra)[k].inc;
  register long i;
  lputc('(',port);
  if (k < ARRAY_NDIM(ra) - 1) {
    for (i=ARRAY_DIMS(ra)[k].lbnd; i<ARRAY_DIMS(ra)[k].ubnd; i++) {
      rapr1(ra, base, k+1, port, writing);
      lputc(' ',port);
      base += inc;
    }
    if (i==ARRAY_DIMS(ra)[k].ubnd) /* could be zero size. */
      rapr1(ra, base, k+1, port, writing);
  }
  else {
    for (i=ARRAY_DIMS(ra)[k].lbnd; i<ARRAY_DIMS(ra)[k].ubnd; i++) {
      iprin1(aref(ARRAY_V(ra),MAKINUM(base),EOL), port, writing);
      lputc(' ',port);
      base += inc;
    }
    if (i==ARRAY_DIMS(ra)[k].ubnd)
      iprin1(aref(ARRAY_V(ra),MAKINUM(base),EOL), port, writing);
  }
  lputc(')',port);
}

int raprin1(exp,port,writing)
     SCM exp;
     SCM port;
     int writing;
{
  lputc('#',port);
  intprint(ARRAY_NDIM(exp),10,port);
  switch TYP7(ARRAY_V(exp)) {
  case tc7_vector:
    break;
  case tc7_string:
    lputc('a',port);
    break;
  case tc7_bvect:
    lputc('b',port);
    break;
  case tc7_uvect:
    lputc('u',port);
    break;
  case tc7_ivect:
    lputc('e',port);
    break;
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect:
    lputc('s',port);
    break;
#endif /*SINGLES*/
  case tc7_dvect:
    lputc('i',port);
    break;
  case tc7_cvect:
    lputc('c',port);
    break;
#endif /*FLOATS*/
  }
  rapr1(exp, ARRAY_BASE(exp), 0, port, writing);
  return 1;
}

SCM clist2array(c, ndim, lst)
     int c;
     SCM ndim;
     SCM lst;
{
  switch (c) {
  default: return BOOL_F;
  case DIGITS:
    return list2ura(ndim,listofnull,lst);
  case 'a': case 'A':
    return list2ura(ndim,MAKICHR('a'),lst);
  case 'b': case 'B':
    return list2ura(ndim,BOOL_T,lst);
  case 'u': case 'U':
    return list2ura(ndim,MAKINUM(1),lst);
  case 'e': case 'E':
    return list2ura(ndim,MAKINUM(-1),lst);
#ifdef FLOATS
#ifdef SINGLES
  case 's': case 'S':
    return list2ura(ndim,flo0,lst);
#endif /*SINGLES*/
  case 'i': case 'I':
    return list2ura(ndim,makdbl(1.0/3.0,0.0),lst);
  case 'c': case 'C':
    return list2ura(ndim,makdbl(1.0,1.0),lst);
#endif /*FLOATS*/
  }
}
static char s_array_prot[] = "array-prototype";
SCM array_prot(ra)
     SCM ra;
{
  ASRTGO(NIMP(ra), badarg);
 loop:
  switch TYP7(ra) {
  default: badarg: wta(ra, (char *)ARG1, s_array_prot);
  case tc7_smob: ASRTGO(ARRAYP(ra), badarg);
    ra = ARRAY_V(ra);
    goto loop;
  case tc7_vector: return EOL;
  case tc7_bvect: return BOOL_T;
  case tc7_string: return MAKICHR('a');
  case tc7_uvect: return MAKINUM(1);
  case tc7_ivect: return MAKINUM(-1);
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect: return makdbl(1.0,0.0);
#endif
  case tc7_dvect: return makdbl(1.0/3.0,0.0);
  case tc7_cvect: return makdbl(0.0, 1.0);
#endif
  }
}

static sizet cind(ra, inds)
     SCM ra;
     long *inds;
{
  sizet i;
  int k;
  if (!ARRAYP(ra))
    return *inds;
  i = ARRAY_BASE(ra);
  for (k = 0; k < ARRAY_NDIM(ra); k++)
    i += (inds[k] - ARRAY_DIMS(ra)[k].lbnd)*ARRAY_DIMS(ra)[k].inc;
  return i;
}
				/* Check that ra1 is at least as big as ra0 */
static int ra_matchp(ra0, ra1)
     SCM ra0, ra1;
{
  array_dim dims = {0,0,0};
  array_dim *s0 = &dims;
  array_dim *s1;
  int ndim = 1;

  switch TYP7(ra0) {
  default: return 0;
  case tc7_vector: case tc7_string: case tc7_bvect: case tc7_uvect:
  case tc7_ivect: case tc7_fvect: case tc7_dvect:
    s0->ubnd = LENGTH(ra0) - 1;
    break;
  case tc7_smob:
    if (!ARRAYP(ra0)) return 0;
    ndim = ARRAY_NDIM(ra0);
    s0 = ARRAY_DIMS(ra0);
    break;
  }
  switch TYP7(ra1) {
  default: return 0;
  case tc7_vector: case tc7_string: case tc7_bvect: case tc7_uvect:
  case tc7_ivect: case tc7_fvect: case tc7_dvect:
    return (1==ndim && s0->lbnd >= 0 && s0->ubnd < LENGTH(ra1));
  case tc7_smob:
    if (!ARRAYP(ra1) || ndim != ARRAY_NDIM(ra1)) return 0;
    s1 = ARRAY_DIMS(ra1);
    while (ndim--) {
      if (s0->lbnd < s1->lbnd || s0->ubnd > s1->ubnd) return 0;
      s0++;
      s1++;
    }
    return 1;
  }
}
static char s_ra_mismatch[] = "array shape mismatch";
int ramapc(cproc, data, ra0, lra, what)
     int (*cproc)();
     SCM data, ra0, lra;
     char *what;
{
  SCM indv, y, z;
  SCM shap1, vra0, ra1, vra1;
  SCM lvra = EOL, *plvra = &lvra;
  long indss[3] = {0L,0L,0L};
  long *inds = indss;
  int k, kmax;
#ifndef RECKLESS
  for (z = lra; NNULLP(z); z = CDR(z))
    ASSERT(ra_matchp(ra0, CAR(z)), UNDEFINED, s_ra_mismatch, what);
#endif
  if (!ARRAYP(ra0))
    return (UNBNDP(data) ? cproc(ra0, lra): cproc(ra0, data, lra));
  kmax = ARRAY_NDIM(ra0) - 1;
  shap1 = cons(MAKINUM(ARRAY_DIMS(ra0)[kmax].ubnd - 
		       ARRAY_DIMS(ra0)[kmax].lbnd + 1), EOL);
  vra0 = shap2ra(shap1, "");
  ARRAY_DIMS(vra0)->inc = ARRAY_DIMS(ra0)[kmax].inc;
  ARRAY_V(vra0) = ARRAY_V(ra0);
  lvra = EOL;
  for (z = lra; NIMP(z); z = CDR(z)) {
    ra1 = CAR(z);
    vra1 = shap2ra(shap1, "");
    if ARRAYP(ra1) {
      ARRAY_DIMS(vra1)->inc = ARRAY_DIMS(ra1)[kmax].inc;
      ARRAY_V(vra1) = ARRAY_V(ra1);
    }
    else {
      ARRAY_DIMS(vra1)->inc = 1;
      ARRAY_V(vra1) = ra1;
    }
    *plvra = cons(vra1,EOL);
    plvra = &CDR(*plvra);
  }
  if (ARRAY_NDIM(ra0)>3) {
    indv = make_uve(MAKINUM(ARRAY_NDIM(ra0)), MAKINUM(-1));
    inds = (long *)VELTS(indv);
  }
  for (k = 0; k <= kmax; k++)
    inds[k] = ARRAY_DIMS(ra0)[k].lbnd;
  k = kmax;
  do {
    if (k==kmax) {
      ARRAY_BASE(vra0) = cind(ra0, inds);
      for (z = lvra, y = lra; NIMP(z); z = CDR(z), y = CDR(y))
	ARRAY_BASE(CAR(z)) = cind(CAR(y), inds);
      if (0 == (UNBNDP(data) ? cproc(vra0, lvra): cproc(vra0, data, lvra)))
	return 0;
      k--;
      continue;
    }
    if (inds[k] < ARRAY_DIMS(ra0)[k].ubnd) {
      inds[k]++;
      k++;
      continue;
    }
    inds[k] = ARRAY_DIMS(ra0)[k].lbnd - 1;
    k--;
  } while (k >= 0);
  return 1;
}

static char s_array_copy[] = "array-copy!";
static int racp();
SCM array_copy(src, dst)
     SCM src;
     SCM dst;
{
  ramapc(racp, UNDEFINED, src, cons(dst,EOL), s_array_copy);
  return UNDEFINED;
}
static int racp(src, dst)
     SCM dst, src;
{
  long n = LENGTH(src);
  long inc_s = 1, inc_d = 1;
  sizet i_s = 0, i_d = 0;
  if ARRAYP(src) {
    i_s = ARRAY_BASE(src);
    inc_s = ARRAY_DIMS(src)->inc;
    n = (ARRAY_DIMS(src)->ubnd - ARRAY_DIMS(src)->lbnd + 1);
    src = ARRAY_V(src);
  }
  dst = CAR(dst);
  if ARRAYP(dst) {
    i_d = ARRAY_BASE(dst);
    inc_d = ARRAY_DIMS(dst)->inc;
    dst = ARRAY_V(dst);
  }
  switch (TYP7(src) == TYP7(dst) ? TYP7(src) : tc7_vector) {
  default: case tc7_vector:
    for (; n--; i_s += inc_s, i_d += inc_d)
      aset(dst, aref(src,MAKINUM(i_s),EOL), MAKINUM(i_d));
    break;
  case tc7_string: {
    char *sv = CHARS(src) + i_s;
    char *dv = CHARS(dst) + i_d;
    for (; n--; sv += inc_s, dv += inc_d)
      *dv = *sv;
    break;
  }
  case tc7_bvect: {
    long *sv = (long *)VELTS(src) + i_s;
    long *dv = (long *)VELTS(dst) + i_d;
    for (; n--; i_s += inc_s, i_d += inc_d)
      if (sv[i_s/LONG_BIT] & (1L << (i_s%LONG_BIT)))
	dv[inc_d/LONG_BIT] |= (1L << (i_s%LONG_BIT));
      else
	dv[inc_d/LONG_BIT] &= ~(1L << (i_s%LONG_BIT));
    break;
  }
  case tc7_uvect: case tc7_ivect: {
    long *sv = (long *)VELTS(src) + i_s;
    long *dv = (long *)VELTS(dst) + i_d;
    for (; n--; sv += inc_s, dv += inc_d)
      *dv = *sv;
    break;
  }
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect: {
    float *sv = (float *)VELTS(src) + i_s;
    float *dv = (float *)VELTS(dst) + i_d;
    for (; n--; sv += inc_s, dv += inc_d)
      *dv = *sv;
    break;
  }
#endif /* SINGLES */
  case tc7_dvect: {
    double *sv = (double *)VELTS(src) + i_s;
    double *dv = (double *)VELTS(dst) + i_d;
    for (; n--; sv += inc_s, dv += inc_d)
      *dv = *sv;
    break;
  }
  case tc7_cvect: {
    double *sv = (double *)VELTS(src) + i_s;
    double *dv = (double *)VELTS(dst) + i_d;
    for (; n--; sv += 2*inc_s, dv += 2*inc_d) {
      *dv = *sv;
      dv[1] = sv[1];
    }
    break;
  }
#endif /* FLOATS */
  }
  return 1;
}

static char s_array_map[] = "array-map!";
static int ramap();
SCM array_map(ra0, proc, lra)
     SCM ra0, proc, lra;
{
  ASSERT(BOOL_T==procedurep(proc), proc, ARG2, s_array_map);
  ramapc(ramap, proc, ra0, lra, s_array_map);
  return UNDEFINED;
}
static int ramap(ra0, proc, ras)
     SCM ra0, proc, ras;
{
  sizet i = 0;
  long n = LENGTH(ra0);
  if ARRAYP(ra0) {
    i = ARRAY_DIMS(ra0)->lbnd;
    n = (ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1);
  }
  if NULLP(ras)
    for (; i < n; i++)
      aset(ra0, apply(proc,EOL,EOL), MAKINUM(i));
  else if NULLP(CDR(ras)) {
    ras = CAR(ras);
    for (; i < n; i++)
      aset(ra0, apply(proc, aref(ras,MAKINUM(i),EOL), listofnull), MAKINUM(i));
  }
  else {
    SCM args, *ve;
    int k;
    ras = vector(ras);
    ve = VELTS(ras);
    for (; i < n; i++) {
      args = EOL;
      for (k = LENGTH(ras); k--;)
	args = cons(aref(ve[k],MAKINUM(i),EOL), args);
      aset(ra0, apply(proc,args,EOL), MAKINUM(i));
    }
  }
  return 1;
}

static char s_array_for_each[] = "array-for-each";
static int rafe();
SCM array_for_each(proc, ra0, lra)
     SCM proc, ra0, lra;
{
  ASSERT(BOOL_T==procedurep(proc), proc, ARG1, s_array_for_each);
  ramapc(rafe, proc, ra0, lra, s_array_for_each);
  return UNDEFINED;
}
static int rafe(ra0, proc, ras)
     SCM ra0, proc, ras;
{
  sizet i = 0;
  long n = LENGTH(ra0);
  if ARRAYP(ra0) {
    i = ARRAY_DIMS(ra0)->lbnd;
    n = (ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1);
  }
  if NULLP(ras)
    for (; i < n; i++)
      apply(proc, aref(ra0,MAKINUM(i),EOL), listofnull);
  else if NULLP(CDR(ras)) {
    ras = CAR(ras);
    for (; i < n; i++)
      apply(proc, 
	    cons2(aref(ra0,MAKINUM(i),EOL), aref(ras,MAKINUM(i),EOL), EOL),
	    EOL);
  }
  else {
    SCM args, *ve;
    int k;
    ras = vector(ras);
    ve = VELTS(ras);
    for (; i < n; i++) {
      args = EOL;
      for (k = LENGTH(ras); k--;)
	args = cons(aref(ve[k],MAKINUM(i),EOL), args);
      apply(proc, cons(aref(ra0,MAKINUM(i),EOL),args), EOL);
    }
  }
  return 1;
}

static char s_array_imap[] = "array-index-map!";
SCM array_imap(ra, proc)
     SCM ra, proc;
{
  sizet i;
  ASSERT(NIMP(ra), ra, ARG1, s_array_imap);
  ASSERT(BOOL_T==procedurep(proc), proc, ARG2, s_array_imap);
  switch TYP7(ra) {
  default: badarg: wta(ra, (char *)ARG1, s_array_imap);
  case tc7_vector:
    {
      SCM *ve = VELTS(ra);
      for (i = 0; i < LENGTH(ra); i++)
	ve[i] = apply(proc, MAKINUM(i), listofnull);
      return UNSPECIFIED;
    }
  case tc7_string: case tc7_bvect: case tc7_uvect: case tc7_ivect:
  case tc7_fvect: case tc7_dvect: case tc7_cvect:
    for (i = 0; i < LENGTH(ra); i++)
      aset(ra, apply(proc,MAKINUM(i),listofnull), MAKINUM(i));
    return UNSPECIFIED;
  case tc7_smob: ASRTGO(ARRAYP(ra), badarg);
    {
      SCM inds = make_uve(MAKINUM(ARRAY_NDIM(ra)), MAKINUM(-1));
      long *vinds = VELTS(inds);
      int k, kmax = ARRAY_NDIM(ra) - 1;
      for (k = 0; k <= kmax; k++)
	vinds[k] = ARRAY_DIMS(ra)[k].lbnd;
      k = kmax;
      do {
	if (k==kmax) {
	  vinds[k] = ARRAY_DIMS(ra)[k].lbnd;
	  i = cind(ra, vinds);
	  for (; vinds[k] <= ARRAY_DIMS(ra)[k].ubnd; vinds[k]++) {
	    aset(ARRAY_V(ra), apply(proc,array2list(inds),EOL), MAKINUM(i));
	    i += ARRAY_DIMS(ra)[k].inc;
	  }
	  k--;
	  continue;
	}
	if (vinds[k] < ARRAY_DIMS(ra)[k].ubnd) {
	  vinds[k]++;
	  k++;
	  continue;
	}
	vinds[k] = ARRAY_DIMS(ra)[k].lbnd - 1;
	k--;
      } while (k >= 0);
      return UNSPECIFIED;
    }
  }
}

static iproc subr3s[] = {
	{"uniform-vector-set1!", aset},
	{s_uve_pos, position},
	{s_bit_set, bit_set},
	{s_bit_count, bit_count},
	{s_list2ura, list2ura},
	{0,0}};

static iproc subr2s[] = {
	{s_uve_fill, uve_fill},
	{s_resizuve,resizuve},
	{s_count, lcount},
	{s_array_copy, array_copy},
	{0,0}};

static iproc subr1s[] = {
	{"array-rank", array_rank},
	{s_array_dims, array_dims},
	{s_array2list, array2list},
	{s_uve_len, uve_len},
	{s_bit_inv, bit_inv},
	{s_strdown, strdown},
	{s_strup, strup},
	{s_array_prot, array_prot},
	{s_array_contp, array_contp},
	{0,0}};

static iproc lsubr2s[] = {
  {s_make_sh_array, make_sh_array},
  {s_dims2ura, dims2ura},
  {s_array_inbp, array_inbp},
  {s_aind, aind},
  {s_aref, aref},
  {s_aset, aset},
  {s_array_map, array_map},
  {s_array_for_each, array_for_each},
  {s_array_imap, array_imap},
  {0,0}};

static iproc subr2os[] = {
  {"array?", arrayp},
  {s_uve_rd, uve_read},
  {s_uve_wr, uve_write},
  {0,0}};

static SCM markra(ptr)
     SCM ptr;
{
  if GC8MARKP(ptr) return BOOL_F;
  SETGC8MARK(ptr);
  return ARRAY_V(ptr);
}
static sizet freera(ptr)
     CELLPTR ptr;
{
  must_free(CHARS(ptr));
  return sizeof(array) + ARRAY_NDIM(ptr)*sizeof(array_dim);
}
static int raeql(ra0, lra)
     SCM ra0, lra;
{
  SCM ra1 = CAR(lra);
  sizet i0 = ARRAY_BASE(ra0);
  long inc0 = ARRAY_DIMS(ra0)->inc;
  sizet i1 = ARRAY_BASE(ra1);
  long inc1 = ARRAY_DIMS(ra1)->inc;
  sizet n = ARRAY_DIMS(ra0)->ubnd - ARRAY_DIMS(ra0)->lbnd + 1;
  ra0 = ARRAY_V(ra0);
  ra1 = ARRAY_V(ra1);
  switch TYP7(ra0) {
  case tc7_vector: case tc7_bvect: default:
    for (; n--; i0+=inc0, i1+=inc1)
      if (BOOL_F==equal(aref(ra0,MAKINUM(i0),EOL), aref(ra1,MAKINUM(i1),EOL)))
	return 0;
    return 1;
  case tc7_string: {
    char *v0 = CHARS(ra0) + i0;
    char *v1 = CHARS(ra1) + i1;
    for (; n--; v0 += inc0, v1 += inc1)
      if (*v0 != *v1) return 0;
    return 1;
  }
  case tc7_uvect: case tc7_ivect: {
    long *v0 = (long *)VELTS(ra0) + i0;
    long *v1 = (long *)VELTS(ra1) + i1;
    for (; n--; v0 += inc0, v1 += inc1)
      if (*v0 != *v1) return 0;
    return 1;
  }
#ifdef FLOATS
#ifdef SINGLES
  case tc7_fvect: {
    float *v0 = (float *)VELTS(ra0) + i0;
    float *v1 = (float *)VELTS(ra1) + i1;
    for (; n--; v0 += inc0, v1 += inc1)
      if (*v0 != *v1) return 0;
    return 1;
  }
#endif /* SINGLES */
  case tc7_dvect: {
    double *v0 = (double *)VELTS(ra0) + i0;
    double *v1 = (double *)VELTS(ra1) + i1;
    for (; n--; v0 += inc0, v1 += inc1)
      if (*v0 != *v1) return 0;
    return 1;
  }
  case tc7_cvect: {
    double *v0 = (double *)VELTS(ra0) + i0;
    double *v1 = (double *)VELTS(ra1) + i1;
    for (; n--; v0 += 2*inc0, v1 += 2*inc1)
      if (*v0 != *v1 || v0[1] != v1[1]) return 0;
    return 1;
  }
#endif /* FLOATS */
  }
}
static SCM raequal(ra0, ra1)
     SCM ra0, ra1;
{
  int k, inc_match = 1, vlen = 1;
  if (ARRAY_NDIM(ra0) != ARRAY_NDIM(ra1)) return BOOL_F;
  if (TYP7(ARRAY_V(ra0)) != TYP7(ARRAY_V(ra1))) return BOOL_F;
  for (k = ARRAY_NDIM(ra0); k--;) {
    if (ARRAY_DIMS(ra0)[k].lbnd != ARRAY_DIMS(ra1)[k].lbnd || 
	ARRAY_DIMS(ra0)[k].ubnd != ARRAY_DIMS(ra1)[k].ubnd) return BOOL_F;
    if (inc_match) {
      inc_match = (ARRAY_DIMS(ra0)[k].inc == ARRAY_DIMS(ra1)[k].inc);
      vlen *= ARRAY_DIMS(ra0)[k].ubnd - ARRAY_DIMS(ra1)[k].lbnd + 1;
    }
  }
  if (inc_match && ARRAY_BASE(ra0) == ARRAY_BASE(ra1)) {
    if (ARRAY_V(ra0) == ARRAY_V(ra1))
      return BOOL_T;
    if (0==ARRAY_BASE(ra0) && vlen==LENGTH(ARRAY_V(ra0)) &&
	vlen==LENGTH(ARRAY_V(ra1)))
      return equal(ARRAY_V(ra0), ARRAY_V(ra1));
  }
  return (ramapc(raeql, UNDEFINED, ra0, cons(ra1,EOL), "") ? BOOL_T : BOOL_F);
}

static smobfuns rasmob = {markra,freera,raprin1,raequal};

void init_unif()
{
  init_iprocs(subr3s, tc7_subr_3);
  init_iprocs(subr2s, tc7_subr_2);
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(lsubr2s, tc7_lsubr_2);
  init_iprocs(subr2os, tc7_subr_2o);
  tc16_array = newsmob(&rasmob);
  add_feature(s_array);
  add_feature(s_array_for_each);
}
