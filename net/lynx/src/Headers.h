/*	Macros for general use					HTUtils.h
**
**	See also: the system dependent file "tcp.h"
*/

#ifndef HEADERS_H
#define HEADERS_H

#ifdef NOT  /* ignore - defined previously */
#define PUBLIC			/* Accessible outside this module     */
#define PRIVATE static		/* Accessible only within this module */
#endif NOT

#ifdef __STDC__ 
#define CONST const		/* "const" only exists in STDC */
#define NOPARAMS (void)
#define PARAMS(parameter_list) parameter_list
#define NOARGS (void)

#ifdef NOT  /* ignore ARGS1 - ARGS10 */
#define ARGS1(t,a) \
		(t a)
#define ARGS2(t,a,u,b) \
		(t a, u b)
#define ARGS3(t,a,u,b,v,c) \
		(t a, u b, v c)
#define ARGS4(t,a,u,b,v,c,w,d) \
		(t a, u b, v c, w d)
#define ARGS5(t,a,u,b,v,c,w,d,x,e) \
		(t a, u b, v c, w d, x e)
#define ARGS6(t,a,u,b,v,c,w,d,x,e,y,f) \
		(t a, u b, v c, w d, x e, y f)
#define ARGS7(t,a,u,b,v,c,w,d,x,e,y,f,z,g) \
		(t a, u b, v c, w d, x e, y f, z g)
#define ARGS8(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h) \
		(t a, u b, v c, w d, x e, y f, z g, s h)
#define ARGS9(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h,r,i) \
		(t a, u b, v c, w d, x e, y f, z g, s h, r i)
#define ARGS10(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h,r,i,q,j) \
		(t a, u b, v c, w d, x e, y f, z g, s h, r i, q j)
#endif NOT  /* ignore ARGS1 - ARGS10 */
#define ARGS11(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h,r,i,q,j,p,k) \
		(t a, u b, v c, w d, x e, y f, z g, s h, r i, q j, p k)
#define ARGS12(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h,r,i,q,j,p,k,n,l) \
		(t a, u b, v c, w d, x e, y f, z g, s h, r i, q j, p k, n l)
#define ARGS13(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h,r,i,q,j,p,k,n,l,m2,m) \
	  (t a, u b, v c, w d, x e, y f, z g, s h, r i, q j, p k, n l, m2 m)

#else  /* not ANSI */

#define CONST
#define NOPARAMS ()
#define PARAMS(parameter_list) ()
#define NOARGS ()

#ifdef NOT  /* ignore ARGS1 - ARGS10 */
#define ARGS1(t,a) (a) \
		t a;
#define ARGS2(t,a,u,b) (a,b) \
		t a; u b;
#define ARGS3(t,a,u,b,v,c) (a,b,c) \
		t a; u b; v c;
#define ARGS4(t,a,u,b,v,c,w,d) (a,b,c,d) \
		t a; u b; v c; w d;
#define ARGS5(t,a,u,b,v,c,w,d,x,e) (a,b,c,d,e) \
		t a; u b; v c; w d; x e;
#define ARGS6(t,a,u,b,v,c,w,d,x,e,y,f) (a,b,c,d,e,f) \
		t a; u b; v c; w d; x e; y f;
#define ARGS7(t,a,u,b,v,c,w,d,x,e,y,f,z,g) (a,b,c,d,e,f,g) \
		t a; u b; v c; w d; x e; y f; z g;
#define ARGS8(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h) (a,b,c,d,e,f,g,h) \
		t a; u b; v c; w d; x e; y f; z g; s h;
#define ARGS9(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h,r,i) (a,b,c,d,e,f,g,h,i) \
		t a; u b; v c; w d; x e; y f; z g; s h; r i;
#define ARGS10(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h,r,i,q,j) (a,b,c,d,e,f,g,h,i,j) \
		t a; u b; v c; w d; x e; y f; z g; s h; r i; q j;
#endif NOT  /* ignore ARGS1 - ARGS10 */
#define ARGS11(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h,r,i,q,j,p,k) (a,b,c,d,e,f,g,h,i,j,k)\
		t a; u b; v c; w d; x e; y f; z g; s h; r i; q j; p k;
#define ARGS12(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h,r,i,q,j,p,k,n,l) (a,b,c,d,e,f,g,h,i,j,k,l)\
		t a; u b; v c; w d; x e; y f; z g; s h; r i; q j; p k; n l;
#define ARGS13(t,a,u,b,v,c,w,d,x,e,y,f,z,g,s,h,r,i,q,j,p,k,n,l,m2,m) (a,b,c,d,e,f,g,h,i,j,k,l,m)\
		t a; u b; v c; w d; x e; y f; z g; s h; r i; q j; p k; n l; m2 m;
		
	
#endif /* __STDC__ (ANSI) */

#ifndef NULL
#define NULL ((void *)0)
#endif

#endif HEADERS_H
