/* run.c
 *
 * COPYRIGHT (c) 1989 by AT&T Bell Laboratories.
 */

/*  7Dec92  e  ported to Macintosh/THINK_C  */

#include <stdio.h>
#include "ml_os.h"
#ifdef THINK_C
#include <unix.h>
#include <fcntl.h>
#else
#include <sys/stat.h>
#ifdef SOLARIS
#include <fcntl.h>
#else
#include <sys/file.h>
#endif
#endif
#include <signal.h>
#ifdef AUX
#include <compat.h>
#endif

#include "ml_state.h"
#include "ml_types.h"
#include "tags.h"
#include "prim.h"

extern ML_val_t cstruct[];

static ML_val_t load();
static void enroll();

extern int new_size;
extern int resettimers();
extern MLState_ptr mp_init();
extern ML_val_t apply_ml_fn();

#if defined(M68) || defined(C)
extern ML_val_t mathvec[];
#endif

extern ML_val_t gcmessages0[], ratio0[], softmax0[], pstruct0[];
#define gcmessages (gcmessages0[1])
#define pstruct (pstruct0[1])
#define ratio (ratio0[1])
#define softmax (softmax0[1])

int		isExported = 0;
char		**global_argv;

#ifdef THINK_C
extern int 	mac_init( char ***p_argv );
extern void 	nalert(char *);
extern void 	restarter(char *);
extern char 	*prefix;
char  		*imageName = 0;
#endif

main (argc, argv)
    int		argc; 
    char	*argv[];
{
    ML_val_t	    argname, perv, core, math, loader, obj;
    MLState_ptr     msp;
    char	    **p = argv+1;
    int		    xflag = 0;

#ifdef AUX
    setcompat (COMPAT_BSD & ~COMPAT_EXEC);
#endif

#ifdef THINK_C
    argc = mac_init(&argv);
    p = argv+1;
#endif

    global_argv = argv;

    if (isExported)
	restart_ml();

    gcmessages	= INT_CtoML(2);
    ratio	= INT_CtoML(5);
    softmax	= INT_CtoML(1024*1024*100);
    while (*p && **p == '-') {
	switch (p[0][1]) {
	  case 'h':
	    if (p[1]) {
		new_size = 1024*atoi(p[1]);
		p+=2;
	    }
	    else
		quit("no -h value");
	    break;
	  case 'r':
	    if (p[1]) {
		int r = atoi(p[1]);
		p += 2;
		if (r < 3)
		    quit ("bad -r value");
		ratio = INT_CtoML(r);
	    }
	    else
		quit("no -r value");
	    break;
	  case 'm':
	    if (p[1]) {
		softmax = INT_CtoML(1024*atoi(p[1]));
		p+=2;
	    }
	    else
		quit("no -m value");
	    break;
	  case 'g':
	    if (p[1]) {
	 	gcmessages0[1] = INT_CtoML(atoi(p[1]));
		p+=2;
	    }
	    else
		quit("no -g value");
	    break;
	  case 'x':
	    xflag = 1;  p++;
	    break;
	  case 'y':
	    xflag = 2;  p++;
	    break;
	  case 'z':
	    xflag = 3;  p++;
	    break;
#ifdef THINK_C
	  case 'i':
	    if (p[1]) {
	    	imageName = p[1];
	    	isExported = 1;
	    	p+=2;
	    }
	    else
		quit("no -i value");
	    break;
	  case 'd':
	    if (p[1]) {
	    	prefix = p[1];
	    	p+=2;
	    }
	    else
		quit("no -d value");
	    break;
#endif
	} /* end of switch */
    } /* end of while */

#ifdef THINK_C
    if(isExported) restarter(imageName);
    /* else... */
#endif

#if (!defined(C))
    if ((*p == NULL) && (xflag == 0))
	quit("no file to execute\n");
#endif
    msp = mp_init(FALSE);
    setup_signals (msp, TRUE);
    resettimers (msp);
#ifdef THINK_C
    chatting("[Initializing memory...]\n");
#endif
    init_gc (msp);
    init_externlist ();

    perv = load(msp, ML_alloc_string(msp, "CoreFunc"));
    enroll (ML_alloc_string(msp, "Core"), 
	    core = apply_ml_fn(msp, perv, PTR_CtoML(cstruct+1)));
#if defined(M68) || defined(C)
    math = PTR_CtoML(mathvec+1);
    enroll (ML_alloc_string(msp, "Math"), math);
#else
    math = load(msp, ML_alloc_string(msp, "Math"));
#endif
    perv = pstruct = load(msp, ML_alloc_string(msp, "Initial"));
    if (xflag==1) {
	chatting("Result is %#x\n", REC_SELINT(perv, 0));
	_exit(0);
    }

    loader = load(msp, ML_alloc_string(msp, "Loader"));
    if (xflag==3) {
	chatting("Result is %#x\n", REC_SELINT(loader, 0));
	_exit(0);
    }

#if (!defined(C))
    argname = ML_alloc_string(msp, *p);
#else
    argname = ML_alloc_string(msp, "bogus");
#endif
    REC_ALLOC4 (msp, obj, core, perv, math, argname);
    apply_ml_fn (msp, loader, obj);

    mp_shutdown(msp, 0);
}


/** The table of objects we need to boot. **/

static struct {
    ML_val_t	    name;	/* an ML string */
    ML_val_t	    obj;
} objtbl[10];
int objcount;

/* enroll:
 * Add the (name, obj) pair to the object table.
 */
static void enroll (name, obj)
    ML_val_t	    name, obj;
{
    objtbl[objcount].name = name;
    objtbl[objcount].obj  = obj;
    objcount++;
}

/* lookup:
 * Search for name in the object table, return the corresponding object or 0.
 */
static ML_val_t lookup (name)
    ML_val_t	    name;
{
    int		    i;

    for (i = 0;  i < objcount;  i++) {
	if (ML_eqstr(objtbl[i].name, name))
	    return objtbl[i].obj;
    }
    return 0;
}


/* openread:
 * Return a pointer to the code for the specified structure.  If the structure
 * is not in the data list, then read it into the heap from its ".mo" file.
 */
static ML_val_t openread (msp, s)
    MLState_ptr     msp;
    char	    *s;
{
    int		    fd = -1, i, len;
    register char   *p, *q;
    ML_val_t	    ss = ML_alloc_string(msp, s);
    ML_val_t	    d;

  /* search the datalist for the file */
    for (d = PTR_CtoML(datalist+1);  d != MOLST_nil;  d = MOLST_next(d)) {
	if (ML_eqstr(ss, MOLST_name(d)))
	    return MOLST_closure(d);
    }

  /* not in the datalist, so open the file */
#if defined(HPUX)
    fd = open(s, 0);
#else
#ifdef THINK_C
    fd = eopen(s, O_RDONLY);
#else
    fd = open(s, O_RDONLY, 0666);
#endif
#endif
    if (fd < 0)
	quit("cannot open %s\n",s);

  /* get the file length */
#ifdef THINK_C
	len = lseek(fd, 0L, SEEK_END);
	if (len == EOF)
	    quit("cannot lseek %s\n", s);
	lseek(fd, 0L, SEEK_SET);
#else
    {
	struct stat     buf;

	if (fstat(fd, &buf) == -1)
	    quit("cannot stat %s\n", s);
	len = (buf.st_size + 4);
    }
#endif THINK_C

  /* check the available space */
    if ((msp->ml_allocptr + len) > msp->ml_limitptr)
	quit("insufficient space to load %s\n", s);

  /* allocate and initialize the code string in the heap */
    p = (char *)(msp->ml_allocptr);
    msp->ml_allocptr += sizeof(int);  /* space for descriptor */
    while ((i = read(fd, (char *)(msp->ml_allocptr), len)) > 0)
	msp->ml_allocptr += i;
    if (i == -1)
	quit("error reading %s\n", s);
#ifdef THINK_C
    close(fd);
#endif THINK_C
    q = (char *)(msp->ml_allocptr);
    msp->ml_allocptr = ((((int)q) + 3) & ~3);
    *(int*)p = MAKE_DESC(q - (p + 4), TAG_string);

  /* flush the instruction cache */
    FlushICache (((int)p) + 4, len);

    REC_ALLOC1 (msp, d, PTR_CtoML((long)p + 8));
    return d;

} /* end of openread */


/* loadlist:
 */
static ML_val_t loadlist (msp, names)
    MLState_ptr     msp;
    ML_val_t	    names;
{
    if (names == ML_nil)
	return ML_nil;
    else {
	ML_val_t	obj  = load(msp, ML_hd(names));
	ML_val_t	rest = loadlist(msp, ML_tl(names));
	return ML_cons (msp, obj, rest);
    }
} /* end of loadlist */

#ifdef THINK_C
/*
#define LOADNAME_PREFIX "Wren:LightSpeedC D:smlnj D:mo.m68:"
*/
#define LOADNAME_PREFIX "mo/"
#else
#define LOADNAME_PREFIX "mo/"
#endif
#define LOADNAME_P_SIZE (sizeof(LOADNAME_PREFIX)-1)

/* load:
 */
static ML_val_t load (msp, name)
    MLState_ptr     msp;
    ML_val_t	    name;
{
    ML_val_t	    p, args;
    char	    buf[64];

    if (p = lookup(name))
	return p;
    else {
	strcpy (buf, "mo/");
	strncpy (buf+3, (char *)PTR_MLtoC(name), OBJ_LEN(name));
	strcpy (buf+3+OBJ_LEN(name), ".mo");

#if (!defined(C))
	chatting("[Loading %s]\n", buf);
	p = openread(msp, buf);
	p = apply_ml_fn (msp, p, ML_unit);
	chatting("[Executing %s]\n",buf);
#else
	p = openread(msp, buf);
	REC_ALLOC1 (msp, p, PTR_CtoML(p))
	p = apply_ml_fn (msp, p, ML_unit);
#endif
	args = loadlist (msp, ((int*)p)[1]);
#if 0
        p = REC_SEL(apply_ml_fn(msp, REC_SEL(p, 0), args), 0);
#else
        p = /*REC_SEL(*/apply_ml_fn(msp, REC_SEL(p, 0), args)/*, 0)*/;
#endif
	enroll (name, p);

	return p;
    }
} /* end of load */


int quit (s, a, b, c, d, e, f)
    char *s;
{
    char dbuf[1024];
    sprintf(dbuf, s, a, b, c, d, e, f);
    write(2, dbuf, strlen(dbuf));
    mp_shutdown (find_self(),2);
}

int die (s, a, b, c, d, e, f)
    char *s;
{
    char dbuf[1024];
    sprintf(dbuf, s, a, b, c, d, e, f);
    write(2, dbuf, strlen(dbuf));
    /* abort(); */
#ifdef THINK_C
    nalert(dbuf);
#endif
    mp_shutdown (find_self(),3);
}

int chatting (s, a, b, c, d, e, f, g)
    char *s;
{
    char dbuf[1024];
    sprintf(dbuf, s, a, b, c, d, e, f, g);
    write(2, dbuf, strlen(dbuf));
}

#ifdef MP_DEBUG
int pchatting (msp, s, a, b, c, d, e, f, g)
     MLState_ptr msp;
     char *s;
{
    char dbuf[1024];
    int offset;
    extern ML_val_t gcmessages;

    if (gcmessages >= INT_CtoML(4)) {
      offset = sprintf(dbuf, "%d:", msp->self);
      sprintf(dbuf+offset, s, a, b, c, d, e, f, g);
      write(2, dbuf, strlen(dbuf));
    }
}
#endif MP_DEBUG
