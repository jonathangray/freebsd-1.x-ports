/*
 * top - a top users display for Unix
 *
 * SYNOPSIS:  For a BSD/386 system
 *	      Note memory statistic and process sizes could be wrong,
 *	      but ps gets them wrong too...
 *
 * DESCRIPTION:
 * This is the machine-dependent module for BSD/386
 * Works for:
 *	hp300
 *	i386
 *
 * LIBS: -lkvm
 *
 * AUTHOR:  Christos Zoulas <christos@ee.cornell.edu>
 */

#include <sys/types.h>
#include <sys/signal.h>
#include <sys/param.h>

#include <stdio.h>
#include <nlist.h>
#include <math.h>
#ifdef __bsdi__
#include <sys/time.h>
#include <sys/proc.h>
#include <sys/vmmeter.h>
#endif
#include <kvm.h>
#include <sys/errno.h>
#include <sys/kinfo.h>
#include <sys/kinfo_proc.h>
#ifdef notyet
#define time __time
#define hz __hz
#include <sys/kernel.h>
#undef time
#undef hz
#endif
#include <sys/dir.h>
#ifdef __bsdi__
#include <sys/cpustats.h>
#include <sys/sysinfo.h>
#else
#include <sys/dkstat.h>
#endif
#include <sys/file.h>
#include <sys/time.h>


#define DOSWAP

#include "top.h"
#include "machine.h"

#ifdef __bsdi__
#define	VMUNIX	"/bsd"
#else
#define VMUNIX	"/vmunix"
#endif
#define KMEM	"/dev/kmem"
#define MEM	"/dev/mem"
#ifdef DOSWAP
#define SWAP	"/dev/drum"
#endif

/* get_process_info passes back a handle.  This is what it looks like: */

struct handle
{
    struct kinfo_proc **next_proc;	/* points to next valid proc pointer */
    int remaining;		/* number of pointers remaining */
};

/* declarations for load_avg */
#include "loadavg.h"

#define PP(pp, field) ((pp)->kp_proc . field)
#define EP(pp, field) ((pp)->kp_eproc . field)
#define VP(pp, field) ((pp)->kp_eproc.e_vm . field)

/* define what weighted cpu is.  */
#define weighted_cpu(pct, pp) (PP((pp), p_time) == 0 ? 0.0 : \
			 ((pct) / (1.0 - exp(PP((pp), p_time) * logcpu))))

/* what we consider to be process size: */
#define PROCSIZE(pp) (VP((pp), vm_tsize) \
		      + VP((pp), vm_dsize) \
		      + VP((pp), vm_ssize))

/* definitions for indices in the nlist array */
#define X_CCPU		0
#ifdef __bsdi__
#define X_TOTAL	1
#else
#define X_CP_TIME	1
#endif
#define X_HZ		2
#define X_AVENRUN	3

static struct nlist nlst[] = {
    { "_ccpu" },		/* 0 */
#ifdef __bsdi__
    { "_total" },		/* 1 */
#else
    { "_cp_time" },		/* 1 */
#endif
    { "_hz" },			/* 2 */
    { "_averunnable" },		/* 3 */
    { 0 }
};

/*
 *  These definitions control the format of the per-process area
 */

static char header[] =
  "  PID X        PRI NICE   SIZE   RES STATE   TIME   WCPU    CPU COMMAND";
/* 0123456   -- field to fill in starts at header+6 */
#define UNAME_START 6

#define Proc_format \
	"%5d %-8.8s %3d %4d%6dK %4dK %-5s%4d:%02d %5.2f%% %5.2f%% %.14s"


/* process state names for the "STATE" column of the display */
/* the extra nulls in the string "run" are for adding a slash and
   the processor number when needed */

static char *state_abbrev[] =
{
    "", "sleep", "WAIT", "run\0\0\0", "start", "zomb", "stop"
};


static kvm_t *kd;

/* values that we stash away in _init and use in later routines */

static double logcpu;

/* these are retrieved from the kernel in _init */

static long	hz;
static load_avg	ccpu;
static int	ncpu = 0;

/* these are offsets obtained via nlist and used in the get_ functions */

#ifdef __bsdi__
static unsigned long total_offset;
#else
static unsigned long cp_time_offset;
#endif
static unsigned long avenrun_offset;

#ifndef __bsdi__
/* these are for calculating cpu state percentages */

static u_long cp_time[CPUSTATES];
static u_long cp_old[CPUSTATES];
static u_long cp_diff[CPUSTATES];
#endif

/* these are for detailing the process states */

static int process_states[7];
static char *procstatenames[] = {
    "", " sleeping, ", " ABANDONED, ", " running, ", " starting, ",
    " zombie, ", " stopped, ",
    NULL
};

/* these are for detailing the cpu states */

static int cpu_states[CPUSTATES];
static char *cpustatenames[CPUSTATES+1] = {
    "user", "nice", "system", "idle", NULL
};

/* these are for detailing the memory statistics */

static int memory_stats[8];
static char *memorynames[] = {
    "Real: ", "K/", "K ", "Virt: ", "K/",
    "K ", "Free: ", "K", NULL
};

/* these are for keeping track of the proc array */

static int bytes;
static int nproc;
static int onproc = -1;
static int pref_len;
static struct kinfo_proc *pbase;
static struct kinfo_proc **pref;

/* these are for getting the memory statistics */

static int pageshift;		/* log base 2 of the pagesize */

/* define pagetok in terms of pageshift */

#define pagetok(size) ((size) << pageshift)

/* useful externals */
long percentages();

machine_init(statics)

struct statics *statics;

{
    register int i = 0;
    register int pagesize;

    if ((kd = kvm_open(VMUNIX, MEM, SWAP, O_RDONLY, "kvm_open")) == NULL)
	return -1;


    /* get the list of symbols we want to access in the kernel */
    (void) kvm_nlist(kd, nlst);
    if (nlst[0].n_type == 0)
    {
	fprintf(stderr, "top: nlist failed\n");
	return(-1);
    }

    /* make sure they were all found */
    if (i > 0 && check_nlist(nlst) > 0)
    {
	return(-1);
    }

    /* get the symbol values out of kmem */
    (void) getkval(nlst[X_HZ].n_value,     (int *)(&hz),	sizeof(hz),
	    nlst[X_HZ].n_name);
    (void) getkval(nlst[X_CCPU].n_value,   (int *)(&ccpu),	sizeof(ccpu),
	    nlst[X_CCPU].n_name);

    /* stash away certain offsets for later use */
#ifdef __bsdi__
    total_offset = nlst[X_TOTAL].n_value;
#else
    cp_time_offset = nlst[X_CP_TIME].n_value;
#endif
    avenrun_offset = nlst[X_AVENRUN].n_value;

    /* this is used in calculating WCPU -- calculate it ahead of time */
    logcpu = log(loaddouble(ccpu));

    pbase = NULL;
    pref = NULL;
    nproc = 0;
    onproc = -1;
    /* get the page size with "getpagesize" and calculate pageshift from it */
    pagesize = getpagesize();
    pageshift = 0;
    while (pagesize > 1)
    {
	pageshift++;
	pagesize >>= 1;
    }

    /* we only need the amount of log(2)1024 for our conversion */
    pageshift -= LOG1024;

    /* fill in the statics information */
    statics->procstate_names = procstatenames;
    statics->cpustate_names = cpustatenames;
    statics->memory_names = memorynames;

    /* all done! */
    return(0);
}

char *format_header(uname_field)

register char *uname_field;

{
    register char *ptr;

    ptr = header + UNAME_START;
    while (*uname_field != '\0')
    {
	*ptr++ = *uname_field++;
    }

    return(header);
}

get_system_info(si)

struct system_info *si;

{
    register u_long total;
    load_avg avenrun[3];
#ifdef __bsdi__
    struct cpustats cpu;
    struct sysinfo sys;
    int size;
#else
    load_avg *avenrunp = avenrun;
#endif

    /* get the various high-level data structures */
#ifdef __bsdi__
    size = sizeof(struct cpustats);
    if (getkerninfo(KINFO_CPU, &cpu, &size, 0) < 0) {
	perror("getkerninfo#1");
	abort();
    }
#ifdef notyet
    size = sizeof(struct sysinfo);
    if (getkerninfo(KINFO_SYSINFO, &sys, &size, 0) < 0) {
	perror("getkerninfo#2");
	abort();
    }
#endif /*notyet*/
#else
    (void) getkval(cp_time_offset, (int *)cp_time, sizeof(cp_time),
		   "_cp_time");
    (void) getkval(avenrun_offset, (int *)avenrun, sizeof(avenrun),
		   "_avenrun");
#endif

    /* convert load averages to doubles */
    {
	register int i;
	register double *infoloadp = si->load_avg;

	for (i = 0; i < CPUSTATES; i++)
	{
#ifdef __bsdi__
	    *infoloadp++ = ((double) cpu.cp_averunnable[i]) / FSCALE;
#else
	    *infoloadp++ = loaddouble(*avenrunp++);
#endif
	}
    }

    /* convert cp_time counts to percentages */
#ifdef __bsdi__
    {
	register int i;
	register double total, pct;

	total = 0.0;
	for (i = 0; i < CPUSTATES; i++)
		total += (double) cpu.cp_time[i];
	if (total == 0)
		pct = 0;
	else
		pct = 100 / total;
	for (i = 0; i < CPUSTATES; i++)
		cpu_states[i] = 10.0 * ((double)cpu.cp_time[i]) * pct;
    }
#else
    total = percentages(CPUSTATES, cpu_states, cp_time, cp_old, cp_diff);
#endif

    /* sum memory statistics */
    {
	struct vmtotal total;
	int size;

#ifdef __bsdi__
	(void) getkval(total_offset, (int*)&total, sizeof(total),
		       "_total");
#else
	/* get total -- systemwide main memory usage structure */
	size = sizeof(struct vmtotal);
	getkerninfo(KINFO_METER, &total, &size, 0);
#endif
	/* convert memory stats to Kbytes */
	memory_stats[0] = -1;
	memory_stats[1] = pagetok(total.t_arm);
	memory_stats[2] = pagetok(total.t_rm);
	memory_stats[3] = -1;
	memory_stats[4] = pagetok(total.t_avm);
	memory_stats[5] = pagetok(total.t_vm);
	memory_stats[6] = -1;
	memory_stats[7] = pagetok(total.t_free);
    }

    /* set arrays and strings */
    si->cpustates = cpu_states;
    si->memory = memory_stats;
    si->last_pid = -1;
}

static struct handle handle;

caddr_t get_process_info(si, sel, compare)

struct system_info *si;
struct process_select *sel;
int (*compare)();

{
    register int i;
    register int total_procs;
    register int active_procs;
    register struct kinfo_proc **prefp;
    register struct kinfo_proc *pp;

    /* these are copied out of sel for speed */
    int show_idle;
    int show_system;
    int show_uid;
    int show_command;

    
    pbase = kvm_getprocs(kd, KINFO_PROC_ALL, 0, &nproc);
    if (nproc > onproc)
	pref = (struct kinfo_proc **) realloc(pref, sizeof(struct kinfo_proc *)
		* (onproc = nproc));
    if (pref == NULL || pbase == NULL) {
	(void) fprintf(stderr, "top: Out of memory.\n");
	quit(23);
    }
    /* get a pointer to the states summary array */
    si->procstates = process_states;

    /* set up flags which define what we are going to select */
    show_idle = sel->idle;
    show_system = sel->system;
    show_uid = sel->uid != -1;
    show_command = sel->command != NULL;

    /* count up process states and get pointers to interesting procs */
    total_procs = 0;
    active_procs = 0;
    memset((char *)process_states, 0, sizeof(process_states));
    prefp = pref;
    for (pp = pbase, i = 0; i < nproc; pp++, i++)
    {
	/*
	 *  Place pointers to each valid proc structure in pref[].
	 *  Process slots that are actually in use have a non-zero
	 *  status field.  Processes with SSYS set are system
	 *  processes---these get ignored unless show_sysprocs is set.
	 */
	if (PP(pp, p_stat) != 0 &&
	    (show_system || ((PP(pp, p_flag) & SSYS) == 0)))
	{
	    int p_stat = PP(pp, p_stat);

	    total_procs++;
	    if (p_stat < 1 || p_stat > 6)
		abort();
	    process_states[p_stat]++;
	    if ((PP(pp, p_stat) != SZOMB) &&
		(show_idle || (PP(pp, p_pctcpu) != 0) || 
		 (PP(pp, p_stat) == SRUN)) &&
		(!show_uid || EP(pp, e_pcred.p_ruid) == (uid_t)sel->uid))
	    {
		*prefp++ = pp;
		active_procs++;
	    }
	}
    }

    /* if requested, sort the "interesting" processes */
    if (compare != NULL)
    {
	qsort((char *)pref, active_procs, sizeof(struct kinfo_proc *), compare);
    }

    /* remember active and total counts */
    si->p_total = total_procs;
    si->p_active = pref_len = active_procs;

    /* pass back a handle */
    handle.next_proc = pref;
    handle.remaining = active_procs;
    return((caddr_t)&handle);
}

char fmt[128];		/* static area where result is built */

char *format_next_process(handle, get_userid)

caddr_t handle;
char *(*get_userid)();

{
    register struct kinfo_proc *pp;
    register long cputime;
    register double pct;
    int where;
    struct handle *hp;

    /* find and remember the next proc structure */
    hp = (struct handle *)handle;
    pp = *(hp->next_proc++);
    hp->remaining--;
    

    /* get the process's user struct and set cputime */
    if ((PP(pp, p_flag) & SLOAD) == 0) {
	/*
	 * Print swapped processes as <pname>
	 */
	char *comm = PP(pp, p_comm);
#define COMSIZ sizeof(PP(pp, p_comm))
	char buf[COMSIZ];
	(void) strncpy(buf, comm, COMSIZ);
	comm[0] = '<';
	(void) strncpy(&comm[1], buf, COMSIZ - 2);
	comm[COMSIZ - 2] = '\0';
	(void) strncat(comm, ">", COMSIZ - 1);
	comm[COMSIZ - 1] = '\0';
    }

    cputime = PP(pp, p_utime.tv_sec) + PP(pp, p_stime.tv_sec);

    /* calculate the base for cpu percentages */
    pct = pctdouble(PP(pp, p_pctcpu));

    /* format this entry */
    sprintf(fmt,
	    Proc_format,
	    PP(pp, p_pid),
	    (*get_userid)(EP(pp, e_pcred.p_ruid)),
	    PP(pp, p_pri) - PZERO,
	    PP(pp, p_nice) - NZERO,
	    pagetok(PROCSIZE(pp)),
	    pagetok(VP(pp, vm_rssize)),
	    state_abbrev[PP(pp, p_stat)],
	    cputime / 60l,
	    cputime % 60l,
	    100.0 * weighted_cpu(pct, pp),
	    100.0 * pct,
	    printable(PP(pp, p_comm)));

    /* return the result */
    return(fmt);
}


/*
 * check_nlist(nlst) - checks the nlist to see if any symbols were not
 *		found.  For every symbol that was not found, a one-line
 *		message is printed to stderr.  The routine returns the
 *		number of symbols NOT found.
 */

int check_nlist(nlst)

register struct nlist *nlst;

{
    register int i;

    /* check to see if we got ALL the symbols we requested */
    /* this will write one line to stderr for every symbol not found */

    i = 0;
    while (nlst->n_name != NULL)
    {
	if (nlst->n_type == 0)
	{
	    /* this one wasn't found */
	    fprintf(stderr, "kernel: no symbol named `%s'\n", nlst->n_name);
	    i = 1;
	}
	nlst++;
    }

    return(i);
}


/*
 *  getkval(offset, ptr, size, refstr) - get a value out of the kernel.
 *	"offset" is the byte offset into the kernel for the desired value,
 *  	"ptr" points to a buffer into which the value is retrieved,
 *  	"size" is the size of the buffer (and the object to retrieve),
 *  	"refstr" is a reference string used when printing error meessages,
 *	    if "refstr" starts with a '!', then a failure on read will not
 *  	    be fatal (this may seem like a silly way to do things, but I
 *  	    really didn't want the overhead of another argument).
 *  	
 */

getkval(offset, ptr, size, refstr)

unsigned long offset;
int *ptr;
int size;
char *refstr;

{
    if (kvm_read(kd, offset, (char *) ptr, size) != size)
    {
	if (*refstr == '!')
	{
	    return(0);
	}
	else
	{
	    fprintf(stderr, "top: kvm_read for %s: %s\n",
		refstr, strerror(errno));
	    quit(23);
	}
    }
    return(1);
}
    
/* comparison routine for qsort */

/*
 *  proc_compare - comparison function for "qsort"
 *	Compares the resource consumption of two processes using five
 *  	distinct keys.  The keys (in descending order of importance) are:
 *  	percent cpu, cpu ticks, state, resident set size, total virtual
 *  	memory usage.  The process states are ordered as follows (from least
 *  	to most important):  WAIT, zombie, sleep, stop, start, run.  The
 *  	array declaration below maps a process state index into a number
 *  	that reflects this ordering.
 */

static unsigned char sorted_state[] =
{
    0,	/* not used		*/
    3,	/* sleep		*/
    1,	/* ABANDONED (WAIT)	*/
    6,	/* run			*/
    5,	/* start		*/
    2,	/* zombie		*/
    4	/* stop			*/
};
 
proc_compare(pp1, pp2)

struct kinfo_proc **pp1;
struct kinfo_proc **pp2;

{
    register struct kinfo_proc *p1;
    register struct kinfo_proc *p2;
    register int result;
    register pctcpu lresult;

    /* remove one level of indirection */
    p1 = *pp1;
    p2 = *pp2;

    /* compare percent cpu (pctcpu) */
    if ((lresult = PP(p2, p_pctcpu) - PP(p1, p_pctcpu)) == 0)
    {
	/* use cpticks to break the tie */
	if ((result = PP(p2, p_cpticks) - PP(p1, p_cpticks)) == 0)
	{
	    /* use process state to break the tie */
	    if ((result = sorted_state[PP(p2, p_stat)] -
			  sorted_state[PP(p1, p_stat)])  == 0)
	    {
		/* use priority to break the tie */
		if ((result = PP(p2, p_pri) - PP(p1, p_pri)) == 0)
		{
		    /* use resident set size (rssize) to break the tie */
		    if ((result = VP(p2, vm_rssize) - VP(p1, vm_rssize)) == 0)
		    {
			/* use total memory to break the tie */
			result = PROCSIZE(p2) - PROCSIZE(p1);
		    }
		}
	    }
	}
    }
    else
    {
	result = lresult < 0 ? -1 : 1;
    }

    return(result);
}

/*
 * proc_owner(pid) - returns the uid that owns process "pid", or -1 if
 *		the process does not exist.
 *		It is EXTREMLY IMPORTANT that this function work correctly.
 *		If top runs setuid root (as in SVR4), then this function
 *		is the only thing that stands in the way of a serious
 *		security problem.  It validates requests for the "kill"
 *		and "renice" commands.
 */

int proc_owner(pid)

int pid;

{
    register int cnt;
    register struct kinfo_proc **prefp;
    register struct kinfo_proc *pp;

    prefp = pref;
    cnt = pref_len;
    while (--cnt >= 0)
    {
	pp = *prefp++;	
	if (PP(pp, p_pid) == (pid_t)pid)
	{
	    return((int)EP(pp, e_pcred.p_ruid));
	}
    }
    return(-1);
}
