/*
 * readnews
 *
 *	Michael Rourke (UNSW) April 1984
 */

#include "defs.h"

#define ARTSEP "/"

char admsub[BUFLEN]	 = "general";
char dfltsub[BUFLEN]	 = "general";
char mailvia[BUFLEN]	 = "";
char *mailpath	 = MAIL;

#define	MAXARGV	10		/* used in building argv[]s below */

bool iflag;		/* -i ignore .newsrc */
bool lflag;		/* -l print headers only */
bool cflag;		/* -c check for news only */
bool pflag;		/* -p print everything selected */
bool Cflag;		/* -C verbose -c */
bool sflag;		/* -s print newsgroup subscription list */
bool splus;		/* -s+ */
bool slistall;		/* -s? */
bool sminus;		/* -s- */
char *sarg;		/* arg to -s[+-] */
char *nflag;		/* -n newsgroups */
extern char *rcgrps;	/* -n newsgroups from newsrc file */
bool n_on_cline;	/* nflag set on command line */

extern newsrc	*rc;		/* internal .newsrc */

active *alist;		/* internal active list */

#if MANGRPS
char *mangrps;	/* mandatory subsciption list */
#endif

long now;		/* current time */
bool interrupt;		/* if interrupt hit */
char *newsdir;		/* %news */
bool su;		/* if super user (not used) */

applycom list(), check(), commands();
void *onintr();
bool ureject(), seen(), subs(), subsub();

#if MANGRPS
char *getmangrps();
#endif

char *progname;

main(argc, argv)
int argc;
char *argv[];
{
	char buf[BUFSIZ], *p;

	progname = argv[0];
	setbuf(stdout, buf);
	if (options(--argc, ++argv, true)) {
		(void) fprintf(stderr, "Usage: readnews [-n newsgroups] [-i] [-clpC] [-s[-+? [group]]]\n");
		exit(1);
	}
	now = time(&now);

	newsdir = newstr(fullartfile((char *)NULL));
	getctl();

	if (!iflag)
		readnewsrc();

	if (nflag)
		convgrps(nflag);
	else
		nflag = dfltsub;
	if (rcgrps)
		convgrps(rcgrps);
	if (!n_on_cline) {
#if MANGRPS
		int addsub();

		if (mangrps = getmangrps(pe.pw_cmask))
			applyng(mangrps, addsub, &nflag);
#endif
		if (!ngmatch(admsub, nflag))
			nflag = newstr3(admsub, NGSEPS, nflag);
	}
	if ((int) sflag + (int) lflag + (int) cflag + (int) pflag > 1)
		error("-clpsC flags are mutually exclusive.");

	/* user has private mailer? */
	if ((p = getenv("MAILER")) != NULL)
		mailpath = newstr(p);

	alist = readactive();

	if (sflag) {
		if (subs() && !iflag)
			writenewsrc(alist);
	} else if (lflag)
		apply(alist, nflag, list, false);
	else if (cflag)
		apply(alist, nflag, check, false);
	else {
		if (!pflag) {
			if (signal(SIGINT, SIG_IGN) != SIG_IGN)
				(void) signal(SIGINT, onintr);
			if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
				(void) signal(SIGQUIT, onintr);
		}
		apply(alist, nflag, commands, true);
		if (!iflag)
			writenewsrc(alist);
	}
	fflush(stdout);
	exit(0);
}

#if MANGRPS
/*
 * make a subscription list of all class groups the user belongs too
 * (mandatory subscription)
 */
char *
getmangrps(cmask)
char *cmask;
{
	static char *weekday[] = { 
		"mon", "tue", "wed", "thu", "fri" 	};
	register char **classes;
	register char *s, *end;
	register char *grp;
	register int i, size;
	extern char **getclasses();

	grp = NIL(char);
	if ((classes = getclasses(cmask)) == NIL(char *))
		error("Can't get classes.");
	while (*classes) {
		if (isdigit(**classes)) {
			/*
			 * trim string after numeric class
			 * if it is a day of the week
			 */
			s = *classes;
			while (isdigit(*s) || *s == '.')
				s++;
			if (*s) {
				end = s;
				while (isalpha(*end))
					end++;
				if (*end && end != s && end - s <= 3) {
					size = end - s;
					for (i = 0; i < 5; i++)
						if (CMPN(s, weekday[i], size) == 0)
							break;
					if (i != 5)
						*s = '\0';
				}
			}
		}
		grp = (grp? catstr2(grp, ",class.", *classes):
			newstr2("class.", *classes));
		classes++;
	}
	return grp;
}

/*
 * if newsgroup "ng" isn't subscribed to, add it to subscription list
 */
addsub(ng, slist)
char *ng;
char **slist;
{
	if (!ngmatch(ng, *slist))
		*slist = newstr3(ng, NGSEPS, *slist);
}

#endif

void *
onintr()
{
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		(void) signal(SIGINT, onintr);
	if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
		(void) signal(SIGQUIT, onintr);
	interrupt = true;
}

/*
 * process options
 * can be called from readnewsrc()
 */
options(argc, argv, cline)
int argc;
char *argv[];
bool cline;
{
	register char c;

	/* TODO: use getopt(3) */
	while (argc > 0) {
		if (argv[0][0] != '-')
			break;
		while (c = *(++(argv[0]))) {
			switch (c) {
			case 'n':
				if (cline)
					nflag = argv[1], n_on_cline = true;
				else {
					if (!n_on_cline)
						nflag = (nflag?
							catstr2(nflag, NGSEPS, argv[1]):
							newstr(argv[1]));
					rcgrps = (rcgrps?
						catstr2(rcgrps, NGSEPS, argv[1]):
						newstr(argv[1]));
				}
				argc--, argv++; 
				break;
			case 'i':
				iflag = true; 
				continue;
			case 's':
				sflag = true;
				switch (argv[0][1]) {
				case '\0':
					continue;
				case '+':
					splus = true; 
					break;
				case '?':
					slistall = true, ++(argv[0]); 
					continue;
				case '-':
					sminus = true; 
					break;
				default:
					argc = -1; 
					break;
				}
				if (argc > 0) {
					sarg = newstr(argv[1]);
					argc--, argv++;
				}
				break;
			case 'p':
				pflag = true; 
				continue;
			case 'l':
				lflag = true; 
				continue;
			case 'c':
				cflag = true; 
				continue;
			case 'C':
				cflag = Cflag = true; 
				continue;
			default:
				argc = -1; 
				break;
			}
			break;
		}
		argc--, argv++;
	}
	return argc != 0;
}

/*
 * subscription list handling
 * return true if newsrc is to be re-written
 */
bool
subs()
{
	register newsrc	*np;
	register active	*ap;
	register char *tmp, *com;
	register FILE *f;

	if (slistall) {
		(void) printf("Active newsgroups:\n");
		(void) fflush(stdout);
		/* possibly vestigial code here, old #ifdefs deleted */
		f = stdout;
		com = 0;		/* for lint */
		com = com;		/* for lint */
		for (ap = alist; ap; ap = ap->a_next)
			(void) fprintf(f, "%s\n", ap->a_name);
		return false;
	} else if (splus || sminus) {
		if (strpbrk(sarg, BADGRPCHARS)) {
			(void) printf("%s: Illegal char in newsgroup.\n", sarg);
			return false;
		}
		if (ngmatch(sarg, nflag)) {
			/*
			 * normally we subscribe, check for an exclusion
			 */
			for (np = rc; np; np = np->n_next)
				if (CMP(sarg, np->n_name) == 0)
					break;
			if (np) {
				/*
				 * altering subscribe flag is all
				 * we need to change
				 */
				np->n_subscribe = splus;
				return true;
			}
			if (sminus) {
				/*
				 * try deleting from sub list
				 */
				if (subsub(sarg, rcgrps))
					return true;
				/*
				 * add specific exclusion
				 */
				rcgrps = newstr4(rcgrps, NGSEPS, NEGS, sarg);
				return true;
			}
		} else if (splus) {
			/*
			 * we don't subscribe,
			 * try deleting !sarg first
			 */
			tmp = newstr2(NEGS, sarg);
			subsub(tmp, rcgrps);
			if (!ngmatch(sarg, rcgrps))
				/*
				 * didn't work, so add explicit subscription
				 */
				rcgrps = rcgrps? newstr3(rcgrps, NGSEPS, sarg):
					newstr(sarg);
			return true;
		}
	} else {
		(void) printf("Subscription list: %s", nflag);
		for (np = rc; np; np = np->n_next)
			if (!np->n_subscribe && ngmatch(np->n_name, nflag))
				(void) printf(",!%s", np->n_name);
		(void) printf("\n");
	}
	return false;
}


/*
 * try and delete group from subscription list
 * return true if successful
 */
bool
subsub(grp, slist)
char *grp;
char *slist;
{
	register char *delim;

	while (*slist) {
		if (delim = strchr(slist, NGSEPCHAR))
			*delim = '\0';
		if (CMP(grp, slist) == 0) {
			if (delim)
				(void) strcpy(slist, delim + 1);
			else if (slist[-1] == ',')
				slist[-1] = '\0';
			else
				slist[0] = '\0';
			return true;
		}
		if (delim)
			*delim = NGSEPCHAR, slist = delim + 1;
		else
			break;
	}
	return false;
}

char *
ltoa(l)
long l;
{
	static char buf[30];

	sprintf(buf, "%ld", l);
	return buf;
}

/*
 * list titles command (-l)
 */
applycom
list(ap, np)
active *ap;
newsrc *np;
{
	static active *lastap;
	static bool first = true;
	register char *fname;
	register FILE *f;
	header h;
	ino_t ino;

	np->n_last++;
	fname = convg(newstr5(newsdir, "/", ap->a_name, ARTSEP,
		ltoa(np->n_last)));
	ino = 0;
	f = fopen(fname, "r");
	free(fname);
	if (!f || seen(f, &ino))
		return next;
	gethead(f, &h);
	if (first) {
		(void) printf("News articles:\n");
		first = false;
	}
	if (lastap != ap)
		(void) printf("  %s:\n", ap->a_name);
	lastap = ap;
	(void) printf("    %-4d %s\n", np->n_last, h.h_subject);
	(void) fclose(f);
	freehead(&h);
	if (ino)
		seen(NIL(FILE), &ino);
	return next;
}

/*
 * check command (-c or -C)
 */
applycom
check(ap, np)
active *ap;
newsrc *np;
{
	static bool done;

	np->n_last++;
	if (Cflag) {
		register long num;

		if (!done)
			(void) printf("You have news:\n");
		done = true;
		num = ap->a_seq - np->n_last + 1;
		(void) printf("\t%s at most %ld article%s\n",
			ap->a_name, num, (num > 1? "s": ""));
		return nextgroup;
	} else {
		(void) printf("You have news.\n");
		fflush(stdout);
		exit(0);
		/* NOTREACHED */
	}
}

/*
 * normal command handler (or pflag)
 * commands:
 *
 * \n 		print current article
 * n 		go to next article
 * q		quit
 * r		reply
 * f 		followup
 * p 		postnews
 * N [newsgrp]	next newsgroup
 * s [file]	save
 * U		unsubscribe from group
 * !stuff	shell escape
 * number or .	go to number
 * - 		back to previous article (toggle)
 * x		quick exit
 * h		long header info
 * H		full header
 *
 * inside r, f or p:
 *	.e	edit
 *	.i	interpolate
 *	. or EOT terminate message
 *	.!comd	shell escape
 */
applycom
commands(ap, np, last, pushed)
active *ap;
newsrc *np;
bool last;
bool pushed;
{
	register char *com, *arg;
	register int c, size;
	register long i;
	register FILE *f;
	char *fname;
	header		h;
	newsrc		ntmp;
	ino_t		ino;
	bool printed, pheader, verbose, hadinterrupt;
	applycom	nextact;
	static char errmess[] = "Unknown command; type `?' for help.\n";
	static char form[]    = "%s: %s\n";
	static char savedsys[BUFSIZ / 2];
	static active	*lastap, *rlastap;
	static newsrc	lastn;
	static char number[20];
	static active	*wantap;
	extern char t_from[], t_subject[], t_date[];
	extern char t_newsgroups[], t_path[], t_sender[];
	extern char t_replyto[], t_organization[];
	extern active	*activep();

	if (last) {
		/*
		 * give user one last chance to
		 * see this article
		 */
		ap = rlastap;
		np = &lastn;
		wantap = NIL(active);
		if (!ap || pflag)
			return stop;
	} else if (wantap)
		/*
		 * doing an "n newsgroup" command
		 */
		if (wantap != ap)
			return nextgroup;
		else
			wantap = NULL;

	fname = convg(newstr5(newsdir, "/", ap->a_name, ARTSEP,
		ltoa(np->n_last + 1)));
	f = fopen(fname, "r");
	ino = 0;
	if (!f || !last && !pushed && seen(f, &ino)) {
		if (pushed)
			(void) printf("Article %ld (%s) no longer exists.\n",
				np->n_last + 1, ap->a_name);
		else
			np->n_last++;
		if (f)
			(void) fclose(f);
		free(fname);
		return next;
	}

	gethead(f, &h);

	(void) printf("\n");
	interrupt = hadinterrupt = verbose = false;
	if (last) {
		(void) printf("No more articles (press RETURN again to quit).\n");
		printed = pheader = true;
	} else
		printed = pheader = false;

	while (1) {
		if (lastap != ap) {
			size = strlen(ap->a_name) + sizeof("Newsgroup");
			for (i = 0; i < size; i++)
				(void) putc('-', stdout);
			(void) printf("\nNewsgroup %s\n", ap->a_name);
			for (i = 0; i < size; i++)
				(void) putc('-', stdout);
			(void) printf("\n\n");
		}
		lastap = ap;
		if (!pheader) {
			time_t itsdate;
			(void) printf("Article %ld of %ld (%s)",
				np->n_last + 1, ap->a_seq, ap->a_name);
			if (h.h_lines != 0)
				(void) printf(" (%s lines)", h.h_lines);
			if (h.h_date != NULL) {
				itsdate = atot(h.h_date);
				(void) printf(" %s", ctime(&itsdate));
			} else
				(void) printf(" %s", "<no date!>\n");
			(void) printf(form, t_subject, h.h_subject);
			(void) printf(form, t_from, h.h_from);
			if (verbose || pflag) {
				(void) printf(form, t_date, h.h_date);
				(void) printf(form, t_newsgroups, h.h_newsgroups);
				(void) printf(form, t_path, h.h_path);
				if (h.h_sender)
					(void) printf(form, t_sender, h.h_sender);
				if (h.h_replyto)
					(void) printf(form, t_replyto, h.h_replyto);
				if (h.h_organisation)
					(void) printf(form, t_organization, h.h_organisation);
				verbose = false;
			}
			pheader = true;
		}
		if (!pushed && number[0])
			/*
			 * just returned from a number command
			 * and have another to do
			 */
			com = number;
		else if (pflag)
			/*
			 * just print it
			 */
			com = "";
		else {
			(void) printf("? ");
			if (fflush(stdout) == EOF) {
				(void) printf("\n? ");
				(void) fflush(stdout);
			}
			interrupt = false;
			if ((com = mgets()) == NIL(char)) {
				if (interrupt)
					if (!hadinterrupt) {
						clearerr(stdin);
						(void) printf("Interrupt\n");
						hadinterrupt = true;
						interrupt = false;
						continue;
					}
					else
						exit(1);
				nextact = stop;
				break;
			}
			hadinterrupt = false;
		}
		if (*com == '!') {
			if (com[1] == '!') {
				(void) printf("!%s\n", savedsys);
				com = savedsys;
			} else
				com++;
			(void) fflush(stdout);
#ifdef F_SETFD
			(void) fcntl(fileno(f), F_SETFD, 1);	/* close on exec */
#endif
			(void) system(com);
			if (com != savedsys)
				strncpy(savedsys, com, sizeof(savedsys) - 1);
			(void) printf("!\n");
			if (!printed)
				pheader = false;
			continue;
		}
		/*
		 * check command syntax
		 */
		if (*com && !isdigit(*com) && com[1] && (!isspace(com[1]) ||
		    strchr("Nsm", *com) == NULL)) {
			(void) printf(errmess);
			continue;
		}
		if (c = *com) {
			arg = com;
			while (isspace(*++arg))
				;
		} else
			arg = NULL;
		switch (c) {
		case 0:
		case '.':
			if (!printed || c == '.') {
				if (pflag)
					(void) printf("\n");
				print(&h, f);
				if (pflag) {
					nextact = next;
					break;
				}
				printed = true;
				continue;
			}
		case 'n':			/* B compatible */
		case '+':
		case ';':
			nextact = next;
			break;
		case '?':
			help();
			continue;
		case 'r':
			reply(&h, fname);
			continue;
		case 'f':
			followup(&h, fname);
			continue;
		case 'p':
			pnews(ap->a_name);
			continue;
		case 'U':
			if (ngmatch(np->n_name, admsub)
#if MANGRPS
			 || ngmatch(np->n_name, mangrps))
#else
				)
#endif
				 {
					(void) printf(
					"Group \"%s\" can't be unsubscribed.\n",
						np->n_name);
					continue;
				}
			np->n_subscribe = false;
			nextact = nextgroup;
			break;
		case 'N':			/* B compatible */
			if (!*arg) {
				nextact = nextgroup;
				break;
			}
			if ((wantap = activep(arg)) == NIL(active)) {
				(void) printf("%s: non-existent newsgroup.\n", arg);
				continue;
			}
			if (!ngmatch(arg, nflag)) {
				(void) printf("%s: is not subscribed to!\n", arg);
				wantap = NULL;
				continue;
			}
			nextact = searchgroup;
			break;
		case 's':
			save(&h, f, arg);
			continue;
		case 'q':
			nextact = stop;
			break;
		case 'x':
			fflush(stdout);
			exit(0);
		case 'h':
			verbose = true;
			pheader = false;
			continue;
		case 'H':
			puthead(&h, stdout, printing);
			continue;
		case '-':
			if (pushed) {
				nextact = next;
				break;
			}
			if (!rlastap || !lastn.n_name) {
				(void) printf("Can't go back!\n");
				continue;
			}
			nextact = commands(rlastap, &lastn, false, true);
			/*
			 * number commands, after a "-" act on the
			 * group of the "-" command
			 */
			while (number[0]) {
				ntmp = lastn;
				ntmp.n_last = atol(number) - 1;
				number[0] = '\0';
				nextact = commands(rlastap, &ntmp, false, true);
			}
			if (nextact != next)
				break;
			(void) printf("\n");
			pheader = false;
			continue;
		default:
			if (isdigit(c)) {
/*				i = atol(arg);		*/
				i = c - '0';
				while (isdigit(*arg))
					i = i * 10 + *arg++ - '0';
			}
			if (!isdigit(c) || *arg != '\0') {
				(void) printf(errmess);
				continue;
			}
			number[0] = '\0';
			if (i < ap->a_low || i > ap->a_seq) {
				(void) printf(
				    "Articles in \"%s\" group range %ld to %ld.\n",
				    np->n_name, ap->a_low, ap->a_seq);
				continue;
			}
			if (pushed) {
				sprintf(number, "%ld", i);
				nextact = next;
				break;
			}
			ntmp = *np;
			ntmp.n_last = i - 1;
			if ((nextact = commands(ap, &ntmp, false, true)) != next)
				break;
			if (!number[0]) {
				(void) printf("\n");
				pheader = false;
			}
			continue;
		}
		break;
	}
	rlastap = ap;
	lastn = *np;
	if (!pushed && (nextact == next || printed)) {
		np->n_last++;
		if (ino)
			seen(NIL(FILE), &ino);
	}
	freehead(&h);
	(void) fclose(f);
	free(fname);
	return nextact;
}


/*
 * see if the article has links, if so have we seen it?
 * close file if we return true
 *
 * called twice,
 *	first (with f set) to test (and possibly set *ino)
 *	again to put *ino in memory
 */
bool
seen(f, ino)
FILE *f;
ino_t *ino;
{
	static int num;
	static ino_t	*ilist;
	struct stat statb;
	register int i;

	if (f) {
		if (fstat(fileno(f), &statb) != 0 || statb.st_nlink <= 1)
			return false;
		for (i = 0; i < num; i++)
			if (ilist[i] == statb.st_ino) {
				(void) fclose(f);
				return true;
			}
		*ino = statb.st_ino;
		return false;
	} else if (*ino) {
		num++;
		ilist = (ino_t * ) (ilist ? myrealloc((char *) ilist, (int) sizeof(ino_t) *
		    num) : myalloc((int) sizeof(ino_t)));
		ilist[num - 1] = *ino;
	}
	return true;
}


/*
 * print out help file
 */
help()
{
	register FILE	*f;
	register int c;
	register char *helppath;

	helppath = ctlfile("readnews.help");
	if ((f = fopen(helppath, "r")) == NIL(FILE)) {
		(void) printf("Can't open %s\n", helppath);
		return;
	}
	while ((c = getc(f)) != EOF)
		(void) putc(c, stdout);
	(void) fclose(f);
}

/*
 * reply to sender by mail
 */
/* ARGSUSED fname */
reply(hp, fname)
header *hp;
char *fname;
{
	char *argv[MAXARGV];
	register int argc;

	argc = 0;
	argv[argc++] = "mail";
#ifdef UNSWMAIL
	argv[argc++] = "-s";
	if ((argv[argc++] = getsubject(hp)) == NIL(char))
		return;
	argv[argc++] = "-i";
	argv[argc++] = fname;
#endif

	if ((argv[argc++] = getretaddr(hp)) == NIL(char)) {
		(void) printf("Can't work out an address!\n");
		return;
	}

	argv[argc++] = NIL(char);

	run(mailpath, argv, false);

	free(argv[argc - 2]);
}



/*
 * generate correct headers for a followup article
 * then call postnews.
 */
followup(hp, fname)
header *hp;
char *fname;
{
	char tmpf[50];
	register FILE *fo;
	char *s = getsubject(hp);

	if (s == NULL)
		return;
	(void) strcpy(tmpf, "/tmp/rfXXXXXX");
	(void) mktemp(tmpf);
	fo = fopen(tmpf, "w");
	if (fo == NULL)
		error("can't create `%s'", tmpf);
	fprintf(fo, "Newsgroups: %s\n", (hp->h_followupto) ? hp->h_followupto :
							hp->h_newsgroups);
	fprintf(fo, "Subject: %s\n", s);
	free(s);
	if (hp->h_references && hp->h_messageid)
		fprintf(fo, "References: %s %s\n", hp->h_references, hp->h_messageid);
	else if (hp->h_messageid)
		fprintf(fo, "References: %s\n", hp->h_messageid);
	(void) fclose(fo);

	s = newstr3(binfile("inject/postnews"), " -h ", tmpf);
	system(s);
	free(s);

	(void) unlink(tmpf);
}

/*
 * get correct "Subject: Re: .." line
 */
char *
getsubject(hp)
register header *hp;
{
	register char *s;

	if (!hp->h_subject) {
		(void) printf("Subject: Re: ");
		(void) fflush(stdout);
		if ((s = mgets()) == NIL(char) || !*s) {
			(void) printf("The Subject field is mandatory.\n");
			return NIL(char);
		}
		return newstr2("Re: ", s);
	} else if (CMPN(hp->h_subject, "Re: ", 4) != 0 && CMPN(hp->h_subject,
	     "re: ", 4) != 0)
		return newstr2("Re: ", hp->h_subject);
	else
		return newstr(hp->h_subject);
}


/*
 * run a command, optionally closing stdin
 */
run(com, argv, closein)
char *com;
char *argv[];
bool closein;
{
	int pid, status, r;

	switch (pid = fork()) {
	default:
		/* parent */
		break;
	case 0:
		/* child */
		if (closein)
			close(fileno(stdin));
		execvp(com, argv);
		error("can't exec %s", com);
		exit(1);

	case -1:
		error("can't fork");
	}

	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		(void) signal(SIGINT, SIG_IGN);
	if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
		(void) signal(SIGQUIT, SIG_IGN);

	while ((r = wait(&status)) != pid && r != -1)
		;

	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		(void) signal(SIGINT, onintr);
	if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
		(void) signal(SIGQUIT, onintr);
}

/*
 * call postnews
 */
pnews(group)
char *group;
{
	register char *s = newstr3(binfile("inject/postnews"), " ", group);
	system(s);
	free(s);
}

/*
 * save an article
 */
save(hp, f, s)
header *hp;
FILE *f;
char *s;
{
	register long pos;
	register int c;
	register char *cp;
	register FILE	*sf;
	register char *aname;
	long then;
	extern char *getenv();

	if (!*s) {
		if ((aname = getenv("HOME")) == NIL(char)) {
			(void) printf("No $HOME in environment.\n");
			return;
		}
		s = aname = newstr3(aname, "/", ARTICLES);
	} else
		aname = NIL(char);
	if ((sf = fopen(s, "a")) == NIL(FILE)) {
		(void) fprintf(stderr, "readnews: can't open %s\n", s);
		return;
	}
	if (aname)
		free(aname);
	pos = ftell(f);
	rewind(f);
	if (cp = strchr(hp->h_from, ' '))
		*cp = '\0';
	if (hp->h_date)
		then = atot(hp->h_date);
	else
		then = 0L;
	(void) fprintf(sf, "From %s %s", hp->h_from, ctime(then ? &then : &now));
	if (cp)
		*cp = ' ';
	while ((c = getc(f)) != EOF)
		(void) putc(c, sf);
	(void) putc('\n', sf);
	(void) fclose(sf);
	fseek(f, pos, 0);
}



/*
 * print an article, if it's long enough call page()
 */
/* ARGSUSED */
print(hp, f)
header *hp;
FILE *f;
{
	register int c;
	register long pos;

	pos = ftell(f);
	if (!pflag)
		page(f);
	else
		while ((c = getc(f)) != EOF)
			(void) putchar(c);
	(void) fseek(f, pos, 0);
}


/*
 * copy article text to stdout, and break into pages
 */
page(f)
FILE *f;
{
	register int c;
	register unsigned lineno;
	char lbuf[80];

	lineno = 1;
	while (!interrupt) {
		for (; lineno < PAGESIZE - 4 && !interrupt; lineno++) {
			while ((c = getc(f)) != EOF && c != '\n')
				(void) putchar(c);
			if (c == EOF)
				goto fastexit;
			if (lineno < PAGESIZE - 5)
				(void) putchar('\n');
		}
		if (interrupt)
			break;
		if (fflush(stdout) == EOF)
			break;
		if (read(fileno(stdin), lbuf, sizeof(lbuf)) <= 0)
			break;
		lineno = 0;
	}
	if (lineno)
		(void) putchar('\n');
	interrupt = false;
fastexit:	;
}

/* VARARGS1 */
error(s, a0, a1, a2, a3)
char *s;
{
	(void) fprintf(stderr, "readnews: ");
	(void) fprintf(stderr, s, a0, a1, a2, a3);
	(void) fprintf(stderr, "\n");
	fflush(stdout);		/* just on principle */
	exit(1);
}

/*
 - unprivileged - no-op needed to keep the pathname stuff happy
 */
void
unprivileged(reason)
char *reason;
{
}

/*
 - getctl - pick up control file for subscriptions etc.
 */
getctl()
{
	register FILE *f;
	register char *fname;
	char line[BUFLEN];
	register char *p;

	fname = ctlfile("readnews.ctl");
	f = fopen(fname, "r");
	if (f == NULL)
		return;

	while (fgets(line, sizeof(line), f) != NULL) {
		line[strlen(line)-1] = '\0';	/* dispose of newline */
		p = strchr(line, '\t');
		if (p == NULL)
			p = strchr(line, ' ');
		if (line[0] != '#' && p != NULL) {
			while (*p == ' ' || *p == '\t')
				*p++ = '\0';
			if (strcmp(line, "defsub") == 0)
				(void) strcpy(dfltsub, p);
			else if (strcmp(line, "mustsub") == 0)
				(void) strcpy(admsub, p);
			else if (strcmp(line, "mailvia") == 0)
				(void) strcpy(mailvia, p);
		}
	}

	(void) fclose(f);
}
