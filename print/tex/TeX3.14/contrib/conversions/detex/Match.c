/* COPYRIGHT (C) 1987 Kamal Al-Yahya */
#define IN_TM
#include   "setups.h"

Match(fp)			/* check matching */
FILE *fp;
{

int line=1;			/* line counter */
int ld=0;			/* single left dollar signs */
int rd=0;			/* single right dollar signs */
int ldd=0;			/* left double dollar signs */
int rdd=0;			/* right double dollar signs */
int disp=0;			/* \[  \] */
int disp_line=1;		/* line number of \[ */
int form=0;			/* \(  \) */
int lform=1;			/* line number of \( */
int lp=0;			/* left parenthesis */
int rp=0;			/* right parenthesis */
int lb=0;			/* left brackets */
int rb=0;			/* right brackets */
int lbr=0;			/* left braces */
int rbr=0;			/* right braces */
int c=' ';			/* current character */
int c1=' ';			/* previous character */
int lbrl=0;			/* line number of left braces */
int lbl=0;			/* line number of left bracket */
int lpl=0;			/* line number of left parenthesis */
int ldl=1;			/* line number of left single dollar sign */
int lddl=1;			/* line number of left double dollar sign */
int warn=0;			/* warning status */
int env_count = 0;		/* environment counter */
int i=0, j=0;
char w[MAXWORD];
char *p;
int cc;
extern char *malloc();

while ((c =getc(fp)) != EOF)
	{
	if (ldd == 1 && ld == 1 && c != '$')
		{
		fprintf(stderr,"line %d: a double dollar sign is closed by a single dollar sign\n",line);
		ld=0.;	ldd=0.;			/* reset dollar signs */
/* Give warning about unclosed openings */
		if ((lbr-rbr) > 0)
	fprintf(stderr,"line %d: %d unclosed braces in equation\n",lddl,lbr-rbr);
		if ((lb-rb) > 0)
	fprintf(stderr,"line %d: %d unclosed brackets in equation\n",lddl,lb-rb);
		if ((lp-rp) > 0)
	fprintf(stderr,"line %d: %d unclosed parentheses in equation\n",lddl,lp-rp);
/* clear registers */
		lp=0; lb=0; lbr=0;
		rp=0; rb=0; rbr=0;
		lpl=0; lbrl=0; lbl=0;
		}
	switch(c)
		{
		case '\n':
			line++;		/* increment line counter */
/* check to see if a single dollar sign is not closed at the same line */
			if (ld == 1 && warn == 0)
				{
				fprintf(stderr,"line %d: single dollar sign is not closed on the same line\n",line-1);
				warn=1;		/* warning has been given */
				}
			break;
		case '%':		/* ignore commented text */
			while ((c =getc(fp)) != EOF)
				if (c == '\n')	{line++;	break;}
			break;
		case '{':
			if (lbrl == 0)	lbrl=line;
			lbr++;
			break;
		case '}':
			rbr++;
			if (rbr > lbr)
				{
				fprintf(stderr,"line %d: unmatched brace\n",line);
				rbr--;		/* reset counter */
				}
			if (lbr == rbr)	lbrl=0;
			break;
		case '[':
			if (lbl == 0)	lbl=line;
			lb++;
			break;
		case ']':
			rb++;
			if (rb > lb)
				{
			     fprintf(stderr,"line %d: unmatched bracket\n",line);
				rb--;		/* reset counter */
				}
			if (lb == rb)	lbl=0;
			break;
		case '(':
			if (lpl == 0)	lpl=line;
			lp++;
			break;
		case ')':
			rp++;
			if (rp > lp)
			    {
			   fprintf(stderr,"line %d: unmatched parenthesis\n",line);
			    rp--;		/* reset counter */
			    }
			if (lp == rp)	lpl=0;
			break;
		case '$':
			if (c1 == '$')		/* double dollar sign */
				{
				if (ld == 0)
					{
					fprintf(stderr,"line %d: single dollar sign is closed by a duble dollar sign\n",line);
					c=' ';		/* reset the dollar sign */
					break;
					}
				if (ldd == 1)
					{
					rdd=1; /* right double dollar sign */
/* Give warning about unclosed openings */
					if ((lbr-rbr) > 0)
	fprintf(stderr,"line %d: %d unclosed braces in equation\n",lddl,lbr-rbr);
					if ((lb-rb) > 0)
	fprintf(stderr,"line %d: %d unclosed brackets in equation\n",lddl,lb-rb);
					if ((lp-rp) > 0)
	fprintf(stderr,"line %d: %d unclosed parentheses in equation\n",lddl,lp-rp);
/* clear registers */
					lp=0; lb=0; lbr=0;
					rp=0; rb=0; rbr=0;
					lpl=0; lbrl=0; lbl=0;
					}
				else
					{
					ldd=1;	/* left double dollar sign */
					lddl=line;	/* line number */
/* Give warning about unclosed openings */
					if ((lbr-rbr) > 0)
	fprintf(stderr,"line %d: %d unclosed braces before equation, first opened at line %d\n",lddl,lbr-rbr,lbrl);
					if ((lb-rb) > 0)
	fprintf(stderr,"line %d: %d unclosed brackets before equation, first opened at line %d\n",lddl,lb-rb,lbl);
					if ((lp-rp) > 0)
	fprintf(stderr,"line %d: %d unclosed parentheses before equation, first opened at line %d\n",lddl,lp-rp,lpl);
/* clear registers */
					lp=0; lb=0; lbr=0;
					rp=0; rb=0; rbr=0;
					lpl=0; lbrl=0; lbl=0;
					}
				}
			if (ld == 1)	rd=1;	/* right dollar sign */
			else
				{
				ld=1; 	/* left dollar sign */
				ldl=line;	/* line number */
				warn=0;	/* no warning has been given */
				}
			break;
		case '\\':
/* check for \begin{...} and \end{...} */
			i = get_file_word(fp,w,&line,&cc);
			if (i == 0 && cc == '[')
				{
				if (disp == 1)
	fprintf(stderr,"line %d: displayed equation starts, previous one at line %d not closed\n",line,disp_line);
				disp=1; 	/* left   \] */
				disp_line=line;
/* Give warning about unclosed openings */
				if ((lbr-rbr) > 0)
	fprintf(stderr,"line %d: %d unclosed braces before equation\n",line,lbr-rbr);
				if ((lb-rb) > 0)
	fprintf(stderr,"line %d: %d unclosed brackets before equation\n",line,lb-rb);
				if ((lp-rp) > 0)
	fprintf(stderr,"line %d: %d unclosed parentheses before equation\n",line,lp-rp);
/* clear registers */
				lp=0; lb=0; lbr=0;
				rp=0; rb=0; rbr=0;
				lpl=0; lbrl=0; lbl=0;
				}
			else if (i == 0 && cc == ']')
				{
				if (disp == 0)
	fprintf(stderr,"line %d: displayed equation ends but no beginning\n",line);
				disp=0;		/* right \] */
/* Give warning about unclosed openings */
				if ((lbr-rbr) > 0)
	fprintf(stderr,"line %d: %d unclosed braces in equation\n",line,lbr-rbr);
				if ((lb-rb) > 0)
	fprintf(stderr,"line %d: %d unclosed brackets in equation\n",line,lb-rb);
				if ((lp-rp) > 0)
	fprintf(stderr,"line %d: %d unclosed parentheses in equation\n",line,lp-rp);
/* clear registers */
				lp=0; lb=0; lbr=0;
				rp=0; rb=0; rbr=0;
				lpl=0; lbrl=0; lbl=0;
				}
			else if (i == 0 && cc == '(')
				{
				if (form == 1)
	fprintf(stderr,"line %d: formula starts but previous one not closed\n",line);
				form=1;			/* left \( */
				lform=line;		/* line of \( */
				}
			else if (i == 0 && cc == ')')
				{
				if (form == 0)
	fprintf(stderr,"line %d: formula ends but no beginning\n",line);
				form=0;			/* right \) */
				}
			else if (strcmp(w,"begin") == 0)
				{
				int ii=0;

				ii=get_file_word(fp,w,&line,&cc);
				if ((j=is_new_env(w,env_count)) < 0)
					{
					j = env_count;
					env[j].env_beg = 0;
					env[j].env_end = 0;
					p = (char *) malloc((unsigned)(ii*sizeof(char)+1));
					strcpy(p,w);
					env[env_count++].env_name = p;
					}
				env[j].beg_line = line;
				env[j].env_beg++;
/* Give warning about unclosed openings before these environments */
				if (strcmp(env[j].env_name,"equation") == 0 ||
				    strcmp(env[j].env_name,"eqnarray") == 0 ||
				    strcmp(env[j].env_name,"eqnarray*") == 0 ||
				    strcmp(env[j].env_name,"displaymath") == 0)
					{
					if ((lbr-rbr) > 0)
	fprintf(stderr,"line %d: %d unclosed braces before environment ``%s'', first opened at line %d\n",line,lbr-rbr,env[j].env_name,lbrl);
					if ((lb-rb) > 0)
	fprintf(stderr,"line %d: %d unclosed brackets before environment ``%s'', first opened at line %d\n",line,lb-rb,env[j].env_name,lbl);
					if ((lp-rp) > 0)
	fprintf(stderr,"line %d: %d unclosed parentheses before environment ``%s'', first opened at line %d\n",line,lp-rp,env[j].env_name,lpl);
/* clear registers */
					lp=0; lb=0; lbr=0;
					rp=0; rb=0; rbr=0;
					lpl=0; lbrl=0; lbl=0;
					}
				}
			else if (strcmp(w,"end") == 0)
				{
				(void) get_file_word(fp,w,&line,&cc);
				if ((j=is_new_env(w,env_count)) < 0)
	fprintf(stderr,"line %d: unmatched end for environment ``%s''\n",line,w);
				else
				{
				env[j].env_end++;
				if (env[j].env_end > env[j].env_beg)
					{
	fprintf(stderr,"line %d: unmatched end for environment ``%s''\n",line,
		env[j].env_name);
					env[j].env_end--;	/* reset */
					}
/* Give warning about unclosed openings in these environments */
				if (strcmp(env[j].env_name,"equation") == 0 ||
				    strcmp(env[j].env_name,"eqnarray") == 0 ||
				    strcmp(env[j].env_name,"eqnarray*") == 0 ||
				    strcmp(env[j].env_name,"displaymath") == 0)
					{
					if ((lbr-rbr) > 0)
	fprintf(stderr,"line %d: %d unclosed braces in environment ``%s''\n",
line,lbr-rbr,env[j].env_name);
					if ((lb-rb) > 0)
	fprintf(stderr,"line %d: %d unclosed brackets in environment ``%s''\n",
line,lb-rb,env[j].env_name);
					if ((lp-rp) > 0)
	fprintf(stderr,"line %d: %d unclosed parentheses in environment ``%s''\n",
line,lp-rp,env[j].env_name);
/* clear registers */
					lp=0; lb=0; lbr=0;
					rp=0; rb=0; rbr=0;
					lpl=0; lbrl=0; lbl=0;
					}
				}
				}
			else if (strcmp(w,"def") == 0)
				(void) def_file(fp,&line,1);
			else if (strcmp(w,"newcommand") == 0)
				(void) comm_file(fp,&line,1);
			else if (strcmp(w,"newenvironment") == 0)
				(void) getenv_file(fp,&line,1);
			else if (i > 0)		fseek(fp,-1,1);
			break;
		default:
			break;
		}
	c1=c;					/* update previous character */
	if (ld == 1 && rd == 1)
		{ld=0.;		rd=0.;}		/* matched dollar signs */
	if (ldd == 1 && rdd == 1)
		{ldd=0.;	rdd=0.;}	/* matched double dollar signs */
	}
if ((lbr-rbr) > 0)
	fprintf(stderr,"file ends: %d unclosed left braces, first opened at line %d \n",lbr-rbr,lbrl);
if ((lb-rb) > 0)
	fprintf(stderr,"file ends: %d unclosed left brackets, first opened at line %d\n",lb-rb,lbl);
if ((lp-rp) > 0)
	fprintf(stderr,"file ends: %d unclosed left parentheses, first opened at line %d\n",lp-rp,lpl);
if (ld == 1)
	fprintf(stderr,"file ends: single dollar sign opened at line %d unclosed\n",ldl);
if (ldd == 1)
	fprintf(stderr,"file ends: double dollar sign opened at line %d unclosed\n",lddl);
if (disp == 1)
	fprintf(stderr,"file ends: displayed equation opened at line %d unclosed\n",disp_line);
if (form == 1)
	fprintf(stderr,"file ends: formula opened at line %d unclosed\n",lform);
for (i=0; i<env_count; i++)
	if (env[i].env_beg > env[i].env_end)
	fprintf(stderr,"file ends: environment ``%s'' begun at line %d not ended\n",env[i].env_name,env[i].beg_line);
}
