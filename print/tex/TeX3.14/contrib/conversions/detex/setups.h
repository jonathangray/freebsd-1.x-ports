/* setup file */

#include        <stdio.h>
#ifdef MSC
#include        <string.h>
#include	<stdlib.h>	/* for type declarations */
#include	<io.h>		/* for type declarations */
#else
#include        <strings.h>
#include        <sys/ioctl.h>
#include        <sgtty.h>
#endif

#define MAXLEN	65535		/* maximum number of chars in a document */
#define     MAXWORD	100	/* maximum word length */
#define     MAXLINE	250	/* maximum line length */
#define     MAXENV	50	/* maximum number of environments */

extern char *malloc();

#ifdef IN_TM		/* can only declare globals once */
#else
extern
#endif
struct environment {
	char *env_name;		/* name of environment */
	int env_beg;		/* counter for \begin{environment} */
	int env_end;		/* counter for \end{environment} */
	int beg_line;		/* line number for \beging{environment} */
	} env[MAXENV];

#ifdef ANSI
int	begin_to_end(char*,char*);
int	command(char*,char*);
int	comm_file(FILE*,int*);
int	comment(char*);
int	def(char*,char*);
int	def_file(FILE*,int*);
int	display(char*);
int	dollar(char*,FILE*);
int	formula(char*);
int	get_buf_word(char*,char*);
int	getenv_file(FILE*,int*);
int	get_file_word(FILE*,char*,int*,int*);
int	is_new_env(char*,int);
int	one_dollar(char*);
void	scrbuf(FILE*,FILE*);
void	tmpbuf(FILE*,char*);
int	two_dollars(char*,FILE*);
#else
int	begin_to_end();
int	command();
int	comm_file();
int	comment();
int	def();
int	def_file();
int	display();
int	dollar();
int	formula();
int	get_buf_word();
int	getenv_file();
int	get_file_word();
int	is_new_env();
int	one_dollar();
void	scrbuf();
void	tmpbuf();
int	two_dollars();
#endif
