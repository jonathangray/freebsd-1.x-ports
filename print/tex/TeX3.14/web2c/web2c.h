/* web2c.h: general includes for web2c.  */

#include <stdio.h>
#include "site.h"
#include "common.h"

#define TRACE_YYTEXT( where ) \
  fprintf( stderr, "--- %s ---\t yytext[%x](%x)=\"%s\"\n", \
	   where, (long)&yytext, (long)yytext, yytext ); 

extern char *yytext, *saved_yytext;

#define SAVE_YYTEXT saved_yytext=yytext;
#define CORRECT_YYTEXT yytext=saved_yytext;

#ifdef SYSV
#define bcopy(s,d,n)	memcpy((d),(s),(n))
#define bcmp(s1,s2,n)	memcmp((s1),(s2),(n))
#define bzero(s,n)	memset((s),0,(n))
#endif

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#ifdef sparc
#include <alloca.h>
#else
char *alloca ();
#endif
#endif

#define ex_32 (2)
#define ex_real (3)
#define max(a,b) ((a>b)?a:b)

extern int indent;
extern int line_pos;
extern int last_brace;
extern int block_level;
extern int ii;
extern int last_tok;

extern char safe_string[80];
extern char var_list[200];
extern char field_list[200];
extern char last_id[80];
extern char z_id[80];
extern char next_temp[];

extern long last_i_num;
extern int ii, l_s;
extern long lower_bound, upper_bound;
extern FILE *fopen();
extern FILE *std;
extern int pf_count;

#include "symtab.h"

extern char strings[];
extern int hash_list[];
extern short global;
#ifdef	MS_DOS
extern struct sym_entry huge sym_table[];
#else
extern struct sym_entry sym_table[];
#endif
extern int next_sym_free, next_string_free;
extern int mark_sym_free, mark_string_free;

/* #ifdef	FLEX_SCANNER
extern char *yytext; 

#else	
#ifdef	HP
extern unsigned char yytext[];
#else
extern char yytext[];
#endif

#endif
*/

#ifdef	ANSI
extern void exit(int);
extern int yyparse(void);
void find_next_temp(void);
void normal(void);
void new_line(void);
void indent_line(void);
void my_output(char *);
void semicolon(void);
void yyerror(char *);
int hash(char *);
int search_table(char *);
int add_to_table(char *);
void remove_locals(void);
void mark(void);
void initialize(void);
void main(int,char * *);
#else /* not ANSI */
void find_next_temp(), normal(), new_line(), indent_line(), my_output();
void semicolon(), yyerror(), remove_locals(), mark(), initialize();
#endif /* not ANSI */
