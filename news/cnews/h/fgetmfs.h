/* fgetmfs compatibility on top of fgetfln */
/* values for fgetmfs flag */
#define CONT_NO 0		/* no continuations */
#define CONT_NOSPC 1		/* continue & remove leading whitespace */
#define CONT_SPC 2		/* continue & keep leading whitespace */

extern char *fgetmfs(), *fgetms(), *cfgetms();
