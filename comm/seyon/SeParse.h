#include <strings.h>
/*---------------------------------------------------------------------------+
| External parser interface
+---------------------------------------------------------------------------*/
extern void SignalBeginFunction(char *);
extern void SignalArg(char *);
extern void SignalEndFunction();
extern void ParseThis();

extern void yyerror(char *);

/*---------------------------------------------------------------------------+
| Control and escape characters
+---------------------------------------------------------------------------*/
#define CTRL_CHAR '^'
#define BACK_CHAR '\\'

extern void scSetInputBuffer(char *);
extern int yyparse();
