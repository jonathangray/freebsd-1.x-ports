#include "defs.h"

/* routines to handle special traversal feature */
extern char *HText_getTitle();

PUBLIC BOOLEAN lookup ARGS2(char *,fname, char *,target)
{
	 FILE *ifp;
	char buffer[200], line[200];

        if((ifp = fopen(TRAVERSE_FILE,"r")) == NULL) {
                perror("unable to open traversal file");
                exit(1);
        }

   	sprintf(line,"%s	%s\n",fname,target);

	while(fgets(buffer, 200, ifp) != NULL) {

	    if(STREQ(line,buffer)) {
		fclose(ifp);
		return(TRUE);
	    }
	} /* end while */

	fclose(ifp);
	return(FALSE);
}

PUBLIC void add_to_table ARGS2(char *,fname, char *,target)
{

	FILE *ifp;

	if((ifp = fopen(TRAVERSE_FILE,"a+")) == NULL) {
		perror("unable to open traversal file");
		exit(1);
	}

	 fprintf(ifp,"%s	%s\n",fname,target);

	fclose(ifp);
}

PUBLIC void add_to_traverse_list ARGS2(char *,fname, char *,prev_link_name)
{

	FILE *ifp;
	char *title;

	if((ifp = fopen(TRAVERSE_FOUND_FILE,"a+")) == NULL) {
		perror("unable to open traversal found file");
		exit(1);
	}

	title = HText_getTitle();

	 fprintf(ifp,"%s	%s	%s\n",fname, title, prev_link_name);

	fclose(ifp);
}

PUBLIC dump_traversal_history()
{
	int x;
        FILE *ifp;

        if((ifp = fopen(TRAVERSE_FILE,"a+")) == NULL) {
                perror("unable to open traversal file");
                exit(1);
        }

	fprintf(ifp,"\n\nTRAVERSAL WAS INTERUPTED\n\n\
here is a list of the history stack so that you may rebuild\n\n");

        for(x=nhist-1; x >= 0; x--) {
            fprintf(ifp,"%s	%s\n", history[x].hfname, history[x].hightext);
        }

        fclose(ifp);
}
