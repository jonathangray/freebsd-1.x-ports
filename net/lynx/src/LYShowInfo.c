#include "LYCurses.h"
#include "HTUtils.h"
#include "LYUtils.h"
#include "LYStructs.h"
#include "LYStrings.h"
#include "LYGlobalDefs.h"

PRIVATE void display_and_wrap PARAMS((char * string));
/* 
 * showinfo prints a page of info about the current file and the link
 * that the cursor is on
 */
	     
PUBLIC int showinfo ARGS3(document *,doc, int,size_of_file, char *,owner_address)
{
	char temp[100];
	int url_type;

	if(links[doc->link].lname != 0 &&
	   (url_type = is_url(links[doc->link].lname)) != 0 &&
	   url_type == LYNXEXEC_URL_TYPE) {
	    char *last_slash = strrchr(links[doc->link].lname,'/');
	    if(last_slash-links[doc->link].lname ==
	    	   strlen(links[doc->link].lname)-1)
	        links[doc->link].lname[strlen(links[doc->link].lname)-1] = '\0';
	    }

	clear();

	
	addstr("\n                  YOU HAVE REACHED THE INFORMATION PAGE\n\n");
	
	addstr("File that you are currently viewing\n\n");
	addstr("   Linkname:  ");
	display_and_wrap(doc->title);
	addch('\n');

	addstr("        URL:  ");
	display_and_wrap(doc->address);
	addch('\n');

	addstr("   Owner(s):  ");
	if(!owner_address)
	   	addstr("None\n");
	else {
		display_and_wrap(owner_address);
	        addch('\n');
	}

	if(child_lynx)
	    refresh();

	sprintf(temp,"       size:  %d lines\n",size_of_file);
	addstr(temp);

	addstr("  lynx mode:  ");
	addstr(lynx_mode == FORMS_LYNX_MODE ? "forms mode" :
			 			    "normal");
	addch('\n');


     if(nlinks > 0) {
	addstr("\nLink that you currently have selected\n\n");
	addstr("   Linkname:  ");
	display_and_wrap(links[doc->link].hightext);
	addch('\n');
	addstr("   Filename:  ");
	display_and_wrap(links[doc->link].lname);
	addch('\n');

	if(child_lynx)
	    refresh();

     }
     else
	addstr("\n\nNo Links on the current page\n");

	refresh();

	LYgetch();

	return(0);
}

PRIVATE void display_and_wrap ARGS1(char *,string)
{
   int i,j=14;

   for(i=0; string[i] != '\0'; i++, j++) {
 	if(j == (LYcols-1)) {
	   addch('\n');
	   j=0;
	}
	addch(string[i]);
    }
}
