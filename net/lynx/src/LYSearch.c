#include "HTUtils.h"
#include "LYUtils.h"
#include "LYStrings.h"
#include "LYSearch.h"
#include "LYGlobalDefs.h"

/*
 * search for the target string inside of the links
 * that are currently displayed on the screen beginning
 * with the one after the currently selected one!
 * if found set cur to the new value and return true
 * in not found do not reset cur and return false.
 */

PUBLIC int check_for_target_in_links ARGS2(int *,cur, char *,new_target)
{
    int i = *cur+1;

    if(nlinks==0)
	return(FALSE);

    for(; i < nlinks; i++)
        if(case_sensitive) {
	    if(strstr(links[i].hightext,new_target) != NULL)
		break;
        } else {
	    if(LYstrstr(links[i].hightext,new_target) != NULL)
		break;
	}

    if (i == nlinks)
	return(FALSE);
 
    /* else */
        *cur = i;
        return(TRUE);
}

/*
 *  Textsearch checks the prev_target variable to see if it is empty.
 *  If it is then it requests a new search string.  It then searches 
 *  the current file for the next instance of the search string and
 *  finds the line number that the string is on
 * 
 *  This is the primary USER search engine and is case sensitive
 *  or case insensitive depending on the 'case_sensitive' global
 *  variable
 *
 */
		
PUBLIC void textsearch ARGS2(document *,cur_doc, char *,prev_target)
{
	int offset;
        int oldcur = cur_doc->link;

	if(strlen(prev_target) == 0 ) {
	  statusline("Enter a search string: ");
	  LYgetstr(prev_target, VISIBLE);
	}

	if(strlen(prev_target) == 0) { /* return if empty string */
	  return;
	}

	if(check_for_target_in_links(&cur_doc->link, prev_target)) {
	   /* found in link, changed cur */
           highlight(OFF, oldcur);
	   return; 
	}
	
	/* start from the link you are on or the next page */
	if(nlinks == 0)
	    offset = display_lines+1;
	else
	    offset = links[cur_doc->link].ly;

	/* sets www_search_result if found */
	www_user_search(cur_doc->line+offset, prev_target);
}
