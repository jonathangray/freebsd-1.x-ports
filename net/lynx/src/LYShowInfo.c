#include "LYCurses.h"
#include "HTUtils.h"
#include "LYStrings.h"
#include "LYUtils.h"
#include "LYStructs.h"
#include "LYGlobalDefs.h"
#include "LYShowInfo.h"

#ifdef DIRED_SUPPORT
#include <pwd.h>
#include <grp.h>
#include <time.h>
#include <sys/stat.h>

#define S_IRWXU         0000700 /* rwx, owner */
#define         S_IRUSR 0000400 /* read permission, owner */
#define         S_IWUSR 0000200 /* write permission, owner */
#define         S_IXUSR 0000100 /* execute/search permission, owner */
#define S_IRWXG         0000070 /* rwx, group */
#define         S_IRGRP 0000040 /* read permission, group */
#define         S_IWGRP 0000020 /* write permission, grougroup */
#define         S_IXGRP 0000010 /* execute/search permission, group */
#define S_IRWXO         0000007 /* rwx, other */
#define         S_IROTH 0000004 /* read permission, other */
#define         S_IWOTH 0000002 /* write permission, other */
#define         S_IXOTH 0000001 /* execute/search permission, other */
#endif /* DIRED_SUPPORT */

/* 
 * showinfo prints a page of info about the current file and the link
 * that the cursor is on
 */
	     
PUBLIC int showinfo ARGS4(document *,doc, int,size_of_file, document *,newdoc,
							char *,owner_address)
{
	static char * tempfile=NULL;
        char * print_url=0;
	int url_type;
	FILE *fp0;

#ifdef DIRED_SUPPORT
	char temp[300];
	char *cp;
	struct tm *tm;
	struct stat dir_info;
	struct passwd *pw;
	struct group *grp;
#endif
        if(tempfile == NULL) {
            tempfile = (char *) malloc(127);
            tempname(tempfile,NEW_FILE);
            /* make the file a URL now */
#ifndef VMS
        }
#else
        } else {
            remove(tempfile);   /* put VMS code to remove duplicates here */
        }
#endif /* VMS */


#ifdef VMS
            StrAllocCopy(print_url,"file://localhost/");
#else
            StrAllocCopy(print_url,"file://localhost");
#endif /* VMS */
            StrAllocCat(print_url,tempfile);

        if((fp0 = fopen(tempfile,"w")) == NULL) {
            statusline("Unable to open print options file");
            sleep(2);
            return(0);
        }

	newdoc->address = print_url; /*point the address pointer at this Url */


	if(nlinks > 0 && links[doc->link].lname != NULL &&
	   (url_type = is_url(links[doc->link].lname)) != 0 &&
	   url_type == LYNXEXEC_URL_TYPE) {
	    char *last_slash = strrchr(links[doc->link].lname,'/');
	    if(last_slash-links[doc->link].lname ==
	    	   strlen(links[doc->link].lname)-1)
	        links[doc->link].lname[strlen(links[doc->link].lname)-1] = '\0';
	    }

	fprintf(fp0,"<title>%s</title>\n",SHOWINFO_TITLE);
	fprintf(fp0,"<h1>YOU HAVE REACHED THE INFORMATION PAGE</h1>\n");

#ifdef DIRED_SUPPORT	
	if (lynx_edit_mode) {
	   fprintf(fp0,"<h2>Directory that you are currently viewing</h2>\n<pre>");

	   cp = doc->address;
	   if(!strncmp(cp,"file://localhost",16)) 
	      cp += 16;
	   else if(!strncmp(cp,"file:",5))
	      cp += 5;
	   strcpy(temp,cp);
	   HTUnEscape(temp);

	   fprintf(fp0,"   Name:  %s\n",temp);
	   fprintf(fp0,"    URL:  %s\n",doc->address);

	   cp = links[doc->link].lname;
	   if(!strncmp(cp,"file://localhost",16)) 
	     cp += 16;
	   else if(!strncmp(cp,"file:",5))
	     cp += 5;
	   strcpy(temp,cp);
	   HTUnEscape(temp);
	   if (stat(temp,&dir_info) == -1) {
	      statusline("Failed to obtain status of current link!");
	      sleep(2);
	   } else {
	      if (((dir_info.st_mode) & S_IFMT) == S_IFDIR) {
		 fprintf(fp0,"\nDirectory that you have currently selected\n\n");
	      } else if (((dir_info.st_mode) & S_IFMT) == S_IFREG) {
		 fprintf(fp0,"\nFile that you have currently selected\n\n");
	      } else if (((dir_info.st_mode) & S_IFMT) == S_IFLNK) {
		 fprintf(fp0,"\nSymbolic link that you have currently selected\n\n");
	      } else {
		 fprintf(fp0,"\nItem that you have currently selected\n\n");
	      }
	      fprintf(fp0,"       Full name:  %s\n",temp);
	      pw = getpwuid(dir_info.st_uid);
 	      fprintf(fp0,"   Name of owner:  %s\n",pw->pw_name);
	      grp = getgrgid(dir_info.st_gid);
	      fprintf(fp0,"      Group name:  %s\n",grp->gr_name);
	      if (((dir_info.st_mode) & S_IFMT) == S_IFREG) {
		 sprintf(temp,"       File size:  %d (bytes)\n",dir_info.st_size);
		 fprintf(fp0,"%s",temp);
	      }
/*
   Include date and time information
*/
	      cp = ctime(&dir_info.st_ctime);
	      fprintf(fp0,"   Creation date:  %s",cp);

	      cp = ctime(&dir_info.st_mtime);	      
	      fprintf(fp0,"   Last modified:  %s",cp);

	      cp = ctime(&dir_info.st_atime);
	      fprintf(fp0,"   Last accessed:  %s\n",cp);

	      fprintf(fp0,"   Access Permissions\n");
	      fprintf(fp0,"      Owner:  ");
	      if ((dir_info.st_mode & S_IRUSR))
		fprintf(fp0,"read");
	      if ((dir_info.st_mode & S_IWUSR))
		fprintf(fp0,", write");
	      if ((dir_info.st_mode & S_IXUSR)) {
		if (((dir_info.st_mode) & S_IFMT) == S_IFDIR) 
		  fprintf(fp0,", search\n");
	        else
		  fprintf(fp0,", execute\n");
	      } else
		 fprintf(fp0,"\n");
	      fprintf(fp0,"      Group:  ");
	      if ((dir_info.st_mode & S_IRGRP)) 
		fprintf(fp0,"read");
	      if ((dir_info.st_mode & S_IWGRP))
		fprintf(fp0,", write");
	      if ((dir_info.st_mode & S_IXGRP)) {
		if (((dir_info.st_mode) & S_IFMT) == S_IFDIR) 
		  fprintf(fp0,", search\n");
	        else
		  fprintf(fp0,", execute\n");
	      } else
		fprintf(fp0,"\n");
	      fprintf(fp0,"      World:  ");
	      if ((dir_info.st_mode & S_IROTH))
		fprintf(fp0,"read");
	      if ((dir_info.st_mode & S_IWOTH))
		fprintf(fp0,", write");
	      if ((dir_info.st_mode & S_IXOTH)) {
		if (((dir_info.st_mode) & S_IFMT) == S_IFDIR) 
		  fprintf(fp0,", search\n");
	        else
		  fprintf(fp0,", execute\n");
	       } else
		 fprintf(fp0,"\n");
	   }
	} else {
#endif	

	fprintf(fp0,"<h2>File that you are currently viewing</h2>\n<dl compact>");

	fprintf(fp0,"<dt>Linkname: %s\n",doc->title);
	fprintf(fp0,"<dt>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;URL: %s\n",doc->address);

	if(doc->post_data) {
	    fprintf(fp0,"<dt>Post Data: %s\n",doc->post_data);
	    fprintf(fp0,"<dt>Post Content Type: %s\n",doc->post_content_type);
	}
	fprintf(fp0,"<dt>Owner(s): %s\n",
	           (!owner_address ? "None" : owner_address));

	fprintf(fp0,"<dt>&nbsp;&nbsp;&nbsp;&nbsp;size: %d lines\n",size_of_file);

	fprintf(fp0,"<dt>&nbsp;&nbsp;&nbsp;&nbsp;mode: %s\n",
		   (lynx_mode == FORMS_LYNX_MODE ? "forms mode" : "normal"));

	fprintf(fp0,"</dl>");  /* end of list */

        if(nlinks > 0) {
	    fprintf(fp0,"<h2>Link that you currently have selected</h2>\n<dl compact>");
	    fprintf(fp0,"<dt>Linkname: %s\n",links[doc->link].hightext);
	    fprintf(fp0,"<dt>Filename: %s\n",links[doc->link].lname);
	    fprintf(fp0,"</dl>");  /* end of list */

         } else
	    fprintf(fp0,"<h2>No Links on the current page</h2>");

#ifdef DIRED_SUPPORT
     }
#endif

         refresh();

         fclose(fp0);

         return(1);
}
