/* Routines to manipulate the local filesystem. */
/* Written by: Rick Mallett, Carleton University */
/* Report problems to rmallett@ccs.carleton.ca */

#ifdef DIRED_SUPPORT

#include "LYCurses.h"
#include "HTUtils.h"
#include "LYGlobalDefs.h"
#include "LYUtils.h"
#include "LYStrings.h"
#include "LYStructs.h"
#include "LYGetFile.h"
#include "LYLocal.h"

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <errno.h>

/* Remove all tagged files and directories. */

PRIVATE BOOLEAN remove_tagged ()
{ 
   int ans;
   char *cp,*tp;
   char tmpbuf[1024];
   char testpath[512];
   struct stat dir_info;
   int pid,status;
   int count,i;
   taglink *tag;

   if (tagged == NULL) return 0; /* should never happen */

   statusline("Remove all tagged files and directories (y or n): ");
   ans=toupper(LYgetch());

   count = 0;
   tag = tagged;
   while(ans == 'Y' && tag != NULL) {
      cp = tag->name;
      if(is_url(cp) == FILE_URL_TYPE) { /* unecessary check */
	 tp = cp;
	 if(!strncmp(tp,"file://localhost",16))
	   tp += 16;
	 else if(!strncmp(tp,"file:",5))
	   tp += 5;
	 strcpy(testpath,tp);
	 HTUnEscape(testpath);
	 if((i = strlen(testpath)) && testpath[i-1] == '/')
	   testpath[i-1] = '\0';

/* check the current status of the path to be deleted */

	 if (stat(testpath,&dir_info) == -1) {
	    sprintf(tmpbuf,"System error - failed to get status of %s ",testpath);
	    statusline(tmpbuf);
	    sleep(3);
	    return count;
	 } else {
	    if((dir_info.st_mode & S_IFMT) != S_IFDIR) 
	      unlink(testpath);
	    else {
	       pid = fork(); /* fork and execute rm -rf */
	       switch (pid) {
		case -1:
		  sprintf(tmpbuf,"System error - unable to process request for %s",testpath);
		  statusline(tmpbuf);
		  sleep(3);
		  return count;
		case 0:  /* child */
		  execl("/bin/rm","rm","-rf",testpath,(char *) 0);
		  sprintf(tmpbuf,"System error - failed to complete request for %s",testpath);
		  statusline(tmpbuf);
		  sleep(3);
		  return count;
		default:  /* parent */
		  while(wait(&status) != pid) /* do nothing */ ;
		  errno = 0;
		  if(status) {
		     sprintf(tmpbuf,"System error processing request for %s",testpath);
		     statusline(tmpbuf);
		     sleep(3);
		     return count;
		  }
	       }
	    }
	    ++count;
	 }
      }
      tag = tag->next;
   }
   return count;
}

/* Move all tagged files and directories to a new location. */
/* Input is current directory. */

PRIVATE BOOLEAN modify_tagged ARGS1 (char *,testpath)
{
   int ans;
   char *cp;
   dev_t dev;
   ino_t inode;
   uid_t owner;
   char tmpbuf[1024];
   char savepath[512];
   struct stat dir_info;
   int pid,status;
   int count;
   taglink *tag;

   if (tagged == NULL) return 0; /* should never happen */

   statusline("Enter new location for tagged items: ");

   tmpbuf[0] = '\0';
   LYgetstr(tmpbuf,VISIBLE);
   if (strlen(tmpbuf)) {

/* determine the ownership of the current location */

      cp = testpath;
      if (!strncmp(cp,"file://localhost",16))
	cp += 16;
      else if (!strncmp(cp,"file:",5))
	cp += 5;
      strcpy(savepath,cp);
      HTUnEscape(savepath);
      if (stat(savepath,&dir_info) == -1) {
	 sprintf(tmpbuf,"Unable to get status of %s ",savepath);
	 statusline(tmpbuf);
	 sleep(3);
	 return 0;
      } 

/* save the owner of the current location for later use */
/* also save the device and inode for location checking */

      dev = dir_info.st_dev;
      inode = dir_info.st_ino;
      owner = dir_info.st_uid;

/* replace ~/ references to the home directory */

      if (!strncmp(tmpbuf,"~/",2)) {
	 cp = getenv("HOME");
	 strcpy(testpath,cp);
	 strcat(testpath,tmpbuf+1);
	 strcpy(tmpbuf,testpath);
      }

/* if path is relative prefix it with current location */

      if (tmpbuf[0] != '/') {
	 if (savepath[strlen(savepath)-1] != '/')
	   strcat(savepath,"/");
	 strcat(savepath,tmpbuf);
      } else {
	 strcpy(savepath,tmpbuf);
      }

/* stat the target location to determine type and ownership */

      if (stat(savepath,&dir_info) == -1) {
	 sprintf(tmpbuf,"Unable to get status of %s ",savepath);
	 statusline(tmpbuf);
	 sleep(3);
	 return 0;
      }

/* make sure the source and target locations are not the same place */

      if (dev == dir_info.st_dev && inode == dir_info.st_ino) {
	 statusline("Source and destination are the same location - request ignored!");
	 sleep(3);
	 return 0;
      }

/* make sure the target location is a directory which is owned */
/* by the same uid as the owner of the current location */

      if((dir_info.st_mode & S_IFMT) == S_IFDIR) {
	 if(dir_info.st_uid == owner) {
	    count = 0;
	    tag = tagged;

/* move all tagged items to the target location */

	    while (tag != NULL) {
	       cp = tag->name;
	       if(!strncmp(cp,"file://localhost",16))
		 cp += 16;
	       else if(!strncmp(cp,"file:",5))
		 cp += 5;
	       strcpy(testpath,cp);
	       HTUnEscape(testpath);

	       pid = fork(); /* fork and execute mv */
	       switch (pid) {
		case -1:
		  sprintf(tmpbuf,"System error - unable to move %s to %s",testpath,savepath);
		  statusline(tmpbuf);
		  sleep(3);
		  return count;
		case 0:  /* child */
		  execl("/bin/mv","mv",testpath,savepath,(char *) 0);
		  sprintf(tmpbuf,"System error - failed to move %s to %s",testpath,savepath);
		  sleep(3);
		  return count;
		default:  /* parent */
		  while(wait(&status) != pid) /* do nothing */ ;
		  if(status) {
		     sprintf(tmpbuf,"System error moving %s to %s",testpath,savepath);
		     statusline(tmpbuf);
		     sleep(3);
		     return count;
		  }
	       }
	       tag = tag->next;
	       ++count;
	    }
	    return count;
	 } else {
	    statusline("Destination has different owner! Request denied. ");
	    sleep(3);
	    return 0;
	 }
      } else {
	 statusline("Destination is not a valid directory! Request denied. ");
	 sleep(3);
	 return 0;
      }
   }
}

/* Modify the name of the specified item. */

PRIVATE BOOLEAN modify_name ARGS1 (char *,testpath)

{
   char *cp;
   uid_t owner;
   char tmpbuf[512];
   char savepath[512];
   struct stat dir_info;
   int pid,status;

/* Determine the status of the selected item. */

   testpath = strip_trailing_slash(testpath);

   if (stat(testpath,&dir_info) == -1) {
      sprintf(tmpbuf,"Unable to get status of %s ",testpath);
      statusline(tmpbuf);
      sleep(3);
   } else {

/* Change the name of the file or directory. */

      if ((dir_info.st_mode & S_IFMT) == S_IFDIR) {
	 statusline("Enter new name for directory: ");
      } else if ((dir_info.st_mode & S_IFMT) == S_IFREG) {
	 statusline("Enter new name for file: ");
      } else {
	 statusline("The selected item is not a file or a directory! Request ignored. ");
	 sleep(3);
	 return 0;
      }
      tmpbuf[0] = '\0';
      LYgetstr(tmpbuf,VISIBLE);

/* Do not allow the user to also change the location at this time */

      if(strstr(tmpbuf,"../") != NULL) {
	 statusline("Illegal redirection \"../\" found! Request ignored. ");
	 sleep(3);
      } else if(strchr(tmpbuf,'/') != NULL) {
	 statusline("Illegal character \"/\" found! Request ignored. ");
	 sleep(3);
      } else if(strlen(tmpbuf) && (cp = strrchr(testpath,'/')) != NULL) {
	 strcpy(savepath,testpath);
	 *++cp = '\0';
	 strcat(testpath,tmpbuf);

/* Make sure the destination does not already exist. */

	 if (stat(testpath,&dir_info) == -1) {
	    if (errno != ENOENT) {
	       sprintf(tmpbuf,"Unable to determine status of %s ",testpath);
	       statusline(tmpbuf);
	       sleep(3);
	    } else {
	       pid = fork(); /* fork and execute mv */
	       switch (pid) {
		case -1:
		  statusline("Unable to process request due to system error! ");
		  sleep(3);
		  return 0;
		case 0:  /* child */
		  execl("/bin/mv","mv",savepath,testpath,(char *) 0);
		  statusline("Unable to complete request due to system error! ");
		  sleep(3);
		  return 0;
		default:  /* parent */
		  while(wait(&status) != pid) /* do nothing */ ;
		  if(status) {
		     statusline("Possible failure to complete request due to system error! ");
		     sleep(3);
		     return 0;
		  }
	       }
	       return 1; 
	    }
	 } else if ((dir_info.st_mode & S_IFMT) == S_IFDIR) {
	    statusline("There is already a directory with that name! Request ignored. ");
	    sleep(3);
	 } else if ((dir_info.st_mode & S_IFMT) == S_IFREG) {
	    statusline("There is already a file with that name! Request ignored. ");
	    sleep(3);
	 } else {
	    statusline("The specified name is already in use! Request ignored. ");
	    sleep(3);
	 }
      }
   }
   return 0;
}

/* Change the location of a file or directory. */

PRIVATE BOOLEAN modify_location ARGS1 (char *,testpath)
{
   int mode;
   char *cp;
   dev_t dev;
   ino_t inode;
   uid_t owner;
   char tmpbuf[1024];
   char savepath[512];
   struct stat dir_info;
   int pid,status;

/* Determine the status of the selected item. */

   testpath = strip_trailing_slash(testpath);

   if (stat(testpath,&dir_info) == -1) {
      sprintf(tmpbuf,"Unable to get status of %s ",testpath);
      statusline(tmpbuf);
      sleep(3);
      return 0;
   } 

/* Change the location of the file or directory */

   if ((dir_info.st_mode & S_IFMT) == S_IFDIR) {
      statusline("Enter new location for directory: ");
   } else if ((dir_info.st_mode & S_IFMT) == S_IFREG) {
      statusline("Enter new location for file: ");
   } else {
      statusline("The specified item is not a file or a directory - request ignored.");
      sleep(3);
      return 0;
   }
   tmpbuf[0] = '\0';
   LYgetstr(tmpbuf,VISIBLE);
   if (strlen(tmpbuf)) {
      strcpy(savepath,testpath);

/* Allow ~/ references to the home directory. */

      if (!strncmp(tmpbuf,"~/",2)) {
	 cp = getenv("HOME");
	 strcpy(testpath,cp);
	 strcat(testpath,tmpbuf+1);
	 strcpy(tmpbuf,testpath);
      }
      if (tmpbuf[0] != '/') {
	 if ((cp = strrchr(testpath,'/')) != NULL) {
	    *++cp = '\0';
	    strcat(testpath,tmpbuf);
	 } else {
	    statusline("Unexpected failure - unable to find trailing \"/\"");
	    sleep(3);
	    return 0;
	 }
      } else {
	 strcpy(testpath,tmpbuf);
      }
      
/* Make sure the source and target have the same owner (uid) */

      dev = dir_info.st_dev;
      mode = dir_info.st_mode;
      inode = dir_info.st_ino;
      owner = dir_info.st_uid;  
      if (stat(testpath,&dir_info) == -1) {
	 sprintf(tmpbuf,"Unable to get status of %s ",testpath);
	 statusline(tmpbuf);
	 sleep(3);
	 return 0;
      }
      if ((dir_info.st_mode & S_IFMT) != S_IFDIR) {
	 statusline("Destination is not a valid directory! Request denied. ");
	 sleep(3);
	 return 0;
      }

/* make sure the source and target are not the same location */

      if (dev == dir_info.st_dev && inode == dir_info.st_ino) {
	 statusline("Source and destination are the same location! Request ignored!");
	 sleep(3);
	 return 0;
      }
      if(dir_info.st_uid == owner) {
	 pid = fork(); /* fork and execute mv */
	 switch (pid) {
	  case -1:
	    statusline("Unable to process request due to system error!");
	    sleep(3);
	    return 0;
	  case 0:  /* child */
	    execl("/bin/mv","mv",savepath,testpath,(char *) 0);
	    statusline("Unable to complete request due to system error!");
	    sleep(3);
	    return 0;
	  default:  /* parent */
	    while(wait(&status) != pid) /* do nothing */ ;
	    if(status) {
	       statusline("Probable failure to complete request due to system error! ");
	       sleep(3);
	       return 0;
	    }
	 }
	 return 1;
      } else {
	 statusline("Destination has different owner! Request denied. ");
	 sleep(3);
	 return 0;
      }
   }
}   

/* Modify name or location of a file or directory on localhost. */

PUBLIC BOOLEAN local_modify ARGS1 (document *,doc)
{
   int ans;
   char *cp;
   char testpath[512]; /* a bit ridiculous */
   int count;

   if (tagged != NULL) {
      cp = doc->address;
      if (!strncmp(cp,"file://localhost",16))
	cp += 16;
      else if (!strncmp(cp,"file:",5))
	cp += 5;
      strcpy(testpath,cp);
      HTUnEscape(testpath);
      count = modify_tagged(testpath);

      if (doc->link > (nlinks-count-1)) doc->link = nlinks-count-1;
      doc->link = doc->link < 0 ? 0 : doc->link; 

      return count;
   } else if (doc->link < 0 || doc->link > nlinks) /* added protection */
      return 0;

/* Do not allow simultaneous change of name and location as in Unix */
/* This reduces functionality but reduces difficulty for the novice */

   statusline("Modify name or location (n or l): ");
   ans=toupper(LYgetch());

   if (strchr("NL",ans) != NULL) {
      cp = links[doc->link].lname;
      if(!strncmp(cp,"file://localhost",16))
	cp += 16;
      else if(!strncmp(cp,"file:",5))
	cp += 5;
      strcpy(testpath,cp);
      HTUnEscape(testpath);

      if (ans == 'N') {

	 return(modify_name(testpath));

      } else if (ans == 'L') {

	 if (modify_location(testpath)) {

	   if (doc->link == (nlinks-1)) --doc->link;

	   return 1;
	}
      } else {

/* code for changing ownership and access permissions needed here */

	 statusline("This feature not yet implemented! ");
	 sleep(3);
      }
   }
   return 0;
}

/* Create a new empty file in the current directory. */

PRIVATE BOOLEAN create_file ARGS1 (char *,current_location)
{
   char tmpbuf[512];
   char testpath[512];
   struct stat dir_info;
   int pid,status;

   statusline("Enter name of file to create: ");

   tmpbuf[0] = '\0';
   LYgetstr(tmpbuf,VISIBLE);
   if(strstr(tmpbuf,"../") != NULL) {
      statusline("Illegal redirection \"../\" found! Request ignored.");
      sleep(3);
   } else if(strstr(tmpbuf,"//") != NULL) {
      statusline("Illegal redirection \"//\" found! Request ignored.");
      sleep(3);
   } else if(strlen(tmpbuf) && strchr(".~/",tmpbuf[0]) == NULL) {
      strcpy(testpath,current_location);
      if(testpath[strlen(testpath)-1] != '/')
	strcat(testpath,"/");

/* append the target filename to the current location */

      strcat(testpath,tmpbuf);

/* make sure the target does not already exist */

      if (stat(testpath,&dir_info) == -1) {
	 if (errno != ENOENT) {
	    sprintf(tmpbuf,"Unable to determine status of %s ",testpath);
	    statusline(tmpbuf);
	    sleep(3);
	    return 0;
	 } 
	 pid = fork(); /* fork and touch the file */
	 switch (pid) {
	  case -1:
	    statusline("Unable to process request due to system error!");
	    sleep(3);
	    return 0;
	  case 0:  /* child */
	    execl("/bin/touch","touch",testpath,(char *) 0);
	    statusline("Unable to complete request due to system error!");
	    sleep(3);
	    return 0;
	  default:
	    while(wait(&status) != pid) /* do nothing */ ;
	    if(status) {
	       statusline("Probable failure to complete request due to system error! ");
	       sleep(3);
	       return 0;
	    }
	 }
	 return 1;
      } else if ((dir_info.st_mode & S_IFMT) == S_IFDIR) {
	 statusline("There is already a directory with that name! Request ignored. ");
	 sleep(3);
      } else if ((dir_info.st_mode & S_IFMT) == S_IFREG) {
	 statusline("There is already a file with that name! Request ignored. ");
	 sleep(3);
      } else {
	 statusline("The specified name is already in use! Request ignored. ");
	 sleep(3);
      }
   }
   return 0;
}

/* Create a new directory in the current directory. */

PRIVATE BOOLEAN create_directory ARGS1 (char *,current_location)
{
   char *cp;
   char tmpbuf[512];
   char testpath[512];
   struct stat dir_info;
   int pid,status;

   statusline("Enter name for new directory: ");

   tmpbuf[0] = '\0';
   LYgetstr(tmpbuf,VISIBLE);
   if(strstr(tmpbuf,"../") != NULL) {
      statusline("Illegal redirection \"../\" found! Request ignored.");
      sleep(3);
   } else if(strstr(tmpbuf,"//") != NULL) {
      statusline("Illegal redirection \"//\" found! Request ignored.");
      sleep(3);
   } else if(strlen(tmpbuf) && strchr(".~/",tmpbuf[0]) == NULL) {
      strcpy(testpath,current_location);

      if(testpath[strlen(testpath)-1] != '/')
	strcat(testpath,"/");

      strcat(testpath,tmpbuf);

/* make sure the target does not already exist */

      if (stat(testpath,&dir_info) == -1) {
	 if (errno != ENOENT) {
	    sprintf(tmpbuf,"Unable to determine status of %s ",testpath);
	    statusline(tmpbuf);
	    sleep(3);
	    return 0;
	 } 
	 pid = fork(); /* fork and execute mkdir */
	 switch (pid) {
	  case -1:
	    statusline("Unable to process request due to system error!");
	    sleep(3);
	    return 0;
	  case 0:  /* child */
	    execl("/bin/mkdir","mkdir",testpath,(char *) 0);
	    statusline("Unable to complete request due to system error!");
	    sleep(3);
	    return 0;
	  default:
	    while(wait(&status) != pid) /* do nothing */ ;
	    if(status) {
	       statusline("Probable failure to complete request due to system error! ");
	       sleep(3);
	       return 0;
	    }
	 }
	 return 1;
      } else if ((dir_info.st_mode & S_IFMT) == S_IFDIR) {
	 statusline("There is already a directory with that name! Request ignored. ");
	 sleep(3);
      } else if ((dir_info.st_mode & S_IFMT) == S_IFREG) {
	 statusline("There is already a file with that name! Request ignored. ");
	 sleep(3);
      } else {
	 statusline("The specified name is already in use! Request ignored. ");
	 sleep(3);
      }
   }
   return 0;
}

/* Create a file or a directory at the current location. */

PUBLIC BOOLEAN local_create ARGS1 (document *,doc)
{
   int ans;
   char *cp;
   char testpath[512];

   statusline("Create file or directory (f or d): ");
   ans = toupper(LYgetch());

   cp = doc->address;
   if(!strncmp(cp,"file://localhost",16))
     cp += 16;
   else if(!strncmp(cp,"file:",5))
     cp += 5;
   strcpy(testpath,cp);
   HTUnEscape(testpath);
   
   if (ans == 'F') 
     return(create_file(testpath));
   else if (ans == 'D') 
     return(create_directory(testpath));
   else return 0;

}

/* Remove a single file or directory. */

PRIVATE BOOLEAN remove_single ARGS1 (char *,testpath) 
{
   int ans;
   char *cp;
   char tmpbuf[1024];
   struct stat dir_info;
   int pid,status,i;

/* lstat first in case its a symbolic link */

   if (lstat(testpath,&dir_info) == -1 && stat(testpath,&dir_info) == -1) {
      sprintf(tmpbuf,"System error - failed to get status of %s. ",testpath);
      statusline(tmpbuf);
      sleep(3);
      return 0;
   } 

/* locate the filename portion of the path */

   if ((cp = strrchr(testpath,'/')) != NULL) {
      ++cp;
   } else {
      cp = testpath;
   }
   if ((dir_info.st_mode & S_IFMT) == S_IFDIR) {
      if(strlen(cp) < 37)
	sprintf(tmpbuf,"Remove %s and all of its contents (y or n): ",cp);
      else
	sprintf(tmpbuf,"Remove directory and all of its contents (y or n): ");
   } else if ((dir_info.st_mode & S_IFMT) == S_IFREG) {
      if(strlen(cp) < 60)
	sprintf(tmpbuf,"Remove file %s (y or n): ",cp);
      else 
	sprintf(tmpbuf,"Remove file (y or n): ");
   } else if ((dir_info.st_mode & S_IFMT) == S_IFLNK) {
      if(strlen(cp) < 50)
	sprintf(tmpbuf,"Remove symbolic link %s (y or n): ",cp);
      else 
	sprintf(tmpbuf,"Remove symbolic link (y or n): ");
   } else {
      sprintf(tmpbuf,"Unable to determine status of %s. ",testpath);
      statusline(tmpbuf);
      sleep(3);
      return 0;
   }
   statusline(tmpbuf);

   if(toupper(LYgetch()) == 'Y') {
      if ((dir_info.st_mode & S_IFMT) != S_IFDIR)
	unlink(testpath);
      else {
	 pid = fork(); /* fork and execute rm */
	 switch (pid) {
	  case -1:
	    statusline("Unable to process request due to system error!");
	    sleep(3);
	    return 0;
	  case 0:  /* child */
	    execl("/bin/rm","rm","-rf",testpath,(char *) 0);
	    statusline("Unable to complete request due to system error!");
	    sleep(3);
	    return 0;
	  default:  /* parent */
	    while(wait(&status) != pid) /* do nothing */ ;
	    if(status) {
	       statusline("Probable failure to complete request due to system error! ");
	       sleep(3);
	       return 0;
	    }
	 }
      }
      return 1;
   }
   return 0;
}

/* Remove a file or a directory. */

PUBLIC BOOLEAN local_remove ARGS1 (document *,doc)
{  
   char *cp,*tp;
   char testpath[512];
   int count,i;

   if (tagged != NULL) {

      count = remove_tagged();

      if (doc->link > (nlinks-count-1)) doc->link = nlinks-count-1;
      doc->link = doc->link < 0 ? 0 : doc->link; 

      return count;
   } else if (doc->link < 0 || doc->link > nlinks)
      return 0;
   cp = links[doc->link].lname;
   if(is_url(cp) == FILE_URL_TYPE) {
      tp = cp;
      if(!strncmp(tp,"file://localhost",16))
	tp += 16;
      else if(!strncmp(tp,"file:",5))
	tp += 5;
      strcpy(testpath,tp);
      HTUnEscape(testpath);
      if((i = strlen(testpath)) && testpath[i-1] == '/')
	testpath[i-1] = '\0';

      if (remove_single(testpath)) {

	 if (doc->link == (nlinks-1)) --doc->link;

	 return 1;
      }
   }
   return 0;
}

PUBLIC BOOLEAN is_a_file ARGS1 (char *,testname)
{ 
   char *cp;
   char testpath[512];
   struct stat dir_info;

   cp = testname;
   if(!strncmp(cp,"file://localhost",16))
     cp += 16;
   else if(!strncmp(cp,"file:",5))
     cp += 5;
   strcpy(testpath,cp);
   HTUnEscape(testpath);
   if (stat(testpath,&dir_info) == -1) 
      return -1; 
   else
     if (((dir_info.st_mode) & S_IFMT) == S_IFREG)
       return 1;
     else
       return 0;
}

/* display or remove a tag from a given link */

PUBLIC void tagflag ARGS2(int,flag, int,cur)
{
    if (nlinks > 0 && links[cur].lx == 3) {
       move(links[cur].ly, links[cur].lx-2);
       stop_reverse();
       if (flag == ON)
	  addch('+');
       else
	  addch(' ');

#ifdef FANCY_CURSES
      if(!LYShowCursor)
          move(LYlines-1,LYcols-1);  /* get cursor out of the way */
      else
#endif /* FANCY CURSES */
	  /* never hide the cursor if there's no FANCY CURSES */
          move(links[cur].ly, links[cur].lx);

      if(flag)
          refresh();
    }
}

PUBLIC void showtags ARGS1 (taglink *, t)
{
    int i;
    taglink *s;
    
    for(i=0;i<nlinks;i++) {
      s = t;
      while(s != NULL) {
	 if(!strcmp(links[i].lname,s->name)) {
	    tagflag(ON,i);
	    break;
	 } else
	    s = s->next;
      }
   }
}

PUBLIC char * strip_trailing_slash ARGS1 (char *, dirname)
{
   int i;

   i = strlen(dirname) - 1;
   while (i && dirname[i] == '/') dirname[i--] = '\0';
   return dirname;
}

/* Perform file management operations for LYNXDIRED URL's */

PUBLIC int local_dired ARGS1(document *,doc)
{
   char *line;
   char *cp,*tp,*qp;
   char tmpbuf[256];
   char buffer[512];
   char *p1,*p2;

   DocAddress WWWDoc;  /* a WWW absolute doc address struct */

   line = doc->address;
   HTUnEscape(line);

   if (!strncmp(line,"LYNXDIRED://NEW_FILE",20)) {
      if (create_file(&line[20])) ++LYforce_no_cache;
   } else if (!strncmp(line,"LYNXDIRED://NEW_FOLDER",22)) {
      if (create_directory(&line[22])) ++LYforce_no_cache;
   } else if (!strncmp(line,"LYNXDIRED://MODIFY_NAME",23)) {
      if (modify_name(&line[23])) ++LYforce_no_cache;
   } else if (!strncmp(line,"LYNXDIRED://MODIFY_LOCATION",27)) {
      if (modify_location(&line[27])) ++LYforce_no_cache;
   } else if (!strncmp(line,"LYNXDIRED://MOVE_TAGGED",23)) {
      if (modify_tagged(&line[23])) ++LYforce_no_cache;
   } else if (!strncmp(line,"LYNXDIRED://REMOVE_SINGLE",25)) {
      if (remove_single(&line[25])) ++LYforce_no_cache;
   } else if (!strncmp(line,"LYNXDIRED://REMOVE_TAGGED",25)) {
      if (remove_tagged()) ++LYforce_no_cache;
   } else if (!strncmp(line,"LYNXDIRED://UPLOAD",18)) {
     if (LYUpload(line)) ++LYforce_no_cache;
   } else {
      if (line[strlen(line)-1] == '/')
	line[strlen(line)-1] = '\0';
      if ((cp = strrchr(line,'/')) == NULL)
	return 0;

/* Construct the appropriate system command taking care to escape all
   path references to avoid spoofing the shell. */

      *buffer = '\0';
      if (!strncmp(line,"LYNXDIRED://DECOMPRESS",22)) {
	tp = quote_pathname(line+22); 
	sprintf(buffer,"uncompress %s",tp);
	free(tp);
#ifdef OK_TAR
      } else if (!strncmp(line,"LYNXDIRED://UNTAR_Z",19)) {
	tp = quote_pathname(line+19);
	*cp++ = '\0';
	cp = quote_pathname(line+19);
	sprintf(buffer,"zcat %s | (cd %s; tar -xfe -)",tp,cp);
	free(cp);
	free(tp);
      } else if (!strncmp(line,"LYNXDIRED://TAR_Z",17)) {
	*cp++ = '\0';
        cp = quote_pathname(cp);
	tp = quote_pathname(line+17);
	sprintf(buffer,"(cd %s; tar -cfe - %s) | compress >%s/%s.tar.Z",tp,cp,tp,cp);
	free(cp);
	free(tp);
#endif
#ifdef OK_GZIP
      } else if (!strncmp(line,"LYNXDIRED://GZIP",16)) {
	tp = quote_pathname(line+16);
	sprintf(buffer,"gzip -q %s",tp);
	free(tp);
      } else if (!strncmp(line,"LYNXDIRED://UNGZIP",18)) {
	tp = quote_pathname(line+18);
	sprintf(buffer,"gzip -d %s",tp);
	free(tp);
#endif
#if defined(OK_TAR) && defined(OK_GZIP)
      } else if (!strncmp(line,"LYNXDIRED://UNTAR_GZ",20)) {
	tp = quote_pathname(line+20);
	*cp++ = '\0';
	cp = quote_pathname(line+20);
	sprintf(buffer,"gzip -qdc %s | (cd %s; tar -xfe -)",tp,cp);
	free(cp);
	free(tp);
      } else if (!strncmp(line,"LYNXDIRED://TAR_GZ",18)) {
	*cp++ = '\0';
	cp = quote_pathname(cp);
	tp = quote_pathname(line+18);
	sprintf(buffer,"(cd %s; tar -cfe - %s) | gzip -qc >%s/%s.tar.gz",tp,cp,tp,cp);
	free(cp);
	free(tp);
#endif
      } else if (!strncmp(line,"LYNXDIRED://COMPRESS",20)) {
	tp = quote_pathname(line+20);
	sprintf(buffer,"compress %s",tp);
	free(tp);
      }
      if (strlen(buffer)) {
	 if (strlen(buffer) < 60) 
	    sprintf(tmpbuf,"Executing %s ",buffer);
	 else
	    sprintf(tmpbuf,"Executing system command. This might take a while.");
	 statusline(tmpbuf);
	 system(buffer);
	 ++LYforce_no_cache;
      }
   }

   pop(doc);

   WWWDoc.address = doc->address;
   WWWDoc.post_data = doc->post_data;
   WWWDoc.post_content_type = doc->post_content_type;

   if(!HTLoadAbsolute(&WWWDoc))
     return(NOT_FOUND);
   return(NORMAL);

}

/* Provide a menu of file management options. */

PUBLIC int dired_options ARGS2 (document *,doc, char **,newfile)
{
    static char * tempfile=NULL;
    static char * dired_filename=0;
    char testpath[512],curloc[512]; /* much too large */
    char tmpbuf[LINESIZE];
    lynx_html_item_type *nxt;
    struct stat dir_info;
    FILE *fp0;
    char *cp,*tp = NULL;
    char *escaped;
    int count;

    if(tempfile == NULL) {
	tempfile = (char *) malloc(127);
        tempname(tempfile,NEW_FILE);
        /* make the file a URL now */
	StrAllocCopy(dired_filename,"file://localhost");
	StrAllocCat(dired_filename,tempfile);
    }

    if((fp0 = fopen(tempfile,"w")) == NULL) {
       statusline("Unable to open file management menu file");
       sleep(2);
       return(0);
    }
    
    StrAllocCopy(*newfile, dired_filename);
    
    cp = doc->address;
    if(!strncmp(cp,"file://localhost",16))
      cp += 16;
    else if(!strncmp(cp,"file:",5))
      cp += 5;
    strcpy(curloc,cp);
    HTUnEscape(curloc);
    if (curloc[strlen(curloc)-1] == '/')
      curloc[strlen(curloc)-1] = '\0';

    if (doc->link > -1 && doc->link < (nlinks+1)) {
       cp = links[doc->link].lname;
       if(!strncmp(cp,"file://localhost",16))
	 cp += 16;
       else if(!strncmp(cp,"file:",5))
	 cp += 5;
       strcpy(testpath,cp);
       HTUnEscape(testpath);
       if (testpath[strlen(testpath)-1] == '/')
	  testpath[strlen(testpath)-1] = '\0';

       if (stat(testpath,&dir_info) == -1) {
	  sprintf(tmpbuf,"Unable to get status of %s ",testpath);
	  statusline(tmpbuf);
	  sleep(3);
	  return 0;
       } 

       if ((cp = strrchr(testpath,'.')) != NULL && strlen(testpath) > strlen(cp)) {
	  *cp = '\0';
	  tp = strrchr(testpath,'.');
	  *cp = '.';
       }
    } else testpath[0] = '\0';

    escaped = (char *) HTEscape(testpath,(unsigned char) 4);

    fprintf(fp0,"<head><title>%s</title></head><body>",DIRED_MENU_TITLE);

    fprintf(fp0,"\n<h1>File Management Options</h1>");

    fprintf(fp0,"Current directory is %s <br>\n",curloc);

    if (tagged == NULL)
       if (strlen(testpath))
          fprintf(fp0,"Current selection is %s <p>\n",testpath);
       else
          fprintf(fp0,"Nothing currently selected. <p>\n");
    else 
       fprintf(fp0,"Current selection is all tagged items. <p>\n");

    fprintf(fp0,"<a href=\"LYNXDIRED://NEW_FILE%s\"> ",curloc);
    fprintf(fp0,"New File </a> (in current directory)<br>\n");

    fprintf(fp0,"<a href=\"LYNXDIRED://NEW_FOLDER%s\"> ",curloc);
    fprintf(fp0,"New Directory </a> (in current directory)<br>\n");

    if (tagged == NULL && strlen(testpath)) {

       fprintf(fp0,"<a href=\"LYNXDIRED://MODIFY_NAME%s\"> ",escaped);
       fprintf(fp0,"Modify Name </a> (of current selection)<br>\n");

       fprintf(fp0,"<a href=\"LYNXDIRED://MODIFY_LOCATION%s\"> ",escaped);
       fprintf(fp0,"Change Location </a> (of current selection)<br>\n");

       fprintf(fp0,"<a href=\"LYNXDIRED://REMOVE_SINGLE%s\"> ",escaped);
       fprintf(fp0,"Remove </a> (current selection)<br>\n");

       if (tp != NULL && !strcmp(tp,".tar.Z")) {
#ifdef OK_TAR
	  fprintf(fp0,"<a href=\"LYNXDIRED://UNTAR_Z%s\"> ",escaped);
	  fprintf(fp0,"Expand </a> (current selection)<br>\n");
#endif
       } else if (tp != NULL && !strcmp(tp,".tar.gz")) {
#if defined(OK_TAR) && defined(OK_GZIP)
	  fprintf(fp0,"<a href=\"LYNXDIRED://UNTAR_GZ%s\"> ",escaped);
	  fprintf(fp0,"Expand </a> (current selection)<br>\n");
#endif
       } else if (cp != NULL && !strcmp(cp,".Z")) {
	  
	  fprintf(fp0,"<a href=\"LYNXDIRED://DECOMPRESS%s\"> ",escaped);
	  fprintf(fp0,"Uncompress </a> (current selection) <br>\n");
	  
       } else if (cp != NULL && !strcmp(cp,".gz")) {
#ifdef OK_GZIP
	  fprintf(fp0,"<a href=\"LYNXDIRED://UNGZIP%s\"> ",escaped);
	  fprintf(fp0,"Uncompress </a> (current selection) <br>\n");
#endif	  
       } else if (cp != NULL && !strcmp(cp,".zip")) {
#ifdef OK_ZIP	  
	  fprintf(fp0,"<a href=\"LYNXDIRED://UNZIP%s\"> ",escaped);
	  fprintf(fp0,"Expand </a> zip Archive<br>\n");
#endif	  
       } else {

	  if ((dir_info.st_mode & S_IFMT) == S_IFDIR) {
#ifdef OK_TAR
	     fprintf(fp0,"<a href=\"LYNXDIRED://TAR_Z%s\"> ",escaped);
	     fprintf(fp0,"Tar and compress </a> (using Unix compress)<br>\n");
#endif
#if defined(OK_TAR) && defined(OK_GZIP)
	     fprintf(fp0,"<a href=\"LYNXDIRED://TAR_GZ%s\"> ",escaped);
	     fprintf(fp0,"Tar and compress </a> (using GNU gzip)<br>\n");
#endif
	  } else if ((dir_info.st_mode & S_IFMT) == S_IFREG) {

	     fprintf(fp0,"<a href=\"LYNXDIRED://COMPRESS%s\"> ",escaped);
	     fprintf(fp0,"Compress </a> (using Unix compress)<br>\n");
#ifdef OK_GZIP	  
	     fprintf(fp0,"<a href=\"LYNXDIRED://GZIP%s\"> ",escaped);
	     fprintf(fp0,"Compress </a> (using gzip) <br>\n");
#endif
	  }
	  
       }
             
    } else if (tagged != NULL) {

       fprintf(fp0,"<a href=\"LYNXDIRED://MOVE_TAGGED\"> ");
       fprintf(fp0,"Move all tagged items to another location.</a><br>\n");

       fprintf(fp0,"<a href=\"LYNXDIRED://REMOVE_TAGGED\"> ");
       fprintf(fp0,"Remove all tagged files and directories.</a><br>\n");
    }

    if (uploaders != NULL) {
       for (count=0, nxt = uploaders; nxt != NULL; nxt = nxt->next, count++) {
	  fprintf(fp0,"<a href=\"LYNXDIRED://UPLOAD=%d/TO=%s\"> %s </a>\n",count,curloc,nxt->name);
       }
    }

    fclose(fp0);

    free(escaped);

    LYforce_no_cache = 1;

    return(0);
}

#endif /* DIRED_SUPPORT */
