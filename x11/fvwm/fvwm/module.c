/****************************************************************************
 * This module is all original code 
 * by Rob Nation (nation@rocket.sanders.lockheed.com 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

/***********************************************************************
 *
 * code for launching rfvwm modules.
 *
 ***********************************************************************/

#include "../configure.h"

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <X11/keysym.h>

#include "fvwm.h"
#include "menus.h"
#include "misc.h"
#include "parse.h"
#include "screen.h"
#include "module.h"


#ifdef MODULES
int npipes;
int *readPipes;
int *writePipes;
#endif


void initModules(void)
{
#ifdef MODULES
  int i;

#ifndef NO_SYSCONF
  npipes = sysconf(_SC_OPEN_MAX);
#else
  npipes = getdtablesize();
#endif

  writePipes = (int *)malloc(sizeof(int)*npipes);
  readPipes = (int *)malloc(sizeof(int)*npipes);
  
  for(i=0;i<npipes;i++)
    {
      writePipes[i]= -1;
      readPipes[i]= -1;
    }
#endif
}

void ClosePipes(void)
{
#ifdef MODULES
  int i;
  int closed = 0;
#ifdef NEEDS_SIGCHLD
  int waits = 0;
#if defined(SYSV) || defined(SVR4) || defined(_POSIX_SOURCE)
  pid_t pid;
  int status;
#else
#ifdef UNION_WAIT
  union wait status;
#else
  int status;
#endif
  int pid;
#endif
#endif
  for(i=0;i<npipes;i++)   
 {
      if(writePipes[i]>0)
	{
	  close(writePipes[i]);
	  close(readPipes[i]);
	  closed++;
	}
    }

  /* now that we closed the things, we need to wait for
   * them to die. If we don't wait, they turn into
   * zombies, since the SIGCHLD signal gets lost during the
   * re-start */
#ifdef NEEDS_SIGCHLD
  while((closed>0)&&(waits < closed+100))
    {
#if defined(SYSV) || defined(SVR4) || defined(_POSIX_SOURCE)
      pid = waitpid(-1, &status, WNOHANG);
#else
      pid = wait3(&status, WNOHANG, (struct rusage *)0);
#endif
      /* if the process was stopped, kill it */
      if(pid > 0)
	{
	  if (WIFSTOPPED(status))
	    {
	      kill(pid, SIGKILL);
	    }
	  --closed;
	}
      sleep_a_little(100000);
      waits++;
    }
#endif
#endif
}

void executeModule(char *action,FILE *fd, char **win, int *context)
{
#ifdef MODULES
  int fvwm_to_app[2],app_to_fvwm[2];
  int i,val;
  char command[256];
  char *cptr;
  char *aptr;
  char *args[10];
  char *arg1 = NULL;
  char arg2[40];
  char arg3[40];
  char arg5[40];
  char arg6[40];
  char *end;
  extern char *ModulePath;
  extern FILE *config_fd;
  extern char *fvwm_file;

  strcpy(command,action);

  cptr = command;
  while((isspace(*cptr))&&(*cptr != '\n')&&(*cptr != 0))
    cptr++;

  end = cptr;
  while((!(isspace(*end))&&(*end != '\n'))&&(*end != 0)&&(end <(command+256)))
    end++;

  if((*end == 0)||(end >= command+256))
    aptr = NULL;
  else
    aptr = end+1;
  *end = 0;

  if(aptr)
    {
      while((isspace(*aptr)||(*aptr=='\n'))&&(*aptr!=0)&&(aptr<(command+256)))
	aptr++;
      if((*aptr == 0)||(*aptr == '\n'))
	aptr = NULL;
    }

  arg1 = findIconFile(cptr,ModulePath,X_OK);
  if(arg1 == NULL)
    {
      fprintf(stderr,"Fvwm: No such module %s %s\n",ModulePath,cptr);
      return;
    }

  /* Look for an available pipe slot */
  i=0;
  while((i<npipes) && (writePipes[i] >=0))
    i++;
  if(i>=npipes)
    {
      fprintf(stderr,"fvwm: Too many Accessories!\n");
      return;
    }
  
  /* I want one-ended pipes, so I open two two-ended pipes,
   * and close one end of each. I need one ended pipes so that
   * I can detect when the module crashes/malfunctions */
  if(pipe(fvwm_to_app)!=0)
    {
      fprintf(stderr,"Fvwm: Failed to open pipe\n");
      return;
    }
  if(pipe(app_to_fvwm)!=0)
    {
      fprintf(stderr,"Fvwm: Failed to open pipe2\n");
      close(fvwm_to_app[0]);
      close(fvwm_to_app[1]);
      return;
    }
  val = fork();
  if(val > 0)
    {
      /* This fork remains running fvwm */
      /* close appropriate descriptors from each pipe so
       * that fvwm will be able to tell when the app dies */
      close(app_to_fvwm[1]);
      close(fvwm_to_app[0]);

      /* add these pipes to fvwm's active pipe list */
      writePipes[i] = fvwm_to_app[1];
      readPipes[i] = app_to_fvwm[0];
      free(arg1);

      /* make the write pipe non-blocking. Don't want to jam up
	 fvwm because of an uncooperative module */
      fcntl(writePipes[i],F_SETFL,O_NDELAY);
      /* Mark the pipes close-on exec so other programs
       * won`t inherit them */
      if (fcntl(readPipes[i], F_SETFD, 1) == -1) 
	fvwm_err("module close-on-exec failed",NULL,NULL,NULL);
      if (fcntl(writePipes[i], F_SETFD, 1) == -1) 
	fvwm_err("module close-on-exec failed",NULL,NULL,NULL);
    }
  else if (val ==0)
    {
      /* this is  the child */
      /* need to close config_fd if its still open! */
      if(config_fd != (FILE *)NULL)
	/* Fixes some funny stuff with svr4 and stream IO */
	/* fclose(config_fd) */
	close(fileno(config_fd));

      /* this fork execs the module */
      close(fvwm_to_app[1]);
      close(app_to_fvwm[0]);
      sprintf(arg2,"%d",app_to_fvwm[1]);
      sprintf(arg3,"%d",fvwm_to_app[0]);
      sprintf(arg5,"%lx",(unsigned long)win);
      sprintf(arg6,"%lx",(unsigned long)context);
      args[0]=arg1;
      args[1]=arg2;
      args[2]=arg3;
      args[3]=fvwm_file;
      args[4]=arg5;
      args[5]=arg6;
      if(aptr != NULL)
	{
	  args[6] = aptr;
	  args[7] = 0;
	}
      else
	args[6]= 0;
      execvp(arg1,args);
      fprintf(stderr,"Execution of module failed: %s",arg1);      
      perror("");
      close(app_to_fvwm[1]);
      close(fvwm_to_app[0]);
      exit(1);
    }
  else
    {
      fprintf(stderr,"Fork failed\n");
      free(arg1);
    }
  return;
#endif
}

void HandleModuleInput(Window w, int channel)
{
#ifdef MODULES
  char text[256];
  int size;
  int cont,n;
  char *newaction = NULL;

  /* Already read a (possibly NULL) window id from the pipe,
   * Now read an fvwm bultin command line */
  n = read(readPipes[channel], &size, sizeof(int));
  if(n < sizeof(int))
    {
      KillModule(channel,1);
      return;
    }

  if(size >255)
    {
      fprintf(stderr,"Module command is too big (%d)\n",size);
      size=255;
    }
  n = read(readPipes[channel],text, size);
  if(n < size)
    {
      KillModule(channel,2);
      return;
    }
  
  text[n]=0;

  n = read(readPipes[channel],&cont, sizeof(int));
  if(n < sizeof(int))
    {
      KillModule(channel,3);
      return;
    }
  if(cont == 0)
    {
      KillModule(channel,4);
    }
  if(strlen(text)>0)
    {
      char function[256],*ptr;
      MenuRoot *mr=0;
      char *item=NULL;
      extern int func_val_1,func_val_2,func,Context;
      extern struct config func_config[];
      extern unsigned PopupCount;
      extern MenuRoot *PopupTable[MAXPOPUPS];      
      FvwmWindow *tmp_win;
      extern char *orig_tline;

      orig_tline = text;
      Event.xany.type = ButtonRelease;
      Event.xany.window = w;
	
      func_val_1 = 0;
      func_val_2 = 0;
      
      sscanf(text,"%s %d %d",function,&func_val_1,&func_val_2);
  
      func = F_NOP;
      match_string(func_config,function,"bad mouse function:",NULL);
      if((func == F_POPUP)||(func == F_FUNCTION))
	{
	  unsigned i;
	  ptr = stripcpy2(text,0,True);
	  if(ptr != NULL)
	    for (i = 0; i < PopupCount; i++)
	      if (strcasecmp(PopupTable[i]->name,ptr) == 0)
		{
		  mr = PopupTable[i];
		  break;
		}
	  if (!mr)
	    {
	      no_popup(ptr);
	      func = F_NOP;
	    }
	}
      else if((func == F_EXEC)||(func == F_RESTART)||
	      (func == F_CIRCULATE_UP)||(func == F_CIRCULATE_DOWN)||
	      (func == F_WARP)||(func == F_MODULE))
	{
	  if((func == F_EXEC)||(func == F_RESTART)||(func== F_MODULE))
	    {
	      item = stripcpy2(text,0,True);
	      newaction = stripcpy3(text,True);
	    }
	  else
	    {
	      item = stripcpy2(text,0,False);
	      newaction = stripcpy3(text,False);
	    }
	}
      if (XFindContext (dpy, w, FvwmContext, (caddr_t *) &tmp_win) == XCNOENT)
	{
	  tmp_win = NULL;
	  w = None;
	}
      if(tmp_win)
	{
	  Event.xbutton.button = 1;
	  Event.xbutton.x_root = tmp_win->frame_x;
	  Event.xbutton.y_root = tmp_win->frame_y;
	  Event.xbutton.x = 0;
	  Event.xbutton.y = 0;
	  Event.xbutton.subwindow = None;
	}
      else
	{
	  Event.xbutton.button = 1;
	  Event.xbutton.x_root = 0;
	  Event.xbutton.y_root = 0;
	  Event.xbutton.x = 0;
	  Event.xbutton.y = 0;
	  Event.xbutton.subwindow = None;
	}

      Context = GetContext(tmp_win,&Event,&w);
      ExecuteFunction(func,newaction, w, tmp_win, &Event, Context,
		      func_val_1,func_val_2,mr,channel)  ;
    }
  return;
#endif
}


void DeadPipe(int nonsense)
{
  signal(SIGPIPE, DeadPipe);
}

void KillModule(int channel, int place)
{
#ifdef MODULES
  close(readPipes[channel]);
  close(writePipes[channel]);
  
  readPipes[channel] = -1;
  writePipes[channel] = -1;
  return;
#endif
}


void Broadcast(unsigned long event_type, unsigned long num_datum,
	       unsigned long data1, unsigned long data2, unsigned long data3, 
	       unsigned long data4, unsigned long data5, unsigned long data6,
	       unsigned long data7)
{
#ifdef MODULES
  int i;
  unsigned long header[3];
  unsigned long body[7];

  header[0] = START_FLAG;
  header[1] = event_type;
  header[2] = num_datum+3;
    
  if(num_datum>0)
    body[0] = data1;
  if(num_datum>1)
    body[1] = data2;
  if(num_datum>2)
    body[2] = data3;
  if(num_datum>3)
    body[3] = data4;
  if(num_datum>4)
    body[4] = data5;
  if(num_datum>5)
    body[5] = data6;
  if(num_datum>6)
    body[6] = data7;

  for(i=0;i<npipes;i++)   
    {
      if(writePipes[i]>0)
	{
	  write(writePipes[i],header,3*sizeof(unsigned long));
	  if(num_datum>0)
	    write(writePipes[i],body,num_datum*sizeof(unsigned long));
	}
    }
#endif
}




void SendPacket(int module, unsigned long event_type, unsigned long num_datum,
	       unsigned long data1, unsigned long data2, unsigned long data3, 
	       unsigned long data4, unsigned long data5, unsigned long data6,
	       unsigned long data7)
{
#ifdef MODULES
  unsigned long header[3];
  unsigned long body[7];

  header[0] = START_FLAG;
  header[1] = event_type;
  header[2] = num_datum+3;
    
  if(num_datum>0)
    body[0] = data1;
  if(num_datum>1)
    body[1] = data2;
  if(num_datum>2)
    body[2] = data3;
  if(num_datum>3)
    body[3] = data4;
  if(num_datum>4)
    body[4] = data5;
  if(num_datum>5)
    body[5] = data6;
  if(num_datum>6)
    body[6] = data7;
  if(writePipes[module]>0)
    {
      write(writePipes[module],header,3*sizeof(unsigned long));
      if(num_datum>0)
	write(writePipes[module],body,num_datum*sizeof(unsigned long));
    }
#endif
}

void SendConfig(int module, unsigned long event_type, FvwmWindow *t)
{
#ifdef MODULES
  unsigned long header[HEADER_SIZE];
  unsigned long body[MAX_BODY_SIZE];

  header[0] = START_FLAG;
  header[1] = event_type;
  header[2] = 25;
  body[0] = t->w;
  body[1] = t->frame;
  body[2] = (unsigned long)t;
  body[3] = t->frame_x;
  body[4] = t->frame_y;
  body[5] = t->frame_width;
  body[6] = t->frame_height;
  body[7] = t->Desk;
  body[8] = t->flags;
  body[9] = t->title_height;
  body[10] = t->boundary_width;
  body[11] = t->hints.base_width;
  body[12] = t->hints.base_height;
  body[13] = t->hints.width_inc;
  body[14] = t->hints.height_inc;
  body[15] = t->hints.min_width;
  body[16] = t->hints.min_height;
  body[17] = t->hints.max_width;
  body[18] = t->hints.max_height;
  body[19] = t->icon_w;
  body[20] = t->icon_pixmap_w;
  body[21] = t->hints.win_gravity;
  
  if(writePipes[module]>0)
    {
      write(writePipes[module],header,3*sizeof(unsigned long));
      write(writePipes[module],body,22*sizeof(unsigned long));
    }
#endif
}


void BroadcastConfig(unsigned long event_type, FvwmWindow *t)
{
#ifdef MODULES
  unsigned long header[HEADER_SIZE];
  unsigned long body[MAX_BODY_SIZE];
  int i;

  header[0] = START_FLAG;
  header[1] = event_type;
  header[2] = 25;
  body[0] = t->w;
  body[1] = t->frame;
  body[2] = (unsigned long)t;
  body[3] = t->frame_x;
  body[4] = t->frame_y;
  body[5] = t->frame_width;
  body[6] = t->frame_height;
  body[7] = t->Desk;
  body[8] = t->flags;
  body[9] = t->title_height;
  body[10] = t->boundary_width;
  body[11] = t->hints.base_width;
  body[12] = t->hints.base_height;
  body[13] = t->hints.width_inc;
  body[14] = t->hints.height_inc;
  body[15] = t->hints.min_width;
  body[16] = t->hints.min_height;
  body[17] = t->hints.max_width;
  body[18] = t->hints.max_height;
  body[19] = t->icon_w;
  body[20] = t->icon_pixmap_w;
  body[21] = t->hints.win_gravity;
  for(i=0;i<npipes;i++)   
    {  
      if(writePipes[i]>0)
	{
	  write(writePipes[i],header,3*sizeof(unsigned long));
	  write(writePipes[i],body,22*sizeof(unsigned long));
	}
    }
#endif
}

void BroadcastName(unsigned long event_type, unsigned long data1,
		   unsigned long data2, unsigned long data3, char *name)
{
#ifdef MODULES
  int i,l;
  char *c;
  unsigned long header[3],body[3];

  l=strlen(name)/(sizeof(unsigned long))+1;
  c = calloc(l,sizeof(unsigned long));
  if(c == (char *)NULL)
    {
      fprintf(stderr,"Fvwm: Couldn't malloc space for Broadcast\n");
      return;
    }
  strcpy(c,name);

  header[0] = START_FLAG;
  header[1] = event_type;
  header[2] = l+6;
    
  body[0] = data1;
  body[1] = data2;
  body[2] = data3;

  for(i=0;i<npipes;i++)   
    {
      if(writePipes[i]>0)
	{
	  write(writePipes[i],header,3*sizeof(unsigned long));
	  write(writePipes[i],body,3*sizeof(unsigned long));
	  if(l>0)write(writePipes[i],c,l*sizeof(unsigned long));
	}
    }
  free(c);
#endif
}


void SendName(int module, unsigned long event_type,
	      unsigned long data1,unsigned long data2, 
	      unsigned long data3, char *name)
{
#ifdef MODULES
  int l;
  char *c;
  unsigned long header[3],body[3];

  l=strlen(name)/(sizeof(unsigned long))+1;
  c = calloc(l,sizeof(unsigned long));
  if(c == (char *)NULL)
    {
      fprintf(stderr,"Fvwm: Couldn't malloc space for SendName\n");
      return;
    }
  strcpy(c,name);

  header[0] = START_FLAG;
  header[1] = event_type;
  header[2] = l+6;

  body[0] = data1;
  body[1] = data2;
  body[2] = data3;
    
  if(writePipes[module]>0)
    {
      write(writePipes[module],header,3*sizeof(unsigned long));
      write(writePipes[module],body,3*sizeof(unsigned long));
      if(l>0)write(writePipes[module],c,l*sizeof(unsigned long));
    }
  free(c);
#endif
}




