/*			File Transfer Protocol (FTP) Client
**			for a WorldWideWeb browser
**			===================================
**
**	A cache of control connections is kept.
**
** Note: Port allocation
**
**	It is essential that the port is allocated by the system, rather
**	than chosen in rotation by us (POLL_PORTS), or the following
**	problem occurs.
**
**	It seems that an attempt by the server to connect to a port which has
**	been used recently by a listen on the same socket, or by another
**	socket this or another process causes a hangup of (almost exactly)
**	one minute. Therefore, we have to use a rotating port number.
**	The problem remains that if the application is run twice in quick
**	succession, it will hang for what remains of a minute.
**
** Authors
**	TBL	Tim Berners-lee <timbl@info.cern.ch>
**	DD	Denis DeLaRoca 310 825-4580 <CSP1DWD@mvs.oac.ucla.edu>
**      LM      Lou Montulli <montulli@ukanaix.cc.ukans.edu>
**      FM      Foteos Macrides <macrides@sci.wfeb.edu>
** History:
**	 2 May 91	Written TBL, as a part of the WorldWideWeb project.
**	15 Jan 92	Bug fix: close() was used for NETCLOSE for control soc
**	10 Feb 92	Retry if cached connection times out or breaks
**	 8 Dec 92	Bug fix 921208 TBL after DD
**	17 Dec 92	Anon FTP password now just WWWuser@ suggested by DD
**			fails on princeton.edu!
**	27 Dec 93 (FM)  Fixed up so FTP now works with VMS hosts.  Path
**			must be Unix-style and cannot include the device
**			or top directory.
**      ?? ??? ?? (LM)  Added code to prompt and send passwords for non
**			anonymous FTP
**      25 Mar 94 (LM)  Added code to recognize different ftp server types
**                      and code to parse dates and sizes on most hosts.
**	27 Mar 93 (FM)  Added code for getting dates and sizes on VMS hosts.
**
** Options:
**	LISTEN		We listen, the other guy connects for data.
**			Otherwise, other way round, but problem finding our
**			internet address!
**
** Notes:
**     			Portions Copyright 1994 Trustees of Dartmouth College
** 			Code for recognizing different FTP servers and
**			parsing "ls -l" output taken from Macintosh Fetch
**			program with permission from Jim Matthews,
**			Dartmouth Software Development Team.
*/

#define LISTEN	 /* @@@@ Test LJM */

/*
BUGS:	@@@  	Limit connection cache size!
		Error reporting to user.
		400 & 500 errors are acked by user with windows.
		Use configuration file for user names
		
**		Note for portablility this version does not use select() and
**		so does not watch the control and data channels at the
**		same time.
*/		

#include "HTFTP.h"	/* Implemented here */

/* this define should be in HTFont.h :( */
#define HT_NON_BREAK_SPACE ((char)1)   /* For now */

#define REPEAT_PORT	/* Give the port number for each file */
#define REPEAT_LISTEN	/* Close each listen socket and open a new one */

/* define POLL_PORTS		 If allocation does not work, poll ourselves.*/
#define LISTEN_BACKLOG 2	/* Number of pending connect requests (TCP)*/

#define FIRST_TCP_PORT	1024	/* Region to try for a listening port */
#define LAST_TCP_PORT	5999	

#define LINE_LENGTH 256
#define COMMAND_LENGTH 256

#include "HTParse.h"
#include "HTUtils.h"
#include "tcp.h"
#include "HTTCP.h"
#include "HTAnchor.h"
#include "HTFile.h"	/* For HTFileFormat() */
#include "HTBTree.h"
#include "HTChunk.h"
#include "HTAlert.h"
#ifndef IPPORT_FTP
#define IPPORT_FTP	21
#endif

#ifdef REMOVED_CODE
extern char *malloc();
extern void free();
extern char *strncpy();
#endif

typedef struct _connection {
    struct _connection *	next;	/* Link on list 	*/
    u_long			addr;	/* IP address		*/
    int				socket;	/* Socket number for communication */
    BOOL			binary; /* Binary mode? */
} connection;

#ifndef NIL
#define NIL 0
#endif

/*		Hypertext object building machinery
*/
#include "HTML.h"

#define PUTC(c) (*targetClass.put_character)(target, c)
#define PUTS(s) (*targetClass.put_string)(target, s)
#define START(e) (*targetClass.start_element)(target, e, 0, 0)
#define END(e) (*targetClass.end_element)(target, e)
#define FREE_TARGET (*targetClass.free)(target)
#define ABORT_TARGET (*targetClass.free)(target)
struct _HTStructured {
	CONST HTStructuredClass *	isa;
	/* ... */
};


/*	Global Variables
**	---------------------
*/
PUBLIC BOOLEAN HTfileSortMethod = FILE_BY_NAME;

/*	Module-Wide Variables
**	---------------------
*/
PRIVATE connection * connections =0;	/* Linked list of connections */
PRIVATE char    response_text[LINE_LENGTH+1];/* Last response from NewsHost */
PRIVATE connection * control;		/* Current connection */
PRIVATE int	data_soc = -1;		/* Socket for data transfer =invalid */

#define GENERIC_SERVER	0
#define MACHTEN_SERVER 	1
#define UNIX_SERVER 	2
#define VMS_SERVER 	3
#define CMS_SERVER 	4
#define DCTS_SERVER    	5
#define TCPC_SERVER	6
#define PETER_LEWIS_SERVER	7
#define NCSA_SERVER	8

PRIVATE int     server_type = GENERIC_SERVER;   /* the type of ftp host */
PRIVATE int     unsure_type = FALSE;            /* sure about the type? */
PRIVATE BOOLEAN use_list = FALSE;		/* use the LIST command? */

PRIVATE interrupted_in_next_data_char = FALSE;

#ifdef POLL_PORTS
PRIVATE	unsigned short	port_number = FIRST_TCP_PORT;
#endif

#ifdef LISTEN
PRIVATE int     master_socket = -1;	/* Listening socket = invalid	*/
PRIVATE char	port_command[255];	/* Command for setting the port */
PRIVATE fd_set	open_sockets; 		/* Mask of active channels */
PRIVATE int	num_sockets;  		/* Number of sockets to scan */
#else
PRIVATE	unsigned short	passive_port;	/* Port server specified for data */
#endif


#define NEXT_CHAR HTGetCharacter()	/* Use function in HTFormat.c */

#define DATA_BUFFER_SIZE 2048
PRIVATE char data_buffer[DATA_BUFFER_SIZE];		/* Input data buffer */
PRIVATE char * data_read_pointer;
PRIVATE char * data_write_pointer;
#define NEXT_DATA_CHAR next_data_char()


/*	Procedure: Read a character from the data connection
**	----------------------------------------------------
*/
PRIVATE char next_data_char
NOARGS
{
    int status;
    if (data_read_pointer >= data_write_pointer) {
	status = NETREAD(data_soc, data_buffer, DATA_BUFFER_SIZE);
      if (status == HT_INTERRUPTED)
        interrupted_in_next_data_char = 1;
      if (status <= 0)
        return (char)-1;
      data_write_pointer = data_buffer + status;
      data_read_pointer = data_buffer;
    }
#ifdef NOT_ASCII
    {
        char c = *data_read_pointer++;
	return FROMASCII(c);
    }
#else
    return *data_read_pointer++;
#endif
}


/*	Close an individual connection
**
*/
#ifdef __STDC__
PRIVATE int close_connection(connection * con)
#else
PRIVATE int close_connection(con)
    connection *con;
#endif
{
    connection * scan;
    int status = NETCLOSE(con->socket);
    if (TRACE) fprintf(stderr, "FTP: Closing control socket %d\n", con->socket);
    if (connections==con) {
        connections = con->next;
	return status;
    }
    for(scan=connections; scan; scan=scan->next) {
        if (scan->next == con) {
	    scan->next = con->next;	/* Unlink */
	    if (control==con) control = (connection*)0;
	    return status;
	} /*if */
    } /* for */
    return -1;		/* very strange -- was not on list. */
}

PRIVATE char *help_message_buffer = 0;  /* global :( */

PRIVATE void init_help_message_cache NOARGS
{
    if(help_message_buffer) free(help_message_buffer);
    help_message_buffer = 0;
}

PRIVATE void help_message_cache_add ARGS1(char *,string)
{
    if(help_message_buffer)
        StrAllocCat(help_message_buffer, string);
    else	
        StrAllocCopy(help_message_buffer, string);

    if(TRACE)
	fprintf(stderr,"Adding message to help cache: %s\n",string);
}

PRIVATE char *help_message_cache_non_empty NOARGS
{
  return(help_message_buffer);
}
PRIVATE char *help_message_cache_contents NOARGS
{
   return(help_message_buffer);
}

/*	Execute Command and get Response
**	--------------------------------
**
**	See the state machine illustrated in RFC959, p57. This implements
**	one command/reply sequence.  It also interprets lines which are to
**	be continued, which are marked with a "-" immediately after the
**	status code.
**
**	Continuation then goes on until a line with a matching reply code
**	an a space after it.
**
** On entry,
**	con	points to the connection which is established.
**	cmd	points to a command, or is NIL to just get the response.
**
**	The command is terminated with the CRLF pair.
**
** On exit,
**	returns:  The first digit of the reply type,
**		  or negative for communication failure.
*/
#ifdef __STDC__
PRIVATE int response(char * cmd)
#else
PRIVATE int response(cmd)
    char * cmd;
#endif
{
    int result;				/* Three-digit decimal code */
    int	continuation_response = -1;
    int status;
    extern int interrupted_in_htgetcharacter;
    
    if (!control) {
          if(TRACE) fprintf(stderr, "FTP: No control connection set up!!\n");
	  return -99;
    }
    
    if (cmd) {
    
	if (TRACE) fprintf(stderr, "  Tx: %s", cmd);

#ifdef NOT_ASCII
	{
	    char * p;
	    for(p=cmd; *p; p++) {
	        *p = TOASCII(*p);
	    }
	}
#endif 
	status = NETWRITE(control->socket, cmd, (int)strlen(cmd));
	if (status<0) {
	    if (TRACE) fprintf(stderr, 
	    	"FTP: Error %d sending command: closing socket %d\n",
		status, control->socket);
	    close_connection(control);
	    return status;
	}
    }

    do {
	char *p = response_text;
	for(;;) {  
	    if (((*p++=NEXT_CHAR) == LF)
			|| (p == &response_text[LINE_LENGTH])) {

		char continuation;

	        if (interrupted_in_htgetcharacter)
                  {
                    if (TRACE)
                      fprintf (stderr, "FTP: Interrupted in HTGetCharacter, apparently.\n");
                    NETCLOSE (control->socket);
                    control->socket = -1;
                    return HT_INTERRUPTED;
                  }

		*p=0;			/* Terminate the string */
		if (TRACE) fprintf(stderr, "    Rx: %s", response_text);

		if(server_type == UNIX_SERVER && 
					!strncmp(response_text,"250-",4))
		    help_message_cache_add(response_text+4);

		sscanf(response_text, "%d%c", &result, &continuation);
		if  (continuation_response == -1) {
			if (continuation == '-')  /* start continuation */
			    continuation_response = result;
		} else { 	/* continuing */
			if (continuation_response == result
				&& continuation == ' ')
			    continuation_response = -1;	/* ended */
		}	
		break;	    
	    } /* if end of line */
	    
	    if (interrupted_in_htgetcharacter)
               {
                    if (TRACE)
                      fprintf (stderr, "FTP: Interrupted in HTGetCharacter, apparently.\n");
                    NETCLOSE (control->socket);
                    control->socket = -1;
                    return HT_INTERRUPTED;
               }

	    if (*(p-1) == (char) EOF) {
		if(TRACE) fprintf(stderr, "Error on rx: closing socket %d\n",
		    control->socket);
		strcpy(response_text, "000 *** TCP read error on response\n");
	        close_connection(control);
	    	return -1;	/* End of file on response */
	    }
	} /* Loop over characters */

    } while (continuation_response != -1);
    
    if (result==421) {
	if(TRACE) fprintf(stderr, "FTP: They close so we close socket %d\n",
	    control->socket);
	close_connection(control);
	return -1;
    }
    return result/100;
}

/* this function should try to set the macintosh server into binary mode
 */
PRIVATE int set_mac_binary NOARGS
{
	/* try to set mac binary mode */
    return(2 == response("MACB\r\n"));
}

/* This function gets the current working directory to help
 * determine what kind of host it is
 */

PRIVATE void get_ftp_pwd ARGS2(int *,server_type, BOOLEAN *,use_list) {

    char *cp;
    /* get the working directory (to see what it looks like) */
    int status = response("PWD\r\n");
    if (status < 0)
        return;
    else 
     {

	cp = strchr(response_text+5,'"');
	if(cp) *cp = '\0';
        if (*server_type == TCPC_SERVER)
         {
            *server_type = response_text[5] == '/' ? NCSA_SERVER : TCPC_SERVER;
         }
        else if (response_text[5] == '/')
         {
            /* path names beginning with / imply Unix,
	     * right? 
	     */
	     if(set_mac_binary())
		   *server_type = NCSA_SERVER;
	     else
		{
                   *server_type = UNIX_SERVER;
                   *use_list = TRUE;
		}
	     return;
         }
        else if (response_text[strlen(response_text)-1] == ']')
         {
             /* path names ending with ] imply VMS, right? */
             *server_type = VMS_SERVER;
	     *use_list = TRUE;
         }
        else
             *server_type = GENERIC_SERVER;

        if ((*server_type == NCSA_SERVER) ||
               (*server_type == TCPC_SERVER) ||
                    (*server_type == PETER_LEWIS_SERVER))
            set_mac_binary();
     }
}

/*	Get a valid connection to the host
**	----------------------------------
**
** On entry,
**	arg	points to the name of the host in a hypertext address
** On exit,
**	returns	<0 if error
**		socket number if success
**
**	This routine takes care of managing timed-out connections, and
**	limiting the number of connections in use at any one time.
**
**	It ensures that all connections are logged in if they exist.
**	It ensures they have the port number transferred.
*/
PRIVATE int get_connection ARGS1 (CONST char *,arg)
{
    int status;
    char * command;
    connection * con = (connection *)malloc(sizeof(*con));

    char * username=0;
    char * password=0;
    static char *user_entered_password=0;
    static char *last_username_and_host=0;
    
    if (!arg) return -1;		/* Bad if no name sepcified	*/
    if (!*arg) return -1;		/* Bad if name had zero length	*/

/* Get node name:
*/
    {
	char *p1 = HTParse(arg, "", PARSE_HOST);
	char *p2 = strrchr(p1, '@');	/* user? */
	char * pw=0;

	if (p2!=NULL) {
	    username = p1;
	    *p2=0;			/* terminate */
	    p1 = p2+1;			/* point to host */
	    pw = strchr(username, ':');
	    if (pw) {
	        *pw++ = 0;
		password = pw;
	    }

	    /* if the password doesn't exist then we are going to have
	     * to ask the user for it.  The only problem is that we
	     * don't want to ask for it every time, so we will store
	     * away in a primitive fashion.
	     */
	    if(!password) {
		char tmp[256];

		sprintf(tmp,"%s@%s",username,p1);
		/* if the user@host is not equal to the last time through
		 * or user_entered_password has no data then we need
		 * to ask the user for the password
		 */
		if(!last_username_and_host ||
			strcmp(tmp,last_username_and_host) ||
						!user_entered_password) {

		    StrAllocCopy(last_username_and_host,tmp);
		    sprintf(tmp,"Enter password for user %s@%s:",username,p1);
		    if(user_entered_password) 
			free(user_entered_password);
		    user_entered_password = (char *)HTPromptPassword(tmp);

		} /* else we already know the password */
		password = user_entered_password;
	    }
	}

        if (!username) free(p1);
    } /* scope of p1 */

        
  con->socket = -1;
  status = HTDoConnect (arg, "FTP", IPPORT_FTP, &con->socket);
   
  if (status < 0)
    {
      if (TRACE)
        {
          if (status == HT_INTERRUPTED)
            fprintf (stderr,
                     "FTP: Interrupted on connect\n");
          else
            fprintf(stderr,
                    "FTP: Unable to connect to remote host for `%s'.\n",
                    arg);
        }
      if (status == HT_INTERRUPTED)
        HTProgress ("Connection interrupted.");
      if (con->socket != -1)
        {
          NETCLOSE(con->socket);
        }
	
      if (username)
          free(username);
      free(con);
      return status;                    /* Bad return */
    }

	
    if (TRACE) 
 	fprintf(stderr, "FTP connected, socket %d\n", con);
    control = con;		/* Current control connection */

    /* Initialise buffering for contron connection */
    HTInitInput(control->socket);


/*	Now we log in		Look up username, prompt for pw.
*/
  {
    int status = response((char *)0);	/* Get greeting */

    if (status == HT_INTERRUPTED)
      {
        if (TRACE)
          fprintf (stderr,
                   "FTP: Interrupted at beginning of login.\n");
        HTProgress ("Connection interrupted.");
        NETCLOSE(control->socket);
        control->socket = -1;
        return HT_INTERRUPTED;
      }
    if (status == 2) {		/* Send username */
	if (username) {
	    command = (char*)malloc(10+strlen(username)+2+1);
	    if (command == NULL) outofmem(__FILE__, "get_connection");
	    sprintf(command, "USER %s%c%c", username, CR, LF);
	} else {
	    command = (char*)malloc(24);
	    if (command == NULL) outofmem(__FILE__, "get_connection");
	    sprintf(command, "USER anonymous%c%c", CR, LF);
        }
	status = response(command);
	free(command);
        if (status == HT_INTERRUPTED)
          {
            if (TRACE)
              fprintf (stderr,
                       "FTP: Interrupted while sending username.\n");
            HTProgress ("Connection interrupted.");
            NETCLOSE(control->socket);
            control->socket = -1;
            return HT_INTERRUPTED;
          }
    }
    if (status == 3) {		/* Send password */
	if (password) {
	    command = (char*)malloc(10+strlen(password)+2+1);
	    if (command == NULL) outofmem(__FILE__, "get_connection");
	    sprintf(command, "PASS %s%c%c", password, CR, LF);
	} else {
	    char * user = getenv("USER");
	    CONST char *host = HTHostName();
	    if (!user) user = "WWWuser";
	    /* If not fully qualified, suppress it as ftp.uu.net
	       prefers a blank to a bad name */
	    if (!strchr(host, '.')) host = "";

	    command = (char*)malloc(20+strlen(host)+2+1);
	    if (command == NULL) outofmem(__FILE__, "get_connection");
	    sprintf(command, "PASS %s@%s%c%c", user ? user : "WWWuser",
	    						host, CR, LF); /*@@*/
        }
	status = response(command);
	free(command);
        if (status == HT_INTERRUPTED)
          {
            if (TRACE)
              fprintf (stderr,
                       "FTP: Interrupted while sending password.\n");
            HTProgress ("Connection interrupted.");
            NETCLOSE(control->socket);
            control->socket = -1;
            return HT_INTERRUPTED;
          }
    }
    if (username) free(username);

    if (status == 3) {
        char temp[80];
	sprintf(temp, "ACCT noaccount%c%c", CR, LF);
	status = response(temp);
	if (status == HT_INTERRUPTED)
          {
            if (TRACE)
              fprintf (stderr,
                       "FTP: Interrupted while sending password.\n");
            HTProgress ("Connection interrupted.");
            NETCLOSE(control->socket);
            control->socket = -1;
            return HT_INTERRUPTED;
          }

    }
    if (status !=2) {
        if (TRACE) fprintf(stderr, "FTP: Login fail: %s", response_text);
    	/* if (control->socket > 0) close_connection(control->socket); */
        return -1;		/* Bad return */
    }
    if (TRACE) fprintf(stderr, "FTP: Logged in.\n");

    /** Check for host type **/
    server_type = GENERIC_SERVER;	/* reset */
    use_list = FALSE; 			/* reset */
    if ((status=response("SYST\r\n")) == 2) {
                /* we got a line -- what kind of server are we talking to? */
         if (strncmp(response_text+4, "UNIX Type: L8 MAC-OS MachTen", 28) == 0)
          {
             server_type = MACHTEN_SERVER;
	     use_list = TRUE;
          }
         else if (strstr(response_text+4, "UNIX") != NULL)
          {
             server_type = UNIX_SERVER;
	     use_list = TRUE;
          }
         else if (strncmp(response_text+4, "VMS", 3) == 0)
	  {
             server_type = VMS_SERVER;
	     use_list = TRUE;
	  }
         else if ((strncmp(response_text+4, "VM/CMS", 6) == 0)
				 || (strncmp(response_text+4, "VM ", 3) == 0))
             server_type = CMS_SERVER;
         else if (strncmp(response_text+4, "DCTS", 4) == 0)
             server_type = DCTS_SERVER;
         else if (strstr(response_text+4, "MAC-OS TCP/Connect II") != NULL)
          {
             server_type = TCPC_SERVER;
             get_ftp_pwd(&server_type, &use_list);
	     unsure_type = TRUE;   
          }
         else if (strncmp(response_text+4, "MACOS Peter's Server", 20) == 0)
          {
             server_type = PETER_LEWIS_SERVER;
             use_list = TRUE;
             set_mac_binary();
          }
	 else 
	  {
	     server_type = GENERIC_SERVER;
             get_ftp_pwd(&server_type, &use_list);
	     unsure_type = TRUE;   
	  }
    } else {
	/* SYST fails :(  try to get the type from the PWD command */
         get_ftp_pwd(&server_type, &use_list);
    }

/*	Now we inform the server of the port number we will listen on
*/
#ifdef NOTREPEAT_PORT
	{
	    int status = response(port_command);
	    if (status !=2) {
	    	if (control->socket) close_connection(control->socket);
	        return -status;		/* Bad return */
	    }
	    if (TRACE) fprintf(stderr, "FTP: Port defined.\n");
	}
#endif
	return con->socket;			/* Good return */
  } /* Scope of con */
}


#ifdef LISTEN

/*	Close Master (listening) socket
**	-------------------------------
**
**
*/
#ifdef __STDC__
PRIVATE int close_master_socket(void)
#else
PRIVATE int close_master_socket()
#endif
{
    int status;
    FD_CLR(master_socket, &open_sockets);
    status = NETCLOSE(master_socket);
    if (TRACE) fprintf(stderr, "FTP: Closed master socket %d\n", master_socket);
    master_socket = -1;
    if (status<0) return HTInetStatus("close master socket");
    else return status;
}


/*	Open a master socket for listening on
**	-------------------------------------
**
**	When data is transferred, we open a port, and wait for the server to
**	connect with the data.
**
** On entry,
**	master_socket	Must be negative if not set up already.
** On exit,
**	Returns		socket number if good
**			less than zero if error.
**	master_socket	is socket number if good, else negative.
**	port_number	is valid if good.
*/
#ifdef __STDC__
PRIVATE int get_listen_socket(void)
#else
PRIVATE int get_listen_socket()
#endif
{
    struct sockaddr_in soc_address;	/* Binary network address */
    struct sockaddr_in* sin = &soc_address;
    int new_socket;			/* Will be master_socket */
    
    
    FD_ZERO(&open_sockets);	/* Clear our record of open sockets */
    num_sockets = 0;
    
#ifndef REPEAT_LISTEN
    if (master_socket>=0) return master_socket;  /* Done already */
#endif

/*  Create internet socket
*/
    new_socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	
    if (new_socket<0)
	return HTInetStatus("socket for master socket");
    
    if (TRACE) fprintf(stderr, "FTP: Opened master socket number %d\n", new_socket);
    
/*  Search for a free port.
*/
    sin->sin_family = AF_INET;	    /* Family = internet, host order  */
    sin->sin_addr.s_addr = INADDR_ANY; /* Any peer address */
#ifdef POLL_PORTS
    {
        unsigned short old_port_number = port_number;
	for(port_number=old_port_number+1;;port_number++){ 
	    int status;
	    if (port_number > LAST_TCP_PORT)
		port_number = FIRST_TCP_PORT;
	    if (port_number == old_port_number) {
		return HTInetStatus("bind");
	    }
	    soc_address.sin_port = htons(port_number);
	    if ((status=bind(new_socket,
		    (struct sockaddr*)&soc_address,
			    /* Cast to generic sockaddr */
		    sizeof(soc_address))) == 0)
		break;
	    if (TRACE) fprintf(stderr, 
	    	"TCP bind attempt to port %d yields %d, errno=%d\n",
		port_number, status, SOCKET_ERRNO);
	} /* for */
    }
#else
    {
        int status;
	int address_length = sizeof(soc_address);
	status = getsockname(control->socket,
			(struct sockaddr *)&soc_address,
			 &address_length);
	if (status<0) return HTInetStatus("getsockname");
	CTRACE(tfp, "FTP: This host is %s\n",
	    HTInetString(sin));
	
	soc_address.sin_port = 0;	/* Unspecified: please allocate */
	status=bind(new_socket,
		(struct sockaddr*)&soc_address,
			/* Cast to generic sockaddr */
		sizeof(soc_address));
	if (status<0) return HTInetStatus("bind");
	
	address_length = sizeof(soc_address);
	status = getsockname(new_socket,
			(struct sockaddr*)&soc_address,
			&address_length);
	if (status<0) return HTInetStatus("getsockname");
    }
#endif    

    CTRACE(tfp, "FTP: bound to port %d on %s\n",
    		(int)ntohs(sin->sin_port),
		HTInetString(sin));

#ifdef REPEAT_LISTEN
    if (master_socket>=0)
        (void) close_master_socket();
#endif    
    
    master_socket = new_socket;
    
/*	Now we must find out who we are to tell the other guy
*/
    (void)HTHostName(); 	/* Make address valid - doesn't work*/
    sprintf(port_command, "PORT %d,%d,%d,%d,%d,%d%c%c",
		    (int)*((unsigned char *)(&sin->sin_addr)+0),
		    (int)*((unsigned char *)(&sin->sin_addr)+1),
		    (int)*((unsigned char *)(&sin->sin_addr)+2),
		    (int)*((unsigned char *)(&sin->sin_addr)+3),
		    (int)*((unsigned char *)(&sin->sin_port)+0),
		    (int)*((unsigned char *)(&sin->sin_port)+1),
		    CR, LF);


/*	Inform TCP that we will accept connections
*/
    if (listen(master_socket, 1)<0) {
	master_socket = -1;
	return HTInetStatus("listen");
    }
    CTRACE(tfp, "TCP: Master socket(), bind() and listen() all OK\n");
    FD_SET(master_socket, &open_sockets);
    if ((master_socket+1) > num_sockets) num_sockets=master_socket+1;

    return master_socket;		/* Good */

} /* get_listen_socket */
#endif

typedef struct _EntryInfo {
    char *       filename;
    char *       type;
    char *       date;
    unsigned int size;
    BOOLEAN      display;  /* show this entry? */
} EntryInfo;

PRIVATE void free_entryinfo_struct_contents ARGS1(EntryInfo *,entry_info)
{
    if(entry_info) {
        if(entry_info->filename) free(entry_info->filename);
        if(entry_info->type) free(entry_info->type);
        if(entry_info->date) free(entry_info->date);
    }
   /* dont free the struct */
}

/*
 * is_ls_date() --
 *      Return TRUE if s points to a string of the form:
 *              "Sep  1  1990 " or
 *              "Sep 11 11:59 " or
 *              "Dec 12 1989  " or
 *              "FCv 23 1990  " ...
 */
PRIVATE BOOLEAN is_ls_date ARGS1(char *,s)
{
        /* must start with three alpha characters */
        if (!isalpha(*s++) || !isalpha(*s++) || !isalpha(*s++))
                return FALSE;

        /* space */
        if (*s++ != ' ')
                return FALSE;

        /* space or digit */
        if ((*s != ' ') && !isdigit(*s))
                return FALSE;
        s++;

        /* digit */
        if (!isdigit(*s++))
                return FALSE;

        /* space */
        if (*s++ != ' ')
                return FALSE;

        /* space or digit */
        if ((*s != ' ') && !isdigit(*s))
                return FALSE;
        s++;

        /* digit */
        if (!isdigit(*s++))
                return FALSE;

        /* colon or digit */
        if ((*s != ':') && !isdigit(*s))
                return FALSE;
        s++;

        /* digit */
        if (!isdigit(*s++))
                return FALSE;

        /* space or digit */
        if ((*s != ' ') && !isdigit(*s))
                return FALSE;
        s++;

        /* space */
        if (*s++ != ' ')
                return FALSE;

        return TRUE;
} /* is_ls_date() */


/*
 * parse_ls_line() --
 *      Extract the name, size, and date from an ls -l line.
 */
PRIVATE void parse_ls_line ARGS2(char *,line, EntryInfo *,entry_info)
{
        short   i, j;
        int    base=1;
	int    size_num=0;

        for (i = strlen(line) - 1;
            (i > 13) && (!isspace(line[i]) || !is_ls_date(&line[i-12])); i--)
                ; /* null body */
        line[i] = '\0';
        if (i > 13) {
            StrAllocCopy(entry_info->date, &line[i-12]);
	    /* replace the 4th location with nbsp if it is a space */
	    if(entry_info->date[4] == ' ')
		entry_info->date[4] = HT_NON_BREAK_SPACE;
	} 
        j = i - 14;
        while (isdigit(line[j]))
        {
                size_num += (line[j] - '0') * base;
                base *= 10;
                j--;
        }
	entry_info->size = size_num;
        StrAllocCopy(entry_info->filename, &line[i + 1]);
} /* parse_ls_line() */

/*
 * parse_vms_dir_entry()
 *      Format the name, date, and size from a VMS LIST line
 *      into the EntryInfo structure
 */
PRIVATE void parse_vms_dir_entry ARGS2(char *,line, EntryInfo *,entry_info)
{
        int i, j, ialloc;
        char *cp, *cpd, *cps, date[16], *sp = " ";
	time_t NowTime;
	static char ThisYear[8];
	static BOOLEAN HaveYear = FALSE; 

        /**  Get rid of blank lines, and information lines.  **/
        /**  Valid lines have the ';' version number token.  **/
        if (!strlen(line) || (cp=strchr(line, ';')) == NULL) {
            entry_info->display = FALSE;
            return;
        }

        /** Cut out file or directory name at VMS version number. **/
	*cp++ ='\0';
	StrAllocCopy(entry_info->filename,line);

        /** Cast VMS file and directory names to lowercase. **/
	for (i=0; entry_info->filename[i]; i++)
            entry_info->filename[i] = tolower(entry_info->filename[i]);

        /** Uppercase terminal .z's or _z's. **/
	if ((--i > 2) && entry_info->filename[i] == 'z' &&
             (entry_info->filename[i-1] == '.' ||
            entry_info->filename[i-1] == '_'))
            entry_info->filename[i] = 'Z';

        /** Convert any tabs in rest of line to spaces. **/
	cps = cp-1;
        while ((cps=strchr(cps+1, '\t')) != NULL)
            *cps = ' ';

        /** Collapse serial spaces. **/
        i = 0; j = 1;
	cps = cp;
        while (cps[j] != '\0') {
            if (cps[i] == ' ' && cps[j] == ' ')
                j++;
            else
                cps[++i] = cps[j++];
        }
        cps[++i] = '\0';

        /** Save the current year, if we don't have it yet.  It  **/
	/** could be wrong on New Year's Eve, if some poor soul  **/
	/** is using Lynx instead of kissing his/her sweetheart. **/
	if (!HaveYear) {
	    NowTime = time(NULL);
 	    strcpy(ThisYear, (char *)ctime(&NowTime)+20);
	    ThisYear[4] = '\0';
	    HaveYear = TRUE;
	}

        /** Track down the date. **/
        if ((cpd=strchr(cp, '-')) != NULL &&
            strlen(cpd) > 9 && isdigit(*(cpd-1)) &&
            isalpha(*(cpd+1)) && *(cpd+4) == '-') {

	    /** Month **/
            *(cpd+4) = '\0';
            *(cpd+2) = tolower(*(cpd+2));
            *(cpd+3) = tolower(*(cpd+3));
	    sprintf(date, "%s ", cpd+1);
	    *(cpd+4) = '-';

	    /** Day **/
	    *cpd = '\0';
	    if (isdigit(*(cpd-2)))
	        sprintf(date+4, "%s ", cpd-2);
	    else
	        sprintf(date+4, " %s ", cpd-1);
	    *cpd = '-';

	    /** Time or Year **/
	    if (!strncmp(ThisYear, cpd+5, 4) &&
	        strlen(cpd) > 15 && *(cpd+12) == ':') {
	        *(cpd+15) = '\0';
	        sprintf(date+7, "%s", cpd+10);
	        *(cpd+15) = ' ';
	    } else {
	        *(cpd+9) = '\0';
	        sprintf(date+7, " %s", cpd+5);
	        *(cpd+9) = ' ';
	    }

            StrAllocCopy(entry_info->date, date);
        }

        /** Track down the size **/
        if ((cpd=strchr(cp, '/')) != NULL) {
            /* Appears be in used/allocated format */
            cps = cpd;
            while (isdigit(*(cps-1)))
                cps--;
            if (cps < cpd)
                *cpd = '\0';
            entry_info->size = atoi(cps);
            cps = cpd+1;
            while (isdigit(*cps))
                cps++;
            *cps = '\0';
            ialloc = atoi(cpd+1);
            /* Check if used is in blocks or bytes */
            if (entry_info->size <= ialloc)
                entry_info->size *= 512;
        }
        else if ((cps=strtok(cp, sp)) != NULL) {
            /* We just initialized on the version number */
            /* Now let's hunt for a lone, size number    */
            while ((cps=strtok(NULL, sp)) != NULL) {
               cpd = cps;
               while (isdigit(*cpd))
                   cpd++;
               if (*cpd == '\0') {
                   /* Assume it's blocks */
                   entry_info->size = atoi(cps) * 512;
                   break;
               }
           }
        }

        /** Wrap it up **/
        if (TRACE) fprintf(stderr,
                         "HTFTP: VMS filename: %s  date: %s  size: %d\n",
                         entry_info->filename,
                         entry_info->date ? entry_info->date : "",
                         entry_info->size);
        return;
} /* parse_vms_dir_entry() */

/*
 *     parse_dir_entry() 
 *      Given a line of LIST/NLST output in entry, return results 
 *      and a file/dir name in entry_info struct
 *
 *      If first is true, this is the first name in a directory.
 */

PRIVATE EntryInfo * parse_dir_entry ARGS2(char *, entry, BOOLEAN *,first)
{
        EntryInfo *entry_info;
        int  i;
        int  len;
	char *cp;
	BOOLEAN remove_size=FALSE;

        entry_info = (EntryInfo *)malloc(sizeof(EntryInfo));    
	entry_info->type = 0;
	entry_info->size = 0;
	entry_info->date = 0;
	entry_info->filename = 0;
	entry_info->display = TRUE;

        switch (server_type)
        {
        case UNIX_SERVER:
        case PETER_LEWIS_SERVER:
        case MACHTEN_SERVER:

                /* interpret and edit LIST output from Unix server */
               len = strlen(entry);
		

	       if (*first) {

		   *first=FALSE;
                   if(!strncmp(entry, "total ", 6) ||
                       		(strstr(entry, "not available") != NULL))
		     {
		        entry_info->display=FALSE;
		        return(entry_info);
		     }
		    else if(unsure_type)
		      {
                         /* this isn't really a unix server! */
                         server_type = GENERIC_SERVER;
		         entry_info->display=FALSE;
		         return(entry_info);
		      }
	       }

               /* check first character of ls -l output */
               if (toupper(entry[0]) == 'D') 
		 {
                   /* it's a directory */
                   StrAllocCopy(entry_info->type, "Directory"); 
		   remove_size=TRUE; /* size is not useful */
		 }
               else if (entry[0] == 'l')
		 {
                    /* it's a symbolic link, does the user care about
		     * knowing if it is symbolic?  I think so since
		     * it might be a directory
		     */
                    StrAllocCopy(entry_info->type, "Symbolic Link"); 
		    remove_size=TRUE; /* size is not useful */

                    /* strip off " -> pathname" */
                    for (i = len - 1; (i > 3) && (!isspace(entry[i])
					|| (entry[i-1] != '>') 
					|| (entry[i-2] != '-')
					|| (entry[i-3] != ' ')); i--)
                             ; /* null body */
                    if (i > 3)
                      {
                        entry[i-3] = '\0';
                        len = i - 3;
                      }
                  } /* link */

	        parse_ls_line(entry, entry_info); 

		if(!strcmp(entry_info->filename,"..") || 
					!strcmp(entry_info->filename,"."))
		    entry_info->display=FALSE;
		
		/* goto the bottom and get real type */
                break;

        case VMS_SERVER:
                /* Interpret and edit LIST output from VMS server */
		/* and convert information lines to zero length.  */
		parse_vms_dir_entry(entry, entry_info);

                /* Get rid of any junk lines */
		if(!entry_info->display)
		    return(entry_info);

		/** Trim off VMS directory extensions **/
		len = strlen(entry_info->filename);
                if ((len > 4) && !strcmp(&entry_info->filename[len-4], ".dir"))
		  {
		    entry_info->filename[len-4] = '\0';
                    StrAllocCopy(entry_info->type, "Directory"); 
		    remove_size=TRUE; /* size is not useful */
		  }
		/* goto the bottom and get real type */
                break;

        case CMS_SERVER:
		/* can't be directory... */
		/*
		 * "entry" already equals the correct filename
		 */
		StrAllocCopy(entry_info->filename,entry);
		/* goto the bottom and get real type */
                break;

        case NCSA_SERVER:
        case TCPC_SERVER:
                /* directories identified by trailing "/" characters */
		StrAllocCopy(entry_info->filename,entry);
                len = strlen(entry);
                if (entry[len-1] == '/')
                {
                        /* it's a dir, remove / and mark it as such */
                        entry[len-1] = '\0';
                        StrAllocCopy(entry_info->type, "Directory");
			remove_size=TRUE; /* size is not useful */
                }
		/* goto the bottom and get real type */
                break;
	
	default:
		/* we cant tell if it is a directory since we only
		 * did an NLST :(  List bad file types anyways? NOT!
		 */
		StrAllocCopy(entry_info->filename,entry);
		return(entry_info); /* mostly empty info */
		break; /* not needed */

        } /* switch (server_type) */


	if(remove_size && entry_info->size)
	  {
	    entry_info->size = 0;
	  }

	/* get real types eventually */
	if(!entry_info->type) {
	    int i=0;
	    char *cp;
    	    HTFormat format;
    	    HTAtom * encoding;  /* @@ not used at all */
    	    format = HTFileFormat(entry_info->filename, &encoding);

	    if(!strncmp(HTAtom_name(format), "application",11)) 
	      {
		   cp = HTAtom_name(format) + 12;
		   if(!strncmp(cp,"x-",2))
			cp+=2;
	      }
	    else
		cp = HTAtom_name(format);

            StrAllocCopy(entry_info->type, cp);
	}

	return(entry_info);

} /* parse_dir_entry */

PUBLIC int compare_EntryInfo_structs ARGS2(EntryInfo *,entry1, 
							EntryInfo *,entry2)
{
    int status;

    switch(HTfileSortMethod)
      {
        case FILE_BY_SIZE:
			/* both equal or both 0 */
                        if(entry1->size == entry2->size)
			    return(strcasecomp(entry1->filename, 
							entry2->filename));
			else
			    if(entry1->size > entry2->size)
				return(1);
			    else
				return(-1);
                        break;
        case FILE_BY_TYPE:
                        if(entry1->type && entry2->type) {
                            status = strcasecomp(entry1->type, entry2->type);
			    if(status)
				return(status);
			    /* else fall to filename comparison */
			}
                        return (strcasecomp(entry1->filename, 
							entry2->filename));
                        break;
        case FILE_BY_DATE:
                        if(entry1->date && entry2->date) {
                                /* We really should change the type :( */
                            status = strcasecomp(entry1->date, entry2->date);
			    if(status)
				return(status);
			    /* else fall to filename comparison */
			}
                        return (strcasecomp(entry1->filename, 
							entry2->filename));
                        break;
        case FILE_BY_NAME:
        default:
                        return (strcasecomp(entry1->filename, 
							entry2->filename));
      }
}


/*	Read a directory into an hypertext object from the data socket
**	--------------------------------------------------------------
**
** On entry,
**	anchor		Parent anchor to link the this node to
**	address		Address of the directory
** On exit,
**	returns		HT_LOADED if OK
**			<0 if error.
*/
PRIVATE int read_directory
ARGS4 (
  HTParentAnchor *,		parent,
  CONST char *,			address,
  HTFormat,			format_out,
  HTStream *,			sink )
{
    int status;
    BOOLEAN WasInterrupted = FALSE;
    HTStructured* target = HTML_new(parent, format_out, sink);
    HTStructuredClass targetClass;
    char *filename = HTParse(address, "", PARSE_PATH + PARSE_PUNCTUATION);
    EntryInfo *entry_info;
    BOOLEAN first=TRUE;
    char string_buffer[64];

    char c = 0;

    char *lastpath=0;  /* prefix for link, either "" (for root) or xxx  */
    char *entry;   /* pointer into lastpath to bit after last slash */

    targetClass = *(target->isa);

    HTProgress ("Receiving FTP directory.");
    HTDirTitles(target, (HTAnchor*)parent);
  
    data_read_pointer = data_write_pointer = data_buffer;

    if (*filename == '\0')  /* Empty filename : use root */
        StrAllocCopy (lastpath, "/");
    else if(!strcmp(filename,"/"))  /* root path */
        StrAllocCopy (lastpath, "/foo/..");
    else 
    {
        char * p = strrchr(filename, '/');  /* find lastslash */
        StrAllocCopy(lastpath, p+1);    /* take slash off the beginning */
    }
    free (filename);

   
    {
        HTBTree * bt = HTBTree_new((HTComparer)compare_EntryInfo_structs);
        char c;
	HTChunk * chunk = HTChunkCreate(128);
	int BytesReceived = 0;
	int BytesReported = 0;
	char NumBytes[64];
	PUTS("\n");  /* prettier LJM */
	for (c=0; c!=(char)EOF;)   /* For each entry in the directory */
	{
	    char * p = entry;
	    HTChunkClear(chunk);

	    if(HTCheckForInterrupt()) {
	    	WasInterrupted = TRUE;
		if(BytesReceived)
		    goto unload_btree;  /* unload btree */
		else
		  {
		    ABORT_TARGET;
		    HTBTreeAndObject_free(bt);
		    return HT_INTERRUPTED;
		  }
	    }

	    /*   read directory entry
	     */
	    for(;;) {                 /* Read in one line as filename */
		c = NEXT_DATA_CHAR;
AgainForMultiNet:
		if(interrupted_in_next_data_char) {
	    	    WasInterrupted = TRUE;
		    if(BytesReceived)
		        goto unload_btree;  /* unload btree */
                    else
                      {
                        ABORT_TARGET;
                        HTBTreeAndObject_free(bt);
                        return HT_INTERRUPTED;
                      }

		} else if (c == '\r' || c == LF) {    /* Terminator? */ 
		    if (chunk->size != 0) {  /* got some text */
		        /* Deal with MultiNet's wrapping of long lines */
                        if (server_type == VMS_SERVER) {
                        /* Deal with MultiNet's wrapping of long lines - F.M. */
                            if (data_read_pointer < data_write_pointer &&
                                *(data_read_pointer+1) == ' ')
                                data_read_pointer++;
                            else if (data_read_pointer >= data_write_pointer) {
                                int status;
                                status = NETREAD(data_soc, data_buffer,
                                                 DATA_BUFFER_SIZE);
                                if (status == HT_INTERRUPTED) {
                                    interrupted_in_next_data_char = 1;
                                    goto AgainForMultiNet;
                                }
                                if (status <= 0) {
                                    c = (char)EOF;
                                    break;
                                }
                                data_write_pointer = data_buffer + status;
                                data_read_pointer = data_buffer;
                                if (*data_read_pointer == ' ')
                                    data_read_pointer++;
                                else
                                    break;
                            }
                            else
                                break;
                        }
			else
		            break;            /* finish getting one entry */
		    }
		} else if (c == (char)EOF) {
		    break;             /* End of file */
		} else {
		    HTChunkPutc(chunk, c);
		}
            }
	    HTChunkTerminate(chunk);

	    BytesReceived += chunk->size;
	    if (BytesReceived > BytesReported + 1024) {
	        sprintf(NumBytes,"Transferred %d bytes",BytesReceived);
		HTProgress(NumBytes);
		BytesReported = BytesReceived;
	    }

	    if (c == (char) EOF && chunk->size == 1)
	    /* 1 means empty: includes terminating 0 */
	        break;
            if(TRACE) fprintf(stderr, "HTFTP: Line in %s is %s\n",
	    					lastpath, chunk->data);

	    entry_info = parse_dir_entry(chunk->data, &first);
	    if(entry_info->display)
	      {
		 if(TRACE)
		    fprintf(stderr,"Adding file to BTree: %s\n",
						entry_info->filename);
	         HTBTree_add(bt, (EntryInfo *)entry_info); 
	      }

	}  /* next entry */

unload_btree:

        HTChunkFree(chunk);

	/* print out the handy help message if it exits :) */
	if(server_type == UNIX_SERVER && help_message_cache_non_empty()) {
	    START(HTML_HR);
	    START(HTML_PRE);
	    PUTS(help_message_cache_contents());
	    init_help_message_cache();  /* to free memory */
	    START(HTML_HR);
	} else {
	    START(HTML_PRE);
	    PUTS("\n");
	}

	/* Put up header 
	 */
	/* PUTS("    Date        Type             Size     Filename\n"); 
	 */
	   
	/* Run through tree printing out in order 
	 */
	{
	    HTBTElement * ele;
	    int i;
	    for (ele = HTBTree_next(bt, NULL);
		 ele != NULL;
		 ele = HTBTree_next(bt, ele))
	    {
		entry_info = (EntryInfo *)HTBTree_object(ele);

		if(entry_info->date) 
		       {
		             PUTS(entry_info->date);
		             PUTS("  ");
		       }
		else
			PUTS("     * ");

		if(entry_info->type) 
		  {
		    for(i = 0; entry_info->type[i] != '\0' && i < 15; i++)
		        PUTC(entry_info->type[i]);
		    for(; i < 17; i++)
		        PUTC(' ');

		  }

		/* start the anchor */
		HTDirEntry(target, lastpath, entry_info->filename);  
		PUTS(entry_info->filename);
		END(HTML_A);

		if(entry_info->size) 
		  {
		          if(entry_info->size < 1024)
			      sprintf(string_buffer,"  %d bytes",
							entry_info->size);
			  else
			      sprintf(string_buffer,"  %dKb",
							entry_info->size/1024);
			  PUTS(string_buffer);
		  }

		PUTC('\n'); /* end of this entry */

		free_entryinfo_struct_contents(entry_info);
	    }
	}
	FREE_TARGET;
	HTBTreeAndObject_free(bt);
    }

    if (lastpath) free(lastpath);
    if (WasInterrupted || HTCheckForInterrupt()) {
        response(NIL);
	HTProgress("Data transfer interrupted.");
	return HT_LOADED;
    }
    return response(NIL) == 2 ? HT_LOADED : -1;
}

/*	Retrieve File from Server
**	-------------------------
**
** On entry,
**	name		WWW address of a file: document, including hostname
** On exit,
**	returns		Socket number for file if good.
**			<0 if bad.
*/
PUBLIC int HTFTPLoad
ARGS4 (
  CONST char *,			name,
  HTParentAnchor *,		anchor,
  HTFormat,			format_out,
  HTStream *,			sink
)
{
    BOOL isDirectory = NO;
    int status;
    int retry;			/* How many times tried? */
    HTFormat format;
    char command[LINE_LENGTH+1];
    

    /* set use_list to NOT since we don't know what kind of server
     * this is yet.  And set the type to GENERIC
     */
    use_list = FALSE;
    server_type = GENERIC_SERVER;

    for (retry=0; retry<2; retry++) {	/* For timed out/broken connections */
    
	status = get_connection(name);
	if (status<0) return status;

#ifdef LISTEN
	status = get_listen_socket();
	if (status<0) {
	    NETCLOSE (control->socket);
            control->socket = -1;
            close_master_socket ();
            /* HT_INTERRUPTED would fall through, if we could interrupt
               somehow in the middle of it, which we currently can't. */
	    return status;
	}
    
#ifdef REPEAT_PORT
/*	Inform the server of the port number we will listen on
*/
	{
	    status = response(port_command);
	    if (status == HT_INTERRUPTED) {
              if (TRACE)
                fprintf (stderr, "FTP: Interrupted in response (port_command)\n");
              HTProgress ("Connection interrupted.");
              NETCLOSE (control->socket);
              control->socket = -1;
              close_master_socket ();
              return HT_INTERRUPTED;
            }
	    if (status !=2) {		/* Could have timed out */
		if (status<0) continue;		/* try again - net error*/
		return -status;			/* bad reply */
	    }
	    if (TRACE) fprintf(stderr, "FTP: Port defined.\n");
	}
#endif
#else	/* Use PASV */
/*	Tell the server to be passive
*/
	{
	    char *p;
	    int reply, h0, h1, h2, h3, p0, p1;	/* Parts of reply */
	    int status;
	    data_soc = status;

	    sprintf(command, "PASV%c%c", CR, LF);
	    status = response(command);
	    if (status !=2) {
		if (status<0) continue;		/* retry or Bad return */
		return -status;			/* bad reply */
	    }
	    for(p=response_text; *p && *p != ','; p++)
		; /* null body */

	    while (--p > response_text && '0' <= *p && *p <= '9')
		; /* null body */
	
           status = sscanf(p+1, "%d,%d,%d,%d,%d,%d",
                   &h0, &h1, &h2, &h3, &p0, &p1);
           if (status<4) {
               fprintf(stderr, "FTP: PASV reply has no inet address!\n");
               return -99;
           }
           passive_port = (p0<<8) + p1;
	   if(TRACE)
               fprintf(stderr, "FTP: Server is listening on port %d\n",
                    passive_port);


/*	Open connection for data:
*/
	    sprintf(command,
            "ftp://%d.%d.%d.%d:%d/",h0,h1,h2,h3,passive_port);
            status = HTDoConnect(name, "FTP", passive_port, &data_soc);

	    if (status<0){
		(void) HTInetStatus("connect for data");
		NETCLOSE(data_soc);
		return status;			/* Bad return */
	    }
	    
	    if (TRACE) fprintf(stderr, "FTP data connected, socket %d\n", data_soc);
	}
#endif /* use PASV */
	status = 0;
        break;	/* No more retries */

    } /* for retries */
    if (status<0) return status;	/* Failed with this code */
    
/*	Ask for the file:
*/    
    {
        char *filename = HTParse(name, "", PARSE_PATH + PARSE_PUNCTUATION);
	char *fname = filename;	/** Save for subsequent free() **/
	BOOL binary;
	HTAtom * encoding;

	if (!*filename) StrAllocCopy(filename, "/");
	HTUnEscape(filename);
	if (TRACE) fprintf(stderr, "FTP: UnEscaped %d\n", filename);
	format = HTFileFormat(filename, &encoding);
	binary = (encoding != HTAtom_for("8bit")
		  && encoding != HTAtom_for("7bit"));
        if (binary != control->binary) {
	    char * mode = binary ? "I" : "A";
	    sprintf(command, "TYPE %s%c%c", mode, CR, LF);
	    status = response(command);
	    if (status != 2) return -status;
	    control->binary = binary;
	}
	if (server_type == VMS_SERVER) {
	    char *cp, *cp1, *cp2;
	    /** Accept only Unix-style filename **/
	    if (strchr(filename, ':') != NULL ||
	        strchr(filename, '[') != NULL) {
		free(fname);
		return -1;
	    }
	    /** Trim trailing slash if filename is not the top directory **/
	    if (strlen(filename) > 1 && filename[strlen(filename)-1] == '/')
	        filename[strlen(filename)-1] = '\0';

#ifdef MAINTAIN_CONNECTION /* Don't need this if always new connection - F.M. */
	    /** Get the current default VMS device:[directory] **/ 
	    sprintf(command, "PWD%c%c", CR, LF);
	    status = response (command);
	    if (status != 2) {
	         free(fname);
		 return -status;
	    }
	    /** Go to the VMS account's top directory **/
	    if ((cp=strchr(response_text, '[')) != NULL &&
	        (cp1=strrchr(response_text, ']')) != NULL) {
		sprintf(command, "CWD %s", cp);
		if ((cp2=strchr(cp, '.')) != NULL && cp2 < cp1)
		    sprintf(command+(cp2-cp)+4, "]%c%c", CR, LF);
		else
		    sprintf(command+(cp1-cp)+4, "]%c%c", CR, LF);
		status = response (command);
		if (status != 2) {
		    free(fname);
		    return -status;
		}
	    }
#endif /* MAINTAIN_CONNECTION */

	    /** If we want the VMS account's top directory, list it now **/
	    if (strlen(filename) == 1 && *filename == '/') {
		isDirectory = YES;
		sprintf(command, "LIST%c%c", CR, LF);
		status = response (command);
		free(fname);
		if (status != 1) return -status;  /* Action not started */

		goto listen;  /* big goto */
	    }
	    /** Otherwise, go to appropriate directory and doctor filename **/
	    if ((cp=strchr(filename, '/')) != NULL &&
	        (cp1=strrchr(cp, '/')) != NULL && cp != cp1) {
		sprintf(command, "CWD [.%s", cp+1);
		sprintf(command+(cp1-cp)+5, "]%c%c", CR, LF);
		while ((cp2=strrchr(command, '/')) != NULL)
		    *cp2 = '.';
		status = response(command);
		if (status != 2) {
		    free(fname);
		    return -status;
		}
		filename = cp1+1;
	    }
	    else
	        filename += 1;
	}
	sprintf(command, "RETR %s%c%c", filename, CR, LF);
	status = response(command);
	if (status != 1) {  /* Failed : try to CWD to it */

	   /* save those handy help messages */
	  if(server_type == UNIX_SERVER)
	      init_help_message_cache();

	  sprintf(command, "CWD %s%c%c", filename, CR, LF);
	  status = response(command);

	  if (status == 2) {  /* Successed : let's NAME LIST it */
	    isDirectory = YES;
	    if(use_list)
	        sprintf(command, "LIST%c%c", CR, LF);
	    else
	        sprintf(command, "NLST%c%c", CR, LF);
	    status = response (command);
	  }
	}
	free(fname);
	if (status != 1) return -status;		/* Action not started */
    }

listen:
#ifdef LISTEN
/*	Wait for the connection
*/
    {
	struct sockaddr_in soc_address;
        int	soc_addrlen=sizeof(soc_address);
	status = accept(master_socket,
			(struct sockaddr *)&soc_address,
			&soc_addrlen);
	if (status<0)
	    return HTInetStatus("accept");
	CTRACE(tfp, "TCP: Accepted new socket %d\n", status);
	data_soc = status;
    }
#else
/* @@ */
#endif
    if (isDirectory) {
        status = read_directory (anchor, name, format_out, sink);
        NETCLOSE(data_soc);
	NETCLOSE(control->socket);
        return status;
      /* returns HT_LOADED or error */
    } else {
        int rv;

	HTProgress ("Receiving FTP file.");
	rv = HTParseSocket(format, format_out, anchor, data_soc, sink);

	if (rv == HT_INTERRUPTED)
	     HTProgress("Data transfer interrupted.");

	HTInitInput(control->socket);
	/* Reset buffering to control connection DD 921208 */
    
	status = NETCLOSE(data_soc);
	if (TRACE) fprintf(stderr, "FTP: Closing data socket %d\n", data_soc);
	if (status<0 && rv != HT_INTERRUPTED && rv != -1)
	    (void) HTInetStatus("close");	/* Comment only */
	data_soc = -1;	/* invalidate it */
	
	status = response(NIL);		/* Pick up final reply */
	if (status!=2 && rv != HT_INTERRUPTED && rv != -1)
	    return HTLoadError(sink, 500, response_text);

	NETCLOSE(control->socket);
	return HT_LOADED;
    }       
} /* open_file_read */
