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
**
** Options:
**	LISTEN		We listen, the other guy connects for data.
**			Otherwise, other way round, but problem finding our
**			internet address!
**
** Bugs:
**	No binary mode! Always uses ASCII! 
*/

#define LISTEN	 /* @@@@ Test LJM */

/*
BUGS:	@@@  	Limit connection cache size!
		Error reporting to user.
		400 & 500 errors are acked by user with windows.
		Use configuration file for user names
		Prompt user for password (FIXED!!!)
		
**		Note for portablility this version does not use select() and
**		so does not watch the control and data channels at the
**		same time.
*/		

#include "HTFTP.h"	/* Implemented here */

#define CR   FROMASCII('\015')	/* Must be converted to ^M for transmission */
#define LF   FROMASCII('\012')	/* Must be converted to ^J for transmission */

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


/*	Module-Wide Variables
**	---------------------
*/
PRIVATE connection * connections =0;	/* Linked list of connections */
PRIVATE char    response_text[LINE_LENGTH+1];/* Last response from NewsHost */
PRIVATE connection * control;		/* Current connection */
PRIVATE int	data_soc = -1;		/* Socket for data transfer =invalid */
PRIVATE BOOLEAN FTPISVMS = FALSE;	/* Becomes TRUE if FTP host is VMS */

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

		*p++=0;			/* Terminate the string */
		if (TRACE) fprintf(stderr, "    Rx: %s", response_text);
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
        char * command;
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
        char * command;
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
	    sprintf(command,
	    "PASS %s@%s%c%c", user ? user : "WWWuser",
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
        HTBTree * bt = HTBTree_new((HTComparer)strcasecomp);
        char c;
	HTChunk * chunk = HTChunkCreate(128);
	int BytesReceived = 0;
	int BytesReported = 0;
	char NumBytes[20];
	START(HTML_MENU);
	PUTS("\n");  /* prettier LJM */
	for (c=0; c!=(char)EOF;)   /* For each entry in the directory */
	{
	    char * filename = NULL;
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
		    if (chunk->size != 0)   /* got some text */
		        break;                /* finish getting one entry */
		} else if (c == (char)EOF) {
		    break;             /* End of file */
		} else {
		    HTChunkPutc(chunk, c);
		}
            }
	    HTChunkTerminate(chunk);

	    BytesReceived += chunk->size;
	    if (BytesReceived > BytesReported + 1024) {
	        sprintf(NumBytes,"Transfered %d bytes",BytesReceived);
		HTProgress(NumBytes);
		BytesReported = BytesReceived;
	    }

	    if (c == (char) EOF && chunk->size == 1)  /* 1 means empty: includes terminating 0 */
	        break;
            if(TRACE) fprintf(stderr, "HTFTP: file name in %s is %s\n", lastpath, chunk->data);
	    if (FTPISVMS) {
	    /** Make chunk->data strings from VMS hosts Unix-like **/
	        char *cp, *cpd = NULL;
		int i;
		/** Trim off VMS version numbers **/
		if ((cp=strrchr(chunk->data, ';')) != NULL)
		    *cp = '\0';
		/** Trim off VMS directory extensions **/
	        if ((strlen(chunk->data) > 3) &&
	            strncasecomp(chunk->data+strlen(chunk->data)-4,
		    		 ".dir", 4) == 0)
		    *(cpd=(chunk->data+(strlen(chunk->data)-4))) = '\0';
		/** Cast VMS file and directory names to lowercase **/
		for (i=0; chunk->data[i]; i++)
		    chunk->data[i] = tolower(chunk->data[i]);
		/** Uppercase terminal .z's or _z's **/
		if ((--i > 2) && chunk->data[i] == 'z' &&
		    (chunk->data[i-1] == '.' || chunk->data[i-1] == '_'))
		    chunk->data[i] = 'Z';
		/** Load doctored VMS filename **/
		StrAllocCopy(filename, chunk->data);
		/** Restore chunk->data string to original length **/
		if (cpd != NULL)
		    *cpd = '.';
		if (cp != NULL)
		    *cp = ';';
	    }
	    else
	        StrAllocCopy(filename, chunk->data);

	    HTBTree_add(bt,filename); 
	     
#ifdef NOT
	    /* Directories are not sorted ;-( */
	     START(HTML_LI);
	     HTDirEntry(target, lastpath, filename);
#endif /* NOT */

	}  /* next entry */

unload_btree:

        HTChunkFree(chunk);

	/* Run through tree printing out in order 
	 */
	{
	    HTBTElement * ele;
	    for (ele = HTBTree_next(bt, NULL);
		 ele != NULL;
		 ele = HTBTree_next(bt, ele))
	    {
	        START(HTML_LI);
		HTDirEntry(target, lastpath, (char *)HTBTree_object(ele));
	    }
	}
	END(HTML_MENU);
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
	FTPISVMS = FALSE;
	/** Check for VMS host **/
	sprintf(command, "SYST%c%c", CR, LF); /** All VMS hosts support SYST **/
	if ((status=response(command)) == 2) {
	    /** Assume SYST succeeded if it's a VMS host **/
	    if (strncmp(response_text+4, "VMS", 3) == 0) {
	        /** Set flag for VMS host **/
	        FTPISVMS = TRUE;
	    }
	}
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
	if (FTPISVMS) {
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
	    /** If we want the VMS account's top directory, list it now **/
	    if (strlen(filename) == 1 && *filename == '/') {
		isDirectory = YES;
		sprintf(command, "NLST%c%c", CR, LF);
		status = response (command);
		free(fname);
		if (status != 1) return -status;  /* Action not started */
		goto listen;
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
	  sprintf(command, "CWD %s%c%c", filename, CR, LF);
	  status = response(command);
	  if (status == 2) {  /* Successed : let's NAME LIST it */
	    isDirectory = YES;
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
