/*	HyperText Tranfer Protocol	- Client implementation		HTTP.c
**	==========================
*/

#include "HTTP.h"

#define HTTP_VERSION	"HTTP/1.0"

#define INIT_LINE_SIZE		1024	/* Start with line buffer this big */
#define LINE_EXTEND_THRESH	256	/* Minimum read size */
#define VERSION_LENGTH 		20	/* for returned protocol version */

#include "HTParse.h"
#include "HTUtils.h"
#include "tcp.h"
#include "HTTCP.h"
#include "HTFormat.h"
#include "HTFile.h"
#include <ctype.h>
#include "HTAlert.h"
#include "HTMIME.h"
#include "HTML.h"
#include "HTInit.h"
#include "HTAABrow.h"

#ifdef NO_BCOPY
#define bcopy(s, d, n) memcpy((d), (s), (n))
#endif /* NO_BCOPY */

/* #define TRACE 1 */

struct _HTStream 
{
  HTStreamClass * isa;
};

extern char * HTAppName;	/* Application name: please supply */
extern char * HTAppVersion;	/* Application version: please supply */
extern char * personal_mail_address;	/* User's name/email address */

extern BOOL using_proxy;    /* are we using an HTTP gateway? */
extern char LYUserSpecifiedURL; /* is the URL a goto? */


/*		Load Document from HTTP Server			HTLoadHTTP()
**		==============================
**
**	Given a hypertext address, this routine loads a document.
**
**
** On entry,
**	arg	is the hypertext reference of the article to be loaded.
**
** On exit,
**	returns	>=0	If no error, a good socket number
**		<0	Error.
**
**	The socket must be closed by the caller after the document has been
**	read.
**
*/
PUBLIC int HTLoadHTTP ARGS4 (
	char *, 		arg,
	HTParentAnchor *,	anAnchor,
	HTFormat,		format_out,
	HTStream*,		sink)
{
  int s;				/* Socket number for returned data */
  char *command;			/* The whole command */
  char *eol;			/* End of line if found */
  char *start_of_data;		/* Start of body of reply */
  int status;				/* tcp return */
  int bytes_already_read;
  char crlf[3];			/* A CR LF equivalent string */
  HTStream *target;		/* Unconverted data */
  HTFormat format_in;			/* Format arriving in the message */
  int do_post = 0;		/* ARE WE posting ? */
  
  BOOL had_header;		/* Have we had at least one header? */
  char *line_buffer;
  char *line_kept_clean;
  BOOL extensions;		/* Assume good HTTP server */
  int compressed;
  char line[256];
  
  int length, doing_redirect, rv;
  int already_retrying = 0;

  if(anAnchor->post_data)
      do_post=TRUE;
  
  if (!arg)
    {
      status = -3;
      HTProgress ("Bad request.");
      goto done;
    }
  if (!*arg) 
    {
      status = -2;
      HTProgress ("Bad request.");
      goto done;
    }
  
  sprintf(crlf, "%c%c", CR, LF);

  /* At this point, we're talking HTTP/1.0. */
  extensions = YES;

 try_again:
  /* All initializations are moved down here from up above,
     so we can start over here... */
  eol = 0;
  bytes_already_read = 0;
  had_header = NO;
  length = 0;
  doing_redirect = 0;
  compressed = 0;
  target = NULL;
  line_buffer = NULL;
  line_kept_clean = NULL;

  status = HTDoConnect (arg, "HTTP", TCP_PORT, &s);
  if (status == HT_INTERRUPTED)
    {
      /* Interrupt cleanly. */
      if (TRACE)
        fprintf (stderr,
                 "HTTP: Interrupted on connect; recovering cleanly.\n");
      HTProgress ("Connection interrupted.");
      /* status already == HT_INTERRUPTED */
      goto done;
    }
  if (status < 0) 
    {
      if (TRACE) 
        fprintf(stderr, 
                "HTTP: Unable to connect to remote host for `%s' (errno = %d).\n", arg, SOCKET_ERRNO);
      HTAlert("Unable to connect to remote host.");
      status = HT_NO_DATA;
      goto done;
    }
  
  /*	Ask that node for the document,
   **	omitting the host name & anchor
   */        
  {
    char * p1 = HTParse(arg, "", PARSE_PATH|PARSE_PUNCTUATION);
    command = malloc(5 + strlen(p1)+ 2 + 31 + 
		/* Referer: field */
		(LYUserSpecifiedURL ? 0 : 
				(strlen((char *)HTLoadedDocumentURL()) + 10)));

    if (do_post)
      strcpy(command, "POST ");
    else
      strcpy(command, "GET ");

    /* if we are using a proxy gateway don't copy in the first slash
     * of say: /gopher://a;lkdjfl;ajdf;lkj/;aldk/adflj
     * so that just gopher://.... is sent.
     */
    if(using_proxy)
        strcat(command, p1+1);
    else
        strcat(command, p1);
    free(p1);
  }
  if (extensions) 
    {
      strcat(command, " ");
      strcat(command, HTTP_VERSION);
    }
  
  strcat(command, crlf);	/* CR LF, as in rfc 977 */
  
  if (extensions) 
    {
      int n, i;
      
      if (!HTPresentations) HTFormatInit();
      n = HTList_count(HTPresentations);
      
      for(i=0; i<n; i++) 
        {
          HTPresentation * pres = HTList_objectAt(HTPresentations, i);
          if (pres->rep_out == WWW_PRESENT)
            {
	      if(pres->rep == WWW_SOURCE) 
		  sprintf(line, "Accept: */*%c%c", CR, LF);
  	      else
                  sprintf(line, "Accept: %s%c%c", 
					HTAtom_name(pres->rep), CR, LF);
              StrAllocCat(command, line);
            }
        }
      
      sprintf(line, "User-Agent:  %s/%s  libwww/%s%c%c",
              HTAppName ? HTAppName : "unknown",
              HTAppVersion ? HTAppVersion : "0.0",
              HTLibraryVersion, CR, LF);
      StrAllocCat(command, line);

      if(personal_mail_address) {
          sprintf(line, "From:  %s%c%c", personal_mail_address, CR,LF);
          StrAllocCat(command, line);
      }

      if(!LYUserSpecifiedURL) {
          StrAllocCat(command, "Referer:  ");
          StrAllocCat(command, HTLoadedDocumentURL());
          sprintf(line, "%c%c", CR, LF);
          StrAllocCat(command, line);
      }
      
      {
        char *docname;
        char *hostname;
        char *colon;
        int portnumber;
        char *auth;
        
        docname = HTParse(arg, "", PARSE_PATH);
        hostname = HTParse(arg, "", PARSE_HOST);
        if (hostname &&
            NULL != (colon = strchr(hostname, ':'))) 
          {
            *(colon++) = '\0';	/* Chop off port number */
            portnumber = atoi(colon);
          }
        else portnumber = 80;
        
        if (NULL!=(auth=HTAA_composeAuth(hostname, portnumber, docname))) 
          {
            sprintf(line, "%s%c%c", auth, CR, LF);
            StrAllocCat(command, line);
          }
        if (TRACE) 
          {
            if (auth)
              fprintf(stderr, "HTTP: Sending authorization: %s\n", auth);
            else
              fprintf(stderr, "HTTP: Not sending authorization (yet)\n");
          }
        FREE(hostname);
        FREE(docname);
      }
    }

  if (do_post)
    {
      if (TRACE)
        fprintf (stderr, "HTTP: Doing post, content-type '%s'\n",
                 anAnchor->post_content_type);
      sprintf (line, "Content-type: %s%c%c",
               anAnchor->post_content_type ? anAnchor->post_content_type 
							: "lose", CR, LF);
      StrAllocCat(command, line);
      {
        int content_length;
        if (!anAnchor->post_data)
          content_length = 4; /* 4 == "lose" :-) */
        else
          content_length = strlen (anAnchor->post_data);
        sprintf (line, "Content-length: %d%c%c",
                 content_length, CR, LF);
        StrAllocCat(command, line);
      }
      
      StrAllocCat(command, crlf);	/* Blank line means "end" */
      
      StrAllocCat(command, anAnchor->post_data);
    }

  StrAllocCat(command, crlf);	/* Blank line means "end" */
  
  if (TRACE)
    fprintf (stderr, "Writing:\n%s----------------------------------\n",
             command);
  
  HTProgress ("Sending HTTP request.");

  status = NETWRITE(s, command, (int)strlen(command));
  free (command);
  if (status <= 0) 
    {
      if (status == 0)
        {
          if (TRACE)
            fprintf (stderr, "HTTP: Got status 0 in initial write\n");
          /* Do nothing. */
        }
      else if 
        ((SOCKET_ERRNO == ENOTCONN || SOCKET_ERRNO == ECONNRESET || SOCKET_ERRNO == EPIPE) &&
         !already_retrying &&
         /* Don't retry if we're posting. */ !do_post)
          {
            /* Arrrrgh, HTTP 0/1 compability problem, maybe. */
            if (TRACE)
              fprintf 
                (stderr, 
                 "HTTP: BONZO ON WRITE Trying again with HTTP0 request.\n");
            HTProgress ("Retrying as HTTP0 request.");
            NETCLOSE(s);
            extensions = NO;
            already_retrying = 1;
            goto try_again;
          }
      else
        {
          if (TRACE)
            fprintf (stderr, "HTTP: Hit unexpected network WRITE error; aborting connection.\n");
          NETCLOSE (s);
          status = -1;
          HTAlert("Unexpected network write error; connection aborted.");
          goto done;
        }
    }
  
  if (TRACE)
    fprintf (stderr, "HTTP: WRITE delivered OK\n");
  HTProgress ("HTTP request sent; waiting for response.");

  /*	Read the first line of the response
   **	-----------------------------------
   */
  
  {
    /* Get numeric status etc */
    BOOL end_of_file = NO;
    HTAtom * encoding = HTAtom_for("8bit");
    int buffer_length = INIT_LINE_SIZE;
    
    line_buffer = (char *) malloc(buffer_length * sizeof(char));
    
    do 
      {	/* Loop to read in the first line */
        /* Extend line buffer if necessary for those crazy WAIS URLs ;-) */
        if (buffer_length - length < LINE_EXTEND_THRESH) 
          {
            buffer_length = buffer_length + buffer_length;
            line_buffer = 
              (char *) realloc(line_buffer, buffer_length * sizeof(char));
          }
        if (TRACE)
          fprintf (stderr, "HTTP: Trying to read %d\n",
                   buffer_length - length - 1);
        status = NETREAD(s, line_buffer + length,
                         buffer_length - length - 1);
        if (TRACE)
          fprintf (stderr, "HTTP: Read %d\n", status);
        if (status <= 0) 
          {
            /* Retry if we get nothing back too; 
               bomb out if we get nothing twice. */
            if (status == HT_INTERRUPTED)
              {
                if (TRACE)
                  fprintf (stderr, "HTTP: Interrupted initial read.\n");
                HTProgress ("Connection interrupted.");
                status = HT_INTERRUPTED;
                goto clean_up;
              }
            else if 
              (status < 0 &&
               (SOCKET_ERRNO == ENOTCONN || SOCKET_ERRNO == ECONNRESET || 
		     SOCKET_ERRNO == EPIPE) && !already_retrying && !do_post)
              {
                /* Arrrrgh, HTTP 0/1 compability problem, maybe. */
                if (TRACE)
                  fprintf (stderr, "HTTP: BONZO Trying again with HTTP0 request.\n");
                NETCLOSE(s);
                if (line_buffer) 
                  free(line_buffer);
                if (line_kept_clean) 
                  free(line_kept_clean);
                
                extensions = NO;
                already_retrying = 1;
                HTProgress ("Retrying as HTTP0 request.");
                goto try_again;
              }
            else
              {
                if (TRACE)
                  fprintf (stderr, "HTTP: Hit unexpected network read error; aborting connection; status %d.\n", status);
                HTAlert("Unexpected network read error; connection aborted.");

                NETCLOSE (s);
                status = -1;
                goto clean_up;
              }
          }

        bytes_already_read += status;
        {
          char line[256];
          sprintf (line, "Read %d bytes of data.", bytes_already_read);
          HTProgress (line);
        }
        
#ifdef UCX  /* UCX returns -1 on EOF */
        if (status == 0 || status == -1) 
#else
        if (status == 0)
#endif
          {
            end_of_file = YES;
            break;
          }
        line_buffer[length+status] = 0;
        
        if (line_buffer)
          {
            if (line_kept_clean)
              free (line_kept_clean);
            line_kept_clean = (char *)malloc (buffer_length * sizeof (char));
            bcopy (line_buffer, line_kept_clean, buffer_length);
          }
        
        eol = strchr(line_buffer + length, LF);
        /* Do we *really* want to do this? */
        if (eol && eol != line_buffer && *(eol-1) == CR) 
          *(eol-1) = ' '; 
        
        length = length + status;

        /* Do we really want to do *this*? */
        if (eol) 
          *eol = 0;		/* Terminate the line */
      }
    /* All we need is the first line of the response.  If it's a HTTP/1.0
       response, then the first line will be absurdly short and therefore
       we can safely gate the number of bytes read through this code
       (as opposed to below) to ~1000. */
    /* Well, let's try 100. */
    while (!eol && !end_of_file && bytes_already_read < 100);
  } /* Scope of loop variables */
    
    
  /*	We now have a terminated unfolded line. Parse it.
   **	-------------------------------------------------
   */
  if (TRACE)
    fprintf(stderr, "HTTP: Rx: %s\n", line_buffer);
  
/* Kludge to work with old buggy servers and the VMS Help gateway.
** They can't handle the third word, so we try again without it.
*/
  if (extensions &&       /* Old buggy server or Help gateway? */
      (0==strncmp(line_buffer,"<TITLE>Bad File Request</TITLE>",31) ||
       0==strncmp(line_buffer,"Address should begin with",25) ||
       0==strncmp(line_buffer,"<TITLE>Help ",12) ||
       0==strcmp(line_buffer,
       		 "Document address invalid or access not authorised"))) {
      if (line_buffer)
	  free(line_buffer);
      if (line_kept_clean) 
          free(line_kept_clean);
      extensions = NO;
      already_retrying = 1;
      if (TRACE) fprintf(stderr,
			 "HTTP: close socket %d to retry with HTTP0\n", s);
      NETCLOSE(s);
      /* print a progress message */
      HTProgress ("Retrying as HTTP0 request.");
      goto try_again;
  }


  {
    int fields;
    char server_version[VERSION_LENGTH+1];
    int server_status;

    server_version[0] = 0;
    
    fields = sscanf(line_buffer, "%20s %d",
                    server_version,
                    &server_status);
    
    if (TRACE)
      fprintf (stderr, "HTTP: Scanned %d fields from line_buffer\n", fields);
    
    /* Rule out HTTP/1.0 reply as best we can. */
    if (fields < 2 || !server_version[0] || server_version[0] != 'H' ||
        server_version[1] != 'T' || server_version[2] != 'T' ||
        server_version[3] != 'P' || server_version[4] != '/' ||
        server_version[6] != '.') 
      {			/* HTTP0 reply */
        HTAtom * encoding;

        if (TRACE)
          fprintf (stderr, "--- Talking HTTP0.\n");
        
        format_in = HTFileFormat(arg, &encoding);
	/* treat all plain text as HTML.
         * this sucks but its the only solution without
         * looking at content.
         */
        if(!strncmp(HTAtom_name(format_in), "text/plain",10)) {
            if(TRACE)
                fprintf(stderr,
                           "HTTP:  format_in being changed to text/HTML\n");
            format_in = WWW_HTML;
        }

        start_of_data = line_kept_clean;
      } 
    else 
      {
        /* Decode full HTTP response */
        format_in = HTAtom_for("www/mime");
        /* We set start_of_data to "" when !eol here because there
           will be a put_block done below; we do *not* use the value
           of start_of_data (as a pointer) in the computation of
           length or anything else in this situation. */
        start_of_data = eol ? eol + 1 : "";
        length = eol ? length - (start_of_data - line_buffer) : 0;
        
        if (TRACE)
          fprintf (stderr, "--- Talking HTTP1.\n");
        
        switch (server_status / 100) 
          {
          case 3:		/* Various forms of redirection */
            /* We now support this in the parser, at least. */
            doing_redirect = 1;
	    if(TRACE)
		fprintf(stderr,"Got Redirect code\n");
            break;
            
          case 4:		/* "I think I goofed" */
            switch (server_status) 
              {
              case 403:
                /* 403 is "forbidden"; display returned text. */
                /* format_in = HTAtom_for("text/html"); */
                break;

              case 401:
                /* length -= start_of_data - text_buffer; */
                if (HTAA_shouldRetryWithAuth(start_of_data, length, s)) 
                  {
                    extern BOOLEAN dump_output_immediately;

                    (void)NETCLOSE(s);
                    if (line_buffer) 
                      free(line_buffer);
                    if (line_kept_clean) 
                      free(line_kept_clean);
                    if(dump_output_immediately)	{
                      fprintf(stderr, "HTTP:  Access authorization required.\n");
                      fprintf(stderr, "       Cannot retrieve non-interactively.\n");
                      status = HT_NO_DATA;
                      goto clean_up;
                    }

                    if (TRACE) 
                      fprintf(stderr, "%s %d %s\n",
                              "HTTP: close socket", s,
                              "to retry with Access Authorization");
                    
                    HTProgress ("Retrying with access authorization information.");
                    goto try_again;
                    break;
                  }
                else 
                  {
                    /* Fall through. */
                  }

              default:
                break;
#if 0
                char *p1 = HTParse(arg, "", PARSE_HOST);
                char *message;
                
                message = (char*)malloc(strlen(text_buffer) +
                                        strlen(p1) + 100);
                sprintf(message,
                        "HTTP server at %s replies:\n%s\n\n%s\n",
                        p1, text_buffer,
                        ((server_status == 401) 
                         ? "Access Authorization package giving up.\n"
                         : ""));
                free(message);
                free(p1);
#endif
#if 0
                HTProgress ("Could not load requested document.");
                status = -1;
                goto clean_up;
#endif
              } /* case 4 switch */
            break;

          case 5:		/* I think you goofed */
            break;
            
          case 2:		/* Good: Got MIME object */
	    if(server_status == 204) {
	        status = HT_NO_DATA;
	        goto done;
	    }
            break;
            
          default:		/* bad number */
            HTAlert("Unknown status reply from server!");
            break;
          } /* Switch on server_status/100 */
        
      }	/* Full HTTP reply */
  } /* scope of fields */

  /* Set up the stream stack to handle the body of the message */
  target = HTStreamStack(format_in,
                         format_out,
                         sink, anAnchor);
  
  if (!target || target == NULL) 
    {
      char buffer[1024];	/* @@@@@@@@ */
      sprintf(buffer, "Sorry, no known way of converting %s to %s.",
              HTAtom_name(format_in), HTAtom_name(format_out));
      HTProgress (buffer);
      status = -1;
      goto clean_up;
    }

  /* Recycle the first chunk of data, in all cases. */
  (*target->isa->put_block)(target, start_of_data, length);

  /* Go pull the bulk of the data down. */
  rv = HTCopy(s, target);

  if (rv == -1)
    {
      /* Intentional interrupt before data were received, not an error */
      /* (*target->isa->abort)(target, NULL);/* already done in HTCopy */
      status = HT_INTERRUPTED;
      NETCLOSE(s);
      goto clean_up;
    }
  if (rv == -2 && !already_retrying && !do_post)
    { 
      /* Aw hell, a REAL error, maybe cuz it's a dumb HTTP0 server */
      if (TRACE)
        fprintf (stderr, "HTTP: Trying again with HTTP0 request.\n");
      /* May as well consider it an interrupt -- right? */
      (*target->isa->abort)(target, NULL);
      NETCLOSE(s);
      if (line_buffer) 
        free(line_buffer);
      if (line_kept_clean) 
        free(line_kept_clean);

      extensions = NO;
      already_retrying = 1;
      HTProgress ("Retrying as HTTP0 request.");
      goto try_again;
    }

  /* 
   * Close socket if partial transmission (was freed on abort)
   * Free if complete transmission (socket was closed before return)
   */
  if (rv == HT_INTERRUPTED)
      NETCLOSE(s);
  else
      (*target->isa->free)(target);

  if (doing_redirect)
    {
      /* OK, now we've got the redirection URL temporarily stored
         in external variable redirecting_url, exported from HTMIME.c,
         since there's no straightforward way to do this in the library
         currently.  Do the right thing. */
      status = HT_REDIRECTING;
    }
  else
    {
      /* If any data were received, treat as a complete transmission */
      status = HT_LOADED;
    }

  /*	Clean up
   */
  
clean_up: 
  if (line_buffer) 
    free(line_buffer);
  if (line_kept_clean) 
    free(line_kept_clean);

 done:
  /* Clear out on exit, just in case. */
  do_post = 0;
  return status;
}


/*	Protocol descriptor
*/

GLOBALDEF PUBLIC HTProtocol HTTP = { "http", HTLoadHTTP, 0 };
