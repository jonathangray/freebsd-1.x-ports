/********************************************************************
 * lindner
 * 3.8
 * 1993/07/29 20:12:18
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/authenticate.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992, 1993 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: authenticate.c
 * Routines to do encryption and decryption, and validate tickets
 *********************************************************************
 * Revision History:
 * authenticate.c,v
 * Revision 3.8  1993/07/29  20:12:18  lindner
 * Removed dead variables
 *
 * Revision 3.7  1993/07/27  05:27:39  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.6  1993/07/07  19:34:17  lindner
 * renamed encrypt() to UMNDESencrypt()
 *
 * Revision 3.5  1993/04/10  06:07:17  lindner
 * none
 *
 * Revision 3.4  1993/04/09  16:51:54  lindner
 * Little mods for combined public gopher and admit1 gopher.
 *
 * Revision 3.3  1993/04/07  05:16:57  lindner
 * Unhid changes for Admit1 stuff.
 *
 * Revision 3.1  1993/03/19  20:02:13  lindner
 * DES and ticket routines
 *
 *
 *********************************************************************/

#include "String.h"
#include <stdio.h>
#include <sys/types.h>

#include "gopherd.h"
#include "tix.h"
#include "command.h"

/* -------------------------------------------------
 *
 * Sofware DES functions 12 Dec 1986 by Phil Karn, KA9Q;
 * large sections from 1977 public-domain program by Jim Gillogly
 * Modified by George Gonzalez to not do initial/final permutations
 * Further bludgeoned by Farhad Anklesaria for AdmitOne authentication.
 *
 ---------------------------------------------------- */

#define  SLEN 255  /* Generic small buffer length */
#include "Debug.h"

static int IntelOrder;     /*Hacked out.  fxa*/

/* Tables defined in the Data Encryption Standard documents */
/* permuted choice table (key) */
static char pc1[] = {
     57, 49, 41, 33, 25, 17,  9,  1, 58, 50, 42, 34, 26, 18,
     10,  2, 59, 51, 43, 35, 27, 19, 11,  3, 60, 52, 44, 36,
     
     63, 55, 47, 39, 31, 23, 15,  7, 62, 54, 46, 38, 30, 22,
     14,  6, 61, 53, 45, 37, 29, 21, 13,  5, 28, 20, 12,  4
     };

/* number left rotations of pc1 */
static char totrot[] = { 1,2,4,6,8,10,12,14,15,17,19,21,23,25,27,28 };

/* permuted choice key (table) */
static char pc2[] = {
     14, 17, 11, 24,  1,  5,  3, 28, 15,  6, 21, 10,
     23, 19, 12,  4, 26,  8, 16,  7, 27, 20, 13,  2,
     41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48,
     44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32
     };

/* The (in)famous S-boxes */
static char si[8][64] = {
     /* S1 */
     14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7,
     0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8,
     4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0,
     15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13,
     
     /* S2 */
     15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10,
     3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5,
     0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15,
     13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9,
     
     /* S3 */
     10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,
     13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,
     13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,
     1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12,
     
     /* S4 */
     7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15,
     13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9,
     10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4,
     3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14,
     
     /* S5 */
     2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9,
     14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6,
     4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14,
     11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3,
     
     /* S6 */
     12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,
     10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,
     9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,
     4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13,
     
     /* S7 */
     4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1,
     13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6,
     1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2,
     6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12,
     
     /* S8 */
     13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,
     1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,
     7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,
     2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11
     };

/* 32-bit permutation  P used on the output of the S-boxes */
static char p32i[] = { 
     16,  7, 20, 21, 29, 12, 28, 17,  1, 15, 23, 26,  5, 18, 31, 10,
     2,  8, 24, 14, 32, 27,  3,  9, 19, 13, 30,  6, 22, 11,  4, 25
     };

/* Lookup tables initialized once only at startup by desinit() */


  
/* bit 0 == left-most in byte */
static short bytebit[] = { 0200,0100,0040,0020,0010,0004,0002,0001 };
  
  
char ticket[SLEN] = "3135323020202020";  /* 1520 */
char pw[SLEN] =     "61646D6974312020";  /* admit1 */
char pt[SLEN] = "1520    ";  /* 1520 */
char pt2[SLEN]; 
unsigned long tNum;

static unsigned char (*kn)[8];
static int (*spbox)[64]; /* Combined S && P boxes */
  
/*-------------MAIN--------------*/
#ifdef ADMIT1TEST

Abort()
{
     exit(-1);
}

main(argc, argv)
  int argc;
  char * argv[];
{
     unsigned long out[3];
     
     printf("ick.  here we go.\n");
     tNum = atoi(pt);
     printf("tNum converted from string: %d\n", tNum);
     sprintf(pt2, "%X%X%X%X%X%X%X%X", pt[0],pt[1],pt[2],pt[3],pt[4],pt[5],pt[6],pt[7] );
     printf("pt2 converted to hex from str: %s\n", pt2);
     printf("Ticket: %s\n", ticket);
     printf("pw: %s\n", pw);
     UMNDESencrypt(ticket, pw, out );
     sprintf(ticket, "%.8lX%.8lX", out[0], out[1]);
     printf("converted to string: %s\n", ticket);
     decrypt(ticket, pw, out);
     
     decrypt("374506700551A6B6", pw, out);
     
     if ( desinit() == 0 ) desdone();
}

#endif

/*--------------------------------*/
/* Initialize the lookup table for the combined S && P boxes */
void spinit()
{
     char pbox[ 32 ];
     short p, i, s, j, t, rowcol;
     int val;
     
     for ( p = 1; p <= 32; p++ ) {
	  for ( i = 0; i <= 31; i++ ) {
	       if ( p32i[ i ] == p ) {
		    pbox[ p - 1 ] = 31 - i;
		    break;
	       }
	  }
     }
     for ( s = 0; s <= 7; s++ ) {
	  for ( i = 0; i <= 63; i++ ) {
	       val = 0;
	       rowcol = (i & 32) | ((i & 1) ? 16 : 0) | ((i >> 1) & 0xf);
	       t = si[ s ] [ rowcol ];
	       for ( j = 0; j <= 3; j++ ) {
		    if ( t & ( 8 >> j ) ) {
			 val |= 1L << pbox[ ( s << 2 ) + j ];
		    }
	       }
	       spbox[ s ] [ i ] = val;
	  }
     }
}


/*--------------------------------*/
/* Allocate space && initialize DES lookup arrays
 * mode == 1: DEA without initial && final permutations for speed
 */

desinit()
{
     if ( spbox != 0 ) return 0; 
     
     spbox = ( int (*)[64] ) malloc( sizeof( int ) * 8 * 64);
     if ( spbox != 0) { 
	  spinit();
	  kn = (unsigned char (*)[8]) malloc( sizeof( char ) * 8 * 16);
	  if ( kn == 0 ) {
	       free( (char *) spbox );
	       spbox = 0;
	  } 
     }
     if ( spbox == 0 || kn == 0 ) {
	  return -1;
     } else {
	  return 0;
     }
}

/*--------------------------------*/
/* Free up storage used by DES */

desdone()
{ 
     if (spbox != 0) {
	  free( (char *) spbox );
	  free( (char *) kn );
	  spbox = 0;
	  kn = 0;
     }
}


/*--------------------------------*/

void setdeskey( key ) char * key;
{
     char pc1m[56];  /* place to modify pc1 into */
     char pcr[56];  /* place to rotate pc1 into */
     register short i, j, k;
     
     /* Clear key schedule */
     for (i=0; i<16; i++) {
	  for (j=0; j<8; j++) {
	       kn[i][j]=0;
	  }
     }
     
     for (j=0; j<56; j++) {  /* convert pc1 to bits of key */
	  k = pc1[ j ] - 1;
	  pc1m[ j ] = ( key[ k >> 3 ]  & bytebit[ k & 07 ]) ? 1 : 0;
     } 
     
     for (i=0; i<16; i++) {  /* key chunk for each iteration */
	  for (j=0; j<56; j++) { /* rotate pc1 the right amount */
	       k = j + totrot[ i ];
	       pcr[ j ] = pc1m[ k < ( j < 28 ? 28 : 56 ) ? k : k - 28 ];
	  }
	  for (j=0; j<48; j++) { /* select bits individually */
	       if ( pcr[ pc2[ j ] - 1 ] ) {
		    kn[ i ] [ j / 6 ] |= bytebit[ j % 6 ] >> 2;
	       }
	  }
     }
}


/*--------------------------------*/

void byteswap( x ) 
  unsigned long * x;
{
     register char * cp;
     register char tmp;
     
     cp = (char *) x;
     tmp = cp[3];
     cp[3] = cp[0];
     cp[0] = tmp;
     
     tmp = cp[2];
     cp[2] = cp[1];
     cp[1] = tmp;
}


/*--------------------------------*/

void mv( in, out ) char * in, *out;
{  short i;
   
   for ( i = 0; i <= 7; i++ ) out[i] = in[i];
   if ( IntelOrder ) {
	byteswap( (unsigned long * ) &out[0] );
	byteswap( (unsigned long * ) &out[4] );
   }
}


/*--------------------------------*/
/* The nonlinear  f(r,k), the heart of DES */

int f( r, subkey ) unsigned long r; unsigned char subkey[8];
{
     register unsigned long rval, rt;
     
     /* Run E(R) ^ K through the combined S & P boxes
      * This ; takes advantage of a convenient regularity in
      * E, namely that each group of 6 bits in E(R) feeding
      * a single S-box == a contiguous segment of R.
      */
     rt = (r >> 1) | ((r & 1) ? 0x80000000 : 0);
     rval = 0;
     rval |= spbox[0][((rt >> 26) ^ *subkey++) & 0x3f];
     rval |= spbox[1][((rt >> 22) ^ *subkey++) & 0x3f];
     rval |= spbox[2][((rt >> 18) ^ *subkey++) & 0x3f];
     rval |= spbox[3][((rt >> 14) ^ *subkey++) & 0x3f];
     rval |= spbox[4][((rt >> 10) ^ *subkey++) & 0x3f];
     rval |= spbox[5][((rt >> 6) ^ *subkey++) & 0x3f];
     rval |= spbox[6][((rt >> 2) ^ *subkey++) & 0x3f];
     rt = (r << 1) | ((r & 0x80000000) ? 1 : 0);
     rval |= spbox[7][(rt ^ *subkey) & 0x3f];
     return rval;
}

/*--------------------------------*/

void pass( num, block ) short num; unsigned long *block;
{
     short b;
     
     b = num & 1;
     block[ b ] ^= f( block[ 1 - b ], kn[ num ] );
}




/*--------------------------------*/
/* In-place UMNDESencryption of 64-bit block */

void endes(block) 
  unsigned long * block;
{
     register short i;
     unsigned long work[3];   /* Working data storage */
     int tmp;
     
     mv( ( char * ) block, (char * ) work ); /* Initial Permutation */
     for (i=0; i<16; i++)
	  pass( i, work );
     tmp = work[0];
     work[0] = work[1]; 
     work[1] = tmp;
     mv( (char *) work, (char *) block );
}



/*--------------------------------*/
/* In-place decryption of 64-bit block */
void dedes(block) 
  unsigned long * block;
{
     register short i;
     unsigned long work[3]; /* Working data storage */
     int tmp;
     ;
     mv( (char * ) block, (char *) work ); /* Initial permutation */
     
     /* Left/right half swap */
     tmp = work[0];
     work[0] = work[1]; 
     work[1] = tmp;
     
     /* Do the 16 rounds in reverse order */
     for ( i = 0; i <=15; i++ )
	  pass( 15 - i, work );
     
     mv( (char *)work, (char *) block );
}

/*--------------------------------*/

short  Hx( c ) 
  char c;
{
     if ( c >= '0' && c <= '9' ) {
	  return( c - '0' );
     } else {
	  if ( c >= 'A' && c <= 'F' )
	       return( c - 'A' + 10 );
	  else if ( c >= 'a' && c <= 'f' )
	       return( c - 'a' + 10);
	  else
	       return( 0 );
     }
}


/*--------------------------------*/
/* Take a character string and convert it into two long ints 
*/

void HexToBin( In, Out ) 
  char * In; 
  unsigned long Out[];
{
     short i;
     int bytes[8];


     for (i=0; i < 8; i++) 
	  bytes[i] = (Hx(In[i*2]) * 16 ) + Hx(In[i*2 + 1]);

     Out[0] = (bytes[0]*16777216) + (bytes[1]*65536) + (bytes[2]*256) + bytes[3];

     Out[1] = (bytes[4]*16777216) + (bytes[5]*65536) + (bytes[6]*256) + bytes[7];

     Debug("Output of HextoBin is %X\n", Out[0]);
     Debug("%X\n", Out[1]);

}


/*--------------------------------*/

decrypt( pwrd, key, out ) 
  char * pwrd; 
  char * key; 
  unsigned long out[];
{
     short i;
     unsigned long bpwrd[3], bkey[3];
     char * OutC;
     
     if ( desinit() != 0 ) Abort("DES could ! init itself.\n");
     
     HexToBin( pwrd, bpwrd );
     HexToBin( key, bkey );
     setdeskey( ( char * ) bkey );
     for ( i = 0; i <= 1; i++ )
	  out[ i ] = bpwrd[ i ];
     dedes( out );
     OutC = (char *) out;
     OutC[ 8 ] = '\0';

     Debug("decrypt: key = %.8lX", bkey[0]);
     Debug("%.8lX\n", bkey[1] );
     Debug("input: %.8lX", bpwrd[0]);
     Debug("%.8lX,", bpwrd[1]);
     Debug(" output = %.8lX", out[0]);
     Debug("%.8lX", out[1]);
     Debug("  '%s'\n", (char *) out);
     
}

/*--------------------------------*/

UMNDESencrypt( pwrd, key, out )
  char * pwrd; 
  char * key; 
  unsigned long out[];
{
     short i;
     unsigned long bpwrd[3], bkey[3];
     
     if ( desinit() != 0 ) Abort("Error initializing DES.\n");
     HexToBin( pwrd, bpwrd );
     HexToBin( key, bkey );
     setdeskey( (char *) bkey );
     for ( i = 0; i <=1; i++ )
	  out[ i ] = bpwrd[ i ];
     endes( out );

     Debug("encrypt: key = %.8lX", bkey[0]);
     Debug("%.8lX\r\n", bkey[1] );
     Debug("input: %.8lX", bpwrd[0]);
     Debug("%.8lX,", bpwrd[1]);
     Debug(" output = %.8lX", out[0]);
     Debug("%.8lX\r\n", out[1]);
}



/*--------------------------------*/
/** Test for a ticket 
    Find out if the request has a ticket,

    If not, print out an error message on sockfd and exit 
**/

PleaseAuthenticate(sockfd, string)
  int  sockfd;
  char *string;
{
     /*** Should be "gplus error...  oh well... ****/
     
     writestring(sockfd, "--2\r\n4 *UMNDES based auth required\r\nServer Requires Authenication\r\n");
	  closenet(sockfd);
     return(0);
}

TixObj *
ValidTicket(sockfd, cmd)
  int sockfd;
  CMDobj *cmd;
{
     int fd;
     TixArray *Tickets;
     char *cp;
     TixObj *tix, *ReturnTix;

     
     Tickets = TIXAnew();
     
     fd = uopen(GDCgetTixfile(Config), O_RDWR, 0644);
     TIXAfromFile(Tickets, fd);
     
     if (CMDgetCommand(cmd) != NULL && strcmp(CMDgetCommand(cmd), "!+TIX")==0) {
	  int i;
	  char outputline[512];
	  
	  
	  i = TIXAsearchUser(Tickets, CMDgetUser(cmd));
	  tix = TIXAgetEntry(Tickets, i);
	  
	  if (i <0) {
	       GplusError(sockfd, 1, "Sorry, you don't have an account...", NULL);
	       return(NULL);
	  }
	  
	  TIXrandomTicket(TIXAgetEntry(Tickets, i));
	  cp = TIXgetCryptedTicket(TIXAgetEntry(Tickets, i));

	  sprintf(outputline, "+-1\r\n+INFO: 1%s\t\t%s\t%d\r\n+TIX: %s\r\n.\r\n", GDCgetSite(Config), GDCgetHostname(Config), GDCgetPort(Config), cp);
	  
	  writestring(sockfd, outputline);
	  TIXusedTicket(tix);
	  lseek(fd, 0 , SEEK_SET);
	  TIXAtoFile(Tickets, fd);
	  
	  TIXAdestroy(Tickets);
	  return(NULL);
     } else {
	  /*** Validate the request ***/
	  int i;
	  
	  if (TIXAvalid(Tickets, CMDgetUser(cmd), CMDgetTicket(cmd)) == FALSE) {
	       AccessDenied(sockfd);
	       return(NULL);
	  }
	  i = TIXAsearchUser(Tickets, CMDgetUser(cmd));
	  tix = TIXAgetEntry(Tickets, i);
	  TIXusedTicket(tix);

	  Debug("Ticket user is %s\n", TIXgetUser(tix));
	  ReturnTix = TIXnew();
	  TIXcpy(ReturnTix, tix);
	  Debug("Ticket user is %s\n", TIXgetUser(tix));
	  
	  lseek(fd, 0 , SEEK_SET);
	  TIXAtoFile(Tickets, fd);
	  TIXAdestroy(Tickets);
	  return(ReturnTix);
     }
}


AccessDenied(sockfd)
  int sockfd;
{
     GplusError(sockfd, 5, "*UMNDES", NULL);
     writestring(sockfd, "--2\r\n5 *UMNDES based auth required\r\nServer Requires Authentication\r\n");
     return(NULL);
}
