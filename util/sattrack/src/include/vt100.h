/******************************************************************************/
/*                                                                            */
/*  Title       : vt100.h                                                     */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 04May88                                                     */
/*  Last change : 30Mar92                                                     */
/*                                                                            */
/*  Synopsis    : This include file contains the VT100 display macros for     */
/*                the satellite tracking program 'sattrack'.                  */
/*                                                                            */
/******************************************************************************/


/******************************************************************************/
/*                                                                            */
/*  alarm: generates a beep as warning                                        */
/*                                                                            */
/******************************************************************************/

#define alarm() putchar('\007')

/******************************************************************************/
/*                                                                            */
/*  gotoXY: cursor positioning, go to column "x" and row "y"                  */
/*  escape sequence : ESC[n;mH     n, m = row, column                         */
/*                                                                            */
/******************************************************************************/

#define gotoXY(x,y) printf("\033[%d;%dH",y,x)

/******************************************************************************/
/*                                                                            */
/*  upCurs: moves cursor up by n lines                                        */
/*  command: ESC[nA   printf("\033[nA")                                       */
/*                                                                            */
/******************************************************************************/

#define upCurs(n) printf("\033[%dA",n)

/******************************************************************************/
/*                                                                            */
/*  downCurs: moves cursor down by n lines                                    */
/*  command: ESC[nB   printf("\033[nB")                                       */
/*                                                                            */
/******************************************************************************/

#define downCurs(n) printf("\033[%dB",n)

/******************************************************************************/
/*                                                                            */
/*  advCurs: advances cursor by n blanks in the same line                     */
/*  command: ESC[nC   printf("\033[nC")                                       */
/*                                                                            */
/******************************************************************************/

#define advCurs(n) printf("\033[%dC",n)

/******************************************************************************/
/*                                                                            */
/*  backCurs: moves cursor backward by n blanks in the same line              */
/*  command: ESC[nD   printf("\033[nD")                                       */
/*                                                                            */
/******************************************************************************/

#define backCurs(n) printf("\033[%dD",n)

/******************************************************************************/
/*                                                                            */
/*  clearCurs: clear the line from cursor position                            */
/*  command for clearing the rest of a line : ESC[K    printf("\033[K")       */
/*                                                                            */
/******************************************************************************/

#define clearCurs() printf("\033[K")

/******************************************************************************/
/*                                                                            */
/*  clearLine: clear the line from cursor position (see also gotoXY())        */
/*  this function is gotoXY() and clearcurs() in one                          */
/*                                                                            */
/******************************************************************************/

#define clearLine(x,y) printf("\033[%d;%dH\033[K",y,x)

/******************************************************************************/
/*                                                                            */
/*  underline: switches terminal (VT100) into underline mode                  */
/*             turn on underline option : ESC[4m                              */
/*             write underline video    : ESC[8p                              */
/*                                                                            */
/******************************************************************************/

#define underline() printf("\033[4m\033[8p")

/******************************************************************************/
/*                                                                            */
/*  reverse: switches terminal (VT100) into reverse mode                      */
/*           turn on reverse option : ESC[7m                                  */
/*           write reverse video    : ESC[16p                                 */
/*                                                                            */
/******************************************************************************/

#define reverse() printf("\033[7m\033[16p")

/******************************************************************************/
/*                                                                            */
/*  reverseblink: switches terminal (VT100) into reverse blink mode           */
/*                turn on reverse and blink option : ESC[5;7m                 */
/*                write reverse blink video        : ESC[18p                  */
/*                                                                            */
/******************************************************************************/

#define reverseblink() printf("\033[5;7m\033[18p")

/******************************************************************************/
/*                                                                            */
/*  normal: switches terminal (VT100) into normal mode, i.e. turn off         */
/*          reverse and blink mode                                            */
/*          turn off reverse and blink option : ESC[0m                        */
/*                                                                            */
/******************************************************************************/

#define normal()  printf("\033[0m")

/******************************************************************************/
/*                                                                            */
/*  nl: prints new line character                                             */
/*                                                                            */
/******************************************************************************/

#define nl()      printf("\n")

/******************************************************************************/
/*                                                                            */
/*  end of include file vt100.h                                               */
/*                                                                            */
/******************************************************************************/
