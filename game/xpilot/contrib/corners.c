/*
 * This program was made by Tom Grydeland (tomg@stud.cs.uit.no). Enjoy!
 */
#include <stdio.h>
#include <stdlib.h>
#define MAX 1024
#define OPEN(y,x)       ((a[(y)][(x)] == ' ') || (a[(y)][(x)] == '.'))
#define FILLED(y,x)     ((a[(y)][(x)] == 'x') || (a[(y)][(x)] == 'F'))

unsigned char a[MAX][MAX];
/* Which then will be the program's representation of the map */


main ()
{
    int i, j, r, k;  /* i and j counters, r is nr of rows, k is nr of columns */
    char str[256];   /* Any of the strings in the map file                    */
    char c;          /* The current char when reading the map into the array  */
        
    scanf ("%dx%d\n", &k, &r); /* Get the map size                  */
    printf ("%dx%d\n", k, r);  /* Print it right back out           */
    for (i=1; i<=3; i++) {     /* Copy the next three lines as well */
        gets (str);            /* as they are of no interest to us  */
        puts (str);
    }
    for (i = 0; (i < r && !feof (stdin)); i++) {
        for (j = 0; j < k; j++) {
            c = getchar();
            if ((c == '\n') || (c == EOF))
                break;          /* I need some tricky testing as the map  */
            a[i][j] = c;        /* format specifies that you can          */
                                /* leave the end of the line or remaining */
        }                       /* lines blank                            */
        if (j==k)
            gets (str);
        
    }
    /************************************
    * This part shouldn't need commenting
    *************************************/
    for (i = 2; i < r-2 ; i++) {
        for (j = 2; j < k-2; j++) {
            if (FILLED(i,j)) {
                if (FILLED(i-1,j-1)) {
                    if ((OPEN(i-1,j))&&(!FILLED(i-1,j+1))&&(!FILLED(i-2,j)))
                        a[i-1][j] = 'w';
                    if ((OPEN(i,j-1))&&(!FILLED(i+1,j-1))&&(!FILLED(i,j-2)))
                        a[i][j-1] = 'a';
                }
                if (FILLED(i-1,j+1)) {
                    if ((OPEN(i-1,j))&&(!FILLED(i-1,j-1))&&(!FILLED(i-2,j)))
                        a[i-1][j] = 'q';
                    if ((OPEN(i,j+1))&&(!FILLED(i+1,j+1))&&(!FILLED(i,j+2)))
                        a[i][j+1] = 's';
                }
                if (FILLED(i+1,j-1)) {
                    if ((OPEN(i+1,j))&&(!FILLED(i+1,j+1))&&(!FILLED(i+2,j)))
                        a[i+1][j] = 's';
                    if ((OPEN(i,j-1))&&(!FILLED(i-1,j-1))&&(!FILLED(i,j-2)))
                        a[i][j-1] = 'q';
                }
                if (FILLED(i+1,j+1)) {
                    if ((OPEN(i+1,j))&&(!FILLED(i+1,j-1))&&(!FILLED(i+2,j)))
                        a[i+1][j] = 'a';
                    if ((OPEN(i,j+1))&&(!FILLED(i-1,j+1))&&(!FILLED(i,j+2)))
                        a[i][j+1] = 'w';
                }
            }
        }
    }
    /**********************************
    * Finally, print out the map to our 
    * waiting audience.
    **********************************/
    for (i = 0; i < r ; i++) {
        for (j = 0; j < k; j++) {
            putchar (a[i][j]);
        }
        putchar ('\n');
    }
}
