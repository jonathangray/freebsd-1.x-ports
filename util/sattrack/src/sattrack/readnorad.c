/******************************************************************************/
/*                                                                            */
/*  Title       : readnorad.c                                                 */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 19Jun91                                                     */
/*  Last change : 27Jan93                                                     */
/*                                                                            */
/*  Synopsis    : This function reads the NORAD 2-line element sets.          */
/*                                                                            */
/*  Return code : 0 = data valid                                              */
/*                1 = specified input data file not available                 */
/*                2 = check sum error in line 1                               */
/*                3 = check sum error in line 2                               */
/*                4 = specified satellite not found                           */
/*                                                                            */
/*  Data for each satellite consist of three lines in the following format:   */
/*                                                                            */
/*  XXXXXXXXXXX                                                               */
/*  1 NNNNNU NNNNNAAA NNNNN.NNNNNNNN +.NNNNNNNN +NNNNN-N +NNNNN-N N NNNNN     */
/*  2 NNNNN NNN.NNNN NNN.NNNN NNNNNNN NNN.NNNN NNN.NNNN NN.NNNNNNNNNNNNNN     */
/*                                                                            */
/*  XXXXXXXXXXX                                                               */
/*  1 AAAAAU 00  0  0 BBBBB.BBBBBBBB  .CCCCCCCC  00000-0  00000-0 0  DDDZ     */
/*  2 AAAAA EEE.EEEE FFF.FFFF GGGGGGG HHH.HHHH III.IIII JJ.JJJJJJJJKKKKKZ     */
/*                                                                            */
/*  KEY:                                                                      */
/*                                                                            */
/*  X = Satellite name                                                        */
/*  A = Catalog number                       G = Eccentricity                 */
/*  B = Epoch time                           H = Argument of perigee          */
/*  C = Decay rate                           I = Mean anomaly                 */
/*  D = Number of the element set            J = Mean motion                  */
/*  E = Inclination                          K = Orbit number                 */
/*  F = RAAN                                 Z = Check sum                    */
/*                                                                            */
/*  Line 0 is an eleven-character name. Lines 1 and 2 are the standard        */
/*  two-line orbital element set format identical to that used by NASA and    */
/*  NORAD. The format description is as follows:                              */
/*                                                                            */
/*  Line 0:                                                                   */
/*  Column     Description                                                    */
/*   01-11     Satellite name                                                 */
/*                                                                            */
/*  Line 1:                                                                   */
/*   01-01     Line number of element set                                     */
/*   03-07     Satellite number                                               *//*   08-08     Classification (U = unclassified)                              */
/*   10-11     International designator (last two digits of launch year)      */
/*   12-14     International designator (launch number of the year)           */
/*   15-17     International designator (piece of launch)                     */
/*   19-20     Epoch year (last two digits of year)                           */
/*   21-32     Epoch (Julian day and fractional portion of the day)           */
/*   34-43     First time derivative of the mean motion                       */
/*             or ballistic coefficient (depending on ephemeris type)         */
/*   45-52     Second time derivative of mean motion (decimal point assumed;  */
/*             blank if n/a)                                                  */
/*   54-61     BSTAR drag term if GP4 general perturbation theory was used;   */
/*             otherwise, radiation pressure coefficient (decimal point       */
/*             assumed)                                                       */
/*   63-63     Ephemeris type                                                 */
/*   65-68     Element set number                                             */
/*   69-69     Check sum (modulo 10)                                          */
/*             (letters, blanks, periods, plus sign = 0; minus sign = 1)      */
/*                                                                            */
/*  Line 2:                                                                   */
/*   01-01     Line number of element set                                     */
/*   03-07     Satellite number                                               */
/*   09-16     Inclination [deg]                                              */
/*   18-25     Right Ascension of the ascending node [deg]                    */
/*   27-33     Eccentricity (decimal point assumed)                           */
/*   35-42     Argument of perigee [deg]                                      */
/*   44-51     Mean anomaly [deg]                                             */
/*   53-63     Mean motion [rev/d]                                            */
/*   64-68     Orbit (revolution number) at epoch [rev]                       */
/*   69-69     Check sum (modulo 10)                                          */
/*                                                                            */
/*  All other columns are blank or fixed.                                     */
/*                                                                            */
/******************************************************************************/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>

#include "satglobalsx.h"
#include "sattrack.h"
#include "vt100.h"

/******************************************************************************/
/*                                                                            */
/*  define directory names and logical variables                              */
/*                                                                            */
/******************************************************************************/

extern double getElement();

int readnorad(home,elementFile,satName,verboseFlag,
              pSatNum,pElementSet,pEpochDay,pInclination,pRAAN,pEccentricity,
              pArgPerigee,pMeanAnomaly,pMeanMotion,pDecayRate,pOrbitNum)

double *pEpochDay, *pInclination, *pRAAN, *pEccentricity, *pArgPerigee;
double *pMeanAnomaly, *pMeanMotion, *pDecayRate;
long   *pSatNum, *pElementSet, *pOrbitNum;
int    verboseFlag;
char   *home, *elementFile, *satName;

{
    double epochYear, epochDay, inclination, RAAN, eccentricity, argPerigee;
    double meanAnomaly, meanMotion, decayRate;
    double satNum, ephemerisType, elementSet, orbitNum;

    int    i, j, error, satNameLen, lineNum, foundSat, checkSum, checkValue;

    char   line0[80], line1[80], line2[80], data[100];
    char   inputFile[80], str[80], strng[10], lowElementFile[80];

    FILE   *fpi;

/******************************************************************************/
/*                                                                            */
/*  check if data file is available                                           */
/*                                                                            */
/******************************************************************************/

    sprintf(data,"%s/%s",home,DATA);
    sprintf(inputFile,"%s/%s",data,elementFile);
    sprintf(lowElementFile,"%s",elementFile);
    lowerCase(lowElementFile);

    if ((fpi = fopen(inputFile, "r")) == NULL)
    {
        sprintf(inputFile,"%s/%s.dat",data,lowElementFile);

        if ((fpi = fopen(inputFile, "r")) == NULL)
        {
            reverseblink();
            printf(" data file '%s.dat' not available \n",elementFile);
            normal();
            alarm();
            return(1);
        }
    }

    satNameLen = strlen(satName);
    foundSat   = FALSE;
    error      = FALSE;

/******************************************************************************/
/*                                                                            */
/*  search for matching satellite name (or fragment thereof)                  */
/*                                                                            */
/******************************************************************************/

    upperCase(satName);

    while (fgets(line0,80,fpi) && !foundSat)
    {
        strcpy(str,line0);
        upperCase(str);

        if (strncmp(satName,str,satNameLen) == 0)
        {
            foundSat = TRUE;

            if (strlen(line0) > 15)
                line0[16] = '\0';

            truncBlanks(line0);                  /* clean up satellite name   */
            sprintf(satName,"%s",line0);
            sprintf(line0,"%s\n",satName);       /* restore clean line 0      */

            fgets(line1,80,fpi);                 /* read line 1               */
            fgets(line2,80,fpi);                 /* read line 2               */

            if (verboseFlag)
            {
                nl();
                printf("%s",line0);
                printf("%s",line1);
                printf("%s",line2);
                nl();
            }

/******************************************************************************/
/*                                                                            */
/*  perform CRC test on lines 1 and 2                                         */
/*                                                                            */
/******************************************************************************/

            for (j = 2; j <= 3; j++)                        /* j = error code */
            {
                if (j == 2) sprintf(str,"%s",line1);
                if (j == 3) sprintf(str,"%s",line2);

                checkSum = 0;

                for (i = 0; i < 68; i++)
                {
                    strng[0]   = str[i];
                    strng[1]   = '\0';
                    checkValue = atoi(strng);

                    if (!strcmp(strng,"-"))
                        checkValue = 1;      /* assign check sum value to '-' */

                    checkSum += checkValue;
                }

                strng[0] = str[68];
                strng[1] = '\0';

                if (atoi(strng) != checkSum % 10)
                {
                    fclose(fpi);
                    reverseblink();
                    printf(" check sum error in line %d: ",j-1);
                    printf("check sum is %d, should be %d \n",
                        checkSum%10,atoi(strng));
                    normal(); alarm();
                    error = j;
                }
            }

/******************************************************************************/
/*                                                                            */
/*  get elements from lines 1 and 2                                           */
/*                                                                            */
/******************************************************************************/

            satNum        = getElement(line1, 3, 8);
            epochYear     = getElement(line1,19,20);
            epochDay      = getElement(line1,21,32);
            decayRate     = getElement(line1,34,43);
            ephemerisType = getElement(line1,63,63);
            elementSet    = getElement(line1,65,68);

            inclination   = getElement(line2, 9,16);
            RAAN          = getElement(line2,18,25);
            eccentricity  = getElement(line2,27,33);
            argPerigee    = getElement(line2,35,42);
            meanAnomaly   = getElement(line2,44,51);
            meanMotion    = getElement(line2,53,63);
            orbitNum      = getElement(line2,64,68);

            epochDay     += epochYear * 1000.0;
            eccentricity *= 1.0e-7;
        }
    }

    fclose(fpi);

    if (!foundSat)
    {
        reverseblink();
        printf(" satellite '%s' not listed in data file '%s.dat' \n",
                        satName,elementFile);
        normal(); alarm();
        return(4);
    }

/******************************************************************************/
/*                                                                            */
/*  return data                                                               */
/*                                                                            */
/******************************************************************************/

    *pSatNum       = (long) satNum;
    *pElementSet   = (long) elementSet;
    *pEpochDay     = epochDay;
    *pInclination  = inclination;
    *pRAAN         = RAAN;
    *pEccentricity = eccentricity;
    *pArgPerigee   = argPerigee;
    *pMeanAnomaly  = meanAnomaly;
    *pMeanMotion   = meanMotion;
    *pDecayRate    = decayRate;
    *pOrbitNum     = (long) orbitNum;

    return(error);
}

/******************************************************************************/
/*                                                                            */
/*  getElement: returns double of orbital element out of ASCII string         */
/*                                                                            */
/******************************************************************************/

double getElement(gstring,gstart,gstop)

int  gstart, gstop;
char gstring[80];

{
    int  k, glength;
    char gstr[80];

    glength = gstop - gstart + 1;

    for (k = 0; k <= glength; k++)
        gstr[k] = gstring[gstart+k-1];

    gstr[glength] = '\0';

    return(atof(gstr));
}

/******************************************************************************/
/*                                                                            */
/*  end of function readnorad.c                                               */
/*                                                                            */
/******************************************************************************/
