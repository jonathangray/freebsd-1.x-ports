/******************************************************************************/
/*                                                                            */
/*  Title       : amsat.c                                                     */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 24Feb92                                                     */
/*  Last change : 28Dec92                                                     */
/*                                                                            */
/*  Synopsis    : This program reads a file with Keplerian elements in the    */
/*                NORAD 2-line format and generates one in the AMSAT format.  */
/*                                                                            */
/*  Input file  : tle.dat or sts-47.dat (for example)                         */
/*  Output file : amsat.dat                                                   */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "vt100.h"

#define  DATA     "SatTrack/Data"
#define  HEADER   "- Current Two-Line Element Sets "

extern   double getElement();

main()

{
    double epochDay, decayRate, inclination, RAAN, eccentricity;
    double argPerigee, meanAnomaly, meanMotion, epochYear;

    int    lineNum, lineNum1, lineNum2, satNum, elementSet, orbitNum;
    int    i, nSat, checkValue, checkSum, checkSumTotal, ephemerisType;

    char   satName[100], line1[100], line2[100], data[100];
    char   sysComm[100], inputData[20], inputFile[100], outputFile[100];
    char   str[80], strng[10];
    char   *strp, *getenv();

    FILE   *InputFile, *OutputFile;

    strp = getenv("HOME");
    sprintf(data,"%s/%s",strp,DATA);
    nl();
    system((char *) sprintf(sysComm,"ls %s",data));

    printf("\nenter input data file (e.g. tle or sts-47): ");
    scanf("%s",inputData);
    sprintf(inputFile,"%s/%s.dat",data,inputData);

    if ((InputFile = fopen(inputFile,"r")) == 0)
    {
        nl(); reverseblink();
        printf("%s not found\n",inputFile);
        normal(); alarm(); nl();
        exit(-1);
    }

    sprintf(outputFile,"%s/amsat.dat",data);

    if ((OutputFile = fopen(outputFile,"w")) == 0)
    {
        nl(); reverseblink();
        printf("can't write %s\n",outputFile);
        normal(); alarm(); nl();
        exit(-1);
    }

    printf("\ncreating %s ....\n\n",outputFile);
    nSat = 0;

    while (fgets(satName,80,InputFile))
    {
        if (!strncmp(satName,HEADER,10))
            fgets(satName,80,InputFile);

        fgets(line1,80,InputFile);
        fgets(line2,80,InputFile);

        sscanf(line1,"%1d",&lineNum1);
        sscanf(line2,"%1d",&lineNum2);

        if (lineNum1 != 1)
            printf("Line 1 not available for satellite %s",satName);

        if (lineNum2 != 2)
            printf("Line 2 not available for satellite %s",satName);

/******************************************************************************/
/*                                                                            */
/*  calculate checksum                                                        */
/*                                                                            */
/******************************************************************************/

        if (lineNum1 == 1 && lineNum2 == 2)
        {
            checkSumTotal = 0;

            for (lineNum = 1; lineNum <=2; lineNum++)
            {
                checkSum = 0;

                if (lineNum == 1)
                    sprintf(str,"%s",line1);
                if (lineNum == 2)
                    sprintf(str,"%s",line2);

                for (i = 0; i < 68; i++)
                {
                    strng[0]   = str[i];
                    strng[1]   = '\0';
                    checkValue = atoi(strng);

                    if (!strcmp(strng,"-"))
                        checkValue = 1;      /* assign check sum value to '-' */

                    checkSum += checkValue;
                }

                strng[0]   = str[68];
                strng[1]   = '\0';

                if (checkSum % 10 != atoi(strng))
                {
                    reverseblink();
                    printf("checksum error in line %d for satellite %s",
                            lineNum,satName);
                    normal(); alarm();
                }

                checkSumTotal += checkSum;
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

            fprintf(OutputFile,"Satellite: %s",satName);
            fprintf(OutputFile,"Catalog number: %10d\n",satNum);
            fprintf(OutputFile,"Epoch time: %14.8lf\n",epochDay);
            fprintf(OutputFile,"Element set: %13d\n",elementSet);
            fprintf(OutputFile,"Inclination: %13.4lf deg\n",inclination);
            fprintf(OutputFile,"RA of node: %14.4lf deg\n",RAAN);
            fprintf(OutputFile,"Eccentricity: %12.7lf\n",eccentricity);
            fprintf(OutputFile,"Arg of perigee: %10.4lf deg\n",argPerigee);
            fprintf(OutputFile,"Mean anomaly: %12.4lf deg\n",meanAnomaly);
            fprintf(OutputFile,"Mean motion: %13.8lf rev/day\n",meanMotion);
            fprintf(OutputFile,"Decay rate: %14.3le rev/day^2\n",decayRate);
            fprintf(OutputFile,"Epoch rev: %15d\n",orbitNum);
            fprintf(OutputFile,"Checksum: %16d\n",checkSumTotal);
            fprintf(OutputFile,"\n");

            nSat++;
            printf("%3d: %s",nSat,satName);
        }
    }

    fclose(InputFile);
    fclose(OutputFile);
    printf("\noutput file contains data for %d satellite",nSat);
    if (nSat > 1) printf("s");
    nl();
    nl();
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
/* end of program amsat.c                                                     */
/*                                                                            */
/******************************************************************************/
