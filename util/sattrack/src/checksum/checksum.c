/******************************************************************************/
/*                                                                            */
/*  Title       : checksum.c                                                  */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 07Feb93                                                     */
/*  Last change : 07Feb93                                                     */
/*                                                                            */
/*  Synopsis    : This program reads a file with Keplerian elements in the    */
/*                NORAD 2-line format and fixes the checksum for use with     */
/*                InstantTrack V1.0.                                          */
/*                                                                            */
/*  Input file  : tle.dat or sts-47.dat (for example)                         */
/*  Output file : tle.chk or sts-47.chk (for example)                         */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include <math.h>

#include "vt100.h"

#define  DATA     "SatTrack/Data"
#define  HEADER   "- Current Two-Line Element Sets "

main()

{
    int    lineNum, lineNum1, lineNum2, satNum;
    int    i, nSat, checkValue, checkSum;

    char   satName[100], line1[100], line2[100], data[100];
    char   sysComm[100], inputData[20], inputFile[100], outputFile[100];
    char   str[80], strng[10];
    char   *strp, *getenv();

    FILE   *InputFile, *OutputFile;

    strp = getenv("HOME");
    sprintf(data,"%s/%s",strp,DATA);
    printf("\nenter input data file (e.g. tle, sel or sts-55): ");
    scanf("%s",inputData);
    sprintf(inputFile,"%s/%s.dat",data,inputData);

    if ((InputFile = fopen(inputFile,"r")) == 0)
    {
        nl(); reverseblink();
        printf("%s not found\n",inputFile);
        normal(); alarm(); nl();
        exit(-1);
    }

    sprintf(outputFile,"%s/%s.chk",data,inputData);

    if ((OutputFile = fopen(outputFile,"w")) == 0)
    {
        nl(); reverseblink();
        printf("can't write %s\n",outputFile);
        normal(); alarm(); nl();
        exit(-1);
    }

    printf("\ncreating %s ....\n",outputFile);
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

        fprintf(OutputFile,"%s",satName);

        if (lineNum1 == 1 && lineNum2 == 2)
        {
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

                    if (!strcmp(strng,"+"))
                        checkValue = 2;      /* assign check sum value to '+' */

                    if (!strcmp(strng,"-"))
                        checkValue = 1;      /* assign check sum value to '-' */

                    checkSum += checkValue;
                }

                str[68] = '\0';

                fprintf(OutputFile,"%s%d\n",str,checkSum%10);
            }

            nSat++;
        }
    }

    fclose(InputFile);
    fclose(OutputFile);
    printf("output file contains data for %d satellite",nSat);
    if (nSat > 1) printf("s");
    nl();
    nl();
}

/******************************************************************************/
/*                                                                            */
/* end of program checksum.c                                                  */
/*                                                                            */
/******************************************************************************/
