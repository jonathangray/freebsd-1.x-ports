/******************************************************************************/
/*                                                                            */
/*  Title       : selectsat.c                                                 */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 10Feb93                                                     */
/*  Last change : 10Feb93                                                     */
/*                                                                            */
/*  Synopsis    : This program reads a file with Keplerian elements in the    */
/*                NORAD 2-line format and selects certain satellites.         */
/*                                                                            */
/*  Input file  : tle.dat                                                     */
/*  Output file : sel.dat                                                     */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include <math.h>

#include "vt100.h"

#define  DATA       "SatTrack/Data"
#define  HEADER     "- Current Two-Line Element Sets "

int      numSats    = 16; /* this is the number of entries in 'satList' */

char     *satList[] = { "Mir", "OSCAR 10", "UoSat 2", "AO-13", "UO-14", 
                        "UO-15", "PACSAT", "DO-17", "WO-18", "LO-19", "FO-20", 
                        "INFORMTR-1", "KITSAT-A", "SARA", "HST", "GRO" };

main()

{
    int    i, lineNum1, lineNum2;

    char   satName[100], line1[100], line2[100], data[100];
    char   sysComm[100], inputData[20], inputFile[100], outputFile[100];
    char   str[80], strng[10];
    char   *strp, *getenv();

    FILE   *InputFile, *OutputFile;

    extern int  numSats;
    extern char *satList[];

    strp = getenv("HOME");
    sprintf(data,"%s/%s",strp,DATA);
    sprintf(inputFile,"%s/tle.dat",data);

    if ((InputFile = fopen(inputFile,"r")) == 0)
    {
        nl(); reverseblink();
        printf("%s not found\n",inputFile);
        normal(); alarm(); nl();
        exit(-1);
    }

    sprintf(outputFile,"%s/sel.dat",data);

    if ((OutputFile = fopen(outputFile,"w")) == 0)
    {
        nl(); reverseblink();
        printf("can't write %s\n",outputFile);
        normal(); alarm(); nl();
        exit(-1);
    }

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

        for (i = 0; i < numSats; i++)
        {
            if (!strncmp(satName,satList[i],strlen(satList[i])))
            {
                if (lineNum1 == 1 && lineNum2 == 2)
                {
                    fprintf(OutputFile,"%s",satName);
                    fprintf(OutputFile,"%s",line1);
                    fprintf(OutputFile,"%s",line2);
                }
            }
        }
    }

    fclose(InputFile);
    fclose(OutputFile);
}

/******************************************************************************/
/*                                                                            */
/* end of program selectsat.c                                                 */
/*                                                                            */
/******************************************************************************/
