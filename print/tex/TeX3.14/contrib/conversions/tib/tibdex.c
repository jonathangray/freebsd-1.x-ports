/*
        Tibdex - makes an inverted index for tib
                 a slight modification of Invert of bib

    input:  records of lines, separated by blank lines
    output: key:file1 start/length ... start/length:file2 start/length ...

Note to non-unix users:
    This program uses a unix command "sort" which sorts records in a file 
    "tmp_file" with the following sort key: 
          ASCII sort on the first field (fields are separated by spaces),
          ASCII sort on the second field,
          numeric sort on the third record,
          numeric sort on the fourth record,
    removing duplicates, and then writes to the same file. The use of "sort"
    is isolated by the "#define SORT_IT" statement just below.  If "sort" 
    is unavailable, replace the sort in the definition of "SORT_IT" by some 
    equivalent sort.  If the format of "SORT_IT" is unacceptable, modify 
    appropriately the one place further below where "SORT_IT" is used.
    It may not be necessary to remove duplicates.
     
*/

#define SORT_IT \
  sprintf(sortcmd,"sort -u +0 -1 +1 -2 +2n -3 +3n %s -o %s",tmp_file,tmp_file);\
    system(sortcmd);


#include "stdio.h"
#include "tib.h"
#define isnull(x)  (*(x) == NULL)
#define makelow(c) ('A'<=(c) && (c)<='Z' ? (c)-'A'+'a' : c)

char    headerline[240] = " ";/* header line -- list of files            */
int     max_kcnt = 100;     /*  max number of keys                      */
int     max_klen =   6;     /*  max length of keys                      */
char    *ignore =           /*  string of line starts to ignore         */
            "CNOPVcnopv\\\%";
char    *common = COMFILE;  /*  name of file of common words            */
char    *INDEX = INDXFILE;  /*  name of output file                     */
char    tmp_file[120];       /*  name of temporary file                  */
char    dirsp2[]=DIRSEP;    /*  directory separator character           */
char    optch2[]=OPTCH;     /*  option character on call                */
int	silent = 0;	    /*  0 => statistics printed			*/
			    /*  1 => no statisitics printed		*/

long int nextrecord(), recsize(), nextline();
char sortcmd[MAXSTR];

int     argc;
char    **argv;

main(argcount,arglist)
int argcount;
char **arglist;
{   char            filename[MAXSTR], *pcom, *getenv(), *ptemp;
    FILE            *input, *output;
    long int        start,length;
    char            word[MAXSTR];
    int             kcnt;
    char            tag_line[MAXSTR];
    char            argchk[3];

    long int	    records = 0;  /*  number of records read           */
    long int	    keys    = 0;  /*  number of keys read (occurences) */
    long int	    distinct;     /*  number of distinct keys          */
    long int	    shorten();
   int i, strcmp();

   /* header */
   for (i = 1; i < argcount; i++) {
      strcpy(argchk,OPTCH);
      strcat(argchk,"z");
      if (strcmp(arglist[i],argchk) == 0)
         silent = true;
   }
   if (silent == false)
      fprintf (stderr, "Tibdex -- version %s, released %s.\n", VERSION, RDATE);

    /* get file names from environment */
    pcom = getenv("COMFILE");
    if (pcom != NULL)
       strcpy(common,pcom);
    ptemp = getenv("TMPDIR");
    if (ptemp != NULL) {
       strcpy(tmp_file,ptemp);
       strcat(tmp_file,dirsp2);
       strcat(tmp_file,"tibdXXXXXX");
    }
    else
       strcpy(tmp_file,INVTEMPFILE);

    argc= argcount-1;
    argv= arglist+1;
    mktemp(tmp_file);
    output= fopen(tmp_file,"w");
    if (output == NULL) {
       fprintf(stderr, "tibdex: can't open temporary output file\n");
       exit(1);
    }

    for ( flags() ; argc>0 ; argc--, argv++ ,flags() )
    {   /* open input file              */
         strcpy(filename, *argv);
         input = fopen(filename, "r");
         if (input == NULL) {
            strcat(filename, ".ref");
            input=fopen(filename, "r");
            if (input == NULL) {
               fprintf(stderr, "can't open %s or %s\n", *argv, filename);
               exit(1);
               }
            }
            strcat(headerline, " ");
            strcat(headerline, filename);
            start=      0L;
            length=     0L;

        for(;;) /* each record  */
        {   /* find start of next record (exit if none)     */
                start= nextrecord(input,start+length);
                if (start==EOF)   break;
            records++;
	    kcnt= 0;
            length= recsize(input,start);
            sprintf(tag_line, " %s %D %D\n", filename, start, length);

            while (ftell(input) < start+length && kcnt < max_kcnt)
            {   getword(input,word,ignore);
                makekey(word,max_klen,common);
                if (!isnull(word))
                {   fputs(word,output); fputs(tag_line,output);
                    kcnt++; keys++;
                }
            }
        }
        fclose(input);
    }
    fclose(output);

    SORT_IT

    distinct = shorten(tmp_file,INDEX);
    if( silent == 0 )
	fprintf(stderr,
	    "%D documents   %D distinct keys  %D key occurrences\n",
	    records, distinct, keys);
}



/*  Flag    Meaning                             Default
    -ki     Keys per record                     100
    -li     max Length of keys                  6
    -%str   ignore lines that begin with %x     CNOPVXcnopv
            where x is in str
            str is a seq of chars
    -cfile  file contains Common words          /??????/common
            do not use common words as keys
    -pfile  name of output file                 INDEX
    -s	    do not print statistics		statistics printed
*/

#define    operand     (strlen(*argv+2)==0 ? (argv++,argc--,*argv) : *argv+2)

flags()
{   for (; argc>0 && *argv[0]==optch2[0];  argc--,argv++)
    {   switch ((*argv)[1])
        {   case 'k':   max_kcnt= atoi(operand);
                        break;
            case 'l':   max_klen= atoi(operand);
                        break;
            case 'c':   common=  operand;
                        break;
            case '%':   ignore=  *argv+2;
                        break;
            case 'p':   INDEX=  operand;
                        break;
	    case 'z':	silent= 1;
			break;
            default:    fprintf(stderr, "unknown flag '%s'\n", *argv);
        }
    }
}


/*  shorten(inf,outf): file "inf" consists of lines of the form:
        key file start length
    sorted by key and file.  replace lines with the same key
    with one line of the form:
        key:file1 start/length ... start/length:file2 start/length ...
    rename as file "outf"
    returns number of lines in output
*/
long shorten(inf,outf)
char *inf, *outf;
{   FILE *in, *out;
    char line[MAXSTR];
    char key[MAXSTR],  newkey[MAXSTR],
         file[MAXSTR], newfile[MAXSTR];
    long int start, length;
    long int lines = 0;

    strcpy(file,"");
    strcpy(key,"");
    in=  fopen(inf, "r");
    out= fopen(outf, "w");
    if (in==NULL || out==NULL)
    {   fprintf(stderr, "tibdex: error in opening file for compression\n");
        return(1);
    }

    fputs(headerline,out);
    fprintf(out, "\n");
    getline(in,line);
    sscanf(line,"%s%s%ld%ld", key, file, &start, &length);
    fprintf(out, "%s :%s %D/%D", key, file, start, length);
    for ( getline(in, line) ; !feof(in);  getline(in, line))
    {   sscanf(line,"%s%s%ld%ld", newkey, newfile, &start, &length);
        if (strcmp(key,newkey)!=0)
        {   strcpy(key, newkey);
            strcpy(file, newfile);
            fprintf(out, "\n%s :%s %D/%D",  key, file, start, length);
	    lines++;
        }
        else if (strcmp(file,newfile)!=0)
        {   strcpy(file,newfile);
            fprintf(out, ":%s %D/%D", file, start, length);
        }
        else
            fprintf(out, " %D/%D", start, length);
    }
    fprintf(out, "\n");
    lines++;

    fclose(in); fclose(out);
    unlink(inf);
    return (lines);
}
