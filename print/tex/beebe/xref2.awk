# Convert a sorted file with entries
#	name<tab>file
# to a libref-style "references" map on stdout (4 entries per line)
# [09-Sep-87]
BEGIN {
    FS = "\t";
    nref = 0;
    caller = "";
    print "*********************************************************************";
    print "*     ..Name..         *  ...Referenced in File...                  *";
    print "*********************************************************************";
    print "*                      *                                            *";
}

{
    if (($1 != caller) && (caller != ""))
    {
	if (length(caller) > 20)
	    the_caller =  substr(caller,1,20);
	else
	    the_caller = caller;
        printf "* %-20s<-",the_caller;
        nref4 = 4*int((nref + 3)/4);

        for (k = nref; k < nref4; ++k)
            refs[k] = " ";
        nref = nref4;
        for (k = 0; k < nref; )
        {
            printf " %-10s",refs[k];
            ++k;
            if ((k % 4) == 0)
            {
                printf "*\n";
                if (k < nref)
                {
                    printf "*                      *";
                }
            }
        }
        nref = 0;
        print "*                      *                                            *";
    }
    caller = $1;
    if (length($2) > 10)
	$2 = substr($2,1,10);
    refs[nref] = $2;
    nref++;	# awk treats refs[nref++] as refs[++nref], sigh...

}

END {
    printf "* %-20s<-",caller;
    nref4 = 4*int((nref + 3)/4);
    for (k = nref; k < nref4; ++k)
        refs[k] = " ";
    nref = nref4;
    for (k = 0; k < nref; )
    {
        printf " %-10s",refs[k];
        ++k;
        if ((k % 4) == 0)
        {
            printf "*\n";
            if (k < nref)
            {
                printf "*                      *";
            }
        }
    }
    nref = 0;
    print "*                      *                                            *";
    print "*********************************************************************";
}
