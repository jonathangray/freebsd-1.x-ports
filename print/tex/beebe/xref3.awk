# Convert a sorted file with entries
#	file<tab>name
# to a libref-style "referenced-by" map on stdout (4 entries per line)
# [09-Sep-87]
BEGIN {
    FS = "\t";
    nref = 0;
    callee = "";
    print "*********************************************************************";
    print "*     ..File..         *  ...Global Symbol...                       *";
    print "*********************************************************************";
    print "*                      *                                            *";
}

{
    if (($1 != "") && ($1 != callee) && (callee != ""))
    {
	if (length(callee) > 20)
	    the_callee =  substr(callee,1,20);
	else
	    the_callee = callee;
        printf "* %-20s->",the_callee;
        nref4 = 4*int((nref + 3)/4);

        for (k = nref; k < nref4; ++k)
            refby[k] = " ";
        nref = nref4;
        for (k = 0; k < nref; )
        {
            printf " %-10s",refby[k];
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
    if ($1 != "")
	callee = $1;	# first type--initial entry for new callee
    if ($2 == "")
	$2 = $4;	# second type--same callee, different caller
    if ($2 != "")	# not third type
    {
	if (length($2) > 10)
	    $2 = substr($2,1,10);
	refby[nref] = $2;
	nref++;	# awk treats refby[nref++] as refby[++nref], sigh...
    }
}

END {
    if (length(callee) > 20)
	the_callee =  substr(callee,1,20);
    else
	the_callee = callee;
    printf "* %-20s->",the_callee;
    nref4 = 4*int((nref + 3)/4);
    for (k = nref; k < nref4; ++k)
        refby[k] = " ";
    nref = nref4;
    for (k = 0; k < nref; )
    {
        printf " %-10s",refby[k];
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
