# Make a "nm"-style cross-reference table which can be later
# converted into neat tabular form.
#
# Usage:
#	cxref filelist | awk -f xref1.awk | sort -u | >foo.nm
#
# Sun OS 3.3 cxref listings look like
#
#dvijep.c:
#	
#	SYMBOL		FILE			FUNCTION   LINE
#	
#	#undef		./machdefs.h		--	    382  404  406  413  427  432  436  441  446  466
#			./main.h		--	    17  45  67  78
#	ABS()
#			./fixpos.h		--	    24
#			./gendefs.h		--	   *16
#			./movedown.h		--	    27
#			./moveover.h		--	    30
#			./option.h		--	    123
#			dvijep.c		--	    554
# etc..
#
# We want to extract from this a file with entries
#	name<tab>xref
#
# We ignore lines with only one field (eliminating the filename lines),
# and reuse the previous name if the first field is empty.  We 
# eliminate leading "./" in filenames, and discard lines for which
# name == 'SYMBOL', or which have an absolute pathname in xref (we
# don't want to cross-reference names defined in /usr/include/*.h),
# or which have only digits or asterisks in xref (line numbers).
# [18-Jul-87]
BEGIN {name = "";}
{
    if (substr($0,1,1) != "	")
    {
	name = $1;
	xref = $2;
    }
    else
	xref = $1;
    if ((name == "SYMBOL") && (xref == "FILE"))
	next;
    else if (substr(xref,1,1) == "/")
	next;
    else if (substr(name,1,1) == "#")
	next;
    else if (xref != "")
    {
	if (substr(xref,1,2) == "./")
	    xref = substr(xref,3)
	if (xref !~ /[0-9*]/)
	    printf "%s\t%s\n",name,xref;
    }
}
