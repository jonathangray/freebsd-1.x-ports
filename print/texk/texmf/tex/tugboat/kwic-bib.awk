# /u/sy/beebe/tex/bib/kwic-bib.awk, Mon Nov 14 11:46:24 1988
# Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
# /u/sy/beebe/tex/kwic-bib.awk, Thu Oct 13 11:58:32 1988
# Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
# ======================================================================
#
# This awk program filters a BibTeX file to produce an input file for
# the Unix ptx(1) program which prepares a keyword-in-context (kwic),
# or permuted, index.  The output of this script is a collection of
# lines, on each of which the initial token is the bibliography key.
#
# Because the output of ptx is permuted, we cannot easily preserve
# font changes (word permutations would lose font boundaries), so we
# simply strip them.  To prevent confusion with quotes (") used in the
# ptx output, we drop umlauts as well.  The backslash control sequence
# is changed to forward slash; otherwise the index would contain
# permutations on \bs.  All remaining braces and backslashes are
# stripped.
#
# To facilitate finding secondary authors, the author field is included
# in the output as well.
#
# The reference tag will appear as the first non-blank field on each
# output line.  The ptx -r option will move it to the 5th argument of
# the .xx macro in its output.
#
# Usage:
#	nawk -f kwic-bib.awk foo.bib | ptx -r -f >foo.nro
#
# The troff output of ptx is converted to lines of the form
# \kwic{head}{pre-key}{key}{post-key}{tag}
# by the step
#
#	sed -f ptx.sed <foo.nro >foo.out
#
# and this can then be input to LaTeX as, e.g., a supertabular
# environment, with a suitable definition of the \kwic macro.
#
# ======================================================================

# Assume bibliography entries are keyed by an initial capital, thus
# omitting the @string{} entries.

# blank trim lines
/[ \t]*$/		{gsub(/[ \t]*$/,"");}

# extract tag from "@field{tag,"
/^@[A-Z][a-z]*/		{
	tag = substr($0,index($0,"{")+1);
	tag = substr(tag,1,length(tag)-1); # strip final comma
	gsub(/[{}\\ \t]/,"",tag);# strip whitespace, braces and backslashes
}

# collect complete author list
/^[ \t]*author[ \t]*=[ \t]*\"/	{\
	author = substr($0,index($0,"\"")+1);
	while (substr(author,length(author)-1) != "\",")
	{
		getline $0;
		author = author " " $0;
	}
	if (substr(author,length(author)-1) == "\",")
		author = substr(author,1,length(author)-2);
	author = filter(author)
	printf("%s\t%s\n",tag,author);
}

# collect complete title list
/^[ \t]*title[ \t]*=[ \t]*\"/	{\
	title = substr($0,index($0,"\"")+1);
	while (substr(title,length(title)-1) != "\",")
	{
		getline $0;
		title = title " " $0;
	}
	if (substr(title,length(title)-1) == "\",")
		title = substr(title,1,length(title)-2);
	title = filter(title);
	printf("%s\t%s\n",tag,title);
}


function filter(s, t)
{
    t = s;
    gsub(/[~ \t]+/," ",t);	# collapse whitespace and ties

    gsub(/\\bs/,"/",t);		# change \bs control sequence to forward slash

    gsub(/\\[a-z][a-z] /," ",t); # remove font changes
    gsub(/\\[a-z][a-z]\\/,"\\",t);

    gsub(/\\"{/,"{",t);		# drop umlauts

    gsub(/[{}\\]/,"",t);	# drop remaining braces and backslashes

    gsub(/&/," and ",t);	# change & to and

    gsub(/_/,"\\_",t);		# protect TeX special characters
    gsub(/%/,"\\%",t);
    gsub(/\$/,"\\$",t);

    gsub(/[ ]+/," ",t);		# collapse whitespace again

    return (t);
}
