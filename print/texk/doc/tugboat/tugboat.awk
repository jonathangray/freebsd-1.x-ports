# /usr/local/lib/tex/tugboat/tugboat.awk, Thu Aug 29 12:01:26 1991
# Edit by Nelson H.F. Beebe <beebe@solitude.math.utah.edu>
# Change "using editor" to "using Anonymous"
# /usr/local/lib/tex/tugboat/tugboat.awk, Thu Jun 28 13:29:19 1990
# Edit by Nelson H.F. Beebe <beebe@plot79.math.utah.edu>
# Remove redundant `and' in authors
# /u/sy/beebe/tex/tugboat/tugboat.awk, Wed Jun  7 15:09:44 1989
# Edit by Nelson H.F. Beebe <beebe@plot79.utah.edu>
# Change unknown author to Anonymous.
# /home/csc-sun/u/sy/beebe/tex/tugboat/tmp/tugboat.awk,
# Mon Oct 31 08:09:28 1988
# Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu)
# ======================================================================
# Filter TUGboat tables of contents files (<tex.tugboat>tb*.cnt)
# to produce BibTeX files.  The new version of AWK (matching the AWK
# book) is required:
#
# Usage:
#	nawk -f tugboat.awk tb*.cnt >tugboat.bib
#
# \def's are collected at written separately on the file tugboat.def
# and a @preamble command is output that will read it.
#
# Known deficiencies:
#
# (1) Reference tags are generated from the last word in the first
# author's name, followed by journal, volume, number and page; these are
# therefore unique, but not as mnemonic as tags that included important
# words from the title.  For automatically-generated tags, it is not
# clear that a better solution can be easily implemented.
#
# (2) Page numbers have only the initial page number; the final page
# number cannot be reliably inferred from the starting number of the
# next article, because an article may end on the same page as the start
# of the following one, or there may be a page break, or advertisements
# between them.  The TUGboat editors have retrofitted page ranges into
# the tb*.cnt files, so this should not a problem in the future.
#
# (3) special hacks have been introduced to handle odd cases like
# missing author and non-empty title (set author = editor), names with
# Jr and II or III, Spanish accented i (\'\i), names/tags with
# accented letters
#
# Author:
#	Nelson H.F. Beebe
#	Center for Scientific Computing
#	South Physics Building
#	University of Utah
#	Salt Lake City, UT 84112
#	USA
#	Tel: (801) 581-5254
#	Email: Beebe@science.utah.edu
#
# ======================================================================

BEGIN {
	"echo \"$USER at `hostname`\"" | getline username;
	"date" | getline date;
	printf("-*-bibtex-*-\n"); # Emacs file type string
	printf("========================================================================\n");
	printf("BibTeX database file for TUGboat created automatically with\n\n");
	printf("\t%s -f tugboat.awk\n\nby %s on %s\n\n",\
		ARGV[0],username,date);
	printf("Input files:\n");
	for (k = 1; k < ARGC; ++k)
		printf("\t%s\n",ARGV[k]);
	printf("========================================================================\n");
	printf("\n@preamble{\"\\input tugboat.def\"}\n\n");
	printf("@string{TUGboat = \"TUGBoat\"}\n\n");
}

# Pattern: "\TUBhead 9, 1, April 1988<"

/^[ \t]*\\TUBhead/	{
	line = trim(strip_comments($0));
	gsub(/{.*}/,"-",line);	# replace glue
	gsub(/,/," ",line);	# remove commas
	split(line,d," ");	# extract fields
	volume = d[2];		# and save them
	number = d[3];
	month = substr(d[4],1,3);
	year = substr(d[5],1,index(d[5],"<")-1);
}

# Pattern: "\subsec Author\\Title\\page<" and "\subquery Author\\Title\\page<"

/^[ \t]*\\sub(sec|query) /	{
	line = trim(strip_comments($0));
#	printf("\n\nDEBUG: [%s]\n",line);
#	printf("DEBUG: [%s]\n",substr(line,length(line),1));
	while (substr(line,length(line),1) != "<")
	{
#		printf("DEBUG: [%s]\n",substr(line,length(line),1));
		getline;
		line = line " " trim(strip_comments($0));
	}
#	printf("DEBUG: [%s]\n",line);
	gsub(/~/," ",line);		# discard ties
	gsub(/\\newline|\\break/," ",line); # and forced line breaks
	gsub(/\.\\thinspace/,". ",line); # and \thinspace after initials only
	line = substr(line,index(line," "));	# discard \subxxx
	line = trim(line);
	gsub(/`\\\\/,"`!!",line);	# protect '\\ from split
	split(line,d,"\\\\\\\\");	# extract fields
	gsub(/`!!/,"`\\\\",d[1]);	# restore `\\ strings
	gsub(/`!!/,"`\\\\",d[2]);
	gsub(/`!!/,"`\\\\",d[3]);
#	printf("DEBUG: d[1] = <%s> d[2] = <%s> d[3] = <%s>\n",d[1],d[2],d[3]);


	author = trim(d[1]);
	gsub(/,/," and ",author);	# convert to BibTeX form
	gsub(/\\and/,"and",author);
	gsub(/   */," ",author); 	# reduce to single blanks
	gsub(/ and and /," and ",author); # and remove redundant `and'
	author = fix_name(author," and III",", III");
	author = fix_name(author," and II",", II");
	author = fix_name(author," and Jr.",", Jr.");
	author = fix_name(author," and Jr",", Jr");
	author = fix_name(author," and S.J.",", S.J.");
	gsub(/\\'\\i{}/,"{\\'{\\i}}",author); # fix  r\'\i{} to {r}{\'{\i}}
	gsub(/\\'\\i /,"{\\'{\\i}}",author); # fix D\'\i az to D{\'{\i}}az
	author = fix_accents(author);
	author = fix_initials(author);

	title = trim(d[2]);
	if ((author == "{}") || (author == ""))
	{				# lines like this DO occur
		if ((title == "{}") || (title == ""))
			next;
		printf("File %s:line %d: Empty author field [%s] -- using Anonymous\n", \
			FILENAME,FNR,line) >> "/dev/tty";
		author = "Anonymous"; # uncertain authorship
	}

	gsub(/   */," ",title); 	# reduce to single blanks
	page = trim(substr(d[3],1,index(d[3],"<")-1));

	# locate first author's last name in [last_blank+1 .. k-1];
	k = index(author," and");
	if (k == 0)
		k = length(author) + 1;
	n = index(author,",");
	if ((k > n) && (n > 0))
 		k = n;	# drop ", Jr" and ", II" in tags
	last_blank = 0;
	for (n = 1; n < k; ++n)
	{
		if (substr(author,n,1) == " ")
			last_blank = n;
	}
	tag = substr(author,last_blank+1,k-last_blank-1) \
		":TB" volume "-" number;
	if (page != "")
 		tag = tag "-" page;
	gsub(/ /,"-",tag); 		# change blanks to hyphen
	gsub(/[^-a-zA-Z0-9:]/,"",tag);	# and strip all illegal chars

	# There are instances of same author on single page with
	# intervening articles, so we cannot just keep track of the
	# previous tag.  Instead, we use awk's wonderful associative
	# arrays, and the fact that uninitialized values are guaranteed
	# to be 0.
	if (tag_use_count[tag])
	{	# multiple articles by same author on one page
		tag_use_count[tag]++;
		tag = tag "-" tag_use_count[tag];
	}
	else
	{
		tag_use_count[tag] = 1;
	}
	printf("@Article{%s,\n",tag);
	last_tag = tag;
	printf("  author =\t\"%s\",\n",author);

	# NB: the formatting of the title must be done with care.  Two
	# levels of braces are needed.  The outer one protects against
	# letter case conversion, and the inner one removes the
	# possibility of BibTeX interpreting a leading macro name as a
	# `special character', in which case it will do letter case
	# conversion inside the braces anyway.  If the title is empty,
	# we omit the braces, because BibTeX would then view it as
	# non-empty, and would format the bibliography entry
	# incorrectly.
	if (length(title) > 0)
		printf("  title =\t\"{{%s}}\",\n",title);
	else
		printf("  title =\t\"\",\n",title);

	printf("  journal =\tTUGboat,\n");
	printf("  year =\t\"%s\",\n",year);
	printf("  volume =\t\"%s\",\n",volume);
	printf("  number =\t\"%s\",\n",number);
	if (page != "")
		printf("  pages =\t\"%s\",\n",page);
	printf("  month =\t%s,\n",month);
	printf("}\n\n");
}

/^[ \t]*\\def\\/	{
 	print $0 >>"tugboat.def";
}

END {}
#=======================================================================
# Utility functions.  NB: awk has no declarations, so to get private
# locals in functions, we declare additional arguments separated by
# space from the main argument(s) that are not supplied when we call
# the function.  If this isn't done, the variables used will be global
# ones, with likely disasterous effects.
#=======================================================================

function fix_accents(t, k,s,r)	# convert \"x to {\"{x}} for " = ", `, '
{
#	Alas, awk does not allow the replacement pattern to specify
#	\1, \2, etc to identify groups matched by a regular expression,
#	so we handle them manually.

	s = t;

	gsub(/\\o( |$)/,"{\\o}",s);	# protect accents
	gsub(/\\oe( |$)/,"{\\oe}",s);
	gsub(/\\ae( |$)/,"{\\ae}",s);
	gsub(/\\aa( |$)/,"{\\aa}",s);
	gsub(/\\ss( |$)/,"{\\ss}",s);
	gsub(/\\l( |$)/,"{\\l}",s);
	gsub(/\\O( |$)/,"{\\O}",s);	# protect accents
	gsub(/\\OE( |$)/,"{\\OE}",s);
	gsub(/\\AE( |$)/,"{\\AE}",s);
	gsub(/\\AA( |$)/,"{\\AA}",s);
	gsub(/\\L( |$)/,"{\\L}",s);


	for (k = 1; k < length(s); ++k)
	{
		if ( ( (substr(s,k,2) == "\\\"") ||
		      (substr(s,k,2) == "\\`") ||
		      (substr(s,k,2) == "\\'") ) &&
		     (substr(s,k+2,1) != "{") )
		{
			r = substr(s,1,k-1);
			r = r "{";
			r = r substr(s,k,2);
			r = r "{";
			r = r substr(s,k+2,1);
			r = r "}}";
			r = r substr(s,k+3);
			s = r;
		}
	}
	return (s);
}

function fix_initials(s, k,t)	# change "P.D.Q. Bach" to "P. D. Q. Back"
{
    t = s;
    for (k = 0; k < length(t); ++k)
    {
	if (isupper(substr(t,k,1)) && (substr(t,k+1,1) == ".") &&
	    isupper(substr(t,k+2,1)))
	    t = substr(t,1,k+1) " " substr(t,k+2);
    }
    return (t);
}

# replace old by new, and brace it with preceding last name
function fix_name(s,old,new, k,t)
{
    k = index(s,old);
    if (k == 0)
	return (s);
    t = substr(s,1,k-1) new "}" substr(s,k+length(old));
    for (; (k > 0) && (substr(t,k,1) != " ");--k)
        ;
    t = substr(t,1,k) "{" substr(t,k+1);
    return (t);
}

function isalpha(t)	# return 1 if t is alphabetic, 0 otherwise
{
    if (index("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",t) > 0)
	return 1;
    else
	return 0;
}

function islower(t)	# return 1 if t is lowercase, 0 otherwise
{
    if (index("abcdefghijklmnopqrstuvwxyz",t) > 0)
	return 1;
    else
	return 0;
}

function isupper(t)	# return 1 if t is uppercase, 0 otherwise
{
    if (index("ABCDEFGHIJKLMNOPQRSTUVWXYZ",t) > 0)
	return 1;
    else
	return 0;
}

# NB: The function, protect(), is no longer used, because an extra
# pair of braces around the title protects the entire string from case
# conversion.  However, there may be other circumstances in which it
# is useful, so I'm preserving it here.
#
# Brace upper-case letters for BibTeX protection, but handle macros
# specially.  Macros with no upper-case letters are preserved intact
# (so {\tt foo} works), but macros with mixed case are preserved, so
# \TeX{} becomes {\TeX}{}.   This means that there had better not be
# any macros containing uppercase letters that need arguments.  It
# also suggests that BibTeX should be revised to leave macros alone;
# currently they get case-twiddled.

function protect(t, k,n,s,t,anyuc)
{
    s = t;
    for (k = 1; k <= length(s); ++k)
    {
	if (isupper(substr(s,k,1)))
	{	# brace consecutive upper-case substring
	    for (n = k+1;\
		 (n <= length(s)) && isupper(substr(s,n,1)); ++n)\
		 ;  # substring k..n-1 is upper-case
	    t = substr(s,1,k-1);
	    t = t "{" substr(s,k,n-k) "}";
	    k = length(t);
	    t = t substr(s,n);
	    s = t;
	}
	else if (substr(s,k,1) == "\\")
	{	# TeX control sequence
	    if (!isalpha(substr(s,k+1,1)))
		n = k+2; # non-alpha control sequence
	    else    # \macro name
	    {
		anyuc = 0;
		for (n = k+1; n <= length(s); ++n)
		{
		    if (!isalpha(substr(s,n,1)))
			break;
		    anyuc = anyuc || isupper(substr(s,n,1));
		}
	    }
#	    printf("DEBUG: %d..%d [%s]\n",k,n,s);
#	    printf("DEBUG: %s{%s}%s\n", \
#		substr(s,1,k-1), substr(s,k,n-k), substr(s,n));
	    if (k > 1)
		t = substr(s,1,k-1);
	    else
		t = "";
	    if (anyuc)
		t = t "{";
	    t = t substr(s,k,n-k);
	    if (anyuc)
		t = t "}";
	    k = length(t);
	    t = t substr(s,n);
	    s = t;
#	    printf("DEBUG:  ----- [%s]\n",s);
	}
    }
    return (s);
}

function strip_comments(s,  k,n,t) # delete trailing comments
{		# recognize \% as not starting a comment
    t = s;
    for (k = 1; k <= length(t); ++k)
    {
	n = k - 1 + index(substr(t,k),"%");
	if (n < k)  # then no comments in t
	    return (t);
	if (substr(t,n-1,1) != "\\")
	{
#	    printf("  DBG: [%s] -> [%s]\n",s,substr(t,1,n-1));
	    return (substr(t,1,n-1));
	}

	k = n;
    }
#   printf("  DBG: [%s] -> [%s]\n",s,t);
    return (t);
}


function trim(t, s) # trim leading and trailing whitespace, return s
{
    s = t;
    gsub(/^[ \t]*/,"",s);   # trim leading whitespace
    gsub(/[ \t]*$/,"",s);   # trim trailing whitespace
    return (s);
}
