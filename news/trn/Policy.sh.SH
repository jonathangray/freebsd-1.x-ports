case $CONFIG in
'') . ./config.sh ;;
esac
case "$0" in
*/*) cd `expr X$0 : 'X\(.*\)/'` ;;
esac
echo "Extracting Policy.sh (with variable substitutions)"
$spitshell <<!GROK!THIS! >Policy.sh
$startsh
#
#  This file was produced by running the Polich.sh.SH script, which
#  gets its values from config.sh, which is generally produced by
#  running Configure.
# 
#  \$Id: Policy.sh.SH,v 1.2 1994/02/22 01:42:46 nate Exp $

# Configuration time: $cf_time
# Configured by: $cf_by
# Target system: $myuname

# login name of the person who configured trn. not particularly interesting.
cf_by='$cf_by'
# time of configuration.  Not particularly interesting.
cf_time='$cf_time'

#		install directives.

#		bin directories (string values)
#	name of the final resting place
bin='$bin'
#	how to get to the final resting place (thank you AFS)
installbin='$installbin'

#		private libraries
#	name of the final resting place for those items in the library
#	directory (string value)
privlib='$privlib'
#	How toget to the library final resting place (thanks AFS)
installprivlib='$installprivlib'

#	interesting questions about man
# 	where to man page sources go?
mansrc='$mansrc'
#	what extention do man pages get?
manext='$manext'

#		path to assorted programs that we might want to override.
#	name of the default editor.  (string value)
defeditor='$defeditor'
#	prefered user shell (string value)
prefshell='$prefshell'
#	favorite local pager (string value)
pager='$pager'
# where is inews?  (string value)
inewsloc='$inewsloc'
#	path to interactive speller or "none" (string value)
ispell_prg='$ispell_prg'
#	spelling options for ispell_prg or "spell" if "none" (string value)
ispell_options='$ispell_options'

#		internal options
#	ingore the ORGANIZATION environment variable? (define/undef)
d_ignoreorg='$d_ignoreorg'
#	does the mailer understand FQDN addressing? (define/undef)
d_internet='$d_internet'
#	Do you have a news admin? (define/undef)
d_newsadm='$d_newsadm'
#	name of the news admin? (string value)
newsadmin='$newsadmin'
#	read via NNTP? (define/undef)
d_nntp='$d_nntp'
#	use the XDATA NNTP extension? (define/undef)
d_xdata='$d_xdata'
#	path to a file containing a server name, or a hostname (string value)
servername='$servername'

#	distribution names (string values)
# local city
citydist='$citydist'
# "local" country
cntrydist='$cntrydist'
# "local" contentient
contdist='$contdist'
# site distribution
locdist='$locdist'
# organizational distribution
orgdist='$orgdist'
# state/province distribution name
statedist='$statedist'

#		Naming information.
#	password file contains names (define/undef)
d_passnames='$d_passnames'
#	berkeley style password entries (name first in GCOS) (define/undef)
d_berknames='$d_berknames'
#	USG style password entries (account number first in GCOS)
#	(define/undef)
d_usgnames='$d_usgnames'
#	what type of name to use.. (bsd/usg/other)
nametype='$nametype'

#	How portable do we want to be? Determines if we do lookups now
#	or wait until run time.  (define/undef)
d_portable='$d_portable'

#		news library information
#	where is the news library (usually /usr/lib/news) may contain ~<usrname>
newslib='$newslib'
#	absolute path name to /usr/lib/news.
newslibexp='$newslibexp'
#	where is the news spool (usually /{var,usr}/spool/news)
newsspool='$newsspool'
#	active file stuff, like where is it, what is its name, etc
#	path to the active file. (string value)
active='$active'
#	do we have an active.times file? (define/undef)
d_acttimes='$d_acttimes'
#	path to the active.times file. (string value)
acttimes='$acttimes'
#	organizations name. path to file, or constant string
orgname='$orgname'

#	only one of the two following is needed
#	command to find the posting hosts name (string value, optional)
phostcmd='$phostcmd'
#	file containing posting hosts name or constant string
#				(string value, optional)
#
phost='$phost'

#	what should we use? mthreads or overview
#	use the mthreads format? (define/undef)
d_usemt='$d_usemt'
#	where do we find the thread files? (string value)
threaddir='$threaddir'
#	use the overview format? (define/undef)
d_useov='$d_useov'
#	where do we find the .overview fils? (string value)
overviewdir='$overviewdir'

#	trn start up options
trn_init='$trn_init'
#	start up with the selector? 
trn_select='$trn_select'

!GROK!THIS!
$eunicefix Policy.sh
if test -f hints/Policy.sh ; then
    echo "Checking for changes."
    if diff hints/Policy.sh Policy.sh >/dev/null ; then
	echo "Policy.sh unchanged, retaining original"
	rm Policy.sh
    else
	echo "installing new Policy.sh, old one left in Policy.sh.old"
	(cd hints; mv Policy.sh Policy.sh.old)
	mv Policy.sh hints
    fi
else
    mv Policy.sh hints
fi
