case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting newsetup (with variable substitutions)"
$spitshell >newsetup <<!GROK!THIS!
$startsh
# $Id: newsetup.SH,v 1.3 1993/11/17 23:03:17 nate Exp $
# 
# This software is Copyright 1991 by Stan Barber. 
#
# Permission is hereby granted to copy, reproduce, redistribute or otherwise
# use this software as long as: there is no monetary profit gained
# specifically from the use or reproduction of this software, it is not
# sold, rented, traded or otherwise marketed, and this copyright notice is
# included prominently in any copy made. 
#
# The author make no claims as to the fitness or correctness of this software
# for any use whatsoever, and it is provided as is. Any use of this software
# is at the user's own risk. 

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

: syntax: newsetup

case $d_portable in
define)
    # where recordings, distributions and moderators are kept
    lib=\`$filexp $newslib\`
    # where important rn things are kept
    rnlib=\`$filexp $privlib\`
    ;;
undef)
    # where recordings, distributions and moderators are kept
    lib="$newslib"
    # where important rn things are kept
    rnlib="$privlib"
    ;;
esac

test=${test-test}
echo=${echo-echo}
cat=${cat-cat}
mv=${mv-mv}
rm=${rm-rm}

!GROK!THIS!
$cat >>newsetup <<'!NO!SUBS!'
dotdir="${DOTDIR-${HOME-$LOGDIR}}"
newsrc="${NEWSRC-$dotdir/.newsrc}"
tmp="${TMPDIR-/tmp}"

subs="$lib/subscriptions"
tmpsubs="$tmp/trnsubs.$$"

$cat <<'EOH'

Welcome to trn.  Here's some important things to remember:

  o  Trn is an extension of rn and has a similar command syntax.
  o  To access all the new features, specify the options -x and -X.  These
     options MAY be on by default, but it won't hurt to be redundant.
  o  Single-character commands don't require a carriage return -- only
     commands that let you type in an argument.
  o  At ANY prompt, you may type 'h' for help.  There are many different help
     menus, depending on the context.  Also, typing <esc>h in the middle of a
     multi-character command will list escape substitutions.
  o  Typing a space to any prompt means to do the normal thing.  You could
     spend all day reading news and never hit anything but the space bar.
  o  If you have never used the news system before, you may find the articles
     in news.announce.newusers to be helpful.
  o  Please consult the man page for complete information.

EOH

$rm -f $newsrc-old
$echo "Creating $newsrc to be used by news programs."

if $test -s "$newsrc"; then
    $echo "Saving your current one as $newsrc-old..."
    $mv -f $newsrc $newsrc-old
fi

if $test -r $subs; then
    cp $subs $newsrc
else
    if $test -r $rnlib/getactive; then
	$rnlib/getactive subscriptions $tmpsubs >/dev/null 2>&1
    fi
    if $test -s $tmpsubs; then
	cp $tmpsubs $newsrc
    else
	$cat <<EOM >$newsrc
news.announce.newusers: 
EOM
    fi
    $rm -f $tmpsubs
fi

$cat <<'EOH'
Done.

To add new group use "a pattern" or "g newsgroup.name".  To get rid of
newsgroups you aren't interested in, use the 'u' command.
EOH
!NO!SUBS!
$eunicefix newsetup
chmod 755 newsetup
