case $CONFIG in
'')
    if test ! -f config.sh; then
	ln ../config.sh . || \
	ln ../../config.sh . || \
	ln ../../../config.sh . || \
	(echo "Can't find config.sh."; exit 1)
    fi
    . ./config.sh
    ;;
esac
case "$0" in
*/*) cd `expr X$0 : 'X\(.*\)/'` ;;
esac

echo "Extracting utils/printmail (with variable substitutions)"
echo $startsh > printmail
cat >>printmail <<'!NO!SUBS!'
# printmail: part of the Elm mail system
# @(#)$Id: printmail.SH,v 1.3 1993/10/09 19:41:15 smace Exp $
#		Copyright (c) 1988-1992 USENET Community Trust
# 		Copyright (c) 1986,1987 Dave Taylor
 
if [ "$1" = "-p" ]; then
  flags="-p";shift
fi
 
if [ "$1" != "" ]; then
  if [ ! -r $1 ]; then
    echo printmail: cannot open folder $1 1>&2
    exit 1
  else
    flags="$flags -f $1"
  fi
fi
 
# and now invoke readmsg
exec readmsg $flags \*
!NO!SUBS!
