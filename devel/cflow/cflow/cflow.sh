#!/bin/sh
# cflow - print a function call hierarchy
# This script is contributed to the public domain by Andrew Moore
# of Talke Studio.

F2C=f2c
LEX=lex
YACC=yacc
CPP="cc -E"
PRCC=prcc
PRCG=prcg
TMP=/tmp/cflow.$$
USAGE="usage: cflow [-agivx] [-d n] [-r fn] [-w n] [cpp-directive...] file..."

trap 'rm -f $TMP; exit' 0 1 2 15
invert=""

while getopts D:I:U:d:ir:vw:x c; do
	case $c in
	D)
		CPP="$CPP -D$OPTARG"
		;;
	I)
		CPP="$CPP -I$OPTARG"
		;;
	U)
		CPP="$CPP -U$OPTARG"
		;;
	a)
		PRCG="$PRCG -a"
		;;
	d)
		PRCG="$PRCG -d$OPTARG"
		;;
	g)	PRCC="$PRCC -g"
		;;
	i)
		PRCG="$PRCG -i"
		invert=1
		;;
	r)
		PRCG="$PRCG -r$OPTARG"
		;;
	v)
		PRCC="$PRCC -v"
		;;
	w)
		PRCG="$PRCG -w$OPTARG"
		;;
	x)
		PRCG="$PRCG -x"
		;;
	\?)
		echo "$USAGE" >&2
		exit 2
		;;
	esac
done
shift `expr $OPTIND - 1`

c=$1
while true; do
	case $c in
	*.c|*.cc|*.C)
		cname=$c
		;;
	*.f)
		cname=`basename $c .f`.c
		cat $c | $F2C >$cname
		;;
	*.F)
		cname=`basename $c .F`.C
		cat $c | $F2C >$cname
		;;
	*.l)
		cname=`basename $c .l`.c
		$LEX $c
		sed '/# line/d' lex.yy.c >$cname
		;;
	*.y)
		cname=`basename $c .y`.c
		$YACC $c
		sed '/# line/d' y.tab.c >$cname
		;;
	*.s)
		echo "cflow: assembler source not supported" >&2
		shift
		[ $# -le 0 ] && break
		c=$1
		continue
		;;
	*.h)
		shift
		[ $# -le 0 ] && break
		c=$1
		continue
		;;
	*)	
		echo "$USAGE" >&2
		exit 2
		;;
	esac
	$CPP $cname >>$TMP
	shift
	[ $# -le 0 ] && break
	c=$1
done

if [ X$invert = X ]; then
	$PRCC < $TMP | $PRCG
else
	$PRCC < $TMP |
	sed 's/\(.*	\)\(.*	\)/\2\1/' |
	sort +0 -1 +2 |
	uniq |
	$PRCG
fi
