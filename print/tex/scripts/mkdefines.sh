#!/bin/sh
case $1 in
-make)	shift ;
	echo -n "$1=" ;
	shift ;
	for i in $* ;
	do
	  echo -n "${i}_386BSD=\"\$(${i})\" " ;
	done
	;;
-sed)	shift ;
	echo -n "$1=" ;
	shift ;
	for i in $* ;
	do
	  echo -n "${i}_386BSD=\\\"\$(${i})\\\" " ;
	done
	;;
-xpand)	shift ;
	echo -n "$1=" ;
	shift ;
	for i in $* ;
	do
	  echo -n "\$(${i}) " ;
	done
	;;
esac

