#!sed
#
# This is to make an shell intro file
#
#s/\([^ ]*_386BSD\)="\([^"]*\)"/\1=\"\2\"\
#/g
#
# Here we make a sedscript
#

s/\([^ ]*_386BSD\)="\([^"]*\)"/s\#$(\1)\#\2\#g\
/g
