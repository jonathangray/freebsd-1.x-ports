.th DELIM QUEL 12/21/84
.sh NAME
delim - specify a name for a pattern of characters
.sh SYNOPSIS
.bd destroy
.bd delim
groupname
(
delimitor, pattern
)
.br
pattern =
[ "[character list]" | "{character list}" ]*
.br
.sh DESCRIPTION
The delim statement allows the user to specify a name for
a certain pattern of characters, which may be used with the substring
facility. The delimitors are stored in groups, which may be used
and unused together.
.s3
The pattern for a delimitor is specified using a modified
version of BNF grammar. A character list is either a list of the
characters to match, or a range of characters separated by a "-",
or a combination or both of the above. A character list in brackets 
indicates that one of the characters must match exactly once,
and a character list in braces indicates that one of the characters
may match zero or more times. The pattern is composed of a list
of character lists.
.s3
After a delimitor has been defined, to be used in
an ingres query it must be activated using the 'use' command.
.sh EXAMPLE
.nf
/\*(** will match a sequence of alphabetic characters beginning
with a capital letter \*(**/
define delim paper(word, "[A-Z]{a-z}")
.s1
/\*(** will match a decimal number \*(**/
define delim math (dec, "[0-9]{0-9}[.][0-9]{0-9}")
.s1
.fi
.sh "SEE ALSO"
destroy(quel), quel(quel), use(quel) 
