.th USE QUEL 12/21/84
.sh NAME
use - specify a group of delimitors to be used
.br
unuse - specify a group of delimitors to no longer be used
.sh SYNOPSIS
.bd use
groupname
.br
.bd unuse
groupname
.br
.sh DESCRIPTION
The use statement specifies a group of delimitors which
are to be checked when a delimitor name is used in a query. The unuse
statement specifies that the group of delimitors should no longer
be checked. When a delimitor is specified in a query, the pattern to
be associated with the name will be searched for among the list
of delimitor groups which have been used.
.s3
.s3
.sh EXAMPLE
.nf
	define delim paper (word, "[A-Z]{a-z}")
	use paper
	unuse paper
.fi
.sh "SEE ALSO"
delim(quel), destroy(quel)
