.th ORDERED QUEL 12/31/84

.sh NAME
ordered \- storage structure type

.sh DESCRIPTION
.it Ordered
relations are a special type of storage structure in INGRES.
They are created by using the modify relation to orderedn command
where n indicates the ordering dimension.  Ordering does not
destroy existing storage structures on a relation.  The resulting
relation is the old relation with n 4-byte integer LID fields
"attached" to the the end of the relation.  These fields are
different from conventional attribute fields because they can
be dynamically adjusted  by the system during updates to maintain
a consistent ordering of tuples in a relation.  Thus a LID
attribute  value may be updated even though a query does not
explicitly affect a lid attribute in that tuple.
.s3
.i0
Updates are fully supported in ordered relations with the following
side effects.
.s3
.i0
Appends - If the user specifies a lid value, the tuple is inserted
in front of the tuple with that lid value.  Thus all lid values
following that tuple are incremented by one.  If a lid is not
specified and it corresponds to the lowest lid level (ie lid3 in
a 3-dimensional ordered relation), the tuple will be inserted
at the end of the lid subtree that it corresponds to.  Otherwise
the lid value is assumed to be "0" which indicates to the system
that a new lid subtree will be created at level n where the lid
value was specified.
.s3
.i0
Deletes - The user can delete tuples by specifying lid values.
The side effect is that lid values will be collapsed due to
the removed tuples.
.s3
.i0
Replaces - Like appends, new tuples will be inserted in front
of the tuple with the old lid value that the user is trying to
replace.  If no new lid values are specified, they're assumed to
be the old ones.  To create a new lid subtree using replace,
a lid value is "0" is to be specified.
.s3
.i0
.sh "SEE ALSO"
modify(quel)
