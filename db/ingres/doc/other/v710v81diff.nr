.ds HE 'Version 8.1 Changes'INGRES'Page %'
.so nmacs
Version 8.1 of INGRES is functionaly upward compatible
with version 7.10 of INGRES.
.sh 1
Monitor
.dn
Escaped characters.
.PS
The INGRES monitor used to interpret \\g as meaning "go", no matter
how it was escaped. You can now send a \\g as part of a string.
For example:
.nf

replace e (string ="quel statment\\\\g") where e.number = 4

.fi
.sh
Nested includes
.PS
A bug in the monitor which caused a "longjmp botch" if there were a
recursive include followed by a user interrupt has been fixed.

.sh 1
Equel
.dn
Error Messages
.PS
The EQUEL precompiler now produces error messages that the
UNIX utility "error(1)" can understand.

.sh
Exit Value
.PS
The EQUEL precompiler now exits with a non-zero status if it finds any
errors.

.sh
Param Commands
.PS
A bug in the param command which caused the last request to be
lost has been fixed.
.sh 1
Parser
.dn
Implicit Range Variables
.PS
The INGRES system now supports implicit range variables. If you issue
a query with an undefined range variable, the system will check to see
if there is a relation with the same name as the variable. If there is,
it will create a range variable over that relation with the name of the
relation.
For example:
.nf

retrieve ( relation.all ) where relation.relid = "attribute"

.fi
.sh
Qualification Clauses With And Or Nodes.
.PS
A bug which caused statements with a qualification clause of the form:
.nf

retrieve ( e.all ) where e.a = f.a of (e.b = f.b and e.c = f.c)

.fi
to core dump has been fixed.
.sh
Block Copy of Tids
.PS
You can now use the copy(QUEL) command to block copy tids out of
a relation. You can NOT use the copy command to block copy them in.
For example:
.nf

copy emp(tid=i4,name=c0,comma=d1,sal=c0,nl=d1) 
	into "/mnt/you/yourfile"

.fi
.sh
New String Manipulation Routines
.PS
There are now a number of complex string manipulation routines
that are available, see quel(QUEL) for details.
.sh
New Storage Structure
.PS
There is now a new storage structure "ordered". This structure
will guarantee the retrieval order of a relation, see ordered(QUEL)
for details.
.sh 1
Qrymod
.dn
Range Variables in Views.
.PS
A bug that caused range variables to disapear after executing a view
multiple times has been removed.
.sh
Protection.
.PS
A bug which kept some protection definitions from being inserted into
the system has been fixed.
.sh
Integrity.
.PS
A bug which caused some integrities to be incorrectly stored has been fixed.
.sh
View Updates.
.PS
A decision to disallow updates to attributes which define a view has been
reversed. That is this now works:
.nf

range of e is emp
range of d is dept

define view viewdef (name = e.emname,deptname = d.dept, deptno = d.deptno) 
	where d.deptno = e.deptno

range of v is viewdef

replace v (v.deptno = 5) where v.deptno = 4
.fi

.sh 1
Support
.dn
Purge.
.PS
Purge will now remove all files when running in the super user mode.
.sh
Copydb
.PS
Copydb now works on the vax.
.sh
Directory Access.
.PS
The INGRES system now uses the directory handling system supplied with 4.2.
.sh
Ingconv
.PS
There is now a program ingconv, which will convert version 7 databases
into version 8 databases.
.sh 1
General
.dn
Concurrency Control
.PS
The concurrency control driver is now a user process, rather than a
kernel device driver.
.sh
Sort Function
.PS
The program ksort which INGRES uses to sort relation has been made
part of INGRES. This should cause programs that have sort clauses
to run faster.
.sh
Syserr
.PS
When INGRES dies because of a fatal error, it uses the function "perror"
now, rather then printing the UNIX error number.
