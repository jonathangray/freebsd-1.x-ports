.th "TABLE OF CONTENTS" INGRES 4/16/86
.s3
.i0
.in +4
.ti -4
APPEND(QUEL) \- append tuples to a relation
.br
.bd append 
[\c
.bd to\c
] relname (target_list) [\c
.bd where 
qual\c
]
.s3
.i0
.in +4
.ti -4
COPY(QUEL) \- copy data into/from a relation from/into a \*(UU file.
.br
.bd copy
relname 
(domname = format {, domname = format })
.br
.ti +10
direction "filename"
.s3
.i0
.in +4
.ti -4
CREATE(QUEL) \- create a new relation
.br
.bd create
relname (domname\c
.it 1
= format {, domname\c
.it 2
= format })
.s3
.i0
.in +4
.ti -4
DEFINE(QUEL) \- define subschema
.br
.in +5
.ti -5
.bd define
.bd view
name (target list) [
.bd where
qual ]
.ti -5
.bd "define permit"
oplist
{
.bd on
|
.bd of
|
.bd to
}
var
[ (attlist) ]
.bd to
name
[
.bd at
term ]
[
.bd from
time
.bd to
time ] 
[
.bd on
day
.bd to
day ] [
.bd where
qual ]
.ti -5
.bd define
.bd integrity
.bd on
var
.bd is
qual
.i0
.s3
.i0
.in +4
.ti -4
DELETE(QUEL) \- delete tuples from a relation
.br
.bd delete
tuple_variable 
[\c
.bd where
qual\c
]
.s3
.i0
.in +4
.ti -4
DELIM(QUEL) \- specify a name for a pattern of characters
.br
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
.s3
.i0
.in +4
.ti -4
DESTROY(QUEL) \- destroy existing relation(s)
.br
.bd destroy 
relname { , relname }
.br
.bd destroy 
[
.bd permit
|
.bd integrity
] relname
[
integer { , integer }
|
.bd all
]
.br
.bd destroy
.bd delim
groupname
.s3
.i0
.in +4
.ti -4
HELP(QUEL) \- get information about how to use \*(II, about relations in the database,
.br
.bd help 
[
relname
] [
"section"
] 
{, relname}{, "section"}
.br
.bd "help view"
relname {, relname}
.br
.bd "help permit" 
relname {, relname}
.br
.bd "help integrity"
relname {, relname}
.br
.bd help 
.bd delim
[
groupname
]
{, groupname}
.br
.s3
.i0
.in +4
.ti -4
INDEX(QUEL) \- create a secondary index on an existing relation.
.br
.bd "index on"
relname 
.bd is
indexname 
(domain\c
.it 1 
{ ,domain\c
.it 2\c
})
.s3
.i0
.in +4
.ti -4
INTEGRITY(QUEL) \- define integrity constraints
.br
.bd "define integrity on" 
var 
.bd is 
qual
.s3
.i0
.in +4
.ti -4
MACROS(QUEL) \- terminal monitor macro facility
.br
.s3
.i0
.in +4
.ti -4
MODIFY(QUEL) \- convert the storage structure of a relation
.br
.in +5
.ti -5
.bd modify 
relname 
.bd to 
storage-structure
[
.bd on 
key1 [ :
.it sortorder
] [ { , key2 [ :
.it sortorder
] } ] ] 
[
.bd where 
[
.bd "fillfactor =" 
.it n
] [
.bd ", minpages =" 
.it n
] [
.bd ", maxpages =" 
.it n
] [
.bd ", lidn ="
.it lidname
] [
.i0
.s3
.i0
.in +4
.ti -4
MONITOR(QUEL) \- interactive terminal monitor
.br
.s3
.i0
.in +4
.ti -4
ORDERED(QUEL) \- storage structure type
.br
.s3
.i0
.in +4
.ti -4
PERMIT(QUEL) \- add permissions to a relation
.br
.bd define
.bd permit
oplist {
.bd on
|
.bd of
|
.bd to
} var [ (attlist) ]
.br
.if n .ti +5
.if t .ti +0.5i
.bd to
name [
.bd at
term ] 
[
.bd from
time
.bd to
time ] 
.br
.if n .ti +5
.if t .ti +0.5i
[
.bd on
day
.bd to
day ] [
.bd where
qual ]
.s3
.i0
.in +4
.ti -4
PRINT(QUEL) \- print relation(s)
.br
.bd print 
relname {, relname}
.s3
.i0
.in +4
.ti -4
QUEL(QUEL) \- \fBQUE\fPry \fBL\fPanguage for \*(II
.br
.s3
.i0
.in +4
.ti -4
RANGE(QUEL) \- declare a variable to range over a relation
.br
.bd "range of"
variable 
.bd is
relname
.s3
.i0
.in +4
.ti -4
REPLACE(QUEL) \- replace values of domains in a relation
.br
.bd replace
tuple_variable (target_list)
[\c
.bd where
qual]
.s3
.i0
.in +4
.ti -4
RETRIEVE(QUEL) \- retrieve tuples from a relation
.br
.bd retrieve 
[[\c
.bd into\c
] relname] (target_list) 
[\c
.bd where 
qual]
.br 
.bd "retrieve unique" 
(target_list) [\c
.bd where
qual]
.s3
.i0
.in +4
.ti -4
SAVE(QUEL) \- save a relation until a date.
.br
.bd save
relname 
.bd until
month day year
.s3
.i0
.in +4
.ti -4
USE(QUEL) \- specify a group of delimitors to be used
.br
.bd use
groupname
.br
.bd unuse
groupname
.br
.s3
.i0
.in +4
.ti -4
VIEW(QUEL) \- define a virtual relation
.br
.bd define
.bd view
name (target-list) [
.bd where
qual ]
.s3
.i0
.in +4
.ti -4
COPYDB(UNIX) \- create batch files to copy out a data base and restore it.
.br
.bd copydb
[
.bd \-u\c
.it name
] database full-path-name-of-directory
[ relation ... ]
.s3
.i0
.in +4
.ti -4
CREATDB(UNIX) \- create a data base
.br
.bd creatdb
[
.bd \-u\c
.it name
] [
.bd \-e
] [
.bd \-m
] [
.bd \*(+-c
] [
.bd \*(+-q
] dbname
.s3
.i0
.in +4
.ti -4
DESTROYDB(UNIX) \- destroy an existing database
.br
.bd destroydb
[
.bd \-s
] [
.bd \-m
] dbname
.s3
.i0
.in +4
.ti -4
EQUEL(UNIX) \- Embedded QUEL interface to C
.br
.bd equel 
[
.bd \-d
] [
.bd \-f
] [ 
.bd \-r
] file.q ...
.s3
.i0
.in +4
.ti -4
GEO-QUEL(UNIX) \- \*(GQ data display system
.br
.bd geoquel
[
.bd \-s
] [
.bd \-d
] [
.bd \-a
] [
.bd \-t\c
.it T
] [ 
.bd \-tn\c
.it T
] dbname
.s3
.i0
.in +4
.ti -4
HELPR(UNIX) \- get information about a database.
.br
.bd helpr
[
.bd \-u\c
.it name
] [
.bd \*(+-w
] database relation ...
.s3
.i0
.in +4
.ti -4
INGRES(UNIX) \- \*(II relational data base management system
.br
.bd ingres
[
.it flags
] dbname [ process_table ]
.s3
.i0
.in +4
.ti -4
PRINTR(UNIX) \- print relations
.br
.bd printr
[
.it flags
] database relation ...
.s3
.i0
.in +4
.ti -4
PURGE(UNIX) \- destroy all expired and temporary relations
.br
.bd purge
[
.bd \-f
] [
.bd \-p
] [
.bd \-a
] [
.bd \-s
] [
.bd \*(+-w
] [ database ... ]
.s3
.i0
.in +4
.ti -4
RESTORE(UNIX) \- recover from an \*(II or \*(UU crash.
.br
.bd restore
[
.bd \-a
] [
.bd \-s
] [
.bd \*(+-w
] [ database ... ]
.s3
.i0
.in +4
.ti -4
SYSMOD(UNIX) \- modify system relations to predetermined storage structures.
.br
.bd sysmod
[
.bd \-s
] [
.bd \-w
] dbname
[
.bd relation
] [
.bd attribute
] [
.bd indexes
] [
.bd tree
] [
.bd protect
] [
.bd integrities
]
.s3
.i0
.in +4
.ti -4
USERSETUP(UNIX) \- setup users file
.br
.bd \&.../bin/usersetup
[ flags [ pathname ] ]
.s3
.i0
.in +4
.ti -4
TTYTYPE(FILES) \- \*(GQ terminal type database
.br
.s3
.i0
.in +4
.ti -4
INTRODUCTION(ERROR) \- Error messages introduction
.br
.s3
.i0
.in +4
.ti -4
PARSER(ERROR) \- Parser error message summary
.br
Error numbers 2000 \- 2999.
.s3
.i0
.in +4
.ti -4
QRYMOD(ERROR) \- Query Modification error message summary
.br
Error numbers 3000 \- 3999.
.s3
.i0
.in +4
.ti -4
OVQP(ERROR) \- One Variable Query Processor error message summary
.br
Error numbers 4000 \- 4499.
.s3
.i0
.in +4
.ti -4
DECOMP(ERROR) \- Decomposition error message summary
.br
Error numbers 4500 \- 4999.
.s3
.i0
.in +4
.ti -4
DBU(ERROR) \- Data Base Utility error message summary
.br
Error numbers 5000 \- 5999
