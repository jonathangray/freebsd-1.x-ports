#ifndef INGRES_ERRORS_H_
#define INGRES_ERRORS_H_

#include <sccs.h>
/*
**
**	@(#)errors.h	1.2	2/8/85
*/
#define	QBUFOVFLO	100	/*Qbuf overflow: query too large*/




#define	NUMINTOCHAR	1000	/*In domain %0 numeric retrieved into char field.*/
#define	NUMOVFLO	1001	/*Numeric overflow during retrieve on domain %0.*/
#define	CHARINTONUM	1002	/*In domain %0, character retrieved into numeric variable.*/
#define	BADRET		1003	/*Bad type in target list of parametrized retrieve "%0".*/
#define	BADSTMT		1004	/*Bad type in target list of parametrized statement "%0".*/




#define	SUMMARY		2000	/*%0 errors were found in quel program*/
#define	NOATTRIN	2100	/*line %0, Attribute '%1' not in relation '%2'*/
#define	RESTYPE		2103	/*line %0, Result domain type does not match type of expression*/
#define	RVOPTYPE	2105	/*line %0, Relation valued operators only defined for relations*/
#define	DBUOFLO		2106	/*line %0, Data base utility command buffer overflow*/
#define	CANTUPDATE	2107	/*line %0, You are not allowed to update this relation: %1*/
#define	RESAPPEX	2108	/*line %0, Result relation (%1) for APPEND does not exist or cannot be accessed by you*/
#define	NOVBLE		2109	/*line %0, Variable '%1' not declared in RANGE statement*/
#define	TOOMANYDOTS	2110	/*line %0, Too many dots in multi-dot attribute*/
#define	INDEXTRA	2111	/*line %0, Too many attributes in key for INDEX*/
#define	RELCTYPE	2112	/*line %0, Invalid types for relation comparison*/
#define	EMPTYTYPE	2113	/*line %0, Type of argument to empty() not relations*/
#define	UORFTYPE	2114	/*line %0, Argument to unary or functional operator can't be RELATION*/
#define	NOINDEX		2115	/*line %0, Pattern matching index must be given following the '##'*/
#define	UNDEFCOP	2116	/*line %0, Undefined relation identifier or constant operator '%1'*/
#define	RNGEXIST	2117	/*line %0, Invalid relation name '%1' in RANGE statement*/
#define	TREEOFLO	2118	/*line %0, Out of space in query tree - Query too long*/
#define	MODTYPE		2119	/*line %0, MOD operator not defined for floating point or character attributes*/
#define	NOPATMAT	2120	/*line %0, most pattern match operators not allowed in the target list*/
#define	CONCATTYPE	2121	/*line %0, Only character type domains are allowed in CONCAT operator*/
#define	NOQUALINDX	2122	/*line %0, Pattern matching index mentioned in target but not qualifier*/
#define	REPALL		2123	/*line %0, '%1.all' not defined for replace*/
#define	DUPINDEX	2124	/*line %0, Pattern matching index repeated in qualifier*/
#define	AVGTYPE		2125	/*line %0, Cannot use aggregates ("avg" or "avgu") on character values*/
#define	SUMTYPE		2126	/*line %0, Cannot use aggregates ("sum" or "sumu") on character values*/
#define	FOPTYPE		2127	/*line %0, Cannot use numerical functions (ATAN, COS, LOG, SIN, SQRT, EXP, ABS) on character values*/
#define	UOPTYPE		2128	/*line %0, Cannot use unary operators ("+" or "-") on character values*/
#define	NUMTYPE		2129	/*line %0, Numeric operations (+ - * /) not allowed on character values*/
#define	RESXTRA		2130	/*line %0, Too many result domains in target list**/
#define	TARGXTRA	2131	/*line %0, Target list too wide*/
#define	AGGXTRA		2132	/*line %0, Too many aggregates in this query*/
#define	RELTYPE		2133	/*line %0, Type conflict on relative operator*/
#define	BADCONSTOP	2134	/*line %0, '%1' is not a constant operator.  Only 'dba' or 'usercode' are allowed.*/
#define	RESEXIST	2135	/*line %0, You cannot duplicate the name of an existing relation(%1)*/
#define	BADHOURS	2136	/*line %0, There is no such hour as %1, use a 24 hour clock system*/
#define	BADMINS		2137	/*line %0, There is no such minute as %1, use a 24 hour clock system*/
#define	BAD24TIME	2138	/*line %0, There is no such time as 24:%1, use a 24 hour clock system*/
#define	NOQRYMOD	2139	/*line %0, Your database does not support query modification*/

/*2140	line %0, You may not call define query from inside define query~
2141	line %0, Query %1 has not been defined~
2142	line %0, No space left to define a query~
2143	line %0, You can not use a $ if you are not defining a query~
2144	line %0, There are no more variables left in the this defined query~
2145	line %0, There are undefined variables in this defined query~
2146	line %0, You can not have aggregates in a defined query~*/

#define	NOTSTAR		2147	/*line %0, Only '*' operator can appear immediately after command name*/
#define	BADRELTL	2148	/*line %0, Target lists containing reln. expressions can have only 1 element*/
#define	NORELEXP	2149	/*line %0, Relation expressions not allowed in target list of this cmd.*/
#define	NXTCMDERR	2500	/*line %0, The word '%1', cannot follow this command*/

/*2501	line %0, The word '%1', cannot follow a RETRIEVE command.~
2502	line %0, The word '%1', cannot follow an APPEND command.~
2503	line %0, The word '%1', cannot follow a REPLACE command.~
2504	line %0, The word '%1', cannot follow a DELETE command.~
2507	line %0, The word '%1', cannot follow a DESTROY command.~
2508	line %0, The word '%1', cannot follow a HELP command.~
2510	line %0, The word '%1', cannot follow a MODIFY command.~
2511	line %0, The word '%1', cannot follow a PRINT command.~
2515	line %0, The word '%1', cannot follow a RETRIEVE UNIQUE command.~
2516	line %0, The word '%1', cannot follow a DEFINE VIEW command.~
2519	line %0, The word '%1', cannot follow a HELP VIEW, HELP INTEGRITY,
or HELP PERMIT command.~
2522	line %0, The word '%1', cannot follow a DEFINE PERMIT command.~
2523	line %0, The word '%1', cannot follow a DEFINE INTEGRITY command.~
2526	line %0, The word '%1', cannot follow a DESTROY INTEGRITY or
DESTROY PERMIT command.~
2528	line %0, The word '%1', cannot follow a DEFINE QUERY command.~
*/
#define	SYMERR		2600	/*syntax error on line %0 last symbol read was: %1*/
/*
2601	line %0, Syntax error on '%1', the correct syntax is: 
	RETRIEVE [[INTO]relname] (target_list) [WHERE qual]
	RETRIEVE UNIQUE (target_list) [WHERE qual]~
2602	line %0, Syntax error on '%1', the correct syntax is: 
	APPEND [TO] relname (target_list) [WHERE qual]~
2603	line %0, Syntax error on '%1', the correct syntax is: 
	REPLACE tuple_variable (target_list) [WHERE qual]~
2604	line %0, Syntax error on '%1', the correct syntax is: 
	DELETE tuple_variable [WHERE qual]~
2605	line %0, Syntax error on '%1', the correct syntax is: 
	COPY relname (domname = format {, domname = format}) direction "filename"~
2606	line %0, Syntax error on '%1', the correct syntax is: 
	CREATE relname (domname1 = format{, domname2 = format})~
2607	line %0, Syntax error on '%1', the correct syntax is: 
	DESTROY relname {,relname}
	DESTROY [PERMIT | INTEGRITY] relname [integer{, integer} | ALL]~
2609	line %0, Syntax error on '%1', the correct syntax is: 
	INDEX ON relname IS indexname (domain1{, domain2}) ~
2610	line %0, Syntax error on '%1', the correct syntax is: 
	MODIFY relname TO storage-structure [ON key1 [: sortorder] [{, key2 [:sortorder]}]]
	[WHERE [FILLFACTOR = n] [, MINPAGES = n] [, MAXPAGES = n] [, LID = name]]~
2611	line %0, Syntax error on '%1', the correct syntax is: 
	PRINT relname{, relname}~
2612	line %0, Syntax error on '%1', the correct syntax is: 
	RANGE OF variable IS relname~
2613	line %0, Syntax error on '%1', the correct syntax is: 
	SAVE relname UNTIL month day year~
2614	line %0, Syntax error on '%1', the correct syntax is: 
	DEFINE VIEW name (target list) [WHERE qual]
	DEFINE PERMIT oplist {ON|OF|TO} var [(attlist]
		TO name [AT term] [FROM time TO time]
		[ON day TO day] [WHERE qual]
	DEFINE INTEGRITY ON var IS qual~
2615	line %0, Syntax error on '%1', the correct syntax is: 
	RETRIEVE UNIQUE (target_list) [WHERE qual]~
2616	line %0, Syntax error on '%1', the correct syntax is:
	DEFINE VIEW name (target_list) [WHERE qual]~
2619	line %0, Syntax error on '%1', the correct syntax is:
	HELP VIEW relname[, relname]
	HELP PERMIT relname[, relname]
	HELP INTEGRITY relname[, relname]~
2622	line %0, Syntax error on '%1', the correct syntax is:
	DEFINE PERMIT oplist {ON|OF|TO} var [(attlist)]
		TO name [AT term] [FROM time TO time]
		[ON day TO day] [WHERE qual]~
2623	line %0, Syntax error on '%1', the correct syntax is:
	DEFINE INTEGRITY ON var IS qual~
2626	line %0, Syntax error on '%1', the correct syntax is:
	DESTROY INTEGRITY relname [ integer { , integer } | all ]~
2628	line %0, Syntax error on '%1', the correct syntax is: 
	EXEC (target_list) [WHERE qual]~
*/

#define	STRTERM		2700	/*line %0, non-terminated string*/
#define	STRLONG		2701	/*line %0, string too long*/
#define	BADOP		2702	/*line %0, invalid operator*/
#define	NAMELONG	2703	/*line %0, Name too long '%1'*/
#define	SBUFOFLO	2704	/*line %0, Out of space in symbol table - Query too long*/
#define	COMMTERM	2705	/*line %0, non-terminated comment*/
#define	FCONSTERR	2707	/*line %0, bad floating constant: %1*/
#define	CNTRLCHR	2708	/*line %0, control character passed in pre-converted string*/
#define	NUMBUFOFLO	2709	/*line %0, buffer overflow in converting a number*/
#define	BADBNF		2710	/*line %0, illegal BNF string*/
#define	NOGRP		2711	/*line %0, may not unuse a group which has never been used*/
#define	DELEXIST	2712	/*line %0, group of delims does not exist*/
#define	YOVRFLOW	2800	/*line %0, yacc stack overflow in parsing query*/





#define	TOOMANYVARS	3100	/*Too many variables on relation '%0'*/
#define	NOUPDATEDOM	3310	/*%0 on view %1: cannot update some domain*/

/*3320	%0 on view %1: domain occurs in qualification of view~*/

#define	MOREQUERY	3330	/*%0 on view %1: update would result in more than one query*/
#define	VIEWTIDS	3340	/*%0 on view %1: views do not have TID's*/
#define	NOUPDATEAGG	3350	/*%0 on view %1: cannot update an aggregate value*/
#define	NONFUNCUPDATE	3360	/*%0 on view %1: that update might be non-functional*/
#define	NOAGGINT	3490	/*INTEGRITY on %1: cannot handle aggregates yet*/
#define	NOMULTIVAR	3491	/*INTEGRITY on %1: cannot handle multivariable constraints*/
#define	INITCONST	3492	/*INTEGRITY on %1: constraint does not initially hold*/
#define	INTVIEW		3493	/*INTEGRITY: %1 is a view*/
#define	MUSTOWN		3494	/*INTEGRITY: You must own '%1'*/
#define	PVIOL		3500	/*%0 on relation %1: protection violation*/
#define	BADTERM		3590	/*PERMIT: bad terminal identifier "%2"*/
#define	BADUSRNAME	3591	/*PERMIT: bad user name "%2"*/
#define	OWNEDNOT	3592	/*PERMIT: Relation '%1' not owned by you*/
#define	NOTREALREL	3593	/*PERMIT: Relation '%1' must be a real relation (not a view)*/
#define	BADDOW		3594	/*PERMIT on %1: bad day-of-week '%2'*/
#define	NOTDBA		3595	/*PERMIT on %1: only the DBA can use the PERMIT statement*/
/*3700	Tree buffer overflow in query modification~*/
#define	STACKFULL	3701	/*Tree build stack overflow in query modification*/





#define	LISTFULL	4100	/*ovqp query list overflowed*/
#define	BADCONV		4101
#define	BADUOPC		4102
#define	BADMIX		4103	/*Type clash on characters and int/floats*/
#define	BADSUMC		4104
#define	BADAVG		4105
#define	STACKOVER	4106	/*the interpreters stack overflowed -- query too long*/
#define	CBUFULL		4107	/*the buffer for ASCII and CONCAT commands overflowed*/
#define	BADCHAR		4108	/*cannot use arithmetic operators on two character fields*/
#define	NUMERIC		4109	/*cannot use numeric values with CONCAT operator*/
#define	FLOATEXCEP	4110	/*floating point exception occured.*/
#define	CHARCONVERT	4111	/*character value cannot be converted to numeric due to incorrect syntax.*/
#define	NODOVFLOW	4112	/*ovqp query vector overflowed*/
#define	COMPNOSP	4113	/*compiler text space ran out.*/
#define	COMPNOREGS	4114	/*compiler ran out of registers.*/
#define	BADEXECTYPE	4115	/*only character string data may be executed*/
#define	BADDELIM	4116	/*that delimitor has not been defined*/
#define	BADSECINDX	4199	/*you must convert your 6.0 secondary index before running this query!*/
#define	NODESCAG	4602	/*query involves too many relations to create aggregate function intermediate result.*/
#define	QBUFFULL	4610	/*Query too long for available buffer space (qbufsize)*/
/*4611	Query too long for available buffer space (varbufsiz)*/
#define	SQBUFFULL	4612	/*Query too long for available buffer space (sqsiz)*/
/*4613	Query too long for available buffer space (stacksiz)*/
#define	AGBUFFULL	4614	/*Query too long for available buffer space (agbufsiz)*/
#define	AGFTOBIG	4615	/*Aggregate function is too wide or has too many domains.*/
#define	TOOMANYAGGS	4616	/*Too many aggregates*/
#define	RETUTOBIG	4620	/*Target list for "retrieve unique" has more than 49 domains or is wider than 498 bytes.*/





#define	BADRELNAME	5001	/*PRINT: bad relation name %0*/
#define	NOPRINTVIEW	5002	/*PRINT: %0 is a view and can't be printed*/
#define	PROTVIOL	5003	/*PRINT: Relation %0 is protected.*/
#define	DUPRELNAME	5102	/*CREATE: duplicate relation name %0*/
#define	SYSRELNAME	5103	/*CREATE: %0 is a system relation*/
#define	BADATTRNAME	5104	/*CREATE %0: invalid attribute name %1*/
#define	DUPATTRNAME	5105	/*CREATE %0: duplicate attribute name %1*/
#define	BADATTRFORMAT	5106	/*CREATE %0: invalid attribute format "%2" on attribute %1*/
#define	TOOMANYDOMS	5107	/*CREATE %0: excessive domain count on attribute %1*/
#define	RELTOOWIDE	5108	/*CREATE %0: excessive relation width on attribute %1*/
#define	NODESTSYSREL	5201	/*DESTROY: %0 is a system relation*/
#define	RELNOEXIST	5202	/*DESTROY: %0 does not exist or is not owned by you*/
#define	BADINTEG	5203	/*DESTROY: %0 is an invalid integrity constraint identifier*/
#define	BADPROT		5204	/*DESTROY: %0 is an invalid protection constraint identifier*/
#define	NOPRIMREL	5300	/*INDEX: cannot find primary relation*/
#define	TOOMUCHDOMS	5301	/*INDEX: more than maximum number of domains*/
#define	NODOM		5302	/*INDEX: invalid domain %0*/
#define	NOTOWNED	5303	/*INDEX: relation %0 not owned by you*/
#define	ALREADYINDX	5304	/*INDEX: relation %0 is already an index*/
#define	NOINDXSYSREL	5305	/*INDEX: relation %0 is a system relation*/
#define	NOINDVIEW	5306	/*INDEX: %0 is a view and an index can't be built on it*/
#define	NOINDXLID	5307	/*INDEX: %0: can't index on lid attribute "%1"*/
#define	DISPERRBASE	5400	/* base for error messages in display()*/
#define	NORELEXIST	5401	/*HELP: relation %0 does not exist*/
#define	NOMANSEC	5402	/*HELP: cannot find manual section "%0"*/
#define	NOTVIEW		5403	/*HELP: relation %0 is not a view*/
#define	NOPERMS		5404	/*HELP: relation %0 has no permissions on it granted*/
#define	NOINTEG		5405	/*HELP: relation %0 has no integrity constraints on it*/
#define	TBUFOVFLO	5410	/*HELP: tree buffer overflowed*/
#define	TSTACKOVFLO	5411	/*HELP: tree stack overflowed*/
#define	RDELIMERR	5412	/*HELP: can't read rdelim*/
#define	NOREL		5500	/*MODIFY: relation %0 does not exist*/
#define	NOOWN		5501	/*MODIFY: you do not own relation %0*/
#define	NOKEYSHEAP	5502	/*MODIFY %0: you may not provide keys on a heap*/
/*5503	MODIFY %0: too many keys provided~ */
#define	NOMODSYSREL	5504	/*MODIFY %0: cannot modify system relation*/
#define	NOORDINDX	5505	/*MODIFY %0: cannot order a relation that is an index*/
#define	TOOMANYORDKEYS	5506	/*MODIFY %0: provided too many ordering keys*/
#define	DUPKEY		5507	/*MODIFY %0: duplicate key "%1"*/
#define	TOOWIDEISAM	5508	/*MODIFY %0: key width (%1) too large for isam*/
#define	ATTRREMV	5509	/*MODIFY %0: attribute "%1" will be removed during modification, can't be used*/
#define	BADSTORAGE	5510	/*MODIFY %0: bad storage structure "%1"*/
#define	NOTALLOWED	5512	/*MODIFY %0: "%1" not allowed or specified more than once*/
#define	FILLBOUND	5513	/*MODIFY %0: fillfactor value %1 out of bounds*/
#define	MINPGBOUND	5514	/*MODIFY %0: minpages value %1 out of bounds*/
#define	NEEDFILL	5515	/*MODIFY %0: "%1" should be "fillfactor", "maxpages", "minpages" or "lidn" where n <= ordering  dimension*/
#define	MAXPGBOUND	5516	/*MODIFY %0: maxpages value %1 out of bounds*/
#define	MINGTMAX	5517	/*MODIFY %0: minpages value exceeds maxpages value*/
#define	INVALIDSEQ	5518	/*MODIFY %0: invalid sequence specifier "%1" for domain %2.*/
#define	NOMODVIEW	5519	/*MODIFY: %0 is a view and can't be modified*/
#define	BADSEQSPEC	5520	/*MODIFY: %0: sequence specifier "%1" on domain %2 is not allowed with the specified storage structure.*/
#define	INVALIDATTR	5521	/*MODIFY: %0: invalid attribute name %1*/

/*5522	MODIFY: %0 is an ordered relation with only one attribute~*/

#define	BADORDDIM	5523	/*MODIFY %0: Bad ordering dimension %1*/
#define	NOSAVESYSREL	5600	/*SAVE: cannot save system relation "%0"*/
#define	BADMONTH	5601	/*SAVE: bad month "%0"*/
#define	BADDAY		5602	/*SAVE: bad day "%0"*/
#define	BADYEAR		5603	/*SAVE: bad year "%0"*/
#define	RELNOTFOUND	5604	/*SAVE: relation %0 does not exist or is not owned by you*/
#define	BADLID1		5701	/*UPDATE: %0: bad lid value
					**	Lid1 = %1
					*/
#define	BADLID2		5702	/*UPDATE: %0: bad lid value
					**	Lid1 = %1
					**	Lid2 = %2
					*/
#define	BADLID3		5703	/*UPDATE: %0: bad lid value
					**	Lid1 = %1
					**	Lid2 = %2
					**	Lid3 = %3
					*/
#define	NOEXIST		5800	/*COPY: relation %0 doesn't exist*/
#define	ATTRNOEXIST	5801	/*COPY: attribute %0 in relation %1 doesn't exist or it has been listed twice.*/
#define	TOOMANYATTR	5803	/*COPY: too many attributes*/
#define	BADATTRLEN	5804	/*COPY: bad length for attribute %0.  Length="%1"*/
#define	NOFILEOPN	5805	/*COPY: can't open file %0*/
#define	NOFILECRT	5806	/*COPY: can't create file %0*/
#define	UNRECDUMMY	5807	/*COPY: unrecognizable dummy domain "%0"*/
#define	DOMTOOSMALL	5808	/*COPY: domain %0 size too small for conversion.	There were %2 tuples successfully copied from %3 into %4*/
#define	BADINPUT	5809	/*COPY: bad input string for domain %0. Input was "%1".	There were %2 tuples successfully copyied from %3 into %4*/
#define	UNEXEOF		5810	/*COPY: unexpected end of file while filling domain %0.	There were %1 tuples successfully copyied from %2 into %3*/
#define	BADATTRTYPE	5811	/*COPY: bad type for attribute %0. Type="%1"*/
#define	DESTINDEX	5812	/*COPY: The relation "%0" has a secondary index. The index(es) must be destroyed before doing a copy "from".*/
#define	NOUPDT	5813	/*COPY: You are not allowed to update the relation %0.*/
#define	NOTOWNER	5814	/* COPY: You do not own the relation %0.*/
#define	UNDETC0		5815	/*COPY: An unterminated "c0" field occured while filling domain %0. There were %1 tuples successfully copyied from %2 into %3*/
#define	FULLPATH	5816	/*COPY: The full pathname must be specified for the file %0*/
#define	FILETOOWIDE	5817	/*COPY: The maximum width of the output file cannot exceed 1024 bytes per tuple.*/
#define	NOCPVIEW	5818	/*COPY: %0 is a view and can't be copied*/
#define	DUPTUPS		5819	/*COPY: Warning: %0 duplicate tuples were ignored.*/
#define	BADDOMS		5820	/*COPY: Warning: %0 domains had control characters which were converted to blanks.*/
#define	TRUNCCHARS	5821	/*COPY: Warning: %0 c0 character domains were truncated.*/
#define	RELPROTECT	5822	/*COPY: Relation %0 is protected.*/




/*6110	CREATE GLOBAL: Relation %0 already exists at site %1~
6600	Aggregates on distributed relations not yet implemented.~
6601	Multi-variable distributed queries not yet implemented.~
*/

#endif /* !INGRES_ERRORS_H_ */
