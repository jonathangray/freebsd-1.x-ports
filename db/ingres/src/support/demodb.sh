#! /bin/sh
:	"@(#)demodb.sh	8.2	1/28/86"
if test "$1" = "" ; then
	echo no database name specified
	exit
fi
echo creating database $1 -- please wait
# agc - changed the following line, as NetBSD-current couldn't hack it
# changed just to call the creatdb, and not speculate on return code
# I think this is because creatdb execl's sysmod (i.e. ingres), which
# doesn't retrun with a 0 exit code.
#if creatdb $2 $1 ; then echo loading relations ; else exit; fi
creatdb $2 $1
echo loading relations
ingres -s $2 $1 << 'EOF'
	create item (
		number is i2,
		name is c20,
		dept is i2,
		price is i2,
		qoh is i2,
		supplier is i2)

	create sale (
		number is c6,
		date is c8,
		store is i2,
		dept is i2,
		item is i2,
		quantity is i2,
		employee is i2,
		credit is c8)

	create employee (
		number is i2,
		name is c20,
		salary is i2,
		manager is i2,
		birthdate is i2,
		startdate is i2)

	create dept (
		number is i2,
		name is c20,
		store is i2,
		floor is i2,
		manager is i2)

	create supplier (
		number is i2,
		name is c15,
		city is c15,
		state is c6)

	create store (
		number is i2,
		city is c15,
		state is c6)

	create parts (
		pnum is i2,
		pname is c20,
		color is c8,
		weight is i2,
		qoh is i2)

	create supply (
		snum is i2,
		pnum is i2,
		jnum is i2,
		shipdate is c8,
		quan is i2)


	copy item (number is c6,
		name is c20,
		dept is c6,
		price is c6,
		qoh is c6,
		supplier is c6)
	from "{pathname}/demo/item"

	copy sale (
		number is c6,
		date is c8,
		store is c6,
		dept is c6,
		item is c6,
		quantity is c6,
		employee is c6,
		credit is c8)
	from "{pathname}/demo/sale"

	copy employee (
		number is c6,
		name is c20,
		salary is c6,
		manager is c6,
		birthdate is c6,
		startdate is c6)
	from "{pathname}/demo/employee"

	copy dept (
		number is c6,
		name is c20,
		store is c6,
		floor is c6,
		manager is c6)
	from "{pathname}/demo/dept"

	copy supplier (
		number is c6,
		name is c15,
		city is c15,
		state is c6)
	from "{pathname}/demo/supplier"

	copy store (
		number is c6,
		city is c15,
		state is c6)
	from "{pathname}/demo/store"

	copy parts (
		pnum is c6,
		pname is c20,
		color is c8,
		weight is c6,
		qoh is c6)
	from "{pathname}/demo/parts"

	copy supply (
		snum is c6,
		pnum is c6,
		jnum is c6,
		shipdate is c8,
		quan is c6)
	from "{pathname}/demo/supply"

	range of i is item
	define permit all on i to all

	range of s is sale
	define permit all on s to all

	range of e is employee
	define permit all on e to all

	range of d is dept
	define permit all on d to all

	range of s is supplier
	define permit all on s to all

	range of s is store
	define permit all on s to all

	range of p is parts
	define permit all on p to all

	range of s is supply
	define permit all on s to all
	\g
	\q
EOF
echo database $1 created
