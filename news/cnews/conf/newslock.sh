lock="$NEWSCTL/LOCKxxx"		# modify name as appropriate
ltemp="$NEWSCTL/L.$$"
echo $$ >$ltemp
trap "rm -f $ltemp ; exit 0" 0 1 2 15
while true
do
	if newslock $ltemp $lock
	then
		trap "rm -f $ltemp $lock ; exit 0" 0 1 2 15
		break
	fi
	sleep 30
done
