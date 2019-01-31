print()
{
	echo "$1" >> $OUTPUT
}

date_print()
{
	print ""
	print "$1"
	print "`date`"
	print ""
}

execute()
{
	local cmd=$1
	echo "Executing '$cmd'"
	eval "$cmd"
	if [ $? -ne 0 ]; then
		exit 1
	fi
}
