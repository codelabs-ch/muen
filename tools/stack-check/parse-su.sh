#!/bin/bash
# parse .su files so we know the binary names of functions and procedures
find . -name '*.su' | while read file; do
	obj=`dirname $file`/`basename $file .su`.o
	cut -d: -f4 $file |cut -f1 |while read func; do
		F=`objdump -d $obj |grep -i "<.*__$func>:$" |sed "s,.*<\(.*\)>:$,\1,"`
		if [ -z "$F" ]; then
			# TODO: check for Pragma Export, which can unduly rename things
			filenames=`grep "[0-9]*:[0-9]*:$func" $file |cut -d: -f1 |sed "s,.adb,.ad*,"`
			F2=`grep -i "pragma.*export.*(.*C.*,.*$func.*,.*\".*\".*);" $filenames 2>/dev/null | sed 's,.*\"\(.*\)".*);,\1,'`
			if [ -n "$F2" ]; then
				F=$F2
			else
				F=$func
			fi
		fi
		grep "$func" $file | sed "s,^.*$func,$F,"
	done
done
