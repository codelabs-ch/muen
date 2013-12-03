#!/bin/bash
# $1: ELF image to check

if [ -z "$1" ]; then
	echo "usage: $0 ELF-File"
	exit 1
fi

declare -A stackdepth
declare -A deep_stackdepth
declare -A full_stackdepth
while read func size type; do
  if [ "$type" != "static" ]; then
    echo "non-static stack size"
    exit 1
  fi
  stackdepth[$func]=$size
  deep_stackdepth[$func]=0
  full_stackdepth[$func]=$size
done < <(`dirname $0`/parse-su.sh)

declare -A recursors
declare -A f
while read item1 item2; do
	if [ "$item1" = ".." ]; then
		if [ "$caller" = "$item2" ]; then
			if [ "${recursors[$caller]}" != 1 ]; then
				recursors[$caller]=1
				echo potential recursion in $caller >&2
			fi
			continue
		fi
#		echo $caller calls $item2
		f_callees[$item2]=1
	else
		eval f_$caller=\""${!f_callees[@]}"\"
		caller=$item1
		unset f_callees
		declare -A f_callees
		f[$caller]=1
	fi
done < <(`dirname $0`/extract-callgraph.sh $1)

old_num=-1
deepest=0
while [ "${#f[@]}" -gt 0 ]; do
	if [ "${#f[@]}" = $old_num ]; then
		echo "couldn't reduce the call graph anymore. real recursion :-("
		exit 1
	fi
	old_num=${#f[@]}
	for func in "${!f[@]}"; do
		eval callees=\$f_$func
		if [ -z "$callees" ]; then
			full_stackdepth[$func]=$((${stackdepth[$func]:=0} + ${deep_stackdepth[$func]:=0}))
			printf "$func: stack %d + %d -> %d\n" ${stackdepth[$func]:=0} ${deep_stackdepth[$func]} ${full_stackdepth[$func]}
			if [ ${full_stackdepth[$func]} -gt $deepest ]; then
				deepest=${full_stackdepth[$func]}
			fi
			unset f[$func]
		else
			tmp=""
			for callee in $callees; do
				eval c=\$f_$callee
				if [ -z "$c" ]; then
					if [ ${full_stackdepth[$callee]:=0} -gt ${deep_stackdepth[$func]:=0} ]; then
#						echo "updating deep stack depth of $func to ${full_stackdepth[$callee]} due to $callee"
						deep_stackdepth[$func]=${full_stackdepth[$callee]}
					fi
				else
					tmp="$tmp $callee"
				fi
			done
			eval f_$func=\"$tmp\"
		fi
	done
done

echo
echo "deepest stack size: $deepest (not accounting for stack modifications in assembler)"
