#!/bin/bash
#
# Start/restart or stop target host given as first argument using the amtc
# tool [1]
#
# [1] - https://github.com/schnoddelbotz/amtc

set -e -u -o pipefail

TARGET_HOST=$1
ACTION=$2
AMTC=amtc
AMTC_OPTS=""

if [ "$#" -ne 2 ]; then
	echo "$0 <target_host> <start|shutdown>"
	exit 2
fi

SCRIPTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source $SCRIPTDIR/args.sh

# Get target power state
#
# Returns the HTTP result code of the executed AMT command in the first
# argument and the actual target power state in the second argument. The
# returned target power state is only valid if the HTTP status code is 200.
get_target_state()
{
	local _http_status=$1
	local _target_state=$2

	res=`$AMTC $AMTC_OPTS $TARGET_HOST`
	local exit_code=$?
	if [ $exit_code -ne 0 ]; then
		echo "Command '$AMTC $TARGET_HOST' failed with status $exit_code"
		if [ $exit_code -eq 3 ]; then
			echo "AMT_PASSWORD not set"
		fi
		exit 1
	fi

	eval $_http_status=`echo $res | awk '{print $5}' | cut -d: -f2`
	eval $_target_state=`echo $res | awk '{print $7}'`
}

get_target_state http_status target_state
while [ "$http_status" != "200" ]; do
	case $http_status in
		000)
			echo "Unable to connect to host '$TARGET_HOST'"
			exit 1
			;;
		404)
			echo "SOAP connect to '$TARGET_HOST' failed, retrying with WSMAN"
			AMTC_OPTS="$AMTC_OPTS -d"
			get_target_state http_status target_state
			;;
		*)
			echo "Unknown HTTP status '$http_status'"
			exit 1
	esac
done

# shutdown
if [ "$ACTION" == "shutdown" ]; then
	$AMTC $AMTC_OPTS -D $TARGET_HOST
	exit $?
fi

# start/restart
case $target_state in
	S0)
		$AMTC $AMTC_OPTS -R $TARGET_HOST
		;;

	S5)
		$AMTC $AMTC_OPTS -U $TARGET_HOST
		;;
	*)
		echo "Unknown target state '$target_state'"
		exit 1
esac
