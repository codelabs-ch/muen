#!/bin/bash
#
# Start or stop target host given as first argument using the egctl
# tool [1]
#
# [1] - https://github.com/unterwulf/egctl

set -e -u -o pipefail

DEVICE=$1
ACTION=$2
EGCTL=egctl
ONSOCKET="on left left left"
OFFSOCKET="off left left left"

if [ "$#" -ne 2 ]; then
	echo "$0 <devicename> <start|shutdown>"
	exit 2
fi

SCRIPTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source $SCRIPTDIR/args.sh

case $ACTION in
	start)
		$EGCTL $DEVICE $ONSOCKET
		;;
	shutdown)
		$EGCTL $DEVICE $OFFSOCKET
		;;
esac
