#!/bin/bash
#
# Start or stop target host using the GUDE Expert Power Control (EPC) NET power
# connector [1]. The target is assumed to be plugged in port 1.
#
# [1] - http://wiki.gude.info/FAQ_EPC_CmdLine#Beispiele

set -e -u -o pipefail

DEVICE=$1
ACTION=$2
WGET="wget --quiet --delete-after"
URL="http://$DEVICE/ov.html?cmd=1&p=1&s="

if [ "$#" -ne 2 ]; then
	echo "$0 <ip> <start|shutdown>"
	exit 2
fi

SCRIPTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source $SCRIPTDIR/args.sh

case $ACTION in
	start)
		$WGET "${URL}1"
		;;
	shutdown)
		$WGET "${URL}0"
		;;
esac
