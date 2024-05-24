#!/bin/bash
#
# Start/restart or stop target host via iLO redfish API [1].
#
# [1] - https://servermanagementportal.ext.hpe.com/docs/redfishservices/ilos/ilo5/

set -e

TARGET_HOST=$1
ACTION=$2

if [ "$#" -ne 2 ]; then
	echo "$0 <target_host> <start|shutdown>"
	exit 2
fi

SCRIPTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ILO_ADMIN=administrator
PWD_OPT="-u $ILO_ADMIN:$ILO_PASSWORD --insecure"

source $SCRIPTDIR/args.sh

ilo_action() {
	action=$1
	out=$(curl -s -d "{\"ResetType\":\"$action\"}" \
		-H "Content-Type: application/json" \
		-X POST \
		$REDFISH/Actions/ComputerSystem.Reset/ \
		$PWD_OPT)
	res=$(echo $out | jq -r '.error."@Message.ExtendedInfo"[0].MessageId')
	if [[ $res != *"Success"* ]]; then
		echo "Cmd failed with $res"
		exit 1
	fi
}

REDFISH="https://$TARGET_HOST/redfish/v1/Systems/1"
state=$(curl -s $REDFISH $PWD_OPT | jq -r '.PowerState')

if [ -z $state ]; then
	echo "Unable to acquire power state for host $TARGET_HOST"
	exit 1
fi

case $ACTION in
start)
	if [ "$state" = "On" ]; then
		ilo_action ForceRestart
	else
		ilo_action On
	fi
	;;
shutdown)
	if [ "$state" != "Off" ]; then
		ilo_action ForceOff
	fi
	;;
esac
