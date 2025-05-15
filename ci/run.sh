#!/bin/bash

set -euxo pipefail

SCRIPTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

. $SCRIPTDIR/config

IMAGES=$(realpath $SCRIPTDIR/images)
ROOT=$(realpath $SCRIPTDIR/..)
NCI=$SCRIPTDIR/nci

artifacts_dir=""
force=false
build_only=false
deploy_to_hw=false
nci_defines=()

set_buildvars()
{
	IFS='|' read -ra build_vars <<< "$1"

	local i
	for i in "${build_vars[@]}"
	do
		export "$i"
	done
}

unset_buildvars()
{
	IFS='|' read -ra build_vars <<< "$1"

	local i
	for i in "${build_vars[@]}"
	do
		unset $(echo $i | cut -d\= -f1)
	done
}

while getopts "a:bfd" opt; do
	case $opt in
		a) artifacts_dir="$OPTARG" ;;
		b)
			build_only=true
			force=true
			;;
		d)	deploy_to_hw=true
			;;
		f)
			force=true
			;;
		*)
			echo "Usage: $0 [-a artifacts_dir] [-b] [-f]"
			echo "  -b  Build only, selects force"
			echo "  -f  Build images even if image dir already exists"
			echo "  -d  Also deploy to hardware"
			exit 1
			;;
	esac
done

# also save our output to artifacts_dir/
[ -n "$artifacts_dir" ] || artifacts_dir=$(mktemp -d /tmp/nci-XXXXXX)
mkdir -p $artifacts_dir

declare -a targets
targets+=("${targets_qemu[@]}")
if [ "$deploy_to_hw" = true ]; then
	targets+=("${targets_hw[@]}")
fi

for target in "${targets[@]}"; do
	IFS=';' read -r system hw vars image_dir_var <<< "$target"
	img_base=$(basename "$hw")
	img_name=${img_base%.xml}
	destdir=${IMAGES}/${img_name}
	nci_defines+=( "-D${image_dir_var}=${destdir}" )

	if [ ! -f $destdir/muen.iso ] || [ "$force" = true ]; then
		mkdir -p $destdir

		set_buildvars "$vars"
		make -C kernel clean
		make iso -j$(nproc) SYSTEM="$system" HARDWARE="$hw"
		unset_buildvars "$vars"
		cp emulate/muen.iso $destdir/

		# generate kernel metrics of last target
		if [[ "$target" == "${targets[-1]}" ]]; then
			make -C kernel metrics
		fi
	fi
done


if [ "$build_only" = true ]; then
	exit 0
fi

ARGS=""
if [ -n "$artifacts_dir" ]; then
	ARGS+="-a $artifacts_dir"
fi

nci_defines+=( "-DMULOG_DIR=$ROOT/tools/scripts" )
if [ "$deploy_to_hw" = true ]; then
	configs=$SCRIPTDIR/nci-config/x86/*.yaml
else
	configs=$SCRIPTDIR/nci-config/x86/qemu*.yaml
fi

$NCI/nci run $ARGS \
	-c $configs \
	"${nci_defines[@]}" \
	-DCONFIG_DIR=$SCRIPTDIR/nci-config
