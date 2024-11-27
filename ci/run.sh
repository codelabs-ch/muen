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

if [ ! -f $IMAGES/.built ] || [ "$force" = true ]; then
	# build and copy all relevant targets first
	declare -a builds
	builds+=("${targets_qemu[@]}")

	if [ "$deploy_to_hw" = true ]; then
		builds+=("${targets_hw[@]}")
	fi

	mkdir -p $IMAGES
	for build in "${builds[@]}"; do
		system="${build%%;*}"
		remainder="${build#*;}"
		hw="${remainder%%;*}"
		img=$(basename "$hw")
		img="${img%.xml}.iso"
		vars="${remainder#*;}"

		set_buildvars "$vars"
		make -C kernel clean
		make iso -j$(nproc) SYSTEM="$system" HARDWARE="$hw"
		unset_buildvars "$vars"
		cp emulate/muen.iso $IMAGES/$img
	done

	# generate kernel metrics (last target)
	make -C kernel metrics
fi

touch $IMAGES/.built

if [ "$build_only" = true ]; then
	exit 0
fi

pushd $NCI

ARGS=""
if [ -n "$artifacts_dir" ]; then
	ARGS+="-a $artifacts_dir"
fi

if [ "$deploy_to_hw" = true ]; then
	configs=$SCRIPTDIR/nci-config/x86/*.yaml
else
	configs=$SCRIPTDIR/nci-config/x86/qemu*.yaml
fi

./nci run $ARGS \
	-c $configs \
	-DIMAGE_DIR=$IMAGES \
	-DCONFIG_DIR=$SCRIPTDIR/nci-config \
	-DMUEN_DIR=$ROOT | tee $artifacts_dir/nci.log

popd
