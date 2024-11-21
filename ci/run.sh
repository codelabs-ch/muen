#!/bin/bash

set -euxo pipefail

SCRIPTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

IMAGES=$(realpath $SCRIPTDIR/images)
ROOT=$(realpath $SCRIPTDIR/..)
NCI=$SCRIPTDIR/nci

artifacts_dir=""
force=false
build_only=false

while getopts "a:bf" opt; do
	case $opt in
		a) artifacts_dir="$OPTARG" ;;
		b)
			build_only=true
			force=true
			;;
		f)
			force=true
			;;
		*)
			echo "Usage: $0 [-a artifacts_dir] [-b] [-f]"
			echo "  -b  Build only, selects force"
			echo "  -f  Build images even if image dir already exists"
			exit 1
			;;
	esac
done

# also save our output to artifacts_dir/
[ -n "$artifacts_dir" ] || artifacts_dir=$(mktemp -d /tmp/nci-XXXXXX)
mkdir -p $artifacts_dir

if [ ! -d $IMAGES ] || [ "$force" = true ]; then
	# build and copy all relevant targets first
	declare -a builds
	builds+=("xml/demo_system_vtd.xml;hardware/qemu-kvm.xml;qemu-kvm.iso")
	builds+=("xml/demo_system_vtd.xml;hardware/qemu-kvm-coreboot.xml;qemu-kvm-coreboot.iso")

	mkdir -p $IMAGES
	for build in "${builds[@]}"; do
		system="${build%%;*}"
		remainder="${build#*;}"
		hw="${remainder%%;*}"
		img="${remainder#*;}"
		make -C kernel clean
		make iso -j$(nproc) SYSTEM="$system" HARDWARE="$hw"
		cp emulate/muen.iso $IMAGES/$img
	done
fi

if [ "$build_only" = true ]; then
	exit 0
fi

pushd $NCI

ARGS=""
if [ -n "$artifacts_dir" ]; then
	ARGS+="-a $artifacts_dir"
fi

./nci run $ARGS \
	-c $SCRIPTDIR/nci-config/x86/qemu*.yaml \
	-DIMAGE_DIR=$IMAGES \
	-DCONFIG_DIR=$SCRIPTDIR/nci-config \
	-DMUEN_DIR=$ROOT | tee $artifacts_dir/nci.log

popd
