if [ ! $# -eq 3 ]; then
	echo "Usage: $0 <config_file> <commit> <build ID>"
	exit 1
fi

CONFIG=$1
COMMIT=$2
BUILDID=$3

if [ ! -f $CONFIG ]; then
	echo "Unable to read config file '$CONFIG'"
	exit 1
fi

config_header=$(head -n 1 $CONFIG)

if [ "$config_header" != "### muen_integ_config ###" ]; then
	echo "Invalid config file '$CONFIG'"
	exit 1
fi

source $CONFIG
