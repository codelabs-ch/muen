CONFIG=$1

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
