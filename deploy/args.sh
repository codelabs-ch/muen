declare -a ACTIONS=("start" "shutdown")

case " ${ACTIONS[*]} " in
	*\ $ACTION\ *)
		;;
	*)
		echo "Invalid action '$ACTION'"
		exit 1;
esac
