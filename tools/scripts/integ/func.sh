log()
{
	echo $1 | tee -a $LOGFILE
}

execute()
{
	cmd=$1
	log "Executing command '$cmd'"
	$cmd >> $LOGFILE 2>&1
	status=$?
	if [ $status -ne 0 ]; then
		log "! Command '$cmd' failed (status $status)"
		failed
	fi
}

__send_mail()
{
	status=$1
	subject=$2
	message=$3

	for mail in $RECIPIENTS
	do
		echo -e "To: $mail\nFrom: $SENDER\nSubject: $subject\n\n$message" | /usr/sbin/sendmail -t -F "$SENDER" -f "$SENDER"
	done
}
