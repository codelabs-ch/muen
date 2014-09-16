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

send_mail()
{
	status=$1
	message=$2

	for mail in $RECIPIENTS
	do
		echo -e "To: $mail\nFrom: $SENDER\nSubject: [muen-integration] $status: B$BUILDID, $FBRANCH, $COMMIT\n\n$message" | /usr/sbin/sendmail -t -F "$SENDER" -f "$SENDER"
	done
}
