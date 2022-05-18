$! OpenVMS_Performace_Management_manuel - 7.1.3 Obtaining Working Set Values
$!
$! WORKING_SET.COM - Command file to display working set information.
$! Requires ’WORLD’ privilege to display information
$! on processes other than your own.
$!
$! the next symbol is used to insert quotes into command strings
$! because of the way DCL processes quotes, you can’t have a
$! trailing comment after the quotes on the next line.
$!
$ quote = """
$!
$ pid = "" ! initialize to blank
$ context = "" ! initialize to blank
$!
$! Define a format control string which will be used with
$! F$FAO to output the information. The width of the
$! string will be set according to the width of the
$! display terminal (the image name is truncated, if needed).
$!
$ IF F$GETDVI ("SYS$OUTPUT", "DEVBUFSIZ") .LE. 80
$ THEN
$ ctrlstring = "!AS!15AS!5AS!5(6SL)!7SL !10AS"
$ ELSE
$ ctrlstring = "!AS!15AS!5AS!5(6SL)!7SL !AS"
$ ENDIF
$!
$! Check to see if this procedure was invoked with the PID of
$! one specific process to check. If it was, use that PID. If
$! not, the procedure will scan for all PIDs where there is
$! sufficient privilege to fetch the information.
$!
$ IF p1 .NES. "" THEN pid = p1
$!
$! write out a header.
$!
$ WRITE sys$output -
" Working Set Information"
$ WRITE sys$output ""
$ WRITE sys$output -
" WS WS WS WS Pages Page"
$ WRITE sys$output -
"Username Processname State Extnt Quota Deflt Size in WS Faults Image"
$ WRITE sys$output ""
$!
$! Begin collecting information.
$!
$ collect_loop:
$!
$ IF P1 .EQS. "" THEN pid = F$PID (context) ! get this process’ PID
$ IF pid .EQS. "" THEN EXIT ! if blank, no more to
$! ! check, or no privilege
$ pid = quote + pid + quote ! enclose in quotes
$!
$ username = F$GETJPI (’pid, "USERNAME") ! retrieve proc. info.
$!
$ IF username .EQS. "" THEN GOTO collect_loop ! if blank, no priv.; try
$! ! next PID
$ processname = F$GETJPI (’pid, "PRCNAM")
$ imagename = F$GETJPI (’pid, "IMAGNAME")
$ imagename = F$PARSE (imagename,,,"NAME") ! separate name from filespec
$ state = F$GETJPI (’pid, "STATE")
$ wsdefault = F$GETJPI (’pid, "DFWSCNT")
$ wsquota = F$GETJPI (’pid, "WSQUOTA")
$ wsextent = F$GETJPI (’pid, "WSEXTENT")
$ wssize = F$GETJPI (’pid, "WSSIZE")
$ globalpages = F$GETJPI (’pid, "GPGCNT")
$ processpages = F$GETJPI (’pid, "PPGCNT")
$ pagefaults = F$GETJPI (’pid, "PAGEFLTS")
$!
$ pages = globalpages + processpages ! add pages together
$!
$! format the information into a text string
$!
$ text = F$FAO (ctrlstring, -
username, processname, state, wsextent, wsquota, wsdefault, wssize, -
pages, pagefaults, imagename)
$!
$ WRITE sys$output text ! display information
$!
$ IF p1 .NES. "" THEN EXIT ! if not invoked for a
$! ! specific PID, we’re done.
$ GOTO collect_loop ! repeat for next PID