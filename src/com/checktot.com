$! GXCOM:CHECKTOT.COM
$! 
$!
$! V02 04-JAN-96 WXM Report total checksum to the console
$! V01 15-AUG-91 GGL Initial release
$!
$!
$!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$! This item is the property of GTECH Corporation, Providence, Rhode
$! Island, and contains confidential and trade secret information. It
$! may not be transferred from the custody or control of GTECH except
$! as authorized in writing by an officer of GTECH. Neither this item
$! nor the information it contains may be used, transferred,
$! reproduced, published, or disclosed, in whole or in part, and
$! directly or indirectly, except as expressly authorized by an
$! officer of GTECH, pursuant to written agreement.
$!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$!
$!--------------------------------------------------------------
$!  This will obtain a total checksum of ALL .EXE and .ESH files
$!  and display it on the console (SYS$OUTPUT)
$!--------------------------------------------------------------
$!
$	set noon
$	say := write sys$output
$	totcheck = 0
$	count = 0 
$	countt = 0 
$	countc = 0 
$!	
$!	
$	file_type = "EXE"
$	sum_type  = "/IMAGE"
$!
$LOOP:
$!
$	file = f$search("gxtsk:*.''file_type'",1)
$	if file .eqs. "" 
$	then
$		if file_type .eqs. "ESH" 
$		then
$			goto END
$		else
$			file_type = "ESH"
$			sum_type  = " "
$			countt = countc
$			countc = 0
$			goto LOOP
$		endif
$	endif
$!
$	count  = count  + 1
$	countc = countc + 1
$!
$! turn off output since CHECKSUM outputs too much stuff
$!
$	define sys$output NLA0:
$	checksum'sum_type' 'file'
$	deassign sys$output
$!
$	numcheck = f$integer(''checksum$checksum')
$	totcheck = totcheck + numcheck
$	goto LOOP
$!
$END:
$!
$	say ""
$	say f$fao("!4UW!AS!4UW!AS!AS!XL",-
				countt," EXE files   ",-
				countc," ESH files   ",-
				"Total checksum: ",totcheck)
$!
$	opflag = 0
$	if f$search("gxtsk:checkall.fil") .eqs. ""
$	then
$	  goto end2
$	else
$	  open/read/err=end2 chk_file gxtsk:checkall.fil
$	  opflag = 1
$loop2:
$	  read/end=cmp/err=end2 chk_file line
$	  line = f$edit(line,"trim,compress")
$	  filchk = f$extract(1,8,f$element(6," ",line))
$	  goto loop2
$	endif
$cmp:
$	if filchk .eqs. f$fao("!XL",totcheck)
$	then
$	  say "Checksum unchanged"
$	  say f$fao("!AS!AS","Old checksum: ",filchk)
$	else
$	  say "Checksum changed!"
$	  say f$fao("!AS!AS!AS!XL","Old checksum: ",filchk,-
		           "    Current checksum: ",totcheck)
$	endif
$	say ""
$	say "Compare with other Live systems before continuing."
$exit:
$	if opflag then close chk_file
$	say ""
$	inquire ans "Answer Y to continue or N to stop"
$	if .not. ans then stop
$	exit
$end2:
$	say "Error accessing GXTSK:CHECKALL.FIL file"
$	goto exit
