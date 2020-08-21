$! GXCOM:CHECKALL.COM
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
$!  this will obtain a checksum of ALL .EXE and .ESH files.
$!--------------------------------------------------------------
$!
$	set noon
$	say := write sys$output
$	totcheck = 0
$	count = 0 
$	countt = 0 
$	countc = 0 
$!	
$	if f$search("gxtsk:checkall.fil") .nes. ""
$	then delete gxtsk:checkall.fil;*
$	endif
$!
$	open/write out_file gxtsk:checkall.fil
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
$	time = f$time()
$	text = F$FAO("!20AS!6UW!AS!31AS!AS!XL!AS!XL!AS",-
		TIME,COUNT," ",''file'," = ",NUMCHECK," (",TOTCHECK,")")
$
$	write out_file text
$!
$	goto LOOP
$!
$END:
$!
$	if f$trnlnm("out_file") .nes. "" 
$	then 
$	  close out_file
$	  say f$fao("!4UW!AS!4UW!AS!AS!XL",-
				countt," EXE files   ",-
				countc," ESH files   ",-
				"Total checksum: ",totcheck)
$	endif
$!
