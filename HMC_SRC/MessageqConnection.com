$!#####################################################################################
$! Command Procedure to show What MessageQ Millennium is attached in COMOLM
$!
$! for debug purposes use dcl: sh log /table=lnm$job
$! to see the key value pair obtained from pipe subprocess value obtain 
$! and passed to this main process by the usage of this table of jobs 
$! in code "define/job messageq_aux &messageq_pb" to define the key value pair writen 
$! to the table
$!
$!#####################################################################################


$ DEFINE SYS$INPUT 'F$TRNLNM("SYS$COMMAND")'
$!#####################################################################################################
$! pipe SH PROCESS SCMLCOMOLM | SEARCH SYS$PIPE "Devices allocated" > DEVICEAUX.DAT
$! WAIT 00:00:01
$! OPEN/READ HRFILE GXOLM:DEVICEAUX.DAT
$! WRITE SYS$OUTPUT "Start Loop"
$!read_loop:
$! read/end_of_file=done_loop HRFILE opt1
$! WRITE SYS$OUTPUT "''opt1'"
$! DEVICE_AUX = F$EDIT(F$ELEMENT(1, ":", "''opt1'"),"TRIM")
$! WRITE SYS$OUTPUT DEVICE_AUX
$! goto read_loop
$!done_loop:
$! WRITE SYS$OUTPUT "Finished..."
$!#####################################################################################################

$ pipe/nosymbol/nological -
show process SCMLCOMOLM | -
search sys$pipe "Devices allocated:" | -
( read sys$pipe messageq_pb ; define/job messageq_aux &messageq_pb )
$ device_info = f$edit(f$trnlnm("messageq_aux","lnm$job"),"collapse") 
$ IF F$LOCATE("NOMATCHES",device_info) .NE. F$LENGTH(device_info)
$ THEN
$   WRITE SYS$OUTPUT "The process COMOLM does not exist"
$   deassign/job messageq_aux
$   EXIT
$ ENDIF
$ device_aux = f$element(1,":",device_info)
$ deassign/job messageq_aux
$! show symbol device_aux 

$ pipe/nosymbol/nological -
show network "TCP/IP" /FULL | -
search sys$pipe "''device_aux'" | -
( read sys$pipe messageq_host ; define/job messageq_aux &messageq_host )
$ messageq_ip = f$edit(f$trnlnm("messageq_aux","lnm$job"),"collapse")
$ IF F$LOCATE("NOMATCHES",messageq_ip) .NE. F$LENGTH(messageq_ip)
$ THEN
$   WRITE SYS$OUTPUT "There is no Device_socket(''device_aux') in network 'TCP/IP'"
$   deassign/job messageq_aux
$   EXIT
$ ENDIF
$ messageq_ip_pos = F$LOCATE("5000",messageq_ip)+4
$ messageq_ip = F$EXTRACT(messageq_ip_pos,f$length(messageq_ip)-messageq_ip_pos,messageq_ip)   
$ deassign/job messageq_aux
$! show symbol messageq_ip 
$
$ PRIMARY = 1
$ PRIMARY_HOST = ""
$ FAILOVER_HOST = ""
$ PRIMARY_IP = ""
$ FAILOVER_IP = ""
$ IF F$SEARCH("GXOLM:DMQ.INI") .EQS. "" 
$ THEN
$   WRITE SYS$OUTPUT "The DMQ.INI files is missing at :"+F$TRNLNM("GXOLM")
$   EXIT
$ ENDIF
$ OPEN/READ DMQFILE GXOLM:DMQ.INI
$READ_DATA:
$ read/end_of_file=FINISHED DMQFILE RECORD
$! WRITE SYS$OUTPUT F$LOCATE("Hostname=",RECORD)
$! WRITE SYS$OUTPUT F$LENGTH(RECORD)
$ IF F$LOCATE("Hostname=",RECORD) .NE. F$LENGTH(RECORD) 
$ THEN
$   IF PRIMARY .EQ. 1
$   THEN 
$       PRIMARY = 0
$       PRIMARY_HOST = F$EXTRACT(9,f$length(RECORD)-9,RECORD) 
$!       show symbol PRIMARY_HOST 
$   ELSE
$       FAILOVER_HOST = F$EXTRACT(9,f$length(RECORD)-9,RECORD)
$!       show symbol FAILOVER_HOST
$       GOTO FINISHED
$   ENDIF    
$!   show symbol RECORD   
$ ENDIF
$ GOTO READ_DATA
$
$FINISHED:
$ close DMQFILE
$! PRIMARY_HOST = ""
$ IF PRIMARY_HOST .EQS. "" .OR. FAILOVER_HOST .EQS ""
$ THEN
$   WRITE SYS$OUTPUT "It was not possible to obtain the hosts names of messages from DMQ.INI in GXOLM"
$   EXIT
$ ENDIF
$
$ pipe/nosymbol/nological -
TCPIP SH HOSTS | -
search sys$pipe "''messageq_ip'" | -
( read sys$pipe messageq_pb_con ; define/job messageq_con &messageq_pb_con )
$ messageq_connect = f$edit(f$trnlnm("messageq_con","lnm$job"),"collapse")
$ IF F$LOCATE("NOMATCHES",messageq_connect) .NE. F$LENGTH(messageq_connect)
$ THEN
$   WRITE SYS$OUTPUT "There is no reference to ip:''messageq_ip' in Hosts files"
$   deassign/job messageq_connect
$   EXIT
$ ENDIF
$ deassign/job messageq_con
$! show symbol messageq_connect 
$
$ IF F$LOCATE(PRIMARY_HOST,messageq_connect) .NE. F$LENGTH(messageq_connect)
$ THEN
$   WRITE SYS$OUTPUT "Millennium connected to Primary MessageQ: ''messageq_ip'"
$ ELSE
$   IF F$LOCATE(FAILOVER_HOST,messageq_connect) .NE. F$LENGTH(messageq_connect)
$   THEN
$       WRITE SYS$OUTPUT "Millennium connected to FailOver MessageQ: ''messageq_ip'"
$   ELSE
$       WRITE SYS$OUTPUT "It wasn't possible to match the Ip from Hosts file to the Ip used by COMOLM"
$   ENDIF 
$ ENDIF
$
$! WRITE SYS$OUTPUT "finished command procedure"
