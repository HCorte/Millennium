$!#####################################################################################
$! Command Procedure to show What MessageQ Millennium is attached in COMOLM
$!
$! for debug purposes use dcl: sh log /table=lnm$job
$! to see the key value pair obtained from pipe subprocess value obtain 
$! and passed to this main process by the usage of this table of jobs 
$! in code "define/job messageq_aux &messageq_pb" to define the key value pair writen 
$! to the logical table
$!
$!#####################################################################################


$! DEFINE SYS$INPUT 'F$TRNLNM("SYS$COMMAND")' !enter in conflict with SYS$CREPRC SYS$INPUT (that is this script)
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
$! Logical Name PROCOLM = SCMLCOMOLM
$!#####################################################################################################

$ pipe/nosymbol/nological -
show process SCMLCOMOLM | -
search sys$pipe "Devices allocated:" | -
( read sys$pipe messageq_pb ; define/group/nolog millconnect &messageq_pb )
$! device_info = f$edit(f$trnlnm("messageq_aux","lnm$job"),"collapse") 
$ device_info = f$edit(f$trnlnm("millconnect","lnm$group"),"collapse") 
$ IF F$LOCATE("NOMATCHES",device_info) .NE. F$LENGTH(device_info)
$ THEN
$!   WRITE SYS$OUTPUT "The process COMOLM does not exist"
$!   deassign/job messageq_aux
$   pipe/nosymbol/nological - 
    show process SCMLCOMOLM | -
    search sys$pipe "Process name:" | -
    ( read sys$pipe messageq_pb ; define/group/nolog millconnect &messageq_pb )
$   device_info = f$edit(f$trnlnm("millconnect","lnm$group"),"collapse")
$   IF F$LOCATE("NOMATCHES",device_info) .NE. F$LENGTH(device_info)
$   THEN     
$!       define/job/nolog millconnect "process COMOLM missing,ERR"
$       define/group/nolog millconnect "process COMOLM missing,ERR"
$       EXIT
$   ELSE
$!       define/job/nolog millconnect "Not Attached,ERR"
$       define/group/nolog millconnect "Not Attached,ERR"      
$       EXIT
$   ENDIF
$ ENDIF
$ device_aux = f$element(1,":",device_info)
$ show symbol device_aux 
$! deassign/job messageq_aux

$ pipe/nosymbol/nological -
show network "TCP/IP" /FULL | -
search sys$pipe "''device_aux'" | -
( read sys$pipe messageq_host ; define/group/nolog millconnect &messageq_host )
$! messageq_ip = f$edit(f$trnlnm("messageq_aux","lnm$job"),"collapse")
$ messageq_ip = f$edit(f$trnlnm("millconnect","lnm$group"),"collapse")
$! sh symbol messageq_ip
$ IF F$LOCATE("NOMATCHES",messageq_ip) .NE. F$LENGTH(messageq_ip)
$ THEN
$!   WRITE SYS$OUTPUT "There is no Device_socket(''device_aux') in network 'TCP/IP'"
$!   deassign/job messageq_aux
$!   define/job millconnect/nolog "Device Socket missing,ERR"
$   define/group millconnect/nolog "Device Socket missing,ERR"
$   EXIT
$ ENDIF
$ messageq_ip_pos = F$LOCATE("5000",messageq_ip)+4
$ messageq_ip = F$EXTRACT(messageq_ip_pos,f$length(messageq_ip)-messageq_ip_pos,messageq_ip)   
$ show symbol messageq_ip 
$! deassign/job messageq_aux
$
$ PRIMARY = 1
$ PRIMARY_HOST = ""
$ FAILOVER_HOST = ""
$ PRIMARY_IP = ""
$ FAILOVER_IP = ""
$! IF F$SEARCH("GXOLM:DMQ.INI") .EQS. "" 
$ dmq_ini_path = f$edit(f$trnlnm("OLM_DMQCL_INI_FILE","lnm$system"),"collapse")
$ show symbol dmq_ini_path
$ IF F$SEARCH("''dmq_ini_path'") .EQS. "" 
$ THEN
$!   WRITE SYS$OUTPUT "The DMQ.INI files is missing at :"+F$TRNLNM("GXOLM")
$!   define/job/nolog millconnect "DMQ.INI file missing,ERR"
$   define/group/nolog millconnect "DMQ.INI file missing,ERR"
$   EXIT
$ ENDIF
$! validate if Primary and FailOver Host logical names are defined
$ PRIMARY_HOST = f$edit(f$trnlnm("OLM_DMQSRV_PRIMARY_HOST","lnm$job"),"collapse")
$ FAILOVER_HOST = f$edit(f$trnlnm("OLM_DMQSRV_FAILOVER_HOST","lnm$job"),"collapse")
$ sh symbol PRIMARY_HOST
$ sh symbol FAILOVER_HOST
$
$! IF (F$LOCATE("NOMATCHES",PRIMARY_HOST) .EQ. F$LENGTH(PRIMARY_HOST)) .AND. (F$LOCATE("NOMATCHES",PRIMARY_HOST) .EQ. F$LENGTH(PRIMARY_HOST))
$ IF (PRIMARY_HOST .NES. "" .AND. FAILOVER_HOST .NES. "")
$ THEN
$   WRITE SYS$OUTPUT "EXISTS LOGICAL HOST OF PRIMARY AND FAILOVER MESSQ"
$   GOTO EXISTS_LOG_HOST
$ ELSE
$   WRITE SYS$OUTPUT "DOESN'T EXIST LOGICAL HOST READ FROM DMQ.INI FILE"
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
$ show symbol PRIMARY_HOST
$ show symbol FAILOVER_HOST
$ IF PRIMARY_HOST .EQS. "" .OR. FAILOVER_HOST .EQS ""
$ THEN
$!   WRITE SYS$OUTPUT "It was not possible to obtain the hosts names of messages from DMQ.INI in GXOLM"
$!   define/job/nolog millconnect "HOSTS NAMES MISSING IN DMQ.INI,ERR"
$   define/group/nolog millconnect "HOSTS NAMES MISSING IN DMQ.INI,ERR"
$   EXIT
$ ENDIF
$
$! define/job/supervisor/nolog OLM_DMQSRV_PRIMARY_HOST "''PRIMARY_HOST'"
$! define/job/supervisor/nolog OLM_DMQSRV_FAILOVER_HOST "''FAILOVER_HOST'"
$ define/group/supervisor/nolog OLM_DMQSRV_PRIMARY_HOST "''PRIMARY_HOST'"
$ define/group/supervisor/nolog OLM_DMQSRV_FAILOVER_HOST "''FAILOVER_HOST'"
$ WRITE SYS$OUTPUT "Created logical names of primary and failover Hosts"
$
$EXISTS_LOG_HOST:
$ pipe/nosymbol/nological -
TCPIP SH HOSTS | -
search sys$pipe "''messageq_ip'" | -
( read sys$pipe messageq_pb_con ; define/group/nolog millconnect &messageq_pb_con )
$! messageq_connect = f$edit(f$trnlnm("messageq_con","lnm$job"),"collapse")
$ messageq_connect = f$edit(f$trnlnm("millconnect","lnm$group"),"collapse")
$ show symbol messageq_connect
$ IF F$LOCATE("NOMATCHES",messageq_connect) .NE. F$LENGTH(messageq_connect)
$ THEN
$!   WRITE SYS$OUTPUT "There is no reference to ip:''messageq_ip' in Hosts files"
$!   define/job/nolog millconnect "IP:''messageq_ip' MISSING IN HOSTS FILE,ERR"
$   define/group/nolog millconnect "IP:''messageq_ip' MISSING IN HOSTS FILE,ERR"
$!   deassign/job messageq_con
$   EXIT
$ ENDIF
$! deassign/job messageq_con 
$ host = f$element(1,",",messageq_connect)
$ IF(host .EQS. ",") 
$ THEN
$   position = f$length(messageq_ip)
$   host = f$extract(position,f$length(messageq_connect)-position,messageq_connect)
$   IF f$element(0,".",host) .NES. "." THEN host = f$element(0,".",host)       
$ ENDIF
$ show symbol host
$
$ IF F$LOCATE(PRIMARY_HOST,messageq_connect) .NE. F$LENGTH(messageq_connect)
$ THEN
$!   WRITE SYS$OUTPUT "Millennium connected to Primary MessageQ: ''messageq_ip'"
$!   define/job millconnect "Millennium connected to Primary MessageQ: ''messageq_ip'"
$!    define/job/nolog millconnect "Primary,''messageq_ip',''host'"
$    define/group/nolog millconnect "Primary,''messageq_ip',''host'"
$ ELSE
$   IF F$LOCATE(FAILOVER_HOST,messageq_connect) .NE. F$LENGTH(messageq_connect)
$   THEN
$!       WRITE SYS$OUTPUT "Millennium connected to FailOver MessageQ: ''messageq_ip'"
$!       define/job millconnect "Millennium connected to FailOver MessageQ: ''messageq_ip'"
$!        define/job/nolog millconnect "FailOver,''messageq_ip',''host'"
$        define/group/nolog millconnect "FailOver,''messageq_ip',''host'"
$   ELSE
$!       WRITE SYS$OUTPUT "It wasn't possible to match the Ip from Hosts file to the Ip used by COMOLM"
$!       define/job millconnect "It wasn't possible to match the Ip from Hosts file to the Ip used by COMOLM"
$!        define/job/nolog millconnect "No Match found,ERR"
$         define/group/nolog "No Match found,ERR"
$   ENDIF 
$ ENDIF
$
$! WRITE SYS$OUTPUT "finished command procedure"
