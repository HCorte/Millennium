$!Logical Names for MessageqConnection script
$ DEFINE/SYSTEM/EXEC/NOLOG OLM_MILCON_LOG GXOLM:MILLCON
$ DEFINE/SYSTEM/EXEC/NOLOG OLM_ERR_LOG GXOLM:ERR_MILLCON
$ DEFINE/SYSTEM/EXEC/NOLOG OLM_DMQCL_INI_FILE GXOLM:DMQ.INI
$ DEFINE/SYSTEM/EXEC/NOLOG OLM_MESSQ_SEE_ATTACH_SCRIPT GXOLM:MESSAGEQCONNECTION.COM
$! not created here but in other context of this script -> (MessageqConnection.com)
$! DEFINE/JOB/EXEC/NOLOG OLM_DMQSRV_PRIMARY_HOST "....."
$! DEFINE/JOB/EXEC/NOLOG OLM_DMQSRV_FAILOVER_HOST "....."
$! DEFINE/JOB/EXEC/NOLOG millconnect "....."


$! set def DSS$GTECH
$! set def [-]
$! type SYS$SYSDEVICE:[GTECHCOMMANDS.DEVELOPMENT]DEVELOPMENT_STARTUP.COM;3 
