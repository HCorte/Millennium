$!"Alpha" for Alpha, "VAX" for VAX, or "IA64" for Integrity servers.
$ WRITE SYS$OUTPUT F$GETSYI("ARCH_NAME")
$!SYI$_ARCH_TYPE returns 1 on VAX , 2 on Alpha, and 3 on Integrity servers.
$ WRITE SYS$OUTPUT F$GETSYI("ARCH_TYPE")
$!Returns the number of CPUs available in the current boot of the symmetric multiprocessing (SMP) system
$ WRITE SYS$OUTPUT F$GETSYI("AVAILCPU_CNT")
$ IF F$GETSYI("CLUSTER_MEMBER") .EQS. "FALSE" THEN GOTO NOT_CLUSTER
$ CONTEXT = "" $START: $   id = F$CSID (CONTEXT)
$   IF id .EQS. "" THEN EXIT
$   nodename = F$GETSYI ("NODENAME",,id)
$   WRITE SYS$OUTPUT nodename
$   GOTO start
$NOT_CLUSTER:
$ WRITE SYS$OUTPUT "Not a member of a cluster."
$ EXIT
