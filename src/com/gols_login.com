$!
$! GOLS_LOGIN.COM
$!
$!-------------------------------------------------------------------------------
$!
$!
$!-------------------------------------------------------------------------------
$!
$! COPYRIGHT 1994,1995 GTECH CORPORATION.  ALL RIGHTS RESERVED.
$!
$! CONFIDENTIAL PROPRIETARY INFORMATION
$! This item is the property of GTECH Corporation, West Greenwich, Rhode Island,
$! and contains confidential and trade secret information.  It may not be
$! transferred from the custody or control of GTECH except as authorized in
$! writing by an officer of GTECH.  Neither this item nor the information it
$! contains may be used, transferred, reproduced, published or disclosed, in
$! whole or in part, directly or indirectly, except as expressly authorized by
$! an officer of GTECH pursuant to written agreement.
$!
$!------------------------------------------------------------------------------_
$!
$!
$ SET NOON
$!
$ WRITE SYS$OUTPUT "Executing '' F$ENVIRONMENT(""PROCEDURE"")'"
$!
$ USERNAME = F$EDIT(F$GETJPI("","USERNAME"),"TRIM")
$ NODENAME = F$GETSYI("NODENAME")
$!
$!
$ SUBRUN     :== @GXCOM:SUBRUN.COM
$ DETRUN     :== @GXCOM:DETRUN.COM
$ EWATCH     :== @GXCOM:EWATCH.COM
$ ELOG       :== @GXCOM:ELOG.COM
$ TEL*L      :== $GXTSK:TELL
$ CHECKFRAG  :== $GXTSK:CHECKFRAG
$!
$ SET PROCESS/PRIV=BYPASS
$!
$! Define temporary mailbox in system directory
$!
$ SET PROCESS/PRIV=SYSNAM
$ SET PROCESS/PRIV=SYSPRV
$ DEFINE/TABLE=LNM$FILE_DEV         LNM$TEMPORARY_MAILBOX LNM$SYSTEM
$ DEFINE/TABLE=LNM$SYSTEM_DIRECTORY LNM$TEMPORARY_MAILBOX LNM$SYSTEM
$ SET PROCESS/PRIV=NOSYSPRV
$!
$! Define Symbols and Logicals for Software Development
$!
$ GOD'SITECODE3' :== @GOLS$GTECH:GO_GOLS D'SITECODE3' GXSRC
$ GOQ'SITECODE3' :== @GOLS$GTECH:GO_GOLS Q'SITECODE3' GXSRC
$ GOB'SITECODE3' :== @GOLS$GTECH:GO_GOLS B'SITECODE3' GXSRC
$ GOA'SITECODE3' :== @GOLS$GTECH:GO_GOLS A'SITECODE3' GXSRC
$ GO'SITECODE4'  :== @GOLS$GTECH:GO_GOLS 'SITECODE4'  GXTSK
$ GOHOME         :== @GOLS$GTECH:GO_GOLS HOME
$!
$ IF USERNAME .EQS. "CONSOLE"
$   THEN SET DEFAULT GXTSK
$ ENDIF
$!
$ @SYS$SYSDEVICE:[GTECHCOMMANDS.DSS]DSS_INSTALL
$!
$ EXIT
$!
