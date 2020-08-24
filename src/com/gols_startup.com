$!
$! GOLS_STARTUP.COM
$!
$!-------------------------------------------------------------------------------
$!
$!	Application Specific Startup Procedure
$!
$!-------------------------------------------------------------------------------
$!
$!	Site: 
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
$!-------------------------------------------------------------------------------
$!
$ SET NOON
$!
$ WRITE SYS$OUTPUT "Executing ''F$ENVIRONMENT(""PROCEDURE"")'"
$!
$ SITECODE2 = SITECODE2
$ SITECODE3 = SITECODE3
$ SITECODE4 = SITECODE4
$!
$!
$ DEFINE/SYSTEM/EXEC GXCOM GXPROJ:[COM]
$ DEFINE/SYSTEM/EXEC GXLOG GXPROJ:[LOG]
$ DEFINE/SYSTEM/EXEC GXTSK GXPROJ:[TSK]
$ DEFINE/SYSTEM/EXEC GXSHR GXPROJ:[SHR]
$ DEFINE/SYSTEM/EXEC GXSRC GXPROJ:[SRC]
$ DEFINE/SYSTEM/EXEC GXUSR GXPROJ:[USR]
$ DEFINE/SYSTEM/EXEC GXDOC GXPROJ:[DOC]
$ DEFINE/SYSTEM/EXEC GXHLP GXPROJ:[HLP]
$ DEFINE/SYSTEM/EXEC GXTMP GXPROJ:[TMP]
$ DEFINE/SYSTEM/EXEC GXUTL GXPROJ:[UTL]
$ DEFINE/SYSTEM/EXEC GXTST GXPROJ:[TST]
$!
$ DEFINE/SYSTEM/EXEC INCLIB GXSRC
$ DEFINE/SYSTEM/EXEC OBJLIB GXSRC
$ DEFINE/SYSTEM/EXEC STDLIB GXSRC
$ DEFINE/SYSTEM/EXEC TSKLIB GXTSK
$ DEFINE/SYSTEM/EXEC SHRLIB GXTSK
$!
$! Cleanup LOG files
$!
$ IF F$SEARCH("GXCOM:*.LOG;") .NES. ""
$   THEN DELETE GXCOM:*.LOG;*
$ ENDIF
$!
$!
$ SYSX = F$TRNLNM("DISK$SYSX")-":"
$! DEV  = F$TRNLNM("DISK$DEV")-":"
$!
$ DEFINE/SYSTEM/EXEC/TRANS=CON GXPROJ           'sysx':['sitecode4'.]
$ DEFINE/SYSTEM/EXEC/TRANS=CON GX'sitecode4'    'sysx':['sitecode4'.]
$!
$! IF DEV .NES. ""
$!   THEN DEFINE/SYSTEM/EXEC/TRANS=CON GXD'sitecode3'   'dev':[D'sitecode3'.]
$!	 DEFINE/SYSTEM/EXEC/TRANS=CON GXQ'sitecode3'   'dev':[Q'sitecode3'.]
$!	 DEFINE/SYSTEM/EXEC/TRANS=CON GXB'sitecode3'   'dev':[B'sitecode3'.]
$!	 DEFINE/SYSTEM/EXEC/TRANS=CON GXA'sitecode3'   'dev':[A'sitecode3'.]
$!	 DEFINE/SYSTEM/EXEC ARCHIVES GXA'sitecode3'
$! ENDIF
$!
$ DEFINE/SYSTEM/EXEC GTECH_REMSYS4 FNDEVA
$ DEFINE/SYSTEM/EXEC GTECH_REMSYS5 FNDEVB
$ DEFINE/SYSTEM/EXEC GTECH_REMTASK4 GTECH4
$ DEFINE/SYSTEM/EXEC GTECH_REMTASK5 GTECH5
$!
$! Define Sharable Images
$!
$ LOGIMAGE = "GXCOM:''sitecode4'LOGIMAGE.COM"
$ IF F$SEARCH(LOGIMAGE) .NES. ""
$   THEN @'LOGIMAGE'
$   ELSE WRITE SYS$OUTPUT "''logimage' does not exist"
$ ENDIF
$!
$! Define disk logicals
$!
$ LOGDISKS = "GXCOM:''sitecode4'LOGDISKS.COM"
$ IF F$SEARCH(LOGDISKS) .NES. ""
$   THEN @'LOGDISKS'
$   ELSE WRITE SYS$OUTPUT "''logdisks' does not exist"
$ ENDIF
$!
$!
$ EXIT
$!
