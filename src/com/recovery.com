$!
$! $Log:   GXAFXT:[GOLS]RECOVERY.COV  $
$! 
$!    Rev 1.0   18 Dec 1996 15:41:48   HXK
$! Initial revision.
$! 
$!    Rev 1.0   29 Nov 1996  1:14:56   WXW
$! Initial revision.
$! 
$!    Rev 1.0   17 Apr 1996 12:05:28   HXK
$! Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
$! 
$!    Rev 1.0   10 Jul 1993 19:10:10   GXA
$! Initial revision.
$! 
$!    Rev 1.0   25 Feb 1993 11:00:04   EBD
$! Initial revision.
$!
$!
$! V03 12-AUG-11 RXK "Millennium" replaced with "ES Evolution"
$! V02 22-APR-91 TKO  Set working set high
$! V01 11-APR-91 TKO  Initial Release
$!
$! This command file will bring up the system for Maryland
$!
$! COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
$!
$! COPYRIGHT 1991 GTECH CORPORATION.  ALL RIGHTS RESERVED.
$!
$! CONFIDENTIAL PROPRIETARY INFORMATION
$! This item is the property of GTECH Corporation, W. Greenwich, Rhode
$! Island, and contains confidential and trade secret information.  It
$! may not be transferred from the custody or control of GTECH except
$! as authorized in writing by an officer of GTECH.  Neither this item
$! nor the information it contains may be used, transferred,
$! reproduced, published or disclosed, in whole or in part, directly
$! or indirectly, except as expressly authorized by an officer of
$! GTECH pursuant to written agreement.
$! COPYRITF.DEF-------------------------------------------------------
$!
$!
$ WRITE SYS$OUTPUT " "
$ WRITE SYS$OUTPUT " "
$ WRITE SYS$OUTPUT "ES EVOLUTION system Recovery"
$ WRITE SYS$OUTPUT " "
$ WRITE SYS$OUTPUT " "
$!
$! Rename ELOG.FIL to ELOG-YYYY-MM-DD.FIL
$!
$ IF F$SEARCH("GXTSK:ELOG.FIL") .NES. ""
$ THEN
$    DATE  = F$EXTRACT(0,10,F$CVTIME())
$    FNAME = "ELOG-''DATE'.LOG"
$    RENAME GXTSK:ELOG.FIL GXTSK:'FNAME'
$ ENDIF
$ ELOG STOP
$ ELOG
$ EWATCH STOP
$ EWATCH
$!
$! Set XRAM to indicate we are using RAM disk
$!
$ SET MESSAGE /NOFACIL/NOIDENT/NOSEVERITY/NOTEXT
$ DEFINE/JOB XRAM GXTSK
$ DEFINE SYS$INPUT SYS$COMMAND
$ SET MESSAGE /FACIL/IDENT/SEVERITY/TEXT
$!
$ RESET_PRCNAM := REPROC		!SET TO "RESET" OR "REPROC"
$!
$!
$! Get current working set parameters
$!
$ OLDWSQUOTA	= F$GETJPI("","WSQUOTA")	!CURRENT QUOTA
$ WSAUTH	= F$GETJPI("","WSAUTH")		!AUTHORIZED QUOTA
$!
$ ON ERROR     THEN GOTO ALLDONE
$ ON CONTROL_Y THEN GOTO ALLDONE
$!
$ SET WORKING_SET/QUOTA='WSAUTH			!SET TO MAXIMUM
$!
$ RUN GXTSK:RESET
$!
$! START GOLS MONITORING PROGRAM
$@GXCOM:MONGOLS
$!
$ALLDONE:
$ SET WORKING_SET/QUOTA='OLDWSQUOTA		!RESET BACK
$!
