C
C SUBROUTINE NT_MESTRAP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NT_MESTRAP.FOV                               $
C  $Date::   17 Apr 1996 14:15:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - notpro.for;1 **
C
C
C***************************************************************
C NT_MESTRAP
C
C THIS ROUTINE WILL HANDLE THE INTERRUPT WHEN A MESSAGE IS
C PLACED INTO THE MAILBOX, AND WILL DISPLAY IT TO THE
C CONSOLE.
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NT_MESTRAP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:NOTEVN.DEF'
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($IODEF)'
	INCLUDE '($FORIOSDEF)'
        INCLUDE '($SSDEF)'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
C
	INTEGER*4   FUNCOD
	INTEGER*4   STATUS
	INTEGER*4   I4MESS(32)
	CHARACTER   MESS*132
	EQUIVALENCE (I4MESS,MESS)
C
	STRUCTURE /NT_IOSSTRUCT/
          INTEGER*2 STAT                          !VMS STATUS
          INTEGER*2 XSIZE                         !TRANSFER SIZE
          INTEGER*4 PARM            
	END STRUCTURE
	RECORD /NT_IOSSTRUCT/ LOCAL_IOSB
C
	INTEGER*4 ELOG_LUN
	PARAMETER(ELOG_LUN=1)
	LOGICAL*4 ELOG_OPENED/.FALSE./
	CHARACTER*20 ELOG_FILE
	EQUIVALENCE(ELOG_FILE,SCFSFN(1,ERLG))
C
C READ THE MESSAGE FROM THE MAILBOX.
C
        FUNCOD=IO$_READVBLK
        STATUS=SYS$QIOW(,%VAL(NT_MESCHANNEL),%VAL(FUNCOD),
     *                  LOCAL_IOSB,,,I4MESS,%VAL(132),,,,)
        IF(.NOT.STATUS) THEN
          CALL LIB$SIGNAL(%VAL(STATUS))
        ENDIF
C
C IF MESSAGE CONTAINS "_CMD:KILL" STOP THE TASK.
C
	IF(MESS(19:27).EQ.'_CMD:KILL') THEN
	   IF(ELOG_OPENED) CLOSE(ELOG_LUN)
	   CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C WRITE THE MESSAGE TO ELOG.FIL
C
	IF(.NOT.ELOG_OPENED) THEN
	   CALL GETSCONF(SCFREC,STATUS)
	   OPEN(UNIT = ELOG_LUN,
     *          SHARED,
     *          FILE   = ELOG_FILE,
     *          RECL   = 132,
     *          STATUS = 'UNKNOWN',
     *          ACCESS = 'APPEND',
     *          IOSTAT = STATUS)
            IF(STATUS.NE.0) THEN
	       TYPE*,IAM(),'Error opening ', ELOG_FILE
	       CALL GSTOP(GEXIT_FATAL)
	    ENDIF
	    ELOG_OPENED = .TRUE.
	ENDIF
	WRITE(ELOG_LUN,'(A132)') MESS(1:132)
	STATUS = SYS$FLUSH(%VAL(FOR$RAB(ELOG_LUN)),,)
	IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C STARTUP ANOTHER READ OUTSTANDING.
C
	CALL NT_START_MESS
C
	RETURN
	END
