C
C V03 29-SEP-2011 FJG Avoid showing WORKING SET message
C V02 15-FEB-2001 UXN Reopening ELOG file added.
C V01 25-JUL-2000 UXN Initial release.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM EWATCH
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
C
	INTEGER*4    STATUS,WST
	CHARACTER*132 MESS
	CHARACTER*20  ELOG_FILE
	EQUIVALENCE(ELOG_FILE,SCFSFN(1,ERLG))
	INTEGER*4     ELOG_LUN
	PARAMETER(ELOG_LUN=1)
	LOGICAL*4     REOPEN, OFLG
	INTEGER*4     WAIT_CNT
C
	CALL GETSCONF(SCFREC,STATUS)
C
	WAIT_CNT = 0
	REOPEN   = .FALSE.
10	CONTINUE
        OPEN(UNIT = ELOG_LUN,
     *       READONLY,
     *       SHARED,
     *       FILE   = ELOG_FILE,
     *       RECL   = 132,
     *       STATUS = 'OLD',
     *       IOSTAT = STATUS)
	IF(STATUS.NE.0) THEN
           CALL XWAIT(1,2,WST)
	   GOTO 10
        ENDIF
	IF(REOPEN) THEN	
	   REOPEN = .FALSE.
	   TYPE*,IAM(),'Rescanning ',ELOG_FILE
	   GOTO 20
	ENDIF
C
C GO TO THE END OF THE FILE
C
	DO WHILE(1)
	   READ(ELOG_LUN, FMT='(A132)', END=20, ERR=20)  MESS
	ENDDO
C
20	CONTINUE
	READ(ELOG_LUN, FMT='(A132)', IOSTAT=STATUS)  MESS
	IF(STATUS.NE.0) THEN
	    CALL XWAIT(500,1,WST)
	    WAIT_CNT = WAIT_CNT + 1
C
C Check if the file is not replaced by ELOG process.
C
	    IF(WAIT_CNT.GT.120) THEN
               WAIT_CNT = 0
	       INQUIRE(FILE=ELOG_FILE, OPENED=OFLG)
	       IF(.NOT.OFLG) THEN
	          REOPEN = .TRUE.
		  CLOSE(ELOG_LUN)
		  GOTO 10
	       ENDIF
	    ENDIF
	    GOTO 20 
	ENDIF
	WAIT_CNT = 0
	IF(MESS(19:23).EQ.'_DBG:') GOTO 20 ! DON'T PRINT DEBUG MESSAGES.
	IF(MESS(19:51).EQ.'FAILED TO INCREASE WORKING SET BY') GOTO 20 ! DON'T PRINT THIS MESSAGE	
	WRITE(6,'(1X,A132)') MESS
	GOTO 20
	END
