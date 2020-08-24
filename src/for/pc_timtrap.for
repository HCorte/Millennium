C
C SUBROUTINE PC_TIMTRAP
C $Log:   GXAFXT:[GOLS]PC_TIMTRAP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:23:14   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:16:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pc_timtrap.for **
C
C PC_TIMTRAP.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C THIS SUBROUTINE WILL SERVICE TIMER TRAPS
C      BY STARTING THE IO IF ITS NOT UP
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PC_TIMTRAP(PARAM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PCCOM.DEF'
	INCLUDE 'INCLIB:PCEVN.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 K, PARAM, ST
C
C THE QIO FUNCTION CODE FOR THE WRITE
C
        INTEGER*4        WRITEFUNCOD
        PARAMETER       (WRITEFUNCOD= IO$_WRITEVBLK + IO$M_CANCTRLO +
     *                                IO$M_NOFORMAT)
C
	EXTERNAL  PC_IOTRAP
C
	IF(PARAM.LT.1 .OR.PARAM.GT.NUMMES) THEN
	  TYPE *,'INVALID PC_TIMTRAP   MESSAGE # ',PARAM
	  RETURN
	ENDIF
C
	PC_CNTTIM(PARAM)=PC_CNTTIM(PARAM)+1	!KEEP STATISTICS
C
	TIMINPROG(PARAM)=0
C
	IF(TSKSTAT.EQ.ACT) THEN
C
C 	  IF THERE IS NO WRITE IN PROGRESS, THEN DO ANOTHER WRITE
C	  OTHERWISE START ANOTHER TIME TRAP
C
	  IF(IOWPROG.EQ.0 .OR. IOWPROG.EQ.1) THEN   !QUEUE UP WRITES
	      CALL FILBUF(PARAM)
	      IF(IOLEN.GT.0) THEN
C
      		  ST = SYS$QIO  ( , %VAL(LAT_CHANNEL),	      !CHANNEL
     *				    %VAL(WRITEFUNCOD),	      !FUNCTION CODE
     *				    WRITE_IOSB(PARAM),	      !STATUS BLOCK
     *				    PC_IOTRAP,		      !AST ROUTINE
     *				    PC_PARAM(PARAM),	      !AST PARAMETER
     *				    %REF(BUFFER(1,WRITEPNT)), !BUFFER ADDRESS
     *				    %VAL(IOLEN),,	      !BUFFER LENGTH
     *                              %VAL(0),,)
		  IF(.NOT.ST) THEN
		    TYPE *,'ERROR STARTING WRITE IN PC_TIMTRAP'
		    CALL LIB$SIGNAL(%VAL(ST))
		  ENDIF
C
	          IOWPROG=1
C
	          IF(TESTFLG.GT.0) THEN
	            TYPE 900,(BUFFER(K,WRITEPNT),K=1,(IOLEN+3)/4)
 900	            FORMAT(8(1X,Z8))
	          ENDIF
C
C	      NOTHING TO SEND THIS TIME, START ANOTHER TIMER TRAP
C
	      ELSEIF(INTVAL(PARAM).GT.0) THEN
		 CALL PC_START_TIME(PARAM)
	         TIMINPROG(PARAM)=1
	      ENDIF
C
C	  A WRITE IS OUTSTANDING, START A SHORT TIMER TRAP
C
	  ELSE
	     PC_CNTSTIM=PC_CNTSTIM+1	!KEEP STATISTICS
	     CALL PC_START_TIME(-PARAM)	!NEGATING THE PARM
	     TIMINPROG(PARAM)=1		!WILL FORCE A  MS WAIT
	  ENDIF
	ENDIF
C
	RETURN
	END
