C
C SUBROUTINE GSERIAL
C $Log:   GXAFXT:[GOLS]GSERIAL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:28:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:33:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gserial.for **
C
C GSERIAL.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C SUBROUTINE TO ASSIGN SERIAL NUMBERS TO TRANSACTIONS
C CALLING SEQUENCE
C     CALL GSERIAL(NXTOUT,SERIAL,SIZE)
C INPUT
C     SIZE   - RECORD SIZE (IN LOG RECORDS)
C OUTPUT
C     SERIAL - TRANSACTION SERIAL NUMBER
C
C     NXTOUT - NXT SERIAL
C
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
	SUBROUTINE GSERIAL(NXTOUT,SERIAL,SIZE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
	INTEGER*4 INDEX, SIZE, SERIAL, NXTOUT
C
C ASSIGN THEN INCREMENT SERIAL NUMBER
C AND INCREMENT TRANSACTION COUNT
C
	SERIAL=NXTOUT
	NXTOUT=NXTOUT+1
C
C IF TRANSACTION REQUIRES MULTI-RECORDS, THEN INSURE ALL RECORDS
C WILL FIT IN THE SAME LOG BLOCK. IF TRANSACTION WILL NOT FIT,
C THEN SKIP TO NEXT LOG BLOCK.
C
	IF(SIZE.EQ.3) THEN
	  INDEX=MOD(SERIAL-1,LBLK)+1
	  IF(INDEX.EQ.LBLK-1) THEN
	    NXTOUT=NXTOUT+1
	  ELSE
	    IF(INDEX.LT.LBLK-1) THEN
	      NXTOUT=NXTOUT+2
	    ENDIF
	  ENDIF
	  GOTO 20
	ENDIF
C
	IF(SIZE.EQ.2) THEN
	  INDEX=MOD(SERIAL-1,LBLK)+1
	  IF(INDEX.EQ.LBLK) GOTO 20
	  NXTOUT=NXTOUT+1
	ENDIF
C
20	CONTINUE
	RETURN
	END
