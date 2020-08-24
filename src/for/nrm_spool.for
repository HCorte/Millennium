C
C SUBROUTINE SPOOL
C $Log:   GXAFXT:[GOLS]SPOOL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:14:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:40:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_spool.for **
C
C VAX_SPOOL.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SPOOL.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 21-MAR-90 LMF INITIAL RELEASE FOR INDIANA (FROM ILL)
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
C
C
C   SPOOL ROUTINE TO SEND MESSAGE TO THE SPOOLER TO PRINT THE
C   REQUIRED FILE, THE REQUIRED NUMBER OF TIMES, AND FROM THE
C   ACCOUNT THE USER IS PRESENTLY IN.
C
C CALLING SEQNC:
C
C    INPUT
C        FILENAME - REPORT FILE NAME
C        COPIES   - NUMBER OF REPORT COPIES
C
C    OUTPUT
C        STATUS   - ERROR STATUS
C SPECIAL NOTES:
C
C    SAVE THE COPIES BECAUSE ACCNUM WILL CONVERT TO ASCII
C
C    0 = GOOD STATUS
C   -1 = BAD SNDMSG
C   -2 = BAD CALL TO ACCNUM
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
	SUBROUTINE SPOOL(FILENAME,COPIES,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER FILENAME*(*)
	INTEGER*4 COPIES
	INTEGER*4 STATUS
C
	INTEGER*4 K
C
C
C
C
	DO 1100 K = 1, COPIES
	  OPEN(UNIT=7, FILE=FILENAME, STATUS='OLD',
     *         IOSTAT=STATUS, SHARED, DISP='PRINT')
	  IF(STATUS.NE.0)GOTO 9000
	  CLOSE(7)
1100	CONTINUE
C
9000	CONTINUE
	RETURN
	END
