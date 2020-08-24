C
C SUBROUTINE ISREAD
C $Log:   GXAFXT:[GOLS]ISREAD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:41:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:43:04   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshseqi4.for **
C
C HSHSEQI4.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
C
C *** HSHSEQ.FTN ***
C B02 11-MAR-88 TKO  HANDLE TUB I/O
C                    IF INOCHKS WAS CALLED & RECORD WAS FOUND,
C                    RETURN WITH ERROR ERRRFN
C B01 25-MAY-86 TKO  INITIAL RELEASE
C
C This is a set of subroutines to do sequential reads/writes
C to a hashed file.
C
C
C
C
C *** ISREAD
C
C This will return the next non-dummy record in the hash file
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
	SUBROUTINE ISREAD(I4REC,LUN,IOBUF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 I4REC(*)             ! OUTPUT: TO HOLD RECORD
	INTEGER*4 LUN                  !  INPUT: LOGICAL UNIT #
	INTEGER*4 IOBUF(*)             !SCRATCH: I/O BUFFER
	INTEGER*4 STATUS               ! OUTPUT: 0=OK, ELSE ERROR #
C
	INTEGER*4 I4TEMP
	INTEGER*2 I2TEMP(2)
	EQUIVALENCE (I4TEMP,I2TEMP)
C
C
C
1000	CONTINUE
	CALL ISREAD1(I4REC,LUN,IOBUF,STATUS)
	IF(STATUS.EQ.0)THEN
	  I4TEMP=I4REC(FCB(FCBI2K,LUN))
	  IF(I2TEMP(FCB(FCBI2H,LUN)).LT.0)GO TO 1000     !IGNORE DUMMIES
	ENDIF
C
	RETURN
	END
