C
C SUBROUTINE QUEHSF
C $Log:   GXAFXT:[GOLS]QUEHSF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:36:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   24 Sep 1993 23:20:08   GXA
C  Released for Finland Dec Conversion / Oddset.
C  
C     Rev 1.0   21 Jan 1993 17:25:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - hsf_quehsf.for **
C
C QUEHSF.FOR
C
C V02 12-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO QUEUE HASF FINANCIAL TRANSACTIONS
C TO INPUT QUEUE.
C
C CALLING SEQUENCE:
C      CALL QUEHSF(TAB,ST)
C INPUT
C      TAB (6 WORD INTEGER*4 ARRAY)
C          WORD 1 = TERMINAL NUMBER
C          WORD 2 = LEDGER CODE AND NUMBER
C          WORD 3 = AMOUNT(1)
C	   WORD 4 = AMOUNT(2)
C          WORD 5 = NOT EQUAL TO ZERO REMOVE ENTRY FROM LEDGER TABLE
C          WORD 6 = INFORMATIONAL
C                   BYTE 1= WEEK NUMBER
C                   BYTE 2= YEAR
C                   BYTE 3= CHANGE TYPE (0,1,2)
C                   BYTE 4= GAME TYPE
C
C OUTPUT
C      ST  STATUS
C          0 - NO ERROR
C          1 - BAD TERMINAL NUMBER
C          2 - NO SYSTEM BUFFERS AVAILABLE
C
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
	SUBROUTINE QUEHSF(TAB,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
C
	INTEGER*4 I, BUF, ST
	INTEGER*4 TAB(6)
	ST=0
C
C CHECK IF VALID TERMINAL NUMBER
C
	IF(TAB(1).LT.1.OR.TAB(1).GT.NUMAGT) THEN
	  ST=1
	  RETURN
	ENDIF
C
C ALLOCATE PROCOM PROCESSING BUFFER
C IF NO BUFFERS AVAILABLE THEN RETURN
C
	CALL GETBUF(BUF)
	IF(BUF.EQ.0) THEN
	  ST=2
	  RETURN
	ENDIF
C
C TRANSFER HASF TRANSACTION TO PROCOM BUFFER
C
	DO 10 I=1,6
	PRO(INPTAB+I-1,BUF)=TAB(I)
10	CONTINUE
	HPRO(TERNUM,BUF)=TAB(1)
	HPRO(TRCODE,BUF)=TYPHSF
	HPRO(INPLEN,BUF)=25     !24 BYTES OF DATA
C
C QUEUE TRANSACTION TO INPUT QUEUE
C
	CALL QUEINP(BUF,ST)
	RETURN
	END
