C
C SUBROUTINE TERSTATS
C $Log:   GXAFXT:[GOLS]TERSTATS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:33:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   28 Jun 1993 17:28:34   HXK
C  changed err message length from 5 to 6
C  
C     Rev 1.0   21 Jan 1993 17:50:48   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - spe_terstats.for **
C
C TERSTATS.FOR
C
C V02 18-NOV-91 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS TERMINAL ERROR STATISTICS TRANSACTION.
C
C CALLING SEQUENCE:
C      CALL TERSTATS(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TERSTATS(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
C
	BYTE	    MESTAB(*)
	INTEGER*4   MYCHKSUM, CHKLEN, ERRTYP
	INTEGER*2   OUTLEN
	DATA	    ERRTYP/Z90/
C
C CHECK FOR INVALID TRANS
C
	IF(TRABUF(TERR).NE.NOER) THEN
	  TRABUF(TSTAT)=REJT
	  GOTO 1000
	ENDIF
C
C DECODE TRANSACTION
C
C SAVE STATISTICS IN 34 BYTES OF DATA AREA 1 BYTE PER TYPE
C FOR DEFINITION SEE MESSAGE FORMATS
C
	CALL MOVBYT(MESTAB,5,TRABUF(TSOLD),1,34)
C
C BUILD OUTPUT MESSAGE BACK TO TERMINAL
C
	OUTLEN=4
	GOTO 9000
C
C RETURN ERROR
C
1000	CONTINUE
	CALL MOVBYT(ERRTYP,4,MESTAB,2,1)
	CALL MOVBYT(TRABUF(TERR),4,MESTAB,5,1)
        MESTAB(6)=0
	OUTLEN=6
C
C CALCULATE CHECKSUM AND RETURN
C
9000	CONTINUE
	I4CCITT = TRABUF(TCHK)
	MESTAB(3) = I1CCITT(2)
	MESTAB(4) = I1CCITT(1)
	CHKLEN = OUTLEN - 1
	CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
	I4CCITT = MYCHKSUM
	MESTAB(3) = I1CCITT(2)
	MESTAB(4) = I1CCITT(1)
	RETURN
	END
