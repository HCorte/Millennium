C
C SUBROUTINE LOOPSUB
C $Log:   GXAFIP:[GOLS]LOOPSUB.FOV  $
C  
C     Rev 1.1   13 Jan 1997 17:01:36   RXK
C  Minor changes
C  
C     Rev 1.0   17 Apr 1996 13:55:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:55:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - spe_loopsub.for **
C
C LOOPSUB.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01   11-OCT-89   LOU R.   INITIAL RELEASE FINLAND.
C
C SUBROUTINE TO GENERATE LOOPBACK MESSAGE BACK TO TERMINAL
C
C CALLING SEQUENCE:
C     CALL LOOPSUB(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
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
	SUBROUTINE LOOPSUB(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*2 OUTLEN
	INTEGER*4 MESTAB(40), RESERVE2, RESERVE1, STOP_CMD
        BYTE BSTOP_CMD(10) /Z20,ZAC,8*Z00/
C
	IF(P(LOOPDLAY).EQ.0.AND.P(LOOPOUT).EQ.0.AND.P(LOOPIN).EQ.
     *	   0) THEN
          CALL MOVBYT(BSTOP_CMD,1,MESTAB,1,10)
	  OUTLEN=10
	  RETURN
	ELSE
	  CALL MOVBYT(P(LOOPDLAY),1,MESTAB,3,2)    !PLACE DELAY IN MESSAGE
	  RESERVE1=0
	  CALL MOVBYT(RESERVE1,1,MESTAB,5,2)       !RESERVED FOR FUTURE
	  RESERVE2=0
	  CALL MOVBYT(RESERVE2,1,MESTAB,7,2)       !RESERVED FOR FUTURE
	  CALL MOVBYT(P(LOOPIN),1,MESTAB,9,1)      !PLACE LENGTH TO RETURN
	  CALL MOVBYT(P(LOOPOUT),1,MESTAB,10,1)    !LENTH OF DATA TO SEND
	  IF(P(LOOPOUT).GT.0) OUTLEN=10+P(LOOPOUT) !IN BYTES
          PERFRM(1,PERLBK) = PERFRM(1,PERLBK) + 1
	ENDIF
C
C ECHO BACK INPUT MESSAGE
C
	RETURN
	END
