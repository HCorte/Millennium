C
C SUBROUTINE DEC1X2
C $Log:   GXAFXT:[GOLS]DEC1X2.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:50:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:02:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lod1x2pol.for **
C
C DEC1X2.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C 24-APR-90   LOU R. INITIAL RELEASE FOR DENMARK.
C
C DECODE BET INTO OUR FORMAT FOR 1X2
C
C input:
C
C       ALIAN - FOREIGN REPRESENTATION OF BET
C       RECCNT - NUMBER OF BETS DECODED / SYSTEM
C       RBET - NUMBER OF ROWS ON THIS BET
C	NOTATION - CHARACTER*3 CONTAINING DEFINITION LETTERS
C
C output :
C
C       NUM - GTECH REPRESENTATION OF BET
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
	SUBROUTINE DEC1X2(ALIAN,NUM,RECCNT,RBET, NOTATION)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	CHARACTER*3 ALIAN, NOTATION
C
	INTEGER*4 I, RBET, RECCNT, NUM
	INTEGER*4 RINDX /0/
	CHARACTER*3 DISTAB(SPGNBR)
C
C
	RINDX=RINDX+1
	IF(RINDX.EQ.1)THEN
	   DO 10 I=1,SPGNBR           !initialize table for report ...
	      DISTAB(I) = '   '
10	   CONTINUE
	ENDIF
C
C	BUILD NUM AND 'STANDARD' OUTPUT FORMAT FROM THE 'FREE' INPUT
C
	CALL BITMAP1X2(NOTATION, ALIAN, DISTAB(RINDX), NUM)
C
	IF (NUM .EQ. 0) THEN
	    WRITE(5,999) IAM(), RECCNT,ALIAN
	    NUM = 1
	    RETURN
	ENDIF
C
	IF(RINDX.EQ.RBET) THEN
	  WRITE(7,900) RECCNT, (DISTAB(I),I=1,RBET)
	  RINDX=0
	ENDIF
	RETURN
C
900	FORMAT(2X, ' rec cnt ',I6,' SPORTS BET ',13A3)
999	FORMAT(2X, A18, ' rec cnt ',I6,' HAS INVALID BET ',A3)
C
	END
