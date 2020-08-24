C
C SUBROUTINE DEC1X2X
C $Log:   GXAFXT:[GOLS]DEC1X2X.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:50:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:02:46   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dec1x2x.for **
C
C DEC1X2X.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C 24-APR-90   LOU R. INITIAL RELEASE FOR DENMARK.
C
C DECODE BET INTO OUR FORMAT FOR 1X2
C
C input:
C
C       ALIAN2 - FOREIGN REPRESENTATION OF BET
C       RECCNT - NUMBER OF BETS DECODED / SYSTEM
C       RBET - NUMBER OF ROWS ON THIS BET
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
	SUBROUTINE DEC1X2X(ALIAN2,NUM,RECCNT,RBET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	CHARACTER*5 ALIAN2
C
	INTEGER*4 I, RBET, RECCNT, NUM
	INTEGER*4 RINDX /0/
	INTEGER*4 VALUE(14)
	CHARACTER*5 DISTAB2(SPGNBR)
	CHARACTER*5 ASCREP2(14)
	DATA VALUE /1,2,4,3,6,5,7,1,2,4,3,6,5,7/
	DATA ASCREP2 /'  1  ','   X ','    2',
     *	             '  1X ','   X2','  1 2',
     *	             '  1X2',
     *	             '  1  ','   x ','    2',
     *	             '  1x ','   x2','  1 2',
     *	             '  1x2'/
C
C
	RINDX=RINDX+1
	IF(RINDX.EQ.1)THEN
	   DO 10 I=1,SPGNBR           !initialize table for report ...
	      DISTAB2(I) = '     '
10	   CONTINUE
	ENDIF
C
	NUM=1
	DO 20 I=1,14
	  IF(ALIAN2.EQ.ASCREP2(I)) THEN
	     NUM=VALUE(I)
	     DISTAB2(RINDX)=ASCREP2(I)
	     GOTO 30
	  ENDIF
20	CONTINUE
	WRITE(5,999)RECCNT,ALIAN2
	RETURN
30	CONTINUE
C
	IF(RINDX.EQ.RBET) THEN
	  WRITE(7,900) RECCNT, (DISTAB2(I),I=1,RBET)
	  RINDX=0
	ENDIF
	RETURN
C
900	FORMAT(2X,' rec cnt ',I6,' SPORTS BET ',13A5)
999	FORMAT(2X,' rec cnt ',I6,' HAS INVALID BET ',A5)
C
	END
