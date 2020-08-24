C
C SUBROUTINE CHKPAS
C $Log:   GXAFXT:[GOLS]CHKPAS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:32:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:50:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_chkpas.for **
C
C CHKPAS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	SUBROUTINE CHKPAS(PWORD,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	CHARACTER PWORD(8),BLANK,CR
	INTEGER*4 J, I, ST
	DATA BLANK/Z20/,CR/Z0D/
C
C
	ST=0
	DO 20 I=1,8
	IF(PWORD(I).EQ.CR) THEN
	  DO 10 J=I,8
	  PWORD(J)=BLANK
10	  CONTINUE
	  GOTO 30
	ENDIF
20	CONTINUE
C
C
30	CONTINUE
	DO 40 I=1,8
	IF(PWORD(I).NE.BLANK) RETURN
40	CONTINUE
	ST=-1
	RETURN
	END
