C
C FUNCTION BETLIN
C $Log:   GXAFXT:[GOLS]BETLIN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:15:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   12 Nov 1993  3:16:42   HXK
C  CHANGED REPRESENTATION OF BETS IN FINLAND TO 1X2.
C  
C     Rev 1.0   21 Jan 1993 15:41:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - betlin.for **
C
C BETLIN.SWD
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	CHARACTER*29 FUNCTION BETLIN(OFFSETX,NUMROWX)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4  ROWNRS(6) , TAB1X2(6)
	INTEGER*4 OFFSET,OFFSETX,NUMROW,NUMROWX,IND,I
C
	CHARACTER*1      S(0:4) /' ','1','2',' ','X'/
	CHARACTER*29     BET
C
	OFFSET=OFFSETX
	NUMROW=NUMROWX
	IF (OFFSET.EQ.0.OR.NUMROW.LT.1.OR.NUMROW.GT.6) THEN
	   BET = '     --- NOT INITIALIZED ---                  '
	   RETURN
	ENDIF
	CALL OFFODDS(OFFSET,NUMROW,ROWNRS,TAB1X2)
	BET = '                                              '
	IND=-4
	DO 10 I=1,6
	   IF (ROWNRS(I).EQ.0) GOTO 10
	   IND=IND+5
	   WRITE (BET(IND:IND+1),'(I2.2)') ROWNRS(I)
	   BET(IND+2:IND+2) = '-'
	   BET(IND+3:IND+3) = S(TAB1X2(I))
10	CONTINUE
	BETLIN=BET
	RETURN
	END
