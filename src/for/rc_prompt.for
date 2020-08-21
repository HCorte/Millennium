C
C V01 30-JUN-2000 UXN Initial release.
C
C SUBROUTINE RC_PROMPT(CDC,TIME,ST)
C Input:
C        None
C Output:
C        CDC  - CDC  for too late played tickets
C        TIME - time for too late played tickets
C        ST   - <>1 - nothing was sold too late
C                 1 - ticket that are played later than CDC & TIME should
C                     be refunded and odds should be recalculated.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE RC_PROMPT(CDC,TIME,ST)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 CDC, TIME, ST
C
	INTEGER*4 EXT,K
	INTEGER*2 DAT(LDATE_LEN)
C
	CALL PRMYESNO(
     *      'Did this event started before the game was closed [Y/N]',ST)
	IF(ST.NE.1) RETURN
	
10      CONTINUE
	TYPE*,IAM()
	CALL PRMNUM('Enter CDC  when this event started', CDC, 1, DAYCDC, EXT) 
	IF(EXT.NE.0) GOTO 10
	CALL PRMTIM('Enter time when this event started', TIME,EXT)
	IF(EXT.NE.0) GOTO 10
C
	DAT(VCDC) = CDC
	CALL LCDATE(DAT)
C
	TYPE*, IAM()
	TYPE 9000, IAM(),(DAT(K),K=7,13),DISTIM(TIME)
	TYPE*, IAM(), 'will be refunded'
	TYPE*, IAM()
C
	CALL PRMYESNO('Is this correct [Y/N]', ST)
	IF(ST.NE.1) GOTO 10
	RETURN
C
9000	FORMAT(1X,A,'All tickets that were sold after ', 7A2, 1X, A8)
	END
