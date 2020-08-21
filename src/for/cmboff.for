C
C SUBROUTINE CMBOFF.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CMBOFF.FOV                                   $
C  $Date::   17 Apr 1996 12:37:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
C
C
C     GET VIRITUAL OFFSET FOR LOTTO TYPE OF BET
C
C     CALL CMBOFF(BETS,OFFSET,BETTYP)
C     IN - BETS SORTED IN INCREASING ORDER
C          BETTYP - NR OF NRS BET
C
C     OUT - OFFSET - OFFSET FOR LIABILITY TABLE, (STARTING FROM 1)
C
C
C     CALL OFFCMB(BETS,OFFSET,BETTYP)
C     IN - OFFSET - OFFSET CORRESPONDING TO BET COMBINATION
C        - BETTYP - NR OF NRS BET
C     OUT - BETS  - I*4 TABLE WITH NRS BET
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CMBOFF(BETS,OFFSET,BETTYP)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4  MAXNR
	PARAMETER (MAXNR=60)          !MAX OUT OF 61
C
	INTEGER*4 OFF, ACTOFF, I,BETTYP, OFFSET
	INTEGER*4 PERTAB(0:MAXNR,10)
	INTEGER*4 BETS(BETTYP)
C
C
	INTEGER*4 ONCE
	DATA ONCE/0/
C
	IF (ONCE.EQ.0) THEN
	CALL PERM(PERTAB,MAXNR,10,-1) !CREATE PERMUTATIONS TABLE
	   ONCE=-1
	ENDIF
C
C
	OFFSET=0
	DO 200 I=1,BETTYP
	  OFFSET=OFFSET+PERTAB(BETS(I)-1,I)     !INREASE ORDER SORT
200	CONTINUE
	RETURN
C
C
C----------------------
C
C     TO GET NRS HIGHEST VALUE IS SEARCHED THAT IS LOWER THEN OFFSET
C     FOR EACH COLOMN.
	ENTRY OFFCMB(BETS,OFFSET,BETTYP)
C
	IF (ONCE.EQ.0) THEN
	  ONCE=-1
C
	  CALL PERM(PERTAB,MAXNR,10,-1) !CREATE PERMUTATIONS TABLE
	ENDIF
C
C
	ACTOFF=OFFSET-1    !OFFSET IS ARTIFICIALLY INCREMENED BY 1
C                        ;FOR 1-ST NR
	DO 400, OFF=BETTYP,1,-1     !FOR ALL NRS
	   DO 300 I=0,MAXNR
	      IF (OFF.EQ.1) THEN      !FOR 1-ST NR SHOULD BE EQUAL
	         IF (ACTOFF+1.EQ.PERTAB(I,OFF)) GOTO 350
	      ELSE
	         IF (ACTOFF.LT.PERTAB(I,OFF)) GOTO 350  !IT WAS GREATER
	      ENDIF
300	   CONTINUE
C
	   I=MAXNR+1
C
350	   CONTINUE
	   BETS(OFF)=I  !TABLE DEFINED(0:MAXNR) SO - NO ADJUSTMENT
	   IF (I.GT.0) ACTOFF=ACTOFF-PERTAB(I-1,OFF)
400	CONTINUE
	BETS(1)=BETS(1)+1
	RETURN
	END
