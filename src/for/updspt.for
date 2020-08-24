C
C SUBROUTINE UPDSPT
C
C UPDSPT.FOR
C
C V05 01-MAR-2000 UXN XDRAW and PRZCOM removed.
C V04 03-FEB-1994 HXK SET XDRAW TO ZERO ALWAYS.
C V03 07-OCT-1993 GXA Released for Finland Dec Conversion / Oddset.
C                     (Skip pool updating if last draw was postponed).
C V02 21-JAN-1003 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SUBROUTINE TO UPDATE VAKIO POOLS
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE UPDSPT(TRABUF,DUMMY,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
C
	INTEGER*4 DUMMY
	INTEGER*4 IN_QUEUE, ST, I, BASE, POOLNR, POOLBET, PGNUM
	INTEGER*4 GIND, STATUS
	INTEGER*4 OFFSETS(20)
C
C CHECK IF POOLS LOTTO POOLS ARE ACTIVE
C FOR THIS GAME.
C
	STATUS=1
	IF(P(POOLACT).NE.0) RETURN
        IF(P(SUPPUD).NE.0) RETURN
C
C UPDATE POOLS
C
C
	IF (TRABUF(TGAMTYP).NE.TSPT) RETURN
	GIND=TRABUF(TGAMIND)
	PGNUM=LTPOOL_GAMENR(TSPT,GIND)
C
C
	STATUS=2
	IF(PGNUM.LT.1.OR.PGNUM.GT.LTNUMGAMES)  RETURN
	IF(TRABUF(TWBEG).GT.LTPOOLDRAW(PGNUM)) RETURN
	IF(TRABUF(TWEND).LT.LTPOOLDRAW(PGNUM)) RETURN
C
C     PROCESS NON SYSTEM BET
C
	IF (TRABUF(TWSYSN).EQ.0) THEN
	    POOLBET=LTPOOLBET(PGNUM)
	    POOLNR=LTPOOLNR(PGNUM)
	    CALL SPTOFF(TRABUF(TWNBET),TRABUF(TWBORD),POOLBET,OFFSETS)
	    BASE=LTPOOL_BASEOFF(PGNUM)
	    DO 70  I=1,TRABUF(TWNBET)
	        IF(TRABUF(TTYP).EQ.TCAN.OR.TRABUF(TTYP).EQ.TINC) THEN
	           OFFSETS(I)=-OFFSETS(I)-BASE
	        ELSE
	           OFFSETS(I)=OFFSETS(I)+BASE
	        ENDIF
60	        CONTINUE
	        CALL ABL(OFFSETS(I),LTOQ1(1,LTQNUM),STATUS)
	        IF (STATUS .NE. 0) THEN
	          CALL XWAIT(50,1,ST)
	          GOTO 60
	        ENDIF
70	    CONTINUE
	ELSE
C
C     PROCESS NOW SYSTEM BET
C
	      OFFSETS(1)=OFFSIZE*(LTNUMPAG+1)+TRABUF(TWSYSN)
	      IF(TRABUF(TTYP).EQ.TCAN.OR.TRABUF(TTYP).EQ.TINC)
     *	        OFFSETS(1)=-OFFSETS(1)
	      IN_QUEUE=5            !# OF ELEMENTS TO FOLLOW
	      OFFSETS(2)=IN_QUEUE
	      OFFSETS(3)=PGNUM
	      OFFSETS(4)=TRABUF(TWBORD)
	      OFFSETS(5)=TRABUF(TWBORD+1)
C
C     ADDED PASSING U-SYSTEM DEFINITION
C
	      OFFSETS(6)=TRABUF(TWBORD+2)
	      OFFSETS(7)=TRABUF(TWBORD+3)
C
C
C***       DO 90, I=1,4
	      DO 90, I=1,IN_QUEUE+2
80	        CONTINUE
	        CALL ABL(OFFSETS(I),LTOQ1(1,LTQNUM),STATUS)
	        IF (STATUS .NE. 0) THEN
	          CALL XWAIT(50,1,ST)
	          GOTO 80
	        ENDIF
90	      CONTINUE
	ENDIF
	RETURN
	END
