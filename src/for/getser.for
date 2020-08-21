C
C SUBROUTINE GETSER
C $Log:   GXAFXT:[GOLS]GETSER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:22:40   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:28:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - getser.for **
C
C GETSER.FOR
C
C V02 04-APR-2014 SCML Added support for IGS Placard
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C SUBROUTINE TO ASSIGN SERIAL NUMBERS TO TRASNACTIONS
C CALLING SEQUENCE
C     CALL GETSER(SERIAL,SIZE)
C INPUT
C     SIZE   - RECORD SIZE (IN LOG RECORDS)
C OUTPUT
C     SERIAL - TRANSACTION SERIAL NUMBER  (FIRST FOR MULTI-TRANS)
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
	SUBROUTINE GETSER(SERIAL,SIZE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C----+------------------------------------------------------------------
C V02| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C----+------------------------------------------------------------------
C V02| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
	INTEGER*2 TRANS
	INTEGER*4 LOCALSER/0/               !LOCAL SERIAL NUMBER
	INTEGER*4 INDEX, SIZE, SERIAL
	
C----+------------------------------------------------------------------
C V02| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPS(' 61:GETSER:SERIAL',SERIAL,SERIAL)
            CALL OPS(' 61:GETSER:P(NXTTRA)',P(NXTTRA),P(NXTTRA))
            CALL OPS(' 61:GETSER:PERFRM(1,PERTRA)',PERFRM(1,PERTRA),PERFRM(1,PERTRA))
        ENDIF
C----+------------------------------------------------------------------
C V02| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C ASSIGN THEN INCREMENT SERIAL NUMBER
C AND INCREMENT TRANSACTION COUNT
C
	IF(NXTSER.LT.LOCALSER) THEN
	  IF(LOCALSER.NE.0) THEN
	    TYPE *,'invalid nxtser ',NXTSER,' should be ',LOCALSER
	    TYPE *,'reassigned to ',LOCALSER
	    NXTSER=LOCALSER
	  ELSE
	    LOCALSER=NXTSER
	  ENDIF
	ENDIF
	SERIAL=NXTSER
	NXTSER=NXTSER+1
	LOCALSER=LOCALSER+1
C----+------------------------------------------------------------------
C V02| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPS(' 80:GETSER:SERIAL',SERIAL,SERIAL)
            CALL OPS(' 80:GETSER:P(NXTTRA)',P(NXTTRA),P(NXTTRA))
            CALL OPS(' 80:GETSER:PERFRM(1,PERTRA)',PERFRM(1,PERTRA),PERFRM(1,PERTRA))
        ENDIF
C----+------------------------------------------------------------------
C V02| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C INCREMENT SERIAL NUMBER FOR MULTI-RECORD TRANSACTIONS
C
	IF(SIZE.EQ.3) THEN
	  INDEX=MOD(SERIAL-1,LBLK)+1
	  IF(INDEX.EQ.LBLK-1)THEN
	    NXTSER=NXTSER+1
	    LOCALSER=LOCALSER+1
	  ELSE IF(INDEX.LT.LBLK-1)THEN
	    NXTSER=NXTSER+2
	    LOCALSER=LOCALSER+2
	  ENDIF
	  GOTO 20
	ENDIF
C
	IF(SIZE.EQ.2) THEN
	  INDEX=MOD(SERIAL-1,LBLK)+1
	  IF(INDEX.EQ.LBLK) GOTO 20
	  NXTSER=NXTSER+1
	  LOCALSER=LOCALSER+1
	ENDIF
C
20	CONTINUE
	P(NXTTRA)=P(NXTTRA)+1
	PERFRM(1,PERTRA)=PERFRM(1,PERTRA)+1
	SERIAL=SERIAL+SYSSER
C----+------------------------------------------------------------------
C V02| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPS('109:GETSER:SERIAL',SERIAL,SERIAL)
            CALL OPS('109:GETSER:P(NXTTRA)',P(NXTTRA),P(NXTTRA))
            CALL OPS('109:GETSER:PERFRM(1,PERTRA)',PERFRM(1,PERTRA),PERFRM(1,PERTRA))
        ENDIF
C----+------------------------------------------------------------------
C V02| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
	RETURN
	END
