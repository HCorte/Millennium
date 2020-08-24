C
C SUBROUTINE SPTOFF
C $Log:   GXAFXT:[GOLS]SPTOFF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:15:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:41:48   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - sptoff.for **
C
C SPTOFF.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C     SPTOFF.FTN
C
C     V01 WS 7/30/89
C
C SPTOFF(NR_OF_BETS,BETS,NUMROWS,OFFSETS) ;CONVERT SPT BET TO OFFSETS
C     IN:
C     NR_OF_BETS - NO OF BASE BETS TO CONVERT
C     BETS    -    BET IN "NIBLE" FORMAT
C     NUMROWS - # OF ROWS IN BET
C     OUT:
C     OFFSETS -    COMBINATIONS OFFSETS
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
	SUBROUTINE SPTOFF(NR_OF_BETS,BETS,NUMROWS,OFFSETS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C***$INCLUDE GLOBAL.DEF/G
C***$INCLUDE SPTCOM.DEF/G
C
	INTEGER*4 SECOND, FIRST, BYTE, TIMES, OLD_VALUE, OLD_OFFSET
	INTEGER*4 BASE, NEXTBET, OFF, NUMROWS
C
	INTEGER*2 BETS(*)
	INTEGER*4 OFFSETS(*)
	INTEGER*4 NR_OF_BETS
	INTEGER*4 CNVTAB(0:4) /0,0,1,0,2/
C
C
	OFF=0                         !BETS OFFSET
	DO 20, NEXTBET=1,NR_OF_BETS
	   OFFSETS(NEXTBET)=1
	   BASE=1                     !BASE TO CALCULATE POOL
	   OLD_OFFSET=OFF
C
	   IF (MOD(NUMROWS,2).NE.0) THEN   !MAKE SURE LAST NIBLE 0
	      CALL GETNIBLE(OLD_VALUE,BETS,OLD_OFFSET*2+NUMROWS+1)
	      CALL SETNIBLE(0,BETS,OLD_OFFSET*2+NUMROWS+1)
	   ENDIF
C
	   DO 10, TIMES=1,(NUMROWS+1)/2
	     CALL ILBYTE(BYTE,BETS,OFF)      !CALCLULATE POOL INCREASE
C                                          ;FOR NEXT 2 ROWS
	     FIRST=BYTE/16
	     FIRST=CNVTAB(FIRST)             !1-ST NIBLE
	     SECOND=IAND(BYTE,15)
	     SECOND=CNVTAB(SECOND)          !2-ND NIBLE
C
	     OFFSETS(NEXTBET)=OFFSETS(NEXTBET)+BASE*FIRST+BASE*3*SECOND
C
	     BASE=BASE*9
	     OFF=OFF+1
10	   CONTINUE
C
	   IF (MOD(NUMROWS,2).NE.0) THEN   !RESTORE LAST NIBLE
	      CALL SETNIBLE(OLD_VALUE,BETS,OLD_OFFSET*2+NUMROWS+1)
	   ENDIF
C
20	CONTINUE
	RETURN
	END
