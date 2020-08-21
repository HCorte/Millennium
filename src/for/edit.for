C SUBROUTINE EDIT
C  
C V02 03-MAR-2000 OXK Vakio changes
C V01 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C SUBROUTINE TO EDIT SYSTEM BET DEFINITIONS
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
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE EDIT(SYS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSSF.DEF'
C
C
	INTEGER*4 OFF, K, I, EXT, ROW, NUMROW, BROWS, IND, ST, SYS
	INTEGER*4 ROWTAB(SPGNBR)
C
	COMMON /SCOMON/SSFREC
C
	IF(SSFATR(SYS).EQ.FULSYS) THEN
	  TYPE*,' Full system definition - no rows to edit '
	  CALL XWAIT(2,2,ST)
	  RETURN
	ENDIF
C
C
	IND=SSFPTR(SYS)
	BROWS=SPGNBR
	NUMROW=SSFTAB(IND)
10	CONTINUE
	CALL INPNUM('Enter bet # to re-enter E-done ',ROW,1,NUMROW,EXT)
	IF(EXT.LT.0) RETURN
C
C
	DO 20 I=1,SPGNBR
	ROWTAB(I)=1
20	CONTINUE
C
C
30	CONTINUE
	CALL CLRSCR(6)
	TYPE*,'Entry of bet ',ROW ,' System ',SYS
	DO 50 I=1,BROWS
	CALL GETENT(ROWTAB(I),I,ST)
	IF(ST.EQ.-2) THEN
	  DO 40 K=1,SPGNBR
	  ROWTAB(K)=1
40	  CONTINUE
	  GOTO 30
	ENDIF
	IF(ST.EQ.-1) THEN
	  TYPE*,'Bet ',ROW,' Completed'
	  GOTO 100
	ENDIF
50	CONTINUE
C
C
100	CONTINUE
	OFF=IND+(ROW-1)*BROWS+1
	DO 110 I=1,BROWS
	SSFTAB(OFF)=ROWTAB(I)
	OFF=OFF+1
110	CONTINUE
	GOTO 10
	END
