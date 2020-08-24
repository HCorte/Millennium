C
C SUBROUTINE TO EXPAND RESULTS BOARDS
C
C V01 29-NOV-2000 UXN Initial release.
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
	SUBROUTINE TGL_GETROW(TRABUF,ROWS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 ROWS(2,TGGNBR,12)
	INTEGER*4 ROW, NUM, I, RCNT, J, LIMIT, IND
C
C
	IND = 0
	LIMIT = TRABUF(TWSRW)
	CALL FASTSET(0,ROWS,TGGNBR*2*12)
	DO 20 J = 1,TRABUF(TWNBET)
	  RCNT = 0
	  DO 10 I = 1,LIMIT
	    CALL ILBYTE(NUM,TRABUF(TWBORD),IND)
	    IND = IND+1
	    ROW = ISHFT(NUM,-4)
	    RCNT = RCNT+1
	    ROWS(1,RCNT,J) = ROW
	    ROW = IAND(NUM,'0F'X)
	    ROWS(2,RCNT,J) = ROW
10	  CONTINUE
20	CONTINUE
C
	RETURN
	END
