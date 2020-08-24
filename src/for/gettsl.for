C
C SUBROUTINE GETTSL
C
C V05 12-JUL-1999 UXN Bet amount now sent in 4 bytes.
C V04 27-NOV-1997 UXN Chages to allow some rows to be played as single,
C                     double etc.
C V03 17-OCT-1993 GXA Removed Type Statement.
C V02 27-JUL-1993 CXK NEEDED ZEXT AROUND TERMES (IND) FOR ROWS BET
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C SUBROUTINE TO EXTRACT TOTO SELECT BET DATA FROM TERMINAL MESSAGE
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETTSL(TERMES,GIND,IND,BETBUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INTEGER*4 BETBUF(*)
	BYTE TERMES(*)
	INTEGER*4 IND, GIND, I, TEMP1, TEMP2
	INTEGER*4 I4TEMP
	BYTE      I1TEMP(4)
	EQUIVALENCE (I1TEMP,I4TEMP) 
C
C GET AMOUNT AND # OF ROWS BET
C
	BETBUF(2) = TERMES(IND)
        I1TEMP(4) = ZEXT(TERMES(IND+1))
        I1TEMP(3) = ZEXT(TERMES(IND+2))
        I1TEMP(2) = ZEXT(TERMES(IND+3))
        I1TEMP(1) = ZEXT(TERMES(IND+4))
        BETBUF(1) = I4TEMP
	BETBUF(1)=BETBUF(1)*TSLPRC(GIND)
	IF(BETBUF(2).LT.1.OR.BETBUF(2).GT.6) RETURN
	IND=IND+5
C
C GET ROWS BET
C
	DO 10 I=0,BETBUF(2)-1
	    TEMP1=ZEXT(TERMES(IND))
	    BETBUF(4+I*3)=IAND(TEMP1,3)
	    BETBUF(3+I*3)=ISHFT(TEMP1,-2)
	    IND=IND+1
10	CONTINUE

	RETURN

	END
