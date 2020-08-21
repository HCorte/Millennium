C GETBRD_BINFH.FOR
C
C $Log:   GXAFXT:[GOLS]GETBRD_BINGFH.FOV  
C  
C     Rev 1.0   17 Apr 1996 13:18:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   18 Nov 1994 10:30:40   HXK
C  Fixed col,row orientation
C  
C     Rev 1.1   07 Nov 1994 16:05:44   PXB
C  Bug fixes
C  
C     Rev 1.0   27 Oct 1994 17:06:04   HXK
C  Initial revision.
C
C This subroutine will extract a Bingo FULL HOUSE board from the transaction
C record and convert into a Row*Col table.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE OGETBRD_BINGFH(TRABUF,BOFF,BOARDS)
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 BOARDS(BGOCOL,BGOROW)		!Bingo FH bet as numbers. 1Brd
	INTEGER*4 BOFF				!Start Brd Offset in I4's.
	INTEGER*4 ROW			        !Row index into BOARDS
	INTEGER*4 COL				!Column index into BOARDS
	INTEGER*4 NUM				!Number decoded (2 bits) 0-3
C
        INTEGER*4 COLUMN
	INTEGER*4 I4TEMP
	INTEGER*2 I2TEMP(2)
	BYTE	  I1TEMP(4)
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
C
	DO ROW = 1,BGOROW
	    DO COL = 1,BGOCOL
                IF(MOD(COL,2).EQ.0) THEN
                   COLUMN = COL-1
                ELSE
                   COLUMN = COL+1
                ENDIF
		CALL GETNIBLE(NUM,TRABUF(BOFF+ROW-1),COLUMN)
		BOARDS(COL,ROW) = (COL-1)*BGONCL+NUM
	    ENDDO
	ENDDO
C
	RETURN
	END

