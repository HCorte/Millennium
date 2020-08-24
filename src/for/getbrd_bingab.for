C GETBRD_BINAB.FOR
C
C $Log:   GXAFXT:[GOLS]GETBRD_BINGAB.FOV  
C  
C     Rev 1.0   17 Apr 1996 13:18:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   07 Nov 1994 16:05:10   PXB
C  Bug fixes.
C  
C     Rev 1.0   27 Oct 1994 17:06:00   HXK
C  Initial revision.
C
C This subroutine will extract Bingo AB boards from the logrecord and
C convert into a Row*Col table.
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE GETBRD_BINGAB(LOGREC,BOFF,BOARDS)
	IMPLICIT NONE

	!---- Include files used.

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'

	!---- Local variables.

	INTEGER*4 LOGREC(*)			!Log Record Buffer
	INTEGER*4 BOARDS(BGOCOL,BGOROW)		!Bingo AB bet as numbers. 1Brd
	INTEGER*4 BOFF				!Start Brd Offset in I4's.
	INTEGER*4 ROW			        !Row index into BOARDS
	INTEGER*4 COL				!Column index into BOARDS
	INTEGER*4 IND				!Index into LOGBUF
	INTEGER*4 NUM				!Number decoded (2 bits) 0-3
        INTEGER*4 NUM1                          !Number decoded (2 bits) 0-3
	INTEGER*4 OFF				!I2 Offset into I4 
	INTEGER*2 I2PART			!I2 Part of the I4

	INTEGER*4 I4TEMP
	INTEGER*2 I2TEMP(2)
	BYTE	  I1TEMP(4)
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)


C-------------- Start of subroutine --------------------------------

	IND = BOFF
	OFF = 1
	I4TEMP = LOGREC(IND)

	CALL REVNIB (I1TEMP(1))
	CALL REVNIB (I1TEMP(2))
	CALL REVNIB (I1TEMP(3))
	CALL REVNIB (I1TEMP(4))

	I2PART = ZEXT(I2TEMP(OFF))


	DO COL = 1,BGOCOL

	    !---- Work out the number in row five of this column.

            NUM = IAND(I2PART,'C000'X)
            IF(NUM.LT.0) THEN
                NUM1 = '0002'X
                IF(IAND(NUM,'4000'X).NE.0) NUM1=IOR(NUM1,'0001'X)
                NUM=NUM1
            ELSE
                NUM = ISHFT(NUM,-14)
            ENDIF
            BOARDS(COL,BGOROW) =
     *        ((COL-1)*BGOROW + BGOROW - 1)*4 + NUM + 1


	    !---- Now work out the rest of the numbers in this column.

	    DO ROW = 1,BGOROW-1
		NUM = IAND(I2PART,'00C0'X)
                NUM = ISHFT(NUM,-6)
		I2PART = ISHFT(I2PART,2)   !Shift left
		BOARDS(COL,ROW) =
     *		  ((COL-1)*BGOROW + ROW - 1)*4 + NUM + 1
	    ENDDO
            


	    IF(MOD(COL,2).EQ.0) THEN
		IND = IND + 1
		I4TEMP = LOGREC(IND)
		CALL REVNIB (I1TEMP(1))
		CALL REVNIB (I1TEMP(2))
		CALL REVNIB (I1TEMP(3))
		CALL REVNIB (I1TEMP(4))
	    ENDIF

	    OFF = IEOR(OFF,3)	    !Alternate 1 & 2

	    I2PART = I2TEMP(OFF)
	ENDDO

	RETURN
	END

C----------------- End of code ------------------------------------
