C  GXSRC:PRINTRA_BGBET.FOR
C  
C  $Log:   GXAFXT:[GOLS]PRINTRA_BGBET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:28:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   20 Dec 1994 11:15:34   HXK
C  Added Bingo seed 
C  
C     Rev 1.0   23 Nov 1994 17:21:26   HXK
C  Initial revision.
C  
C  
C
C
C Format BINGO bet data for TMIR
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE PRINTRA_BGBET (TRABUF,CBETS,LINES)

        IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

C---- Argument.

        INTEGER*4       LINES             !
        CHARACTER*80    CBETS(14)         !

C---- Local Variables used.

	INTEGER*4  Q
	INTEGER*4  COL
	INTEGER*4  ROW

	INTEGER*4  BOARDFH(BGONBB,BGOCOL,BGOROW)


C------------------------- Start of Program --------------------------------

C---- Get Fullhouse Board.

        CALL GETB_BTMBRD(TRABUF,BOARDFH)

C---- Put boards into display format.

        Q = 1
        DO ROW = 1,BGOROW
           IF(ROW.EQ.1) THEN
              WRITE (CBETS(Q),9002) (BOARDFH(1,ROW,COL),COL=1,BGOCOL),
     *                              (BOARDFH(2,ROW,COL),COL=1,BGOCOL),
     *                              (BOARDFH(3,ROW,COL),COL=1,BGOCOL),
     *                              TRABUF(TWBSED)
           ELSEIF(ROW.EQ.2) THEN
              WRITE (CBETS(Q),9003) (BOARDFH(1,ROW,COL),COL=1,BGOCOL),
     *                              (BOARDFH(2,ROW,COL),COL=1,BGOCOL),
     *                              (BOARDFH(3,ROW,COL),COL=1,BGOCOL),
     *                              TRABUF(TWBLUK)/1000,
     *                              MOD(TRABUF(TWBLUK),1000)
           ELSEIF(ROW.EQ.3) THEN
              WRITE (CBETS(Q),9004) (BOARDFH(1,ROW,COL),COL=1,BGOCOL),
     *                              (BOARDFH(2,ROW,COL),COL=1,BGOCOL),
     *                              (BOARDFH(3,ROW,COL),COL=1,BGOCOL),
     *                               TRABUF(TWBBAS)
           ELSE
              WRITE (CBETS(Q),9005) (BOARDFH(1,ROW,COL),COL=1,BGOCOL),
     *                              (BOARDFH(2,ROW,COL),COL=1,BGOCOL),
     *                              (BOARDFH(3,ROW,COL),COL=1,BGOCOL)
           ENDIF
           Q = Q +1
        END DO

        LINES = Q
        RETURN
C
C------------------------- Format Statements -------------------------------


9002    FORMAT (3(3X,5(1X,I2)),4X,'Seed ',I12.12)

9003    FORMAT (3(3X,5(1X,I2)),4X,'Lucky # ',I4.4,'-',I3.3)

9004    FORMAT (3(3X,5(1X,I2)),4X,'Lucky base ',I3.3)

9005    FORMAT (3(3X,5(1X,I2)))

	END

C------------------------- End of Program   --------------------------------
