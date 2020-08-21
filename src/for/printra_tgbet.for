C
C V01 29-NOV-2000 UXN INITIAL RELEASE.
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
        SUBROUTINE PRINTRA_TGBET(TRABUF,BETS,LINES)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
C
        ! arguments
	CHARACTER*80 BETS(14)
        INTEGER*4  LINES                  !

        ! variables
        INTEGER*4  I                      !
        INTEGER*4  J, K                      !
        INTEGER*4  ROW(0:15)              !
        INTEGER*4  ROWS(2,TGGNBR,12)        !
        INTEGER*4  FLAGS(32)              !

        DATA ROW/'----','0---','-1--','01--','--2-',
     *           '0-2-','-12-','012-','---M','0--M',
     *           '-1-M','01-M','--2M','0-2M','-12M',
     *           '012M'/
C
C
        CALL GETFLG(TRABUF(TWQPF),FLAGS,TRABUF(TWNBET))

        CALL TGL_GETROW(TRABUF,ROWS)
        DO J = 1,TRABUF(TWNBET)
           K=FLAGS(J)+1
	   BETS(J) = ' '
           DO I = 1,TRABUF(TWSRW)
              IF(I.LE.TGGNBR) THEN
                 WRITE(BETS(J)((I-1)*10+1:),900) ROW(ROWS(1,I,J)),
     *                                           ROW(ROWS(2,I,J))
              ENDIF
           END DO
        END DO

        LINES = TRABUF(TWNBET)+1

        RETURN
900	FORMAT(A4,':',A4)
        END
