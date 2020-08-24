C SUBROUTINE SPBET
C  
C V08 30-MAR-2015 MTK Modified Super 14 game
C V07 11-apr-2011 RXK Display of QP flags skipped
C V06 29-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C V05 22-FEB-2000 OXK More fixes to layout
C V04 14-FEB-2000 OXK Layout fixed to fit 14 rows in one line (Vakio changes)
C V03 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V02 08 Jul 1993 SXH Released for Finland
C V01 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C BUILD BET IMAGE FOR SPORTS TRANSACTIONS
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
        SUBROUTINE SPBET(TRABUF,CBIMAGE)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

        ! ARGUMENTS
	CHARACTER*56 CBIMAGE(12)       !

        ! variables
        INTEGER*4 I                    !
        INTEGER*4 J                    !
        INTEGER*4 K                    !
        INTEGER*4 L                    !
        INTEGER*4 ROWS(SPGNBR,12)      !
	INTEGER*4 RROWS(2,TGGNBR,12)
        INTEGER*4 FLAGS(32)            !
	INTEGER*4 BCNT

	CHARACTER*3 ROW(0:7)
        DATA ROW/'---','1--','-X-','1X-','--2',
     *           '1-2','-X2','1X2'/

	CHARACTER*3 RROW(0:7)
        DATA RROW/'---','0--','-1-','01-','--M',
     *            '0-M','-1M','01M'/

C
        CHARACTER*4 QP(3)              !
        DATA QP/'    ','QP  ','WQP '/
C
C
C
	BCNT = 0
	IF(TRABUF(TWSPFRG).NE.0) BCNT = 1

        CALL GETROW(TRABUF,ROWS,RROWS)
        DO J=1,TRABUF(TWNBET)
	    L=5
            DO I = 1, TRABUF(TWSRW)-BCNT
                CBIMAGE(J)(L:L+2) = ROW(ROWS(I,J))
		L=L+3
		IF(MOD(I,2).EQ.0) THEN
		   CBIMAGE(J)(L:L) = ' '
		   L=L+1
		ENDIF
            END DO
        END DO
C
C SUPER14 (RESULTS) ROW
C
        IF(TRABUF(TWSPFRG).EQ.1) THEN
          K=TRABUF(TWNBET)+1
          CBIMAGE(K)(1:4) = QP(1)
          CBIMAGE(K)(5:7) = RROW(RROWS(1,1,1))
          CBIMAGE(K)(8:8) = ' '
          CBIMAGE(K)(9:11) = RROW(RROWS(2,1,1))
          CBIMAGE(K)(12:12) = ' '
        ENDIF

C SUPERR 14 STANDARD 1X2 ROW

        IF(TRABUF(TWSPFRG).EQ.2) THEN
          K=TRABUF(TWNBET)+1
          CBIMAGE(K)(1:4) = QP(1)
          CBIMAGE(K)(5:7) = ROW(RROWS(1,1,1))
          CBIMAGE(K)(8:12) = '     '
        ENDIF

        RETURN
        END
