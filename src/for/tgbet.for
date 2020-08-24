
C SUBROUTINE TGBET
C
C V01 01-DEC-2000 UXN INITIAL RELEASE.  
C
C BUILD BET IMAGE FOR TOTOGOLO TRANSACTIONS
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
        SUBROUTINE TGBET(TRABUF,CBIMAGE)
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
        INTEGER*4 L                    !
	INTEGER*4 MARKS(0:15)
	DATA MARKS/0,1,1, 2,1, 2, 2,  3,1,2,2,3,2,3,3,4/
	CHARACTER*1 CR1(0:8)/'?','0','1','?','2','?','?','?','M'/
	CHARACTER*2 CR2(0:15)/'??','??','??','01','??','02','12',
     *                        '??','??','0M','1M','??','2M','??',
     *                        '??','??'/
	CHARACTER*3 CR3(0:15)/'???','???','???','???','???','???','???',
     *                        '012','???','???','???','01M','???','02M',
     *                        '12M','???'/

        INTEGER*4  ROWS(2,TGGNBR,12)        !
	INTEGER*4  R,K
	CHARACTER*1 CONT(1:2)/':',' '/
C
C
        CALL TGL_GETROW(TRABUF,ROWS)
        DO J=1,TRABUF(TWNBET)
            CBIMAGE(J) = ' '
	    L=5
            DO I = 1, TRABUF(TWSRW)
		DO K=1,2
		    R = ROWS(K,I,J)
		    IF(MARKS(R).LT.2) THEN
		       CBIMAGE(J)(L:) = CR1(R)
		       L = L + 1
		    ELSEIF(MARKS(R).EQ.2) THEN
		       CBIMAGE(J)(L:) = CR2(R)
		       L = L + 2
		    ELSEIF(MARKS(R).EQ.3) THEN
		       CBIMAGE(J)(L:) = CR3(R)
		       L = L + 3
		    ELSE
		       CBIMAGE(J)(L:) = '012M'
		       L = L + 4
		    ENDIF
		    CBIMAGE(J)(L:) = CONT(K)
		    L = L + 1
	        ENDDO
            END DO
        END DO

        RETURN
        END
