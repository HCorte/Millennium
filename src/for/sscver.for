C
C SUBROUTINE SSCVER
C  
C
C SUBROUTINE TO PROCESS ENTRY OF WINNING SUPERSCORE RESULTS.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE SSCVER(GNUM,GIND,DRAW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
	INTEGER*4 GIND, DRAW, K, I, NUM, EXT, FLAG, ST, GNUM
	CHARACTER*70 BUF
C
C
        WRITE(5,901) IAM(),GTNAMES(TSSC),GIND,DRAW,
     *               (DSSMNM(I),I=1,SSNMS_LEN/4)
C
	DO I=1,3
	   DSSHLD(1,I)=0 
	   DSSHLD(2,I)=0 
	ENDDO

100     CONTINUE
        DO 112 I=1,3
          IF(DSSEST(I).NE.GAMOPN) GOTO 112
           WRITE (5,903) IAM(),I,(DSSSNM(K,I),K=1,SSNMS_LEN/4)
           WRITE (BUF,902)
           CALL INPNUM(BUF,NUM,0,15,EXT)
           IF(EXT.NE.0.AND.EXT.NE.-5) GOTO 100
           IF(EXT.EQ.-5) THEN
              GOTO 115
           ENDIF
           DSSHLD(1,I)=NUM
C
110        CONTINUE
           WRITE (BUF,9021)
           CALL INPNUM(BUF,NUM,0,15,EXT)
           IF(EXT.NE.0) GOTO 110
           DSSHLD(2,I)=NUM

112     CONTINUE

        DO I=1,3
           IF(DSSEST(I).NE.GAMOPN) GOTO 114
           WRITE(5,903) IAM(),I,(DSSSNM(K,I),K=1,SSNMS_LEN/4),' entered'
           WRITE(5,9031) IAM(),DSSHLD(1,I),DSSHLD(2,I)
        ENDDO
114     CONTINUE
        CALL WIMG(5,'Are the scores entered correct [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 100
C
C CHECK AGAINST OPERATOR ENTRY
C
115     CONTINUE
	DO I=1,3
	   IF(DSSWIN(1,I).NE.DSSHLD(1,I).OR.
     *	      DSSWIN(2,I).NE.DSSHLD(2,I)) THEN
	      TYPE*,IAM(),'Verification error, please re-enter '
	      OPDONE=0
	      DSSSTS=GAMBFD
	      ST=-1
	      RETURN
	   ENDIF
	ENDDO
C
C
	ST=0
	DSSSTS=GAMENV
	RETURN
C
C
901     FORMAT(1X,A,1X,A8,I1,' set ',I4,1X,<SSNMS_LEN/4>A4)
902     FORMAT('Enter home score [C to cancel event]:')
9021    FORMAT('Enter away score ')
903     FORMAT(1X,A,' Set ',I1,1X,<SSNMS_LEN/4>A4,A)
9031    FORMAT(1X,A,' Home Score - Away score  ',I2,' - ',I2)

	END
