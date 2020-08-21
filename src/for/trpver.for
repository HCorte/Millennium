C
C SUBROUTINE TRPVER
C
C TRPVER.FOR
C
C V02 28-MAY-1999 UXN DTRCMB ADDED. PARTIALLY REWRITTEN.
C V01 XX-XXX-XXXX RXK INITIAL RELEASE.
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF TODAY'S TRIO RESULTS.
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE TRPVER(GNUM,GIND,DRAW,ST)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

C---- Local variables.

	INTEGER*4 GIND
	INTEGER*4 DRAW
	INTEGER*4 K
	INTEGER*4 NUM
	INTEGER*4 EXT
	INTEGER*4 FLAG
	INTEGER*4 ST
	INTEGER*4 GNUM
	INTEGER*4 ROW
	INTEGER*4 I, J
        INTEGER*4 WINNERS(3)
	CHARACTER*6 WHICH(4) /'first ','second','third ','fourth'/

	CHARACTER*55 STRING
        INTEGER*4 WINNUM(3,MAXTRPTI),TOT_WIN
C------------------------- Start of code  -----------------------------

10      CONTINUE

	CALL FASTMOV(DTRSTA,TRROWSTSV(1,1),3*MAXTRPRW)
	CALL FASTSET(0,DTRHLD(1,1),2*MAXSTRTI)

20      CONTINUE

        DO 50 I=1,3
           IF(DTREST(I).EQ.GAMNUL) GOTO 50
           WRITE(5,910)
           WRITE(5,909) IAM(),I,(DTRENM(K,I),K=1,TRPENM_LEN/4)
           WRITE(STRING,800) I

C---- Ask for cancellation of a row in event

25         CONTINUE
           CALL WIMG(5,STRING)
           CALL YESNO(FLAG)
           IF (FLAG .NE. 1) GOTO 30
           CALL INPNUM('Enter row number to cancel ',ROW,1,MAXTRPRW,EXT)
           IF (EXT .LT. 0) GOTO 30
           CALL TRP_CANROW(I,ROW,TRROWSTSV)
           GOTO 25

30         CONTINUE

           DO ROW=1,MAXTRPRW
              IF(TRROWSTSV(ROW,I).EQ.GAMOPN) GOTO 35
           ENDDO
           WRITE(5,913) IAM(),I,(DTRENM(K,I),K=1,TRPENM_LEN/4)

C---- Ask for winners

35         CONTINUE
           DO K=1,MAXTRPTI
             WINNUM(I,K) = 0
           ENDDO
	   WRITE(STRING,950) I
	   CALL INPYESNO(STRING,FLAG)
	   WINNERS(I) = 1
	   IF(FLAG.EQ.1) THEN
	     CALL INPNUM('Enter number of ties ',WINNERS(I),1,4,EXT)
	     IF(EXT.NE.0) GOTO 35
	   ENDIF	   
           DO J=1,WINNERS(I)
40	     CONTINUE
	     IF(WINNERS(I).EQ.1) THEN
		IF(I.EQ.1) THEN
		    WRITE(STRING,970) I
		ELSE
		    WRITE(STRING,971) I
	        ENDIF		    
	     ELSE
	        WRITE(STRING,960) WHICH(J),I
	     ENDIF
             CALL INPNUM(STRING,NUM,1,MAXTRPRW,EXT)
	     IF(WINNERS(I).EQ.1.AND.EXT.EQ.-5) THEN
                DTRSTS=GAMCAN
                WRITE(5,907) IAM(),(DTRMNM(K),K=1,TRPENM_LEN/4)
                CALL FASTSET(0,DTRHLD,3*MAXTRPTI)
                GOTO 60
             ENDIF
             IF(EXT.NE.0) GOTO 35      
             IF(TRROWSTSV(NUM,I).NE.GAMOPN) THEN
                WRITE(5,908) IAM(),NUM,(DTRNMS(K,NUM,I),K=1,TRPNMS_LEN/4)
                GOTO 40
             ENDIF
	     WRITE(5,980) IAM(),NUM,(DTRNMS(K,NUM,I),K=1,TRPNMS_LEN/4)   
	     CALL INPYESNO('Is this correct (Y/N) ',FLAG)
	     IF(FLAG.NE.1) GOTO 40
	     WINNUM(I,J) = NUM
           ENDDO
50      CONTINUE

        DO 55 I=1,3
           IF(DTREST(I).EQ.GAMNUL) GOTO 55
           WRITE(5,912) IAM(),I,(DTRENM(K,I),K=1,TRPENM_LEN/4)
           DO J=1,4
              IF(WINNUM(I,J).NE.0) WRITE(5,911) IAM(),J,WINNUM(I,J),
     *           (DTRNMS(K,WINNUM(I,J),I),K=1,TRPNMS_LEN/4)
           ENDDO
55	CONTINUE
        CALL PRMYESNO('Is this correct <Y/N> ',FLAG)
        IF (FLAG .NE. 1) GOTO 10
C
C Set correct winning combinations
C
        TOT_WIN = 0
        DO 58 I=1,MAXTRPTI
           IF(WINNUM(1,I).LE.0) GOTO 58
           IF(DTREST(2).EQ.GAMNUL) THEN
              TOT_WIN = TOT_WIN + 1
              DTRHLD(1,TOT_WIN) = WINNUM(1,I)
              DTRHLD(2,TOT_WIN) = 0
              DTRHLD(3,TOT_WIN) = 0
              GOTO 58
           ENDIF
           DO 57 J=1,MAXTRPTI
              IF(WINNUM(2,J).LE.0) GOTO 58
              IF(DTREST(3).EQ.GAMNUL) THEN
                 TOT_WIN = TOT_WIN + 1
                 DTRHLD(1,TOT_WIN) = WINNUM(1,I)
                 DTRHLD(2,TOT_WIN) = WINNUM(2,J)
                 DTRHLD(3,TOT_WIN) = 0
                 GOTO 57
              ENDIF
              DO 56 K=1,MAXTRPTI
                 IF(WINNUM(3,K).LE.0) GOTO 57
                 TOT_WIN = TOT_WIN + 1
                 DTRHLD(1,TOT_WIN) = WINNUM(1,I)
                 DTRHLD(2,TOT_WIN) = WINNUM(2,J)
                 DTRHLD(3,TOT_WIN) = WINNUM(3,K)
56            CONTINUE
57         CONTINUE
58      CONTINUE

60      CONTINUE

80	CONTINUE

	DO I = 1,3
	   DO J=1, MAXTRPTI
	      IF (DTRWIN(I,J) .NE. DTRHLD(I,J)) THEN
	         TYPE*,IAM(),'Verification error, please re-enter '
	         OPDONE = 0
	         DTRSTS = GAMBFD
	         ST = -1
	         RETURN
	      ENDIF
 	   ENDDO
	ENDDO

C---- Verify Cancels

	DO I=1,3
	   DO J = 1,MAXTRPRW
	      IF (TRROWSTSE(J,I) .NE. TRROWSTSV(J,I)) THEN
	         TYPE*,IAM(),'Verification error in cancels, please re-enter '
	         OPDONE = 0
	         DTRSTS = GAMBFD
	         ST = -1
	         RETURN
	      ENDIF
	   ENDDO
	ENDDO

	ST = 0
	DTRSTS = GAMENV

C--------------------- Format Statements -----------------------------
800     FORMAT('Do you want to cancel a row in event ',I1,'  (Y/N)')
801     FORMAT('Enter number of winners in event ',I1,' (C=cancel event)')
802     FORMAT('Enter winning row ',I1,' in event ',I1)

901	FORMAT(1X,A,1X,A8,I1,' draw ',I4,' Master event  ',
     *         <TRPENM_LEN/4>A4)
907     FORMAT(1X,A,'Master Event ',1X,<TRPENM_LEN/4>A4,' cancelled')
908     FORMAT(1X,A,'Row ',I2,1X,<TRPNMS_LEN/4>A4,' not open')
909     FORMAT(1X,A,'Event ',I1,2X,<TRPENM_LEN/4>A4)
910     FORMAT(1X,A)
911     FORMAT(1X,A,'Winner # ',I1,'  Row ',I2,2X,<TRPNMS_LEN/4>A4)
912     FORMAT(1X,A,'Event ',I1,2X,<TRPENM_LEN/4>A4)
913     FORMAT(1X,A,' Event ',I1,2X,<TRPENM_LEN/4>A4,' 0 open rows')
950	FORMAT('Are there any tied winners for event ',I1,' (Y/N)')
960	FORMAT('Enter ',A6,' winning row in event ',I1)
970	FORMAT('Enter winning row in event ',I1,' <C to Cancel Event>')
971	FORMAT('Enter winning row in event ',I1)
980	FORMAT(1X,A,'Row ',I2,1X,<TRPNMS_LEN/4>A4)
	RETURN
	END
