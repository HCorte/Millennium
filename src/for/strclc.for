C SUBROUTINE STRCLC
C
C V04 30-JUN-2000 UXN Refund too late played tickets.
C V03 17-MAY-2000 OXK ACTUAL_WINS initialized to 0
C V02 09-AUG-1999 UXN Fix for changing odds for tied winners.
C V01 18-MAY-1999 UXN INITIAL RELEASE.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT

        SUBROUTINE STRCLC(GNUM)
        IMPLICIT NONE

C---- Include files used.

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:RESCOM.DEF'
        INCLUDE 'INCLIB:STROCOM.DEF'
        INCLUDE 'INCLIB:STRFREC.DEF'
C
	COMMON/RESULTS_STRFREC/ STRFREC
C
	INTEGER*4 GNUM
C---- Local variables used.
	

        INTEGER*4 PFDB(7)
        INTEGER*4 ST
        INTEGER*4 RW1,RW2,RW3
        INTEGER*4 FLAG
        INTEGER*4 EXT
        INTEGER*4 I, K, J, IND1,IND2,IND3,IND
        INTEGER*4 ACTUAL_WINS
        INTEGER*4 TOTSAL
        INTEGER*4 UCID
	INTEGER*4 CDC,TIME
	
	INTEGER*4 ODDS
        REAL*8 TOTPOL
	REAL*8 TOTAL(MAXSTRTI)
        REAL*8 RODDS
        REAL*8 RODDS_ALL(MAXSTRTI)
	REAL*8 WINS

C
	CALL RC_PROMPT(CDC,TIME,ST)
	IF(ST.EQ.1) THEN
	    CALL RESPOL_TSTR(GNUM,CDC,TIME)
	    GOTO 10
	ENDIF
C
C---- Get winning odds.
C---- open and read today's triple pool file
C
5	CONTINUE
        CALL OPENQW(4,DSTPFN,4,0,0,ST)
        IF(ST.NE.0) THEN
           WRITE(6,904) (DSTPFN(K),K=1,4),ST
           CALL GPAUSE
	   GOTO 5
        ENDIF
        CALL IOQINIT(PFDB,4,STRFSEC*256)
        CALL READQW(PFDB,1,STRFREC,ST)
	CALL CLOSEQFIL(PFDB)
C
10	CONTINUE
C
C---- Calculate total sales from sales per combination
C
        TOTSAL=0
	ACTUAL_WINS=0

	DO RW1=1,MAXSTRRW
	   DO RW2 = 1,MAXSTRRW
	      DO RW3=1,MAXSTRRW
	         UCID = (RW3-1)*MAXSTRRW*MAXSTRRW + (RW2-1)*MAXSTRRW+RW1
                 IF(STROWSTS(RW1,1).EQ.GAMOPN .AND.
     *              STROWSTS(RW2,1).EQ.GAMOPN .AND.
     *              STROWSTS(RW3,1).EQ.GAMOPN) THEN
                      TOTSAL = TOTSAL + STRFODDS(STRGAMT,UCID)
                 ENDIF
              ENDDO
	   ENDDO
        ENDDO

        TOTPOL = DFLOAT(TOTSAL) * CALPER(DSTSPR)
        TOTPOL = TOTPOL + DFLOAT(DSTPOL(1))+DFLOAT(DSTBRK(1))
        DSTTPL = IDINT(TOTPOL)

	DO 20 I = 1, MAXSTRRW
           RODDS_ALL(I) = 0.0D0
           TOTAL(I) = TOTPOL * DFLOAT(DYN_BETUNIT)
20      CONTINUE

        DO I=1,DSTCMB
           RODDS = 0.0D0
           IND1 = DSTWIN(1,I)
           IND2 = DSTWIN(2,I)
	   IND3 = DSTWIN(3,I)
           IND  = (IND3-1)*MAXSTRRW*MAXSTRRW+(IND2-1)*MAXSTRRW+IND1
           WINS = DFLOAT(STRFODDS(STRGAMT,IND)*DYN_BETUNIT)

           IF(WINS.NE.0.0D0) THEN
              RODDS = TOTAL(I) / WINS
              ACTUAL_WINS = ACTUAL_WINS + 1
           ELSE
              RODDS = 1.0D0
           ENDIF

           RODDS_ALL(I) = RODDS
           ODDS = IDNINT(RODDS*100.0D0)
           DSTODS(I) = ODDS
           IF(DSTODS(I).LT.100) DSTODS(I) = 100
           WRITE(6,901) IAM(),I,IND1,(DSTNMS(K,IND1),K=1,3),
     *                          IND2,(DSTNMS(K,IND2),K=1,3),
     *                          IND3,(DSTNMS(K,IND3),K=1,3),
     *                          DSTODS(I)/100,MOD(DSTODS(I),100)
           CALL PRMYESNO('Do you want to change these odds (Y/N) ',FLAG)
           IF(FLAG.EQ.1) THEN
50            CONTINUE
              CALL INPNUM('Enter new odds [100-999999] :',DSTODS(I),
     *                          100,999999,EXT)
              WRITE(6,902) IAM(),DSTODS(I)/100,MOD(DSTODS(I),100)
              CALL PRMYESNO('Are the new odds entered correct (Y/N) ',FLAG)
              IF(FLAG.NE.1) GOTO 50
              RODDS_ALL(I) = DFLOAT(DSTODS(I))/100.0D0
           ENDIF
        ENDDO

        IF(DSTCMB.GT.1 .AND. ACTUAL_WINS.GT.0) THEN
           TYPE*,IAM()
           TYPE*,IAM(),'Final Adjusted odds because of ties '
           TYPE*,IAM(),'=================================== '
           TYPE*,IAM(),'Number of possible winning ties:',DSTCMB
           TYPE*,IAM(),'Number of actual winning ties:  ',ACTUAL_WINS
           TYPE*,IAM()
           DO J = 1,ACTUAL_WINS
              DSTODS(J) = IDNINT(RODDS_ALL(J)*100.0D0/DFLOAT(ACTUAL_WINS))
              IF(DSTODS(J).LT.100) DSTODS(J)=100
              WRITE(6,903) IAM(),J,DSTODS(J)/100,MOD(DSTODS(J),100)
           ENDDO
           CALL PRMYESNO('Are the adjusted odds correct (Y/N) ',FLAG)
           IF(FLAG.NE.1) GOTO 10
        ENDIF

        RETURN

C------------------------ Format statements ---------------------------

901     FORMAT(1X,A,'Winners: ',I2,/,
     *         T20,'First:   ',I2,2X,3A4,/,
     *         T20,'Second:  ',I2,2X,3A4,/,
     *         T20,'Third:   ',I2,2X,3A4,/,
     *         T20,'Payout is ',I8,'.',I2.2,' to 1')
902     FORMAT(1X,A,1X,' Payout entered is ',I8,'.',I2.2,' to 1')
903     FORMAT(1X,A,1X,'Tie:',I2,' odds adjusted to ',I8,'.',I2.2,' to 1')
904     FORMAT(1X,A,'Cannot open pool file ',5A4,' status>', I4)
        END
	
