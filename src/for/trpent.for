C
C SUBROUTINE TRPENT
C 
C TRPENT.FOR
C
C V06 30-JUN-2000 UXN Refund too late played tickets.
C V05 09-JUN-2000 UXN Fixed to avoid compliler warnings.
C V04 18-JAN-2000 UXN CLOSEQFIL ADDED.
C V03 17-DEC-1999 PXO Added a call to report subroutine
C V02 28-MAY-1999 UXN DTRCMB ADDED. PARTIALLY REWRITTEN.
C V01 XX-XXX-XXXX RXK INITIAL RELEASE.
C
C RESULT ENTRY SUBROUTINE FOR TODAY'S TRIO GAMES
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

        SUBROUTINE TRPENT(GNUM,GIND,DRAW)

        IMPLICIT NONE

C---- Include files used.

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:RESCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:TROCOM.DEF'
        INCLUDE 'INCLIB:TRFREC.DEF'
C
	COMMON/RESULTS_TRFREC/ TRFREC
C
C---- Local variables used.

        INTEGER*4 FDB(7), PFDB(7)
        INTEGER*4 GNUM
        INTEGER*4 GIND
        INTEGER*4 DRAW
        INTEGER*4 ST
        INTEGER*4 ROW
        INTEGER*4 FLAG
        INTEGER*4 EXT
        INTEGER*4 I, K, J, I1, I2, I3
        INTEGER*4 NUM
        INTEGER*4 WINNERS(3)
        INTEGER*4 REALWINNERS
        INTEGER*4 TOTSAL
        INTEGER*4 COUNT
        INTEGER*4 UCID
        INTEGER*4 AMT
        INTEGER*4 GAMST
        CHARACTER*6 WHICH(4) /'first ','second','third ','fourth'/

        CHARACTER*55 STRING

        REAL*8 TOTPOL
        REAL*8 RODDS(MAXTRPTI)
	INTEGER*4 WINNUM(3,MAXTRPTI)
	INTEGER*4 TOT_WIN,CHANGE

	INTEGER*4 CDC,TIME
C
C------------------------ Start of Code -----------------------------

C---- Open and read files.

        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,3,DTRSEC*256)
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

        CALL READW(FDB,DRAW,DTRREC,ST)
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)

C---- Check game status.

        IF (DTRSTS .EQ. GFINAL) THEN
          WRITE(6,900) IAM(),GTNAMES(TTRP),GIND,DRAW,DTRSTS
          CALL GPAUSE
        END IF
C
	DTRLAT(LATCDC) = 0
	DTRLAT(LATTIM) = 0
C
C---- Set row status

10      CONTINUE

        CALL FASTMOV(DTRSTA,TRROWSTSE(1,1),3*MAXTRPRW)
        CALL FASTSET(0,DTRHLD(1,1),3*MAXTRPTI)
        CALL FASTSET(0,DTRWIN(1,1),3*MAXTRPTI)
        GAMST = 0
	CHANGE = 0
20      CONTINUE

        DO 50 I=1,3
           IF(DTREST(I).EQ.GAMNUL) GOTO 50
           WRITE(6,910) 
           WRITE(6,909) IAM(),I,(DTRENM(K,I),K=1,TRPENM_LEN/4)
           WRITE(STRING,800) I

C---- Ask for cancellation of a row in event

25         CONTINUE
           CALL INPYESNO(STRING,FLAG)
           IF (FLAG .NE. 1) GOTO 30
           CALL INPNUM('Enter row number to cancel ',ROW,1,MAXTRPRW,EXT)
           IF (EXT .LT. 0) GOTO 30
           CALL TRP_CANROW(I,ROW,TRROWSTSE)
           GOTO 25  

30         CONTINUE

           DO ROW=1,MAXTRPRW
              IF(TRROWSTSE(ROW,I).EQ.GAMOPN) GOTO 35
           ENDDO
           WRITE(6,913) IAM(),I,(DTRENM(K,I),K=1,TRPENM_LEN/4)

C---- Ask for winners 

35         CONTINUE
	   DO K=1,MAXTRPTI
             WINNUM(I,K) = 0
	   ENDDO
           WINNERS(I) = 1
           WRITE(STRING,950) I
           CALL INPYESNO(STRING,FLAG)
           IF(FLAG.EQ.1) THEN
             CALL INPNUM('Enter number of ties ',WINNERS(I),1,4,EXT)
             IF(EXT.NE.0) GOTO 35
           ENDIF           
           DO J=1,WINNERS(I)
40           CONTINUE
             IF(WINNERS(I).EQ.1) THEN    ! CANCEL ON FIRST ENTRY..
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
                GAMST=GAMCAN
                WRITE(6,907) IAM(),(DTRMNM(K),K=1,TRPENM_LEN/4)
	        DTRCMB = 0
		CALL FASTSET(0,DTRWIN,3*MAXTRPTI)
                GOTO 60
             ENDIF
             IF(EXT.NE.0) GOTO 35      
             IF(TRROWSTSE(NUM,I).NE.GAMOPN) THEN
                WRITE(6,908) IAM(),NUM,(DTRNMS(K,NUM,I),K=1,TRPNMS_LEN/4)
                GOTO 40
             ENDIF
             WRITE(6,980) IAM(),NUM,(DTRNMS(K,NUM,I),K=1,TRPNMS_LEN/4)   
             CALL INPYESNO('Is this correct (Y/N) ',FLAG)
             IF(FLAG.NE.1) GOTO 40
             WINNUM(I,J) = NUM
           ENDDO

50      CONTINUE

        DO 55 I=1,3
           IF(DTREST(I).EQ.GAMNUL) GOTO 55
           WRITE(6,912) IAM(),I,(DTRENM(K,I),K=1,TRPENM_LEN/4)
           DO J=1,4
              IF(WINNUM(I,J).NE.0) WRITE(6,911) IAM(),J,WINNUM(I,J),
     *           (DTRNMS(K,WINNUM(I,J),I),K=1,TRPNMS_LEN/4)             
           ENDDO
55      CONTINUE
        CALL INPYESNO('Is this correct <Y/N> ',FLAG)
        IF (FLAG .NE. 1) GOTO 10
C
C Set correct winning combinations
C
	TOT_WIN = 0
	DO 58 I=1,MAXTRPTI
	   IF(WINNUM(1,I).LE.0) GOTO 58
	   IF(DTREST(2).EQ.GAMNUL) THEN
	      TOT_WIN = TOT_WIN + 1
	      DTRWIN(1,TOT_WIN) = WINNUM(1,I)
	      DTRWIN(2,TOT_WIN) = 0
	      DTRWIN(3,TOT_WIN) = 0
	      GOTO 58
	   ENDIF
	   DO 57 J=1,MAXTRPTI
              IF(WINNUM(2,J).LE.0) GOTO 58
	      IF(DTREST(3).EQ.GAMNUL) THEN
                 TOT_WIN = TOT_WIN + 1
                 DTRWIN(1,TOT_WIN) = WINNUM(1,I)
                 DTRWIN(2,TOT_WIN) = WINNUM(2,J)
                 DTRWIN(3,TOT_WIN) = 0
                 GOTO 57
              ENDIF
	      DO 56 K=1,MAXTRPTI
	         IF(WINNUM(3,K).LE.0) GOTO 57
		 TOT_WIN = TOT_WIN + 1
                 DTRWIN(1,TOT_WIN) = WINNUM(1,I)
                 DTRWIN(2,TOT_WIN) = WINNUM(2,J)
                 DTRWIN(3,TOT_WIN) = WINNUM(3,K)
56	      CONTINUE
57         CONTINUE
58      CONTINUE

	DTRCMB = TOT_WIN

60      CONTINUE

        DTRSTS = GAMEN1
        OPDONE = 1

C---- Wait for verification from remote terminal

2000    CONTINUE

        TYPE*,IAM(),' Waiting for remote terminal verification '
        IF (DTRSTS .EQ. GAMBFD) THEN
          TYPE*,IAM(),' Remote entry does not match, please re-enter'
          GOTO 10
        END IF
        CALL XWAIT(5,2,ST)
        IF (DTRSTS .NE. GAMENV) GOTO 2000

C---- If any of events cancelled then set refunds
 
        IF(GAMST.EQ.GAMCAN) THEN
           DTRODS(1)=100           
           WRITE(6,915) IAM(),(DTRMNM(K),K=1,TRPENM_LEN/4)
           DTRSTS = GAMCAN
           GOTO 2100
        ENDIF    

C---- Get winning odds.
C---- open and read today's triple pool file
C
        WRITE(6,901) IAM(),GTNAMES(TTRP),GIND
        WRITE(6,903) IAM(),GTNAMES(TTRP),GIND
C
	CALL RC_PROMPT(CDC,TIME,ST)
	IF(ST.EQ.1) THEN    
	    CALL RESPOL_TTRP(GNUM,CDC,TIME)
	    GOTO 2020
	ENDIF
2010    CONTINUE
        CALL OPENQW(4,DTRPFN,4,0,0,ST)
        IF(ST.NE.0) THEN
           WRITE(6,900) DTRPFN,ST
           CALL GPAUSE
           GOTO 2010
        ENDIF
        CALL IOQINIT(PFDB,4,TRFSEC*256)
        CALL READQW(PFDB,1,TRFREC,ST)
        CALL CLOSEQFIL(PFDB)
C
2020	CONTINUE
C
C---- Calculate total sales from sales per combination
C
        TOTSAL=0
        DO I1=1,MAXTRPRW
	   IF(DTREST(2).EQ.GAMNUL) THEN
              IF(TRROWSTSE(I1,1).EQ.GAMOPN) THEN
                    UCID = I1
                    TOTSAL = TOTSAL + TRFODDS(TRGAMT,UCID)
              ENDIF
	   ELSE	      
              DO I2=1,MAXTRPRW
	         IF(DTREST(3).EQ.GAMNUL) THEN
                    IF(TRROWSTSE(I1,1).EQ.GAMOPN .AND.
     *                 TRROWSTSE(I2,2).EQ.GAMOPN) THEN
                       UCID = I1+(I2-1)*MAXTRPRW
                       TOTSAL = TOTSAL + TRFODDS(TRGAMT,UCID)
                    ENDIF
	         ELSE
                    DO I3=1,MAXTRPRW
                       IF(TRROWSTSE(I1,1).EQ.GAMOPN .AND.
     *                    TRROWSTSE(I2,2).EQ.GAMOPN .AND.
     *                    TRROWSTSE(I3,3).EQ.GAMOPN) THEN
                           UCID = I1+(I2-1)*MAXTRPRW+(I3-1)*MAXTRPRW*MAXTRPRW
                           TOTSAL = TOTSAL + TRFODDS(TRGAMT,UCID)
                       ENDIF
	            ENDDO
	         ENDIF
              ENDDO
           ENDIF
	ENDDO

        TOTPOL = DFLOAT(TOTSAL) * CALPER(DTRSPR)
        TOTPOL = TOTPOL + DFLOAT(DTRPOL(1))
        DTRTPL = IDINT(TOTPOL)

        REALWINNERS=0
        DO COUNT=1,DTRCMB
	   IF(DTREST(2).EQ.GAMNUL) THEN	      ! ONLY ONE EVENT OPEN
              UCID=DTRWIN(1,COUNT)
	   ELSEIF(DTREST(3).EQ.GAMNUL)	THEN  ! TWO EVENTS OPEN
              UCID=1 + (DTRWIN(1,COUNT)-1) + (DTRWIN(2,COUNT)-1)*MAXTRPRW 
	   ELSE                               ! ALL THREE EVENTS OPEN
              UCID = 1 + (DTRWIN(1,COUNT)-1) + (DTRWIN(2,COUNT)-1)*MAXTRPRW +
     *               (DTRWIN(3,COUNT)-1)*MAXTRPRW*MAXTRPRW
           ENDIF
           AMT=TRFODDS(TRGAMT,UCID)  
           IF(AMT.NE.0) THEN 
              REALWINNERS=REALWINNERS+1
              RODDS(COUNT)=TOTPOL/AMT
              DTRODS(COUNT)=IDNINT(RODDS(COUNT)*100.0D0)
              IF(DTRODS(COUNT).LT.100) DTRODS(COUNT) = 100
           ELSE   
              DTRODS(COUNT)=0
           ENDIF
           WRITE(6,917) IAM(),COUNT
	   WRITE(6,918) 'A',DTRWIN(1,COUNT),
     *                  (DTRNMS(K,DTRWIN(1,COUNT),1),K=1,TRPNMS_LEN/4)
	   IF(DTREST(2).NE.GAMNUL) THEN
     	       WRITE(6,918) 'B',DTRWIN(2,COUNT),
     *                      (DTRNMS(K,DTRWIN(2,COUNT),2),K=1,TRPNMS_LEN/4)
	   ENDIF
	   IF(DTREST(3).NE.GAMNUL) THEN
     	       WRITE(6,918) 'C',DTRWIN(3,COUNT),
     *                      (DTRNMS(K,DTRWIN(3,COUNT),3),K=1,TRPNMS_LEN/4)
	   ENDIF
	   WRITE(6,920) IAM(),DTRODS(COUNT)/100,MOD(DTRODS(COUNT),100),
     *                  CMONY(AMT,10,BETUNIT)
        ENDDO
C
C IF TIED WINNERS THEN ADJUST ODDS
C
        IF(REALWINNERS.LE.1) GOTO 600
C
        TYPE*,IAM(),'Tied winners. Recalculation of odds'

        DO COUNT=1,DTRCMB
           DTRODS(COUNT)=IDNINT(DFLOAT(DTRODS(COUNT))/DFLOAT(REALWINNERS))

           IF(DTRODS(COUNT).GT.0.AND.DTRODS(COUNT).LT.100) DTRODS(COUNT)=100
        ENDDO
C
	CHANGE = 0
400	CONTINUE
	DO COUNT=1,DTRCMB
410	   CONTINUE
           WRITE(6,917) IAM(),COUNT
	   WRITE(6,918) 'A',DTRWIN(1,COUNT),
     *                     (DTRNMS(K,DTRWIN(1,COUNT),1),K=1,TRPNMS_LEN/4)
	   IF(DTREST(2).NE.GAMNUL) THEN
     	      WRITE(6,918) 'B',DTRWIN(2,COUNT),
     *                        (DTRNMS(K,DTRWIN(2,COUNT),2),K=1,TRPNMS_LEN/4)
	   ENDIF
	   IF(DTREST(3).NE.GAMNUL) THEN
     	      WRITE(6,918) 'C',DTRWIN(3,COUNT),
     *                        (DTRNMS(K,DTRWIN(3,COUNT),3),K=1,TRPNMS_LEN/4)
	   ENDIF
	   WRITE(6,919) DTRODS(COUNT)/100,MOD(DTRODS(COUNT),100)
	   IF(CHANGE.EQ.1) THEN
              CALL INPNUM('Enter new odds [100-99999999, E-no change]:',
     *                       DTRODS(COUNT),100,99999999,EXT)
	      CHANGE = 2
	      GOTO 410    ! Display these odds again
	   ENDIF
	   IF(CHANGE.EQ.2) CHANGE = 1 ! Operator wanted to change the odds.
        ENDDO
600	CONTINUE

	IF(CHANGE.EQ.0) THEN ! Ask this question only first time.	 
           CALL PRMYESNO('Do you want to change these odds [Y/N] ',CHANGE)
	   IF(CHANGE.EQ.1) GOTO 400
        ELSE
           CALL PRMYESNO('Are the new odds entered correct [Y/N] ',FLAG)
           IF(FLAG.NE.1) GOTO 400
        ENDIF

        DTRSTS = GAMENV

2100    CONTINUE

        CALL FASTMOV(TRROWSTSE(1,1),DTRSTA,3*MAXTRPRW)
        CALL WRITEW(FDB,DRAW,DTRREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
        CALL CLOSEFIL(FDB)
	CALL TRRESULT(GIND,DRAW)
        RETURN

C------------------------ Format statements ---------------------------

800     FORMAT('Do you want to cancel a row in event ',I1,'  (Y/N)')
801     FORMAT('Enter number of winners in event ',I1,' (C=cancel event)')
802     FORMAT('Enter winning row ',I1,' in event ',I1)

900     FORMAT(1X,A,1X,A8,I1,' event ',I4,' invalid game status> ',I4)
901     FORMAT(1X,A,1X,A8,I1,' results verified at remote terminal')
903     FORMAT(1X,A,' Calculating ',A8,I1,' payout odds')
907     FORMAT(1X,A,'Master Event ',1X,<TRPENM_LEN/4>A4,' cancelled')
908     FORMAT(1X,A,'Row ',I2,1X,<TRPNMS_LEN/4>A4,' not open')
909     FORMAT(1X,A,'Event ',I1,2X,<TRPENM_LEN/4>A4)
910     FORMAT(1X,A)
911     FORMAT(1X,A,'Winner # ',I1,'  Row ',I2,2X,<TRPNMS_LEN/4>A4)
912     FORMAT(1X,A,'Event ',I1,2X,<TRPENM_LEN/4>A4)
913     FORMAT(1X,A,' Event ',I1,2X,<TRPENM_LEN/4>A4,' 0 open rows')
914     FORMAT(1X,A,'Winners: 'I2,
     *         3(/20X,A1,I3,2X,<TRPNMS_LEN/4>A4),' odds are',I8,'.',I2.2,
     *         ' to 1, for ',A10)
9141    FORMAT(1X,A,'Winners: 'I2,
     *         2(/20X,A1,I3,2X,<TRPNMS_LEN/4>A4),' odds are',I8,'.',I2.2,
     *         ' to 1, for ',A10)
9142    FORMAT(1X,A,'Winners: 'I2,
     *         /20X,A1,I3,2X,<TRPNMS_LEN/4>A4,' odds are',I8,'.',I2.2,
     *         ' to 1, for ',A10)
915     FORMAT(1X,A,'Master Event ',<TRPENM_LEN/4>A4,' cancelled')
916     FORMAT(1X,A,'Winners: 'I2,
     *         3(/20X,A1,I3,2X,<TRPNMS_LEN/4>A4))
9161    FORMAT(1X,A,'Winners: 'I2,
     *         2(/20X,A1,I3,2X,<TRPNMS_LEN/4>A4))
9162    FORMAT(1X,A,'Winners: 'I2,
     *         /20X,A1,I3,2X,<TRPNMS_LEN/4>A4)
917     FORMAT(1X,A,'Winners: ',I2)
918     FORMAT(20X,A1,I3,2X,<TRPNMS_LEN/4>A4)
919     FORMAT(T50,' Odds are',I8,'.',I2.2)
920	FORMAT(1X,A,' Odds are',I8,'.',I2.2,' to 1, for ',A10)
9171    FORMAT(1X,A,'Winners: 'I2,
     *         2(/20X,A1,I3,2X,<TRPNMS_LEN/4>A4),' odds are',I8,'.',I2.2)
9172    FORMAT(1X,A,'Winners: 'I2,
     *         1(/20X,A1,I3,2X,<TRPNMS_LEN/4>A4),' odds are',I8,'.',I2.2)
950     FORMAT('Are there any tied winners for event ',I1,' (Y/N)')
960     FORMAT('Enter ',A6,' winning row in event ',I1)
970     FORMAT('Enter winning row in event ',I1,' <C to Cancel Event>')
971     FORMAT('Enter winning row in event ',I1)
980     FORMAT(1X,A,'Row ',I2,1X,<TRPNMS_LEN/4>A4)
        END
