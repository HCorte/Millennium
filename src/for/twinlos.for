C SUBROUTINE TWINLOS
C
C V11 31-MAR-2000 UXN TWAMTFLG added.
C V10 15-MAR-2000 OXK Added a confirmation question before changing odds
C V09 12-JUL-1999 UXN Fix for fractions.
C V08 27-APR-1999 RXK Call of WIMG/YESNO replaced with call of PRMYESNO.
C V07 14-JAN-1999 UXN LTSBRK added.
C V06 27-NOV-1997 UXN Singles,doubles added for PITKA.
C V05 09-SEP-1996 RXK Rfss 282. Option of change odds in Winsel changed
C V04 10-APR-1995 HXK Change for modifying odds on a row during winsel
C V03 19-OCT-1993 GXA Tried to correct rounding of winning amount 
C                     (use IDNINT not IDINT).
C V02 30-SEP-1993 GXA No cents in winning amounts.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE TWINLOS(TRABUF,WINTAB,WIN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INTEGER*4 WINTAB(3,20),BETS(2,6,20)
        INTEGER*4 GIND, K, DIV, SEL, AMT, COUNT, J, ROW, POL
        INTEGER*4 REFUND, WIN, I
        INTEGER*4 NO_CENTS              !Will cut of cents from units.
        
        INTEGER*4 MOD_CDC,MOD_TIM,MOD_NUM,MOD_ROW(10),MOD_ELEM(10)
        INTEGER*4 MOD_POL(10,3),MOD_ODS(10,3)
        INTEGER*4 FLAG,FLAG2,EXT,L,M,TAMT
        REAL*8    WINODS,WAMOUNT
        LOGICAL   FIRST/.TRUE./
        LOGICAL   MOD_FLAG, ODS_SET

        INTEGER*4 FCNT,MAXFRAC
C
        NO_CENTS = (100/DYN_VALUNIT)
        ODS_SET=.FALSE.  
C
        GIND=TRABUF(TGAMIND)

        FCNT     = TRABUF(TFRAC)
        IF(FCNT.EQ.0) FCNT = 10
        MAXFRAC  = MAXFRC(TRABUF(TGAM))
        IF(MAXFRAC.EQ.0) MAXFRAC = 10
        FCNT = MAXFRAC/FCNT

        DO 1000 K=1,TRABUF(TWNBET)
        CALL TEXP(TRABUF,K,BETS,SEL,AMT,COUNT)
        DIV=SEL-2
        IF(SEL.EQ.1) DIV = 5
        IF(SEL.EQ.2) DIV = 6
C
3	CONTINUE

        IF(FIRST) THEN
           FIRST = .FALSE.
           MOD_FLAG = .FALSE.
           CALL PRMYESNO('Do you want to modify row odds [Y/N] ',FLAG)
           IF(FLAG.EQ.1) THEN
              TYPE*,IAM(),'**************************************************'
              TYPE*,IAM(),'* This option has serious consequences for the   *'
              TYPE*,IAM(),'* integrity of the system and must only be used  *'
              TYPE*,IAM(),'* with management approval. Once used it must be *'
              TYPE*,IAM(),'* selected with the same set of values for the   *'
              TYPE*,IAM(),'* remaining part of the round for this game.     *'
              TYPE*,IAM(),'* This option CANNOT handle odds modifications   *'
              TYPE*,IAM(),'* for two cdcs or more within the same round !   *'
              TYPE*,IAM(),'**************************************************'
              CALL PRMYESNO('Do you really want to do this [Y/N] ',FLAG)
              IF(FLAG.NE.1) THEN
		FIRST=.TRUE.
		GOTO 3
	      ENDIF

5             CONTINUE

              MOD_FLAG = .TRUE.

              TYPE*,IAM()
              CALL PRMNUM('Enter cdc from which odds are to be modified ',
     *                    MOD_CDC,1,99999,EXT)
              IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
              TYPE*,IAM(),'Enter time from which odds are to be modified'
              CALL PRMTIM('[Enter 06:00:00 if all that cdc] HH:MM:SS ',
     *                    MOD_TIM,EXT)
              IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
              CALL PRMNUM('Enter number of rows to be modified ',
     *                    MOD_NUM,1,10,EXT)
              IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
              TYPE*,IAM(),'Enter row data in ascending order'
              DO I=1,MOD_NUM
                 TYPE*,IAM()
                 CALL PRMNUM('Enter row number to be modified ',
     *                        MOD_ROW(I),1,40,EXT)
                 IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
                 IF(I.GT.1.AND.MOD_ROW(I).LE.MOD_ROW(I-1)) THEN
                    TYPE*,IAM(),'Rows not selected in ascending order'
                    TYPE*,IAM(),'Modification data entry will restart ...'
                    GOTO 5
                 ENDIF
                 TYPE*,IAM(),'Loop of entering values for row',I
                 TYPE*,IAM(),'Entering results use 1,2 or 3; [1=1,2=2,3=X],',
     *                       ' E-end for row',I 
                 DO J=1,3
                    CALL PRMNUM('Enter row result to be modified ',
     *                        MOD_POL(I,J),1,3,EXT)
                    IF(EXT.LT.0.AND.J.EQ.1) CALL GSTOP(GEXIT_OPABORT)
                    IF(EXT.LT.0) GOTO 7
                    TYPE*,IAM(),'[Enter correspond odds multiplied by 100',
     *                          '  e.g. 1.00 = 100]'
                    CALL PRMNUM('Enter new row result odds',
     *                           MOD_ODS(I,J),0,99999,EXT)
                    IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
                    MOD_ELEM(I)=J
                 ENDDO
7                CONTINUE
              ENDDO
              TYPE*,IAM(),' '
              TYPE*,IAM(),'Entered values ... '
              TYPE*,IAM(),'****************************************'
              TYPE*,IAM(),'  CDC:',MOD_CDC
              WRITE(5,9000) IAM(),DISTIM(MOD_TIM)
              TYPE*,IAM(),'Number of rows:',MOD_NUM
              DO I=1,MOD_NUM
                 TYPE*,IAM(),' '
                 TYPE*,IAM(),I,': row#  :',MOD_ROW(I)
                 TYPE*,IAM(),I,': result:',(MOD_POL(I,J),J=1,MOD_ELEM(I))
                 TYPE*,IAM(),I,': odds  :',(MOD_ODS(I,J),J=1,MOD_ELEM(I))
              ENDDO
              CALL PRMYESNO('Are these values correct [Y/N] ',FLAG2)
              IF(FLAG2.NE.1) GOTO 5
           ENDIF
        ENDIF

        DO 100 J=1,COUNT
        REFUND=0
        DO 10 I=1,SEL
        ROW=BETS(1,I,J)
        POL=BETS(2,I,J)


        IF(LTSSTA(ROW,GIND).EQ.GAMCAN.OR.
     *     LTSSTA(ROW,GIND).EQ.GAMREF) THEN
           REFUND=REFUND+1
        ELSE IF(MOD_FLAG) THEN
           ODS_SET=.FALSE.  
           DO L=1,MOD_NUM
              DO M=1,3
                 IF((ROW.EQ.MOD_ROW(L).AND.TRABUF(TCDC).EQ.MOD_CDC.AND.
     *              TRABUF(TTIM).GE.MOD_TIM.AND.POL.EQ.MOD_POL(L,M)) .OR.
     *              (ROW.EQ.MOD_ROW(L).AND.TRABUF(TCDC).GT.MOD_CDC.AND.
     *              POL.EQ.MOD_POL(L,M))) THEN
                    ODS_SET = .TRUE.
                    GOTO 8
                 ENDIF
              ENDDO  
           ENDDO
8          CONTINUE
           IF(.NOT.ODS_SET.AND.POL.NE.LTSWIN(ROW,GIND)) GOTO 100 
        ELSE IF(.NOT.MOD_FLAG) THEN  
           IF(POL.NE.LTSWIN(ROW,GIND)) GOTO 100
        ENDIF
10      CONTINUE
C
C TICKET HAS A WINNER
C

        WIN=WIN+1
        WINTAB(2,WIN)=0
        IF(REFUND.EQ.SEL) THEN
	  TAMT = AMT
	  IF(TRABUF(TFAMTFLG).EQ.1) TAMT=TAMT/FCNT
          WINTAB(1,WIN)=TAMT
          WINTAB(2,WIN)=1
          WINTAB(3,WIN)=DIV
        ELSE
          WINODS=1.0D0
          DO 20 I=1,SEL
          ROW=BETS(1,I,J)
          POL=BETS(2,I,J)
          IF(MOD_FLAG) THEN
             ODS_SET=.FALSE. 
             DO L=1,MOD_NUM
                DO M=1,3
                   IF((ROW.EQ.MOD_ROW(L).AND.TRABUF(TCDC).EQ.MOD_CDC.AND.
     *                TRABUF(TTIM).GE.MOD_TIM.AND.POL.EQ.MOD_POL(L,M)) .OR.
     *                (ROW.EQ.MOD_ROW(L).AND.TRABUF(TCDC).GT.MOD_CDC.AND.
     *                POL.EQ.MOD_POL(L,M))) THEN
                      TYPE*,IAM(),'ser:',TRABUF(TSER),' winner will have',
     *                         ' odds modified'
                      WINODS=WINODS*MOD_ODS(L,M)
                      ODS_SET=.TRUE.
                      GOTO 15
                   ENDIF
                ENDDO  
             ENDDO
15           CONTINUE
             IF(.NOT.ODS_SET) WINODS=WINODS*LTSODS(POL,ROW,GIND) 
          ELSE
             WINODS=WINODS*LTSODS(POL,ROW,GIND)
          ENDIF
20        CONTINUE
C
          IF(SEL.GT.2) WINODS = DNINT(WINODS/DFLOAT(100**(SEL-2)))
          IF(SEL.GT.1) WINODS = DNINT(WINODS/100.0D0)
          WINODS = WINODS/100.0D0
	  TAMT = AMT
          WAMOUNT = DFLOAT(TAMT) * WINODS
          WINTAB(1,WIN) = IDNINT(WAMOUNT)
          WINTAB(1,WIN) = (WINTAB(1,WIN)*FCNT/NO_CENTS*NO_CENTS)/FCNT
	  IF(TRABUF(TFAMTFLG).EQ.1) WINTAB(1,WIN)=WINTAB(1,WIN)/FCNT
          LTSBRK(GIND) = LTSBRK(GIND) + 
     *       IDINT((WAMOUNT - DFLOAT(WINTAB(1,WIN)))* DFLOAT(DYN_VALUNIT))
          WINTAB(2,WIN)=0
          WINTAB(3,WIN)=DIV
        ENDIF
100     CONTINUE
1000    CONTINUE
        RETURN
9000    FORMAT(1X,A,1X,'TIME:',A8)
        END
