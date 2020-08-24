C SUBROUTINE BL1SNP
C  
C  V07 19-MAY-1996 HXK Wojtek added round to snapshot
C  V06 16-JAN-1995 HXK Allow for 3 digit draw number
C  V05 08-Jan-1995 HXK Corrected command spelling
C  V04 30-NOV-1994 JXP REARRANGE WINNING NUMBERS DISPLAY
C  V03 29-NOV-1994 HXK Transposed Results matrix
C  V02 25-OCT-1994 PXB Fixed screen layout  
C  V01 18-OCT-1994 PXB Initial revision.
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

        SUBROUTINE BL1SNP (NUM,GIND)

        IMPLICIT NONE

C---- INCLUDE FILE USED.

        INCLUDE 'INCLIB:SYSPARAM.DEF /NOLIST'
        INCLUDE 'INCLIB:SYSEXTRN.DEF /NOLIST'

        INCLUDE 'INCLIB:GLOBAL.DEF /NOLIST'
        INCLUDE 'INCLIB:AGTINF.DEF /NOLIST'
        INCLUDE 'INCLIB:PRMAGT.DEF /NOLIST'
        INCLUDE 'INCLIB:VISCOM.DEF /NOLIST'
        INCLUDE 'INCLIB:CONCOM.DEF /NOLIST'
        INCLUDE 'INCLIB:BNGCOM.DEF /NOLIST'
        INCLUDE 'INCLIB:DBNREC.DEF /NOLIST'
        INCLUDE 'INCLIB:DATBUF.DEF /NOLIST'

C---- ARGUMENTS

        INTEGER*4  NUM
        INTEGER*4  GIND

C---- VARIABLES

        INTEGER*4  BPAD(BGODIV+1)
        INTEGER*4  BVAL(BGODIV+1)
        INTEGER*4  BWIN(BGODIV+1)
        INTEGER*4  FDB(7)
        INTEGER*4  TOTWON(2)
        INTEGER*4  TOTPAD(2)
        INTEGER*4  BSAL(12)
        INTEGER*4  LIN
        INTEGER*4  I
        INTEGER*4  BTOT1
        INTEGER*4  K
        INTEGER*4  ST
        INTEGER*4  GNUM
        INTEGER*4  DRAW
        INTEGER*4  Q
        INTEGER*4  Y

        INTEGER*2  CDC(12)                  !
        INTEGER*4  WEK
        INTEGER*4  YEAR

        INTEGER*2  SDAT(LDATE_LEN,12)

        CHARACTER*28 CSDAT(12)

        EQUIVALENCE (CSDAT(1),SDAT(1,1))

        REAL*8  BTOT
        REAL*8  POOL
        REAL*8  KEYS(1)

C---- FUNCTIONS

        INTEGER*4  FRAMT

C---- CHARACTER VARIABLES.

        DATA KEYS/'SUPER   '/

        CHARACTER*8 STATUS


C************************ START OF PROGRAM CODE ***********************

C---- INITIALIZE 

        DRAW = NUM
        Y = 1

C---- CHECK GAME INDEX.

        IF (GIND .LT. 1 .OR. GIND .GT. MAXIND) THEN
            WRITE (CLIN23,3000)
            RETURN
        END IF

C---- CHECK GAME NUMBER.

        GNUM = GTNTAB(TBNG,GIND)

        IF (GNUM .LT. 1) THEN
            WRITE (CLIN23,3010) GIND
            RETURN
        END IF

C---- SET UP DRAW.

        IF(DRAW.LE.0.OR.DRAW.GT.BNGLOB(GIND)) DRAW=BNGLOB(GIND)
        IF(DRAW.LE.0) RETURN
        SMODE = .TRUE.

C---- OPEN GAME FILE.

        CALL OPENW (1,
     *              GFNAMES(1,GNUM),
     *              4,
     *              0,
     *              0,
     *              ST)

C---- ERROR CHECK.

        IF (ST .NE. 0) THEN
            WRITE (CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
            CALL USRCLOS1 (     1)
            RETURN
        END IF

C---- INITIALIZE I/O BUFFER.

        CALL IOINIT (FDB,
     *               1,
     *               DBNSEC*256)

C---- READ DBN RECORD.

        CALL READW (FDB,
     *              DRAW,
     *              DBNREC,
     *              ST)

C---- ERROR CHECK.

        IF (ST .NE. 0) THEN
            WRITE (CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
            CALL USRCLOS1 (     1)
            RETURN
        END IF

C---- CLOSE FILE.

        CALL USRCLOS1 (     1)

C---- START OF DISPLAY SECTION.

100     CONTINUE

C---- INITIALIZE VARIABLES USED.

        BTOT = 0.0D0
        BTOT1 = 0

        DO I = 1,2
            TOTWON(I) = 0
            TOTPAD(I) = 0
        END DO

C---- INITIALIZE DISPLAY DATA ARRAY.

        DO I = 1, BGOENT
            IF (I .EQ. 1) THEN
                IF (GIND .EQ. 1) WRITE (CSDAT(1)(17:26),801)
            END IF

            IF (I .EQ.2) THEN
                IF (GIND .EQ. 1) WRITE (CSDAT(2)(17:26),802)
            END IF

            IF ((I .EQ. 3) .AND. (GIND .EQ. 2)) 
     *          WRITE(CSDAT(3)(17:26),801)

            IF (I .EQ. 12) WRITE (CSDAT(12)(17:26),803)

            IF ((I .GT .2) .AND. (I .LT. 12) .AND. (GIND .EQ. 1)) THEN
                SDAT(VCDC,I) = DBNDAT(CURDRW)-(I-3)
                CALL LCDATE (SDAT(1,I))
            END IF

            IF ((I .GT. 3) .AND. (I .LT. 12) .AND. (GIND .EQ. 2)) THEN
                SDAT(VCDC,I) = DBNDAT(CURDRW)-(I-4)
                CALL LCDATE (SDAT(1,I))
            END IF

            IF (I .LT. 12) THEN
                IF (GIND .EQ. 1) THEN
                    BSAL(I) = DBNSAL(I)
                ELSE
                    IF (I .EQ. 1) BSAL(1) = 0
                    IF (I .EQ. 2) BSAL(2) = DBNSHR(1,BGOBAB) * 
     *                                      DBNSHV(1,BGOBAB)
                    IF (I .EQ. 3) BSAL(3) = DBNSAL(1)
                    IF (I .NE. 1 .AND. I .NE. 2 .AND. I .NE. 3)
     *                BSAL(I) = DBNSAL(I-1)
                    IF (I .EQ. 11) BSAL(12) = DBNSAL(11)
                END IF
            ELSE
                BSAL(12) = BSAL(12) + DBNSAL(I)
            END IF
        END DO

C WEEK NO.

        CDC(VCDC) = DBNESD
        CALL FIGWEK(CDC(VCDC),WEK, YEAR)

C---- POOL DATA

        DO I = 1, BGOENT
            BTOT = BTOT + DFLOAT(DBNSAL(I))
            BTOT1 = BTOT1 + DBNSAL(I)
        END DO

        POOL = BTOT * CALPER(DBNSPR)

        DO I = 1,BGODIV
            BVAL(I) = IDNINT(POOL*CALPER(DBNPER(I,BGOBAB)))
            BVAL(I) = BVAL(I) + DBNPOL(I,BGOBAB)    !--- ADD POOL CARRIED OVER.
            BWIN(I) = 0
            BPAD(I) = 0
        END DO

        IF ((DBNSTS .GE. GAMDON) .OR. (DBNSPR .EQ. 0)) THEN
            DO I = 1,BGODIV
                BVAL(I) = DBNSHV(I,BGOBAB)
                BWIN(I) = DBNSHR(I,BGOBAB) * BVAL(I)
                BPAD(I) = FRAMT(MAXFRC(GNUM),DBNPAD(I,BGOBAB),BVAL(I))
            END DO
        ENDIF

        DO I = 1,BGODIV
            TOTWON(1) = TOTWON(1) + DBNSHR(I,BGOBAB)
            TOTWON(2) = TOTWON(2) + BWIN(I)
            TOTPAD(1) = TOTPAD(1) + DBNPAD(I,BGOBAB)
            TOTPAD(2) = TOTPAD(2) + BPAD(I)
        END DO

C---- CHECK STATUS OF GAME AND SET CHARACTER FIELD TO CORRECT VALUE.

        STATUS = '*CLOSED*'
        IF (DBNSTS .EQ. GAMOPN) STATUS = '**OPEN**'
        IF (DBNSTS .EQ. GAMDON) STATUS = '**DONE**'
        IF (DBNSTS .EQ. GFINAL) STATUS = '*FINAL* '

C---- DISPLAY BANNER.

        WRITE (CLIN1,901) GIND,DRAW

C---- DISPLAY BLANK LINE.

        WRITE (CLIN2,908) 

C---- DISPLAY STATUS.

C***    WRITE (CLIN3,9010) STATUS

        WRITE (CLIN3,9010) WEK,YEAR,STATUS

C---- DISPLAY TITLES.

        WRITE (CLIN4,903)

C---- SET LINE COUNTER

        LIN = 5

C---- DISPLAY DIVISIONS AND SALES INFO.

        WRITE (XNEW(  LIN),9041) Y,
     *                           KEYS(Y),
     *                           CMONY(BVAL(Y),11,VALUNIT),
     *                           DBNSHR(Y,BGOBAB),
     *                           CMONY(BWIN(Y),11,VALUNIT),
     *                           DBNPAD(Y,BGOBAB),
     *                           CMONY(BPAD(Y),11,VALUNIT),
     *                           (SDAT(K,Y),K=9,13),
     *                           CMONY(BSAL(Y),11,BETUNIT)

        Y = Y + 1       
        LIN = LIN + 1
        WRITE (XNEW(  LIN),9042) Y,
     *                           CMONY(BVAL(Y),11,VALUNIT),
     *                           DBNSHR(Y,BGOBAB),
     *                           CMONY(BWIN(Y),11,VALUNIT),
     *                           DBNPAD(Y,BGOBAB),
     *                           CMONY(BPAD(Y),11,VALUNIT),
     *                           (SDAT(K,Y),K=9,13),
     *                           CMONY(BSAL(Y),11,BETUNIT)

        Y = Y + 1       
        LIN = LIN + 1
        WRITE (XNEW(  LIN),9043) Y,
     *                           CMONY(BVAL(Y),11,VALUNIT),
     *                           DBNSHR(Y,BGOBAB),
     *                           CMONY(BWIN(Y),11,VALUNIT),
     *                           DBNPAD(Y,BGOBAB),
     *                           CMONY(BPAD(Y),11,VALUNIT),
     *                           (SDAT(K,Y),K=9,13),
     *                           CMONY(BSAL(Y),11,BETUNIT)

C---- DISPLAY SALES INFO ONLY.

        Y = Y + 1       
        LIN = LIN + 1
        WRITE (XNEW(  LIN),906) (SDAT(K,Y),K=9,13),
     *                          CMONY(BSAL(Y),11,BETUNIT)

        Y = Y + 1       
        LIN = LIN + 1
        WRITE (XNEW(  LIN),906) (SDAT(K,Y),K=9,13),
     *                          CMONY(BSAL(Y),11,BETUNIT)

        Y = Y + 1       
        LIN = LIN + 1
        WRITE (XNEW(  LIN),906) (SDAT(K,Y),K=9,13),
     *                          CMONY(BSAL(Y),11,BETUNIT)

        Y = Y + 1       
        LIN = LIN + 1
        WRITE (XNEW(  LIN),906) (SDAT(K,Y),K=9,13),
     *                          CMONY(BSAL(Y),11,BETUNIT)

        Y = Y + 1       
        LIN = LIN + 1
        WRITE (XNEW(  LIN),906) (SDAT(K,Y),K=9,13),
     *                          CMONY(BSAL(Y),11,BETUNIT)

        IF (DBNSTS .LT. GAMENV) THEN

C---- GAME IS OPEN NO WINING NUMBERS.

          Y = Y + 1     
          LIN = LIN + 1
          WRITE (XNEW(  LIN),9084) (SDAT(K,Y),K=9,13),
     *                             CMONY(BSAL(Y),11,BETUNIT)

          Y = Y + 1     
          LIN = LIN + 1
          WRITE (XNEW(  LIN),9084) (SDAT(K,Y),K=9,13),
     *                             CMONY(BSAL(Y),11,BETUNIT)

          Y = Y + 1     
          LIN = LIN + 1
          WRITE (XNEW(  LIN),9084) (SDAT(K,Y),K=9,13),
     *                             CMONY(BSAL(Y),11,BETUNIT)

          Y = Y + 1     
          LIN = LIN + 1
          WRITE (XNEW(  LIN),9084) (SDAT(K,Y),K=9,13),
     *                             CMONY(BSAL(Y),11,BETUNIT)

          Y = Y + 1     
          LIN = LIN + 1
          WRITE (XNEW(  LIN),9085) CMONY(BTOT1+DBNPOL(1,BGOBAB),11,BETUNIT)

        ELSE

C---- DISPLAY WINNING NUMBERS AND SALES INFO.

          Y = Y + 1     
          LIN = LIN + 1
          WRITE (XNEW(  LIN),9081) (DBNWAB(Q*5+1),Q=0,4),
     *                             (SDAT(K,Y),K=9,13),
     *                             CMONY(BSAL(Y),11,BETUNIT)

          Y = Y + 1     
          LIN = LIN + 1
          WRITE (XNEW(  LIN),9082) (DBNWAB(Q*5+2),Q=0,4),
     *                             (SDAT(K,Y),K=9,13),
     *                             CMONY(BSAL(Y),11,BETUNIT)

          Y = Y + 1     
          LIN = LIN + 1
          WRITE (XNEW(  LIN),9082) (DBNWAB(Q*5+3),Q=0,4),
     *                             (SDAT(K,Y),K=9,13),
     *                             CMONY(BSAL(Y),11,BETUNIT)

          Y = Y + 1     
          LIN = LIN + 1
          WRITE (XNEW(  LIN),9082) (DBNWAB(Q*5+4),Q=0,4),
     *                             (SDAT(K,Y),K=9,13),
     *                             CMONY(BSAL(Y),11,BETUNIT)

          Y = Y + 1     
          LIN = LIN + 1
          WRITE (XNEW(  LIN),9083) (DBNWAB(Q*5+5),Q=0,4),
     *                             CMONY(BTOT1+DBNPOL(1,BGOBAB),11,BETUNIT)

        END IF


        RETURN


C*********************** Format Statements ****************************

800     FORMAT (I1,A1,'/',I1,' ')
801     FORMAT ('Advance   ')
802     FORMAT ('Offline   ')
803     FORMAT ('Previous  ')

C---- Screen header format statements.

901     FORMAT ('Bingolotto ',I1,4X,'/',I3,' game data for draw ',I4)

902     FORMAT (4A4)

C***9010 FORMAT (25X,' game is ',A8)

9010    FORMAT ('Round ',I2,'/',I4,15X,' game is ',A8)

903     FORMAT ('---DIV--- ---PRIZE-- ',4('-'),'SHARES WON',4('-'),
     *      1X,2('-'),'FRACTIONS PAID--',1X,7('-'),'SALES',9('-'))

C---- FORMAT statements to display divisions, Prizes, shares won, etc.

9041    FORMAT (I1,'*',A8,A11,1X,I6,1X,A11,1X,I6,A11,1X,
     *         5A2,A11)

9042    FORMAT (I1,' Double  ',A11,1X,I6,1X,A11,1X,I6,A11,1X,
     *         5A2,A11)

9043    FORMAT (I1,' Bingo   ',A11,1X,I6,1X,A11,1X,I6,A11,1X,
     *         5A2,A11)

C---- Display sales info only.


906     FORMAT (59X,5A2,A11)

C---- Blank line.

908     FORMAT (80(' '))

C---- FORMAT statements to display winning numbers and sales.

9081    FORMAT ('Winning numbers : ',5(I3.3,1X),21X,5A2,A11)
9082    FORMAT ('                  ',5(I3.3,1X),21X,5A2,A11)
9083    FORMAT ('                  ',5(I3.3,1X),21X,'Total',5X,A11)

9084    FORMAT (59X,5A2,A11)
9085    FORMAT (60X,'Total',4X,A11)

3000    FORMAT ('Enter draw number ')

C---- Error statements.

3010    FORMAT ('Bingolotto ',I1,' game not active')
3020    FORMAT (5A4,' open error ',I4)
3030    FORMAT (5A4,' read error ',I4,' record > ',I4)
3040    FORMAT ('Input error ')                                                    
3050    FORMAT ('Value error ')                                                    

        END

C********************** End of Program ********************************
