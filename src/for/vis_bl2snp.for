C SUBROUTINE BL2SNP
C
C V11 27-MAR-2000 OXK Typing error fixed
C V10 27-MAR-2000 UXN Draw number displaying fixed.
C V09 16-MAR-2000 RXK Change of prize amount of Fullhouse allowed.
C V08 19-JAN-2000 RXK Numbers for subphases displayed, star keys for prize 
C                     values set to 1.
C V07 13-JAN-2000 RXK Def-file for Bingo division names added
C V06 18-OCT-1999 UXN OFFLINE sales field removed. RFSS #92
C V05 19-MAY-1996 HXK Wojtek added round to snapshot
C V04 16-JAN-1995 HXK Allow for 3 digit draw number
C V03 08-JAN-1995 HXK Corrected command spelling
C V02 25-OCT-1994 PXB Fixed Screen layout
C V01 18-OCT-1994 PXB Initial revision.
C
C Bingolotto Fullhouse Snapshot. 
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

        SUBROUTINE BL2SNP (NUM,GIND,CLINE)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:BNGDNAM.DEF'

C ARGUMENTS PASSED IN.

        INTEGER*4  NUM
        INTEGER*4  GIND
        INTEGER*4  CLINE(20)

C LOCAL VARIABLES USED.

        INTEGER*4  BPAD(BGODIV+1)
        INTEGER*4  BVAL(BGODIV+1)
        INTEGER*4  BWIN(BGODIV+1)
        INTEGER*4  FDB(7)
        INTEGER*4  TOTWON(2)
        INTEGER*4  TOTPAD(2)
        INTEGER*4  BSAL(12)
        INTEGER*4  LIN
        INTEGER*4  DIV
        INTEGER*4  I
        INTEGER*4  K
        INTEGER*4  ST
        INTEGER*4  GNUM
        INTEGER*4  DRAW
        INTEGER*4  VALUE
        INTEGER*4  POS
        INTEGER*4  KEYNUM
        INTEGER*4  CBUF(CDLEN)
        INTEGER*4  TLINE(20)
        INTEGER*4  TEMP
        INTEGER*4  Q
        INTEGER*4  X
        INTEGER*4  Y

        INTEGER*2  CDC(12)                  !
        INTEGER*4  WEK
        INTEGER*4  YEAR

        INTEGER*2  SDAT(LDATE_LEN,12)

        CHARACTER*28 CSDAT(12)
        CHARACTER    CTEMP(4)
        CHARACTER    CHRLIN(80)

        EQUIVALENCE (CSDAT(1),SDAT(1,1))
        EQUIVALENCE (TEMP,CTEMP)
        EQUIVALENCE (CHRLIN(1),TLINE(1))

        REAL*8  BTOT
        REAL*8  POOL

C FUNCTIONS

        INTEGER*4  FRAMT

C LOCAL CHARACTER VARIABLES USED.

        CHARACTER*1 STAR(2)
        CHARACTER*8 STATUS
        CHARACTER*1 TOTSIGN
        CHARACTER*80 TEMPLINE
        INTEGER*4    LOT_DIV(BGOLOT)

C DATA SETUP.

        DATA STAR/' ','*'/

        REAL*8 NEWKEYS(BGOLOT+2)
        INTEGER*4 INEWKEYS(2*(BGOLOT+2))
        EQUIVALENCE(NEWKEYS,INEWKEYS)

        DATA NEWKEYS(BGOLOT+1)/'PHAsefh '/
        DATA NEWKEYS(13)/'FULlh '/

        INTEGER*4   STARKEYS(BGOLOT+1)

        DATA  STARKEYS/1,1,1,1, 1,1,1,1, 1,1,1,1, 2,1,1,1, 1,1,1,1, 2/ 
C
C START
C
        CALL FASTMOV(IBNGDNAMES,INEWKEYS,2*BGOLOT)
        DRAW = NUM
        Y = 1
        CALL FASTSET(0,LOT_DIV,BGOLOT)
C
C CHECK GAME INDEX.
C
        IF (GIND .LT. 1 .OR. GIND .GT. MAXIND) THEN
            WRITE (CLIN23,3000)
            RETURN
        END IF
C
C CHECK GAME NUMBER.
C
        GNUM = GTNTAB(TBNG,GIND)
        IF (GNUM .LT. 1) THEN
            WRITE (CLIN23,3010) GIND
            RETURN
        END IF
C
C SET UP DRAW.
C
        IF (DRAW .LT. 1) DRAW = DAYDRW(GNUM)
        IF (DRAW .EQ. 0) DRAW = DAYHDR(GNUM)
C
C SET UP INPUT COMMANDS.
C
        VALUE = 0
        TEMP = 0
        POS = 1
C
C CALL ROUTINE TO MATCH COMMAND.
C
        CALL KEY (CLINE,NEWKEYS,22,POS,KEYNUM)
C
        IF (POS .GT. 40) GOTO 5       !--- NO INPUT.
        IF (KEYNUM .EQ. 0) THEN       !--- INPUT ERROR.
            WRITE (CLIN23,3040)
            RETURN
        END IF
C
C CHECK CAN DIVISION BE CHANGED
C
        IF(STARKEYS(KEYNUM).EQ.1) THEN
            WRITE (CLIN23,3040)
            RETURN
        END IF
C
C CALL ROUTINE TO GET VALUE.
C
        CALL NUMB (CLINE,POS,VALUE)
C
C NO VALUE FOUND.
C
        IF (VALUE .LT. 0) THEN
            WRITE (CLIN23,3050)
            RETURN
        END IF
C
C CLEAR COMMAND MESSAGE BUFFER.
C
        CALL FASTSET (0,CBUF,CDLEN)
C
C JUMP TO REQUIRED INPUT SECTION.
C
        GOTO (1,1,1,1,  1,25,1,30,  1,1,1,1,  10,20,1,1,  1,1,40,1, 
     *        60) KEYNUM
C
1       CONTINUE
        RETURN
C
C INPUT FULLHOUSE PRIZE.
C
10      CONTINUE
        CBUF(1) = 3
        CBUF(2) = VALUE
        CBUF(3) = TCBNG
        CBUF(8) = GIND
        GOTO 200
C
C INPUT HIT24 PRIZE.
C
20      CONTINUE
        CBUF(1) = 4
        CBUF(2) = VALUE
        CBUF(3) = TCBNG
        CBUF(8) = GIND
        GOTO 200
C
C INPUT "OTHER" TRIPPLE PRIZE.
C
25      CONTINUE
        CBUF(1) = 5
        CBUF(2) = VALUE
        CBUF(3) = TCBNG
        CBUF(8) = GIND
        GOTO 200
C
C INPUT "OTHER" QUADRUPLE PRIZE.
C
30      CONTINUE
        CBUF(1) = 9
        CBUF(2) = VALUE
        CBUF(3) = TCBNG
        CBUF(8) = GIND
        GOTO 200
C
C INPUT WORST PRIZE.
C
40      CONTINUE
        CBUF(1) = 6
        CBUF(2) = VALUE
        CBUF(3) = TCBNG
        CBUF(8) = GIND
        GOTO 200
C
C INPUT NUMBER 1 PHASE 1.
C
50      CONTINUE
        GOTO 200
C
C INPUT NUMBER FOR FULLHOUSE PHASE.
C
60      CONTINUE
        CBUF(1) = 8
        CBUF(2) = VALUE
        CBUF(3) = TCBNG
        CBUF(8) = GIND
        GOTO 200
C
C SET UP BUFFER.
C
200     CONTINUE
        CBUF(6) = IDNUM
        CALL VISCMD (CBUF,ST)
C
C GET DATA FROM COMMON OR DISK
C
5       CONTINUE
        IF (DRAW .EQ. DAYDRW(GNUM)) THEN
            CALL GAMLOG (TBNG,GIND,DBNREC,BNGSTS)
            GOTO 100
        END IF
C
        SMODE = .TRUE.
C
C OPEN GAME FILE.
C
        CALL OPENW (1,GFNAMES(1,GNUM),4,0,0,ST)
C
C ERROR CHECK.
C
        IF (ST .NE. 0) THEN
            WRITE (CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
            CALL USRCLOS1 (     1)
            RETURN
        END IF
C
C INITIALIZE I/O BUFFER.
C
        CALL IOINIT (FDB,1,DBNSEC*256)
C
C READ DBN RECORD.
C
        CALL READW (FDB,DRAW,DBNREC,ST)
C
C ERROR CHECK.
C
        IF (ST .NE. 0) THEN
            WRITE (CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
            CALL USRCLOS1 (     1)
            RETURN
        END IF
C
C CLOSE FILE.
C
        CALL USRCLOS1 (1)
C
C START OF DISPLAY SECTION.
C
100     CONTINUE
C
C INITIALIZE VARIABLES USED.
C
        BTOT = 0.0D0
        DO I = 1,2
            TOTWON(I) = 0
            TOTPAD(I) = 0
        END DO
C
C INITIALIZE DISPLAY DATA ARRAY.
C
        WRITE (CSDAT(1)(17:26),801)
C       WRITE (CSDAT(2)(17:26),802)
        WRITE (CSDAT(12)(17:26),803)
C
        DO I=1,12
          IF(I.GT.2.AND.I.LT.12) THEN
            SDAT(VCDC,I) = DBNDAT(CURDRW)-(I-3)
            CALL LCDATE (SDAT(1,I))
          ENDIF
          BSAL(I) = DBNSAL(I)
        ENDDO
C
C LOTTERY DIVISIONS
C
        DO I=1,BGODIV
           IF(DBNDNR(I).NE.0) LOT_DIV(DBNDNR(I)) = I
        ENDDO
C
C WEEK NO.
C
        CDC(VCDC) = DBNESD
        CALL FIGWEK(CDC(VCDC),WEK,YEAR)
C
C POOL DATA
C
        DO I = 1,BGOENT
            BTOT = BTOT + DFLOAT(DBNSAL(I))
        END DO
C
        POOL = BTOT * CALPER(DBNSPR)
        IF(IDNINT(POOL).LT.DBNMIN) POOL=DFLOAT(DBNMIN)
C
        IF ((DBNSTS .GE. GAMDON) .OR. (DBNSPR .EQ. 0)) THEN
            DO I = 1,BGODIV
                BVAL(I) = DBNSHV(I,BGOFHS)
                BWIN(I) = DBNSHR(I,BGOFHS) * BVAL(I)
                BPAD(I) = FRAMT(MAXFRC(GNUM),DBNPAD(I,BGOFHS),BVAL(I))
            END DO
        ELSE
            DO I = 1,BGODIV
               BVAL(I) = IDNINT(POOL * CALPER(DBNPER(I,BGOFHS)))
               BVAL(I) = BVAL(I) + DBNPOL(I,BGOFHS)  !--- ADD POOL CARRIED OVER.
               IF(DBNPER(I,BGOFHS).EQ.0) BVAL(I) = DBNSHV(I,BGOFHS)
               BWIN(I) = 0
               BPAD(I) = 0
            ENDDO 
        ENDIF
C
        DO I = 1,BGODIV
            TOTWON(1) = TOTWON(1) + DBNSHR(I,BGOFHS)
            TOTWON(2) = TOTWON(2) + BWIN(I)
            TOTPAD(1) = TOTPAD(1) + DBNPAD(I,BGOFHS)
            TOTPAD(2) = TOTPAD(2) + BPAD(I)
        END DO
C
C CHECK STATUS OF GAME AND SET CHARACTER FIELD TO CORRECT VALUE.
C
        STATUS = '*CLOSED*'
        IF (DBNSTS .EQ. GAMOPN) STATUS = '**open**'
        IF (DBNSTS .EQ. GAMDON) STATUS = '**done**'
        IF (DBNSTS .EQ. GFINAL) STATUS = '*final* '
C
C DISPLAY BANNER.
C
        WRITE (CLIN1,901) GIND,DRAW
C
C DISPLAY BLANK LINE.
C
        WRITE (CLIN2,908) 
C
C DISPLAY STATUS.

        WRITE (CLIN3,9010) WEK,YEAR,STATUS
C
C DISPLAY TITLES.
C
        WRITE (CLIN4,903)
C
C SET LINE COUNTER
C
        LIN = 5
C
C DISPLAY DIVISIONS AND SALES INFO.
C
        DO DIV=1,BGODIV            
           IF(DBNDNR(DIV).NE.0.AND.LIN.LE.15) THEN
              Y=DBNDNR(DIV)                     !DIV # IN 1,...,BGOLOT SCALE
              X=LIN-4                           !LINE # FOR SALES PART
              IF(DBNPER(DIV,BGOFHS).EQ.0.OR.DBNSTS.GE.GAMDON) THEN
                 TOTSIGN = ' '
              ELSE
                 TOTSIGN = 'T'
              ENDIF
              IF(X.NE.2) THEN
                 WRITE (XNEW(LIN),9020) DIV,
     *                           STAR(STARKEYS(Y)),
     *                           NEWKEYS(Y),
     *                           CMONY(BVAL(DIV),10,VALUNIT),
     *                           TOTSIGN,
     *                           DBNSHR(DIV,BGOFHS),
     *                           CMONY(BWIN(DIV),11,VALUNIT),
     *                           DBNPAD(DIV,BGOFHS),
     *                           CMONY(BPAD(DIV),11,VALUNIT),
     *                           (SDAT(K,X),K=9,13),
     *                           CMONY(BSAL(X),11,BETUNIT)
              ELSE
                 WRITE (XNEW(LIN),90201) DIV,
     *                           STAR(STARKEYS(Y)),
     *                           NEWKEYS(Y),
     *                           CMONY(BVAL(DIV),10,VALUNIT),
     *                           TOTSIGN,
     *                           DBNSHR(DIV,BGOFHS),
     *                           CMONY(BWIN(DIV),11,VALUNIT),
     *                           DBNPAD(DIV,BGOFHS),
     *                           CMONY(BPAD(DIV),11,VALUNIT)
              ENDIF
              LIN = LIN + 1
           ENDIF
        ENDDO
C
        IF(LIN.LE.15) THEN       ! LINES 5,...,15 USED FOR DISPLAY OF DIVISIONS
           DO I=LIN,15
              X=I-4
              WRITE (XNEW(I),9021)(SDAT(K,X),K=9,13),
     *                           CMONY(BSAL(X),11,BETUNIT)
           ENDDO   
        ENDIF
C
C TOTALS FOR DIVISIONS AND PREVIOUS SALES TOTAL INFO.
C
        LIN=16
        X=LIN-4
        WRITE (XNEW(LIN),9022) TOTWON(1),
     *                           CMONY(TOTWON(2),11,VALUNIT),
     *                           TOTPAD(1),
     *                           CMONY(TOTPAD(2),11,VALUNIT),
     *                           (SDAT(K,X),K=9,13),
     *                           CMONY(BSAL(X),11,BETUNIT)
        LIN = LIN + 1
C
C NOW DISPLAY SUBPHASE 1-3 AND TOTAL SALES INFO.
C
        TEMPLINE = XNEW(LIN)
        WRITE (TEMPLINE(1:37),9023) DBNSPH(1),DBNSPH(2),DBNSPH(3)
        WRITE(TEMPLINE(60:80),90232) 
     *        CMONY(IDINT(BTOT)+DBNPOL(1,BGOFHS),11,BETUNIT)
        XNEW(LIN) = TEMPLINE
        LIN = LIN + 1
C
C NOW DISPLAY SUBPHASES 4-5, WORST HIT, SECOND WORST HIT.
C
        TEMPLINE = XNEW(LIN)
        IF(DBNSPH(5).EQ.0) DBNSPH(5)=DBNPHS(2)
        WRITE (TEMPLINE(1:24),9024) DBNSPH(4), DBNSPH(5)
        IF(LOT_DIV(19).NE.0) WRITE(TEMPLINE(25:39),90241) DBNWST
        IF(LOT_DIV(20).NE.0) WRITE(TEMPLINE(40:60),90242) DBNWS2
        XNEW(LIN) = TEMPLINE
        LIN = LIN + 1
C
C NOW DISPLAY WINNING NUMBERS.
C
        WRITE (XNEW(  LIN),9081)        !--- TITLE.
        LIN = LIN + 1
C
C RETURN IF GAME STATUS IS OPEN.
C
        IF (DBNSTS .LT. GAMENV) THEN
           DO I =1,3
              WRITE (XNEW(  LIN),908)
              LIN = LIN + 1
           ENDDO
           RETURN
        ENDIF
C
        WRITE (XNEW(  LIN),9082) (DBNWIN(Q),Q=1,25)  !--- NUMS 1-25
        LIN = LIN + 1
C
        WRITE (XNEW(  LIN),9082) (DBNWIN(Q),Q=26,50) !--- NUMS 26-50
        LIN = LIN + 1
C
        WRITE (XNEW(  LIN),9082) (DBNWIN(Q),Q=51,75) !--- NUMS 51-75
C
        RETURN
C
C*********************** Format Statements ****************************
C
800     format (I1,A1,'/',I1,' ')
801     format ('Advance   ')
802     format ('Offline   ')
803     format ('Previous  ')
C
C Screen header format statements.
C
901     format ('Fullhouse  ',I1,4X,' game data for draw ',I4)
902     format (4A4)
9010    format ('Round ',I2,'/',I4,15X,' game is ',A8)
903     format ('---DIV--- ---PRIZE-- ',4('-'),'SHARES WON',4('-'),
     *      1X,2('-'),'FRACTIONS PAID--',1X,7('-'),'SALES',9('-'))
C
C Format statements to display divisions, Prizes, shares won, etc.
C
9020    format (I2,A1,A8,A10,A1,I6,1X,A11,1X,I6,A11,1X,
     *         5A2,A11)
90201   format (I2,A1,A8,A10,A1,I6,1X,A11,1X,I6,A11)
9021    format (2X,' (--)    ',48X,5A2,A11)    !--- No division.
9022    format (1X,13X,'Total',3X,I6,1X,A11,1X,I6,A11,1X,5A2,A11)
9023    format ('Phase1 : ',I2,2X,'Phase2 : ',I2,2X,'Phase3 : ',I2) ! 01:37
90232   FORMAT ('TOTAL   ',2X,A11)            ! 60:80
9024    format ('Phase4 : ',I2,1X,'*PhaseFH: ',I2)   ! 01:24  
90241   FORMAT (2X,'Worst hits:',I2) ! 25:39
90242   FORMAT (1X,'Second Worst hits:',I2) ! 40:60
C
C Blank line.
C
908     format (80(' '))
C
C Format statements to display winning numbers and sales.
C
9081    format ('Winning numbers :   ',60X)
9082    format (25(I2.2,1X))
3000    format ('Enter !bingo game index ')
C
C Error statements.
C
3010    format ('Fullhouse ',I1,' game not active')
3020    format (5A4,' open error ',I4)
3030    format (5A4,' read error ',I4,' record > ',I4)
3040    format ('Input error ')
3050    format ('Value error ')
        END
C
