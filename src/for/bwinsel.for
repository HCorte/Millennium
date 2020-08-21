C PROGRAM BWINSEL.FOR
C
C V12 25-APR-2000 OXK Fix for LBNNSP indexing
C V11 20-JAN-2000 RXK Changed for new Bingo.
C V10 13-JAN-2000 RXK Def-file for Bingo division names added.
C V09 14-DEC-1999 OXK MULTIWIN changes.
C V08 11-JAN-1999 GPW STOPSYS OPTIMIZATION
C V07 08-JAN-1995 HXK Added IAM functions to some output statements
C V06 08-JAN-1995 HXK Quick fix for GIND not being set at SHARE comparison
C V05 06-JAN-1995 HXK Check LBNSHR against those sent from TV studio (B_WRKSHR)
C V04 20-DEC-1994 HXK Changed Operator checks
C V03 30-NOV-1994 JXP LBNWST(GIND) = NUM CHANGE
C V02 23-NOV-1994 HXK Changed logic slightly, made minor bug fixes
C V01 27-OCT-1994 HXK Initial revision.
C  
C BINGO WINNER SELECTION PROGRAM.
C
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM BWINSEL
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:BNGDNAM.DEF'
C
        INTEGER*4 TUBSIZ
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
C
        INTEGER*4 TFDB(7)
        INTEGER*4 LOGREC(LMUREC)

        INTEGER*4 GTYP, WIN, FILCNT
        INTEGER*4 I, S
        INTEGER*4 NUM, EXT, PHS, GIND, IND, FLAG, DUMMY
        INTEGER*4 NTSK
        INTEGER*4 INDTSK              !FUNCTION   
        INTEGER*4 FILES(5,200)
        INTEGER*4 LOT_DIV(BGOLOT)
        INTEGER*4 NSUBPH              !number of subphases

        CHARACTER CFILES(200)*20
        EQUIVALENCE (FILES,CFILES)
        CHARACTER*40 STRPRM                                          

        LOGICAL   DISCREPENCY, EOF, NEW
 
C
        CALL COPYRITE
C
        CALL FASTSET(0,LOT_DIV,BGOLOT)
        CARYSCAN(TBNG) = .FALSE.
C
C
C INITIALIZE WINNER SELECTION COMMON
C
        IF (.NOT.ISSUBPROC()) THEN
            TYPE*,IAM(),
     *            'This program can be run only from *WINTSK or MULTIWIN'
            CALL GSTOP(GEXIT_FATAL)
        ENDIF

        IF (STOPMOD.EQ.WINMANUAL) THEN
             CALL BWIN_WININT(CFILES,FILCNT)
        ELSE
             NTSK=INDTSK('BWINTSK ')
             CALL STORFIL(NTSK,CFILES,DUMMY,FILCNT,2,0)
        ENDIF

        IF(FILCNT.EQ.0) THEN
          TYPE*,IAM(),' Sorry, no Bingo winner selection today'
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL BWIN_WINLOD(1,DUMMY)
C
        DO 1000 GIND = 1,NUMBGO
C
C CHECK IF EVERYTHING IS OK.
C Get Lottery divisions.
C
          DO I=1,BGODIV
            IF(LBNDNR(I,GIND).NE.0) THEN
               LOT_DIV(LBNDNR(I,GIND)) = I
            ENDIF
          ENDDO
C
50        CONTINUE

          TYPE*,IAM(),'Phase1 (bingos) takes     ',LBNSPHH(1,GIND),' numbers'
          TYPE*,IAM(),'Phase2 (doubles) takes    ',LBNSPHH(2,GIND),' numbers'
          TYPE*,IAM(),'Phase3 (triples) takes    ',LBNSPHH(3,GIND),' numbers'
          TYPE*,IAM(),'Phase4 (quadruples) takes ',LBNSPHH(4,GIND),' numbers'
          TYPE*,IAM(),'Phase5 (fullhouse)  takes ',LBNSPHH(5,GIND),' numbers'
          IF(LOT_DIV(19).NE.0) THEN
              TYPE*,IAM(),'Worst Match takes',LBNWSTH(GIND),' numbers'
          ENDIF
          IF(LOT_DIV(20).NE.0) THEN
              TYPE*,IAM(),'Worst 2nd   takes',LBNWS2H(GIND),' numbers'
          ENDIF
          DO I=1,BGODIV
            IF(LBNFST(I,BGOFHS,GIND).NE.0) THEN
              TYPE*,IAM(),'First ',BNGDNAMES(LBNDNR(I,GIND)),' takes ',
     *                                   LBNNDFH(I,GIND),' numbers'
            ENDIF
          ENDDO
C
          CALL PRMYESNO('Are these values correct (Y/N) ',FLAG) 

          IF(FLAG.EQ.1) THEN
             DO IND = 1,NUMBGO
                DO PHS = 1,BGOSPH
                   LBNSPH(PHS,IND) = LBNSPHH(PHS,IND)
                ENDDO
                LBNWST(IND) = LBNWSTH(IND) 
                LBNWS2(IND) = LBNWS2H(IND) 
                DO I=1,BGODIV
                   IF(LBNFST(I,BGOFHS,IND).NE.0) THEN
                      LBNNDF(I,IND) = LBNNDFH(I,IND)
                   ENDIF
                ENDDO
             ENDDO
             GOTO 1000
          ELSEIF(FLAG.EQ.2) THEN
             NSUBPH = LBNNSP(GIND)
             IF(NSUBPH.LE.0) NSUBPH=5
             DO IND = 1,NUMBGO
                DO PHS = 1,NSUBPH
                   WRITE(STRPRM,970) PHS                           
                   CALL PRMNUM(STRPRM,NUM,0,BGONBR,EXT)            
                   IF(EXT.NE.0) CALL GSTOP(GEXIT_FATAL)
                   LBNSPHH(PHS,IND) = NUM
                ENDDO
C
                IF(LOT_DIV(19).NE.0) THEN
                   CALL PRMNUM('Enter the number for Worst match',
     *                          NUM,0,BGONBR,EXT)                  
                   IF(EXT.NE.0) CALL GSTOP(GEXIT_FATAL)
                   LBNWSTH(IND) = NUM
                ELSE
                   LBNWSTH(IND) = 0
                ENDIF
C
                IF(LOT_DIV(20).NE.0) THEN
                   CALL PRMNUM('Enter the number for 2nd Worst match', 
     *                          NUM,0,BGONBR,EXT)                      
                   IF(EXT.NE.0) CALL GSTOP(GEXIT_FATAL)
                   LBNWS2H(IND) = NUM
                ELSE
                   LBNWS2H(IND) = 0
                ENDIF
C
                DO I=1,BGODIV
                   IF(LBNFST(I,BGOFHS,IND).NE.0) THEN
                      WRITE(STRPRM,980) BNGDNAMES(LBNDNR(I,IND))          
                      CALL PRMNUM(STRPRM,NUM,0,BGONBR,EXT)
                      IF(EXT.NE.0) CALL GSTOP(GEXIT_FATAL)
                      LBNNDFH(I,IND) = NUM
                   ENDIF
                ENDDO
             ENDDO
             GOTO 50
          ELSE
             CALL GSTOP(GEXIT_FATAL)
          ENDIF
1000    CONTINUE
C
C START DRAW FILE SCAN.
C
        CALL FASTSET(0,V4BUF,VFLEN*4)

        DO 3000 I=1,FILCNT
           CALL WIN_OPNDRW(FILES(1,I),PTMF)
           CALL IOINIT(TFDB,PTMF,128*256)
           EOF = .FALSE.
           NEW=.TRUE.
           WRITE(6,910) IAM(),(FILES(S,I),S=1,5)

2030       CONTINUE
           CALL READDRWN(LOGREC,TFDB,EOF,NEW)
           IF(EOF) THEN
              CALL USRCLOS1(PTMF)
              GOTO 3000
           ENDIF
           CALL LOGTRA(TRABUF,LOGREC)
           GTYP=TRABUF(TGAMTYP)
           IF(GTYP.NE.TBNG) GOTO 2030
C
C CHECK IF WINNER
C
           CALL BWIN_POST(TRABUF)
           CALL BPHWIN(TRABUF,V4BUF,WIN)
           IF(WIN.NE.0) THEN
              CALL BWIN_WINLOD(2,V4BUF)
              CALL FASTSET(0,V4BUF,VFLEN*4)
           ENDIF
           GOTO 2030
C
C WINSEL/SCAN  COMPLETE
C STORE NEW PHASE CRITERIA TO BE USED BY WINNER SELECTION.
C
3000    CONTINUE
C
C CHECK GTECH SHARES AGAINST TV SHARES
C            
        DO GIND = 1,NUMBGO
           DISCREPENCY = .FALSE.
           DO I=1,BGODIV
              IF(LBNSHR(I,BGOFHS,GIND).NE.B_WRKSHR(I)) DISCREPENCY = .TRUE.
           ENDDO
           IF(DISCREPENCY) THEN
4000          CONTINUE
              WRITE(6,920) IAM()
              DO I=1,BGODIV
                 WRITE(6,930) IAM(),I,LBNSHR(I,BGOFHS,GIND),B_WRKSHR(I)
              ENDDO

              CALL PRMNUM(                                             
     * 'What set do you want to use? 1=Winsel,2=TV,3=Override, (E)xit',
     *        NUM,1,3,EXT)                                             

              IF(EXT.NE.0) CALL GSTOP(GEXIT_FATAL)
              IF(NUM.EQ.2) THEN
                 DO I=1,BGODIV
                    LBNSHR(I,BGOFHS,GIND) = B_WRKSHR(I)
                 ENDDO
              ENDIF
              IF(NUM.EQ.3) THEN
                 DO I=1,BGODIV
                    WRITE(STRPRM,990) I                           
                    CALL PRMNUM(STRPRM,NUM,0,9999999,EXT)         

                    IF(EXT.NE.0) CALL GSTOP(GEXIT_FATAL)
                    LBNSHR(I,BGOFHS,GIND) = NUM
                 ENDDO
              ENDIF
              WRITE(6,950) IAM()
              DO I=1,BGODIV
                 WRITE(6,960) IAM(),I,LBNSHR(I,BGOFHS,GIND)
              ENDDO

              CALL PRMYESNO('Are these values correct (Y/N) ',FLAG) 

              IF(FLAG.NE.1) GOTO 4000
           ENDIF
        ENDDO
C
C WINSEL COMPLETE, FLUSH BUFFERS TO DRAW FILES
C
        CALL BWIN_WINLOD(4,DUMMY)
        CALL BWIN_PSTGDF
        TYPE*,IAM(),' Draw file scan for winner selection is complete'
        CALL GSTOP(GEXIT_SUCCESS)
C
C
900     FORMAT(1X,A,1X,5A4,' read error> ',I4,' block> ',I8)
910     FORMAT(1X,A,1X,'Scanning file ',5A4,' for winners ')
920     FORMAT(1X,A,1X,' Num of shares:  Winsel      TV',/,
     *             19X,' ==============  ------  ------')
930     FORMAT(1X,A,1X,'         div ',I2,':',I8,I8)
940     FORMAT(1X,A,1X,' DIVISION ',I2)
950     FORMAT(1X,A,1X,' Num of shares taken ',/,
     *             19X,' ===================')
960     FORMAT(1X,A,1X,' div ',I2,':',I8)
970     FORMAT('Enter the number for Phase',I2)
980     FORMAT('Enter the number for div ',A8)
990     FORMAT(' Enter number of shares for DIVISION ',I2)
        END
