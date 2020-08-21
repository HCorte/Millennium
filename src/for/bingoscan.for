C BINGOSCAN.FOR
C
C  V08 08-JUN-2000 UXN GOTO 3000 removed.
C  V07 02-MAR-2000 RXK Separate scan for final results added (BFHSCANF).
C  V06 01-FEB-2000 RXK Changed for new Bingo rules
C  V05 30-JAN-1995 HXK Change results display for operators
C  V04 07-JAN-1995 HXK Make results entry for prognosis optional
C  V03 06-JAN-1995 HXK Accept results for bingo A,B prognosis
C  V02 19-DEC-1994 HXK Initialise DBNSHR in RESCOM
C  V01 23-OCT-1994 GXA Initial revision.
C
C This program will build a bitmap from the winning numbers entered here,
C scans Draw files and TMF, dispalys results of scan.
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
        PROGRAM BINGOSCAN
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:BNGDNAM.DEF'


C
        INTEGER*4   ST, I, GIND, GTYP, DRAW, FLAG1, FLAG2, K, EXT
        INTEGER*4   FILCNT, SER
        INTEGER*4   FDB(7)
        INTEGER*4   SUBGAME, NUM, PHS
        INTEGER*4   WRKPHS(BGOSPH),WRKWST,WRKWS2,WRKNDF(BGODIV)
        INTEGER*4   BMSCAN(BGOPHS,BGODIV,BGONBR)!# of wins won on ball N
        INTEGER*4   BFSCAN(BGOPHS,BGODIV,BGONBR)!final # of wins won on ball N
        INTEGER*4   BWSCAN(BGOPHS,BGONBR)       !# worst match on ball N
        INTEGER*4   BW2SCAN(BGOPHS,BGONBR)      !# second worst match on ball N
        INTEGER*4   OTH
C
        CHARACTER*23  INPUT_TEXT
C
        LOGICAL   EOF, NEW, FINAL
        LOGICAL   TMFSCAN /.FALSE./
        LOGICAL   ENDFIL /.FALSE./
C
        INTEGER*4   LOGREC(LMUREC)
C
        INTEGER*4   TEMP
        BYTE        I1TEMP(4)
        EQUIVALENCE (TEMP,I1TEMP)
C
        INTEGER*4   FILES(5,200)
        CHARACTER   CFILES(200)*20
        EQUIVALENCE (FILES,CFILES)
C
        CALL COPYRITE
        TYPE*,IAM()
        TYPE*,IAM(),
     *  '<<<<< BINGOSCAN Bingo Winner Scanning Control Program  V01 >>>>>'
        TYPE*,IAM()
C
        GIND  = 1 
        CALL FASTSET(0,BMSCAN,BGOPHS*BGODIV*BGONBR)
        CALL FASTSET(0,BFSCAN,BGOPHS*BGODIV*BGONBR)
        CALL FASTSET(99,BWSCAN,BGOPHS*BGONBR)
        CALL FASTSET(99,BW2SCAN,BGOPHS*BGONBR)
C
        CALL BWIN_WININT(CFILES,FILCNT)
C
        IF(FILCNT.EQ.0) TYPE*,IAM(),' No draw files found, scanning only TMF.'
C
        DRAW = BNGDRW(GIND)
C
        IF(DAYSTS.EQ.DSOPEN) THEN 
          CALL WIMG(5,'Do you want scan TMF [Y/N] ')
          CALL YESNO(FLAG1)
          IF(FLAG1.EQ.1) TMFSCAN = .TRUE.
        ENDIF
C
        CALL WIMG(5,'Do you want to enter numbers manually [Y/N] ')
        CALL YESNO(FLAG1)
        IF(FLAG1.EQ.1) THEN
           WRITE(5,901) IAM(),GTNAMES(TBNG),GIND,DRAW
100        CONTINUE
           WRITE(5,904) 'Bingo Fullhouse '
           TYPE*,IAM(),' Prognosis numbers entry'
           DO I=1,BGONBR
              WRITE(INPUT_TEXT,903) I
              CALL INPNUM(INPUT_TEXT,NUM,1,BGONBR,EXT)
              IF(EXT.LT.0) GOTO 100
              LBNWIN(I,GIND)=NUM
           ENDDO
           SUBGAME = BGOFHS
           WRITE(5,902) IAM(),(LBNWIN(K,GIND),K=1,BGONBR)
           CALL BNGCHK(SUBGAME,ST)
           IF(ST.NE.0) THEN
              TYPE*,IAM(),' Duplicate numbers entered, please re-enter'
              GOTO 100
           ENDIF
           CALL WIMG(5,'Are the numbers entered correct [Y/N] ')
           CALL YESNO(FLAG2)
           IF(FLAG2.NE.1) GOTO 100
        ELSE
           WRITE (5,905)
           WRITE (5,906) IAM(),(LBNWIN(K,GIND),K=1,BGONBR)
           WRITE (5,907)
           CALL WIMG(5,'Are the numbers correct [Y/N] ')
           CALL YESNO(FLAG2)
           IF(FLAG2.NE.1) CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
C START DRAW FILE SCAN.
C
        FINAL=.FALSE.
1000    CONTINUE
C
        DO 3000 I=1,FILCNT
           CALL WIN_OPNDRW(FILES(1,I),PTMF)
           CALL IOINIT(FDB,PTMF,128*256)
           WRITE(5,910) IAM(),(FILES(K,I),K=1,5)
           EOF = .FALSE.
           NEW=.TRUE.
C
C SCAN DRAW FILES
C
2000       CONTINUE
           CALL READDRWN(LOGREC,FDB,EOF,NEW)
           IF(EOF) THEN
              CALL USRCLOS1(PTMF)
              GOTO 3000
           ENDIF
           CALL LOGTRA(TRABUF,LOGREC)
           GTYP=TRABUF(TGAMTYP)
           IF(GTYP.NE.TBNG) GOTO 2000
C
C CHECK IF WINNER
C
           IF(FINAL) THEN
              CALL BFHSCANF(TRABUF,BFSCAN,WRKPHS)
           ELSE
              CALL BFHSCAN(TRABUF,BMSCAN,BWSCAN,BW2SCAN)
           ENDIF
           GOTO 2000
C
3000    CONTINUE

        IF(FILCNT.GT.0.AND.FINAL)
     *     TYPE*,IAM(),'Draw file scan for phase information is complete '
C
C IF DRAW FILES SCAN COMPLETED THEN SCAN TMF
C
        IF(TMFSCAN) THEN
           CALL OPENW(PTMF,SFNAMES(1,PTMF),0,4,0,ST)
           IF(ST.NE.0) THEN
              CALL FILERR(SFNAMES(1,PTMF),1,ST,0)
              GOTO 5000
           ENDIF
           CALL TOPEN(PTMF)
           TYPE*,IAM(),' Scanning TM for Bingo winners'
           SER=1
4000       CONTINUE
           CALL READTMF(LOGREC,SER,ENDFIL)
           IF(ENDFIL) GOTO 5000
           CALL LOGTRA(TRABUF,LOGREC)
           GTYP=TRABUF(TGAMTYP)
           IF(GTYP.NE.TBNG) GOTO 4000

           IF(FINAL) THEN
              CALL BFHSCANF(TRABUF,BFSCAN,WRKPHS)
           ELSE
              CALL BFHSCAN(TRABUF,BMSCAN,BWSCAN,BW2SCAN)
           ENDIF
           GOTO 4000
C
5000       CONTINUE
           CALL USRCLOS1(PTMF)
           IF(FINAL) TYPE*,IAM(),'Tmf scan for phase information is complete '
        ENDIF
C
C PRELIMINARY SCAN  COMPLETE, DISPLAY RESULTS, 
C IF ANY "OTHER" DIVISION SET THEN DO THE FINAL SCAN
C
        IF(.NOT.FINAL) THEN
           CALL BSCANRES(BMSCAN,BWSCAN,BW2SCAN,WRKPHS,WRKWST,
     *                    WRKWS2,WRKNDF,GIND)
           OTH=0
           DO I=1,8
              IF(LBNOTH(I,BGOFHS,GIND).NE.0) OTH=1
           ENDDO
           IF(OTH.EQ.0) GOTO 6000
           FINAL=.TRUE.
           GOTO 1000
        ENDIF
C
6000    CONTINUE 
        TYPE*,IAM()
        TYPE*,IAM(),'Number for Subphase 1  ',WRKPHS(1) 
        TYPE*,IAM(),'Number for Subphase 2  ',WRKPHS(2) 
        TYPE*,IAM(),'Number for Subphase 3  ',WRKPHS(3) 
        TYPE*,IAM(),'Number for Subphase 4  ',WRKPHS(4) 
        TYPE*,IAM(),'Number for Phase 2 (FH)',WRKPHS(LBNNSP(GIND)) 
        IF(LBNMAT(BGOMAXMAP+1,BGOFHS,GIND).NE.0) 
     *     TYPE*,IAM(),'Number for Worst Match',WRKWST
        IF(LBNMAT(BGOMAXMAP+2,BGOFHS,GIND).NE.0) 
     *     TYPE*,IAM(),'Number for Second Worst Match',WRKWS2
        DO I=1,BGODIV
           IF(LBNFST(I,BGOFHS,GIND).NE.0)
     *     TYPE*,IAM(),'Number for first hit of ',
     *           BNGDNAMES(LBNDNR(I,GIND)),WRKNDF(I)
        ENDDO         

        PHS=1
        DO I=1,10
           IF(LBNDNR(I,GIND).GT.0) THEN
              IF(LBNFST(I,BGOFHS,GIND).GT.0) THEN
                 TYPE*,IAM(),'Division ',BNGDNAMES(LBNDNR(I,GIND)),' won',
     *                        BMSCAN(PHS,I,WRKNDF(I)),' times'         
              ELSE  
                 TYPE*,IAM(),'Division ',BNGDNAMES(LBNDNR(I,GIND)),' won',
     *                        BFSCAN(PHS,I,WRKPHS((I+1)/2)),' times'
              ENDIF
           ENDIF
        ENDDO   

        PHS=2
        DO I=11,15
           IF(LBNDNR(I,GIND).GT.0)
     *     TYPE*,IAM(),'Division ',BNGDNAMES(LBNDNR(I,GIND)),' won',
     *         BMSCAN(PHS,I,WRKPHS(LBNNSP(GIND))),' times'
        ENDDO

        TYPE*,IAM()
        TYPE*,IAM(),'Bingo Winner Scan is completed '
        TYPE*,IAM()
C

900     FORMAT(1X,A,1X,A8,I1,' draw ',I4,' invalid game status> ',I4)
901     FORMAT(1X,A,1X,A8,I1,' draw ',I4)
902     FORMAT(1X,A,' Numbers entered: ', 5(/10X,15(2X,I2.2)))
903     FORMAT(1X,'Enter drawn number ',I2,':')
904     FORMAT(1X,A,1X,'Subgame ',A16)
905     FORMAT(1X,A,1X,'Bingo FH numbers entered .... ')
906     FORMAT(1X,A,5(/10X,15(2X,I2.2)))
907     FORMAT(1X)
910     FORMAT(1X,A,1X,'Scanning file ',5A4,' for winners ')
        END 
