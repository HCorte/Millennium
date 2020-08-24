C
C PROGRAM DMPTOOL
C
C V03 17-MAR-2016 SCML M16 PROJECT
C V02 16-MAR-2011 RXK Format change due increase of NUMAGT
C V01 14-JUN-2000 PXO
C
C PROGRAM TO DUMP DATA FROM GAME/SYSTEM FILES
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
        PROGRAM DMPTOOL
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:RESCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
C
        ! VARIABLES
        INTEGER*4  DRAW
        INTEGER*4  GNUM
        INTEGER*4  GIND
        INTEGER*4  EXT
        INTEGER*4  GTYP
        INTEGER*4  K
        INTEGER*4  I
        INTEGER*4  ST
        INTEGER*4  DMPCDC
        INTEGER*4  SAGT,EAGT
        INTEGER*4  DAT
        INTEGER*4  PLAN
        INTEGER*4  MONEY_UNIT
        INTEGER*4  FILE(5)
        INTEGER*4  REPLUN
        INTEGER*4  FILEORDISPLAY
        INTEGER*4  REPFILE(5)
        CHARACTER  CREPFILE*20
        CHARACTER  HEAD*80
        INTEGER*2  DBUF(LDATE_LEN)
C
        EQUIVALENCE (REPFILE,CREPFILE)
C
        INTEGER*4  TYPDAF, TYPASF, TYPWRF, TYPSCF, TYPPPF, TYPTEB               !SYSTEM FILES TO DUMP
        PARAMETER  (TYPDAF = MAXTYP + 1)
        PARAMETER  (TYPASF = TYPDAF + 1)
        PARAMETER  (TYPWRF = TYPASF + 1)
        PARAMETER  (TYPSCF = TYPWRF + 1)
        PARAMETER  (TYPPPF = TYPSCF + 1)
        PARAMETER  (TYPTEB = TYPPPF + 1)
C
        INTEGER*4  LASTF                                                        !LAST FILE (UPDATE THIS IF YOU CHANGE THE NUMBER OF SYSTEM FILES)
        PARAMETER  (LASTF = TYPTEB)
C
        CALL COPYRITE
C
C
        TYPE*,IAM(),
     *  '<<<<< DMPTOOL >>>>>'
        TYPE*,IAM()
C
C GET SYSTEM CONFIGURATION INFO.
C
        CALL GETSCONF(SCFREC,ST)
        IF(ST.NE.0) THEN
           TYPE*,IAM(),'Unable to get System Configuration info.'
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
        MONEY_UNIT = BETUNIT
C
100     CONTINUE
        WRITE(6,899)  MONEY_UNIT
        WRITE(6,900)  (K,GTNAMES(K),K=1,MAXTYP)
        WRITE(6,901)  TYPDAF, 'DAF file'
        WRITE(6,902)  TYPASF, 'ASF file'
        WRITE(6,9021) TYPWRF, 'WRF file'
        WRITE(6,9022) TYPSCF, 'SCF file'
        WRITE(6,9023) TYPPPF, 'PPF file'
        WRITE(6,903)  TYPTEB, 'Change money unit'
        CALL INPNUM('Enter file type ',GTYP,1,LASTF,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
        IF(GTYP.GT.MAXTYP) GOTO 10
C
        IF(GTYP.EQ.TINS) THEN
          TYPE*
          TYPE*,'Sorry, not applicable to Instant games'
          GOTO 100
        ELSEIF(GTYP.EQ.TEUM) THEN
          TYPE*
          TYPE*,'Sorry, not applicable to Euromillions games'
          GOTO 100
        ELSEIF(GTYP.EQ.TODS) THEN
          TYPE*
          TYPE*,'Sorry, not applicable to Oddset games'
          GOTO 100
        ELSEIF(GTYP.EQ.TRAF) THEN
          TYPE*
          TYPE*,'Sorry, not applicable to Raffle games'
          GOTO 100
        ENDIF
C
        CALL INPNUM('Enter game index ',GIND,1,MAXIND,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
        GNUM=SCFGTN(GTYP,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
           TYPE*,'Sorry, game selected is not active'
           GOTO 100
        ENDIF
        CALL FASTMOV(SCFGFN(1,GNUM),FILE,5)
C
C
        CALL INPNUM('Enter draw number ',DRAW,1,99999,EXT)
        IF(EXT.LT.0) GOTO 100
        WRITE(HEAD,905) (SCFGFN(K,GNUM),K=1,5),GIND,DRAW,MONEY_UNIT
        GOTO 40
C
C IF SELECTION IS NOT A GAME FILE
C
10      CONTINUE
        IF(GTYP.EQ.TYPDAF) THEN                                                 !DAF FILE
          WRITE(6,907), IAM()
          CALL PRMDAT(DMPCDC,EXT)
          IF(EXT.LT.0) GOTO 100
          IF (DMPCDC.EQ.0) DMPCDC = DAYCDC
          CALL FASTMOV(SCFSFN(1,DAF),FILE,5)
          DAT=DMPCDC
          DBUF(5)=DAT
          CALL LCDATE(DBUF)
          WRITE(HEAD,909)FILE,DMPCDC,DBUF(VJUL),DBUF(VYEAR2),
     *                   (DBUF(I),I=VDNAM,13), MONEY_UNIT
          GOTO 40
C
        ELSEIF(GTYP.EQ.TYPASF) THEN                                             !ASF FILE
           CALL FASTMOV(SCFSFN(1,ASF),FILE,5)
           CALL INPNUM('Enter start terminal number :',SAGT,1,NUMAGT,EXT)
           IF(EXT.LT.0) GOTO 100
           CALL INPNUM('Enter end terminal number :',EAGT,SAGT,NUMAGT,EXT)
           IF(EXT.LT.0) GOTO 100
           WRITE(HEAD,910) FILE,SAGT,EAGT,MONEY_UNIT
           GOTO 40
C
        ELSEIF(GTYP.EQ.TYPWRF) THEN                                             !RDF FILE
           CALL FASTMOV(SCFSFN(1,RDF),FILE,5)
           WRITE(HEAD,911) FILE, MONEY_UNIT
           GOTO 40
C
        ELSEIF(GTYP.EQ.TYPSCF) THEN                                             !SCF FILE
           WRITE(HEAD,913) 'FILE:SCF.FIL        ', MONEY_UNIT
           GOTO 40
C
        ELSEIF(GTYP.EQ.TYPPPF) THEN                                             !PPF FILE
           CALL FASTMOV(SCFSFN(1,PPF),FILE,5)
           CALL INPNUM('Enter Plan number :',PLAN,1,1000,EXT)
           IF(EXT.LT.0) GOTO 100
           WRITE(HEAD,912) FILE, PLAN, MONEY_UNIT
           GOTO 40
C
        ELSEIF(GTYP.EQ.TYPTEB) THEN                                             !CHANGE MONEY UNIT
           CALL INPNUM('Enter money unit ',MONEY_UNIT,-9999,9999,EXT)
           IF(EXT.LT.0) GOTO 100
        ENDIF
        GOTO 100
C
40      CONTINUE
        REPLUN = 6
        CALL INPNUM('Report to console(1) or file(2) ?',FILEORDISPLAY,1,2,EXT)
        IF(EXT.LT.0) GOTO 100
        IF(FILEORDISPLAY.EQ.1) GOTO 60
        REPLUN = 7
C
50      CONTINUE
        CALL WIMG(5,'Enter report file name:               ')
        READ(5,904) REPFILE
        IF(CREPFILE(1:2).EQ.'E '.OR.CREPFILE(1:2).EQ.'e ') GOTO 100
        CALL ROPEN(CREPFILE,REPLUN,ST)
        IF(ST.NE.0) THEN
           TYPE*,IAM(),CREPFILE,' Open error  st - ',ST
           CALL USRCLOS1(REPLUN)
           GOTO 50
        ENDIF
C
60      CONTINUE
C
        WRITE(REPLUN,908) HEAD
        WRITE(REPLUN,906)
        WRITE(REPLUN,897)
        WRITE(REPLUN,898)
C
        IF(GTYP.EQ.TLTO) THEN
          CALL DMP_LTO(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TSPT) THEN
          CALL DMP_SPT(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TTGL) THEN
          CALL DMP_TGL(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TNBR) THEN
C          CALL DMP_NBR(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TKIK) THEN
          CALL DMP_KIK(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TSCR) THEN
          CALL DMP_SCR(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TWIT) THEN
          CALL DMP_WIT(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TTSL) THEN
          CALL DMP_TSL(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TBNG) THEN
          CALL DMP_BNG(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TDBL) THEN
          CALL DMP_DBL(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TCPL) THEN
          CALL DMP_CPL(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TSSC) THEN
          CALL DMP_SSC(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TTRP) THEN
          CALL DMP_TRP(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TSTR) THEN
          CALL DMP_STR(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TPAS) THEN
          CALL DMP_PAS(REPLUN,FILE,DRAW,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TYPDAF) THEN
          CALL DMP_DAF(REPLUN,FILE,DMPCDC,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TYPASF) THEN
          CALL DMP_ASF(REPLUN,FILE,SAGT,EAGT,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TYPWRF) THEN
          CALL DMP_WRF(REPLUN,FILE,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TYPSCF) THEN
          CALL DMP_RECSCF(REPLUN,FILE,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TYPPPF) THEN
          CALL DMP_PPF(REPLUN,FILE,PLAN,MONEY_UNIT)
        ELSEIF(GTYP.EQ.TYPTEB) THEN
          CALL DMP_TEB(REPLUN,FILE,DMPCDC,MONEY_UNIT)
        ELSE
          TYPE*,'Invalid game/file type ',GTYP
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        IF(FILEORDISPLAY.EQ.2) CALL USRCLOS1(REPLUN)
        GOTO 100
C
C
897     FORMAT(3X,'Ofs',9X,'Data',16X,'Variable',5X,'Description')
898     FORMAT(1X,5('-'),1X,12('-'),1X,14('-'),1X,12('-'),1X,32('-'))
899     FORMAT(//,1X,'Money unit currently in use:',I4)
900     FORMAT(/, <MAXTYP>(1X,I2,' - ',A8,/))
901     FORMAT(1X,I2,' - ', A)
902     FORMAT(1X,I2,' - ', A)
9021    FORMAT(1X,I2,' - ', A)
9022    FORMAT(1X,I2,' - ', A)
9023    FORMAT(1X,I2,' - ', A)
903     FORMAT(1X,I2,' - ', A,/)
904     FORMAT(5A4)
905     FORMAT(1X,5A4,' Index ',I4,'    Draw ',I4,'    Money unit used ',I4)
906     FORMAT(1X,79('='),/)
907     FORMAT(//3X,A,' Enter the CDC date of the dump')
908     FORMAT(A80)
909     FORMAT(1X,5A4,3X,I5,3X,I4,'/',I4,3X,7A2,' Money unit used ',I4)
910     FORMAT(1X,5A4,1X,'From term # ',I5,' to term # ',I5,
     *         ' Money unit used ',I4,5X)
911     FORMAT(1X,5A4,'    Money unit used ',I4,35X)
912     FORMAT(1X,5A4,' Plan ',I4,'    Money unit used ',I4)
913     FORMAT(1X,A20,'    Money unit used ',I4,35X)
        END
C
C END DMPTOOL.FOR
C
