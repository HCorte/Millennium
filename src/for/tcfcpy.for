C TCFCPY.FOR
C
C V01  1-JUL-92 WLM INITIAL RELEASE FOR POLAND

C PROGRAM TO COPY TCF FILE
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM TCFCPY
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DESPAR.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
C
        INTEGER*4  TUBSIZ, TSIZE
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
        PARAMETER (TSIZE=1000000)
C
        INTEGER*4 BIGTCF(TSIZE)
        INTEGER*4 NEWBUF(TUBSIZ)
        INTEGER*4 TCFBUF(TUBSIZ)
        INTEGER*4 T4BUF(LREC*4)
        BYTE      T1BUF(LREC*4*4)
        EQUIVALENCE (T4BUF,T1BUF)
C
        INTEGER*4 TCFNAM(5), TCCNAM(5)
        CHARACTER*20 TCFNAMC, TCCNAMC
        EQUIVALENCE (TCFNAM(1),TCFNAMC)
        EQUIVALENCE (TCCNAM(1),TCCNAMC)
C
        INTEGER*4 CNTWRIT, ST, TCFSEC, TCCSEC, K, FLAG
C
C
C
        CALL COPYRITE
C
C OPEN INPUT CARRYOVER FILE
C
50      CONTINUE
        CALL WIMG(5,'Enter input carryover file name  ')
        ACCEPT 801,TCFNAMC
        IF(TCFNAMC(1:2).EQ.'E ')CALL GSTOP(GEXIT_OPABORT)
C
        CALL IOPEN(TCFNAM(1),TCF,LREC*2,LCDC,LSER*2-1,ST)
        IF(ST.NE.0) CALL FILERR(TCFNAM(1),1,ST,0)
        CALL ITUBSIZE(TCF,TUBSIZ)
C
C GET OUTPUT FILE NAME
C
        CALL WIMG(5,'Enter output carryover file name ')
        ACCEPT 801,TCCNAMC
        IF(TCCNAMC(1:2).EQ.'E ')CALL GSTOP(GEXIT_OPABORT)
C
        CALL IOPEN(TCCNAM(1),TCC,LREC*2,LCDC,LSER*2-1,ST)
        IF(ST.NE.0) CALL FILERR(TCCNAM(1),1,ST,0)
        CALL IINIB(TCC,BIGTCF,TSIZE)
        CALL INOCHKS(TCC)
        CALL ITUBSIZE(TCC,TUBSIZ)
C
C GET SIZE OF FILE IN SECTORS
C
        CALL VAXGETFSIZ(TCF,TCFSEC)
        CALL VAXGETFSIZ(TCC,TCCSEC)
C
C CONFIRM COPY
C
        TYPE 900,IAM(),IAM(),(TCFNAM(K),K=1,5),TCFSEC,
     *           IAM(),(TCCNAM(K),K=1,5),TCCSEC
        CALL PRMYESNO(' Is this correct [Y/N/Exit] ?',FLAG)
C
        IF(FLAG.EQ.1) GOTO 90           !YES
C
        CALL ICLOSE(TCF,TCFBUF,ST)
        IF(ST.NE.0) CALL FILERR(TCFNAM(1),4,ST,0)
        CALL ICLOSB(TCC,BIGTCF,NEWBUF,ST)
        IF(ST.NE.0) CALL FILERR(TCCNAM(1),4,ST,0)
C
        IF(FLAG.EQ.2) GOTO 50           !NO
C
        CALL GSTOP(GEXIT_OPABORT)       !EXIT
C
C
90      CONTINUE
C
C SCAN CARRYOVER FILE
C
        CNTWRIT=0
100     CONTINUE
        CALL ISREAD(T4BUF,TCF,TCFBUF,ST)
        IF(ST.EQ.ERREND) THEN
          CALL ICLOSE(TCF,TCFBUF,ST)
          IF(ST.NE.0) CALL FILERR(TCFNAM(1),4,ST,0)
          CALL ICLOSB(TCC,BIGTCF,NEWBUF,ST)
          IF(ST.NE.0) CALL FILERR(TCCNAM(1),4,ST,0)
          TYPE*,IAM(),' Total number of copied tickets   = ',CNTWRIT
          TYPE*,IAM(),' Carryover file copy complete'
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        IF(ST.NE.0) CALL FILERR(TCFNAM(1),2,ST,0)
C
        CALL IWRIBF(T4BUF,TCC,BIGTCF,NEWBUF,ST)
        IF(ST.NE.0) CALL FILERR(TCCNAM(1),3,ST,0)
        CNTWRIT=CNTWRIT+1
        IF(MOD(CNTWRIT,50000).EQ.0) TYPE 902,IAM(),CNTWRIT
        GOTO 100
C
C FORMATS
C
801     FORMAT(A)
C
900     FORMAT(1X,A,' Copying carryover records',/,1X,A,'  from ',
     *         5A4,I10,' sectors',/,
     *         1X,A,'  to   ',5A4,I10,' sectors')
902     FORMAT(1X,A,I8,' tickets copied')
C
C
C
        END
