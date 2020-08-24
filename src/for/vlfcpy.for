C VLFCPY.FOR
C
C V02 04-NOV-96 GLS EXPANDED VLF RECORD 
C V01 17-FEB-92 WLM INITIAL RELEASE FOR POLAND
C
C PROGRAM TO COPY VLF FILE
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
        PROGRAM VLFCPY
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DESPAR.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
C
        INTEGER*4  TUBSIZ, VSIZE
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
        PARAMETER (VSIZE=1000000)
C
        INTEGER*4 BIGVLF(VSIZE)
        INTEGER*4 NEWBUF(TUBSIZ)
        INTEGER*4 VLFBUF(TUBSIZ)
        BYTE      V1BUF(VFLEN*4*4)         !V02
        EQUIVALENCE (V4BUF,V1BUF)
C
        INTEGER*4 VLFNAM(5), VLCNAM(5)
        CHARACTER*20 VLFNAMC, VLCNAMC
        EQUIVALENCE (VLFNAM(1),VLFNAMC)
        EQUIVALENCE (VLCNAM(1),VLCNAMC)
C
        INTEGER*4 CNTWRIT, VLFSEC, VLCSEC, FLAG
        INTEGER*4 K,ST
C
C
C
        CALL COPYRITE
C
C OPEN INPUT VALIDATION FILE
C
50      CONTINUE
        CALL WIMG(5,'Enter input validation file name  ')
        ACCEPT 801,VLFNAMC
        IF(VLFNAMC(1:2).EQ.'E ')CALL GSTOP(GEXIT_OPABORT)
C
        CALL IOPEN(VLFNAM(1),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF(ST.NE.0) CALL FILERR(VLFNAM(1),1,ST,0)
        CALL ITUBSIZE(VLF,TUBSIZ)
C
C GET OUTPUT FILE NAME
C
        CALL WIMG(5,'Enter output validation file name ')
        ACCEPT 801,VLCNAMC
        IF(VLCNAMC(1:2).EQ.'E ')CALL GSTOP(GEXIT_OPABORT)
C
        CALL IOPEN(VLCNAM(1),VLC,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF(ST.NE.0) CALL FILERR(VLCNAM(1),1,ST,0)
C
        CALL IINIB(VLC,BIGVLF,VSIZE)
        CALL INOCHKS(VLC)
        CALL ITUBSIZE(VLC,TUBSIZ)
C
C GET SIZE OF FILE IN SECTORS
C
        CALL VAXGETFSIZ(VLF,VLFSEC)
        CALL VAXGETFSIZ(VLC,VLCSEC)
C
C CONFIRM COPY
C
        TYPE 900,IAM(),IAM(),(VLFNAM(K),K=1,5),VLFSEC,
     *           IAM(),(VLCNAM(K),K=1,5),VLCSEC
        CALL PRMYESNO(' Is this correct [Y/N/Exit] ?',FLAG)
C
        IF(FLAG.EQ.1) GOTO 90           !YES 
C
        CALL ICLOSE(VLF,VLFBUF,ST)
        IF(ST.NE.0) CALL FILERR(VLFNAM(1),4,ST,0)
        CALL ICLOSB(VLC,BIGVLF,NEWBUF,ST)
        IF(ST.NE.0) CALL FILERR(VLCNAM(1),4,ST,0)
C
        IF(FLAG.EQ.2) GOTO 50           !NO
C
        CALL GSTOP(GEXIT_OPABORT)       !EXIT
C
90      CONTINUE
C
C SCAN VALIDATION FILE
C
        CNTWRIT=0
100     CONTINUE
        CALL ISREAD(V4BUF,VLF,VLFBUF,ST)
        IF(ST.EQ.ERREND) THEN
          CALL ICLOSE(VLF,VLFBUF,ST)
          IF(ST.NE.0) CALL FILERR(VLFNAM(1),4,ST,0)
          CALL ICLOSB(VLC,BIGVLF,NEWBUF,ST)
          IF(ST.NE.0) CALL FILERR(VLCNAM(1),4,ST,0)
          TYPE*,IAM(),' Total number of copied tickets   = ',CNTWRIT
          TYPE*,IAM(),' Validation file copy complete'
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        IF(ST.NE.0) CALL FILERR(VLFNAM(1),2,ST,0)
        CALL IWRIBF(V4BUF,VLC,BIGVLF,NEWBUF,ST)
        IF(ST.NE.0) CALL FILERR(VLCNAM(1),3,ST,0)
C
        CNTWRIT=CNTWRIT+1
        IF(MOD(CNTWRIT,50000).EQ.0) TYPE 902,IAM(),CNTWRIT
        GOTO 100
C
C FORMATS
C
801     FORMAT(A)
C
900     FORMAT(1X,A,' Copying validation records',/,1X,A,'  from ',
     *         5A4,I10,' sectors',/,
     *         1X,A,'  to   ',5A4,I10,' sectors')
902     FORMAT(1X,A,I8,' tickets copied')
C
C
C
        END
