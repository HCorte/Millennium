C MLCOPVLF.FOR
C
C V02 14-JUL-1999 UXN Text changes.
C V01 25-MAY-1999 RXK INITIAL RELEASE FOR FINLAND (FROM MLMRGVLF.FOR)
C
C VLF -> VLC COPY PROGRAM
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1999 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
C
        PROGRAM MLCOPVLF
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:PRMVAL.DEF'
        INCLUDE 'INCLIB:PRMVLF.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:MULNAM.DEF'
C
        INTEGER*4  VSIZE, TUBSIZ
        INTEGER*4  I4VLWSIZ
        PARAMETER (I4VLWSIZ=8192)
        PARAMETER (VSIZE=8000000)
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
C
        INTEGER*4 OFDB(7),NFDB(7)
        INTEGER*4 COPBUF(12800)
        INTEGER*4 BLOCK
        INTEGER*4 NEWSIZ, OLDSIZ,K,ST
C
C
        CALL COPYRITE
        CALL SNIF_AND_WRKSET
C
C OPEN SCF
C
	CALL GETSCONF(SCFREC,ST)
C
C OPEN VLF FILE AND VLC FILE
C
        CALL OPENW(VLF,SCFSFN(1,VLF),4,0,0,ST)
        CALL IOINIT(OFDB,VLF,200*256)
        IF(ST.NE.0) THEN
          WRITE(5,900) IAM(),(SCFSFN(K,VLF),K=1,5),ST
          CALL GPAUSE
        ENDIF
C
        CALL OPENW(VLC,SCFSFN(1,VLC),4,0,0,ST)
        CALL IOINIT(NFDB,VLC,200*256)
        IF(ST.NE.0) THEN
          WRITE(5,900) IAM(),(SCFSFN(K,VLC),K=1,5),ST
          CALL GPAUSE
        ENDIF
C
C CHECK SIZE OF OLD AND NEW FILES
C
        CALL GETSIZ(VLF,OLDSIZ)
        CALL GETSIZ(VLC,NEWSIZ)
        IF(OLDSIZ.NE.NEWSIZ) THEN
          TYPE*,IAM(),'File size of VLF copy must be the same as VLF'
          TYPE*,IAM(),'Correct file size and rerun MLCOPVLF'
          TYPE*,IAM(),'        '
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C
        IF(VLCSTS.NE.WCLR) THEN
          TYPE*,IAM(),'VLF copy file is not clear'
          TYPE*,IAM(),'Run WINCLR and rerun MLCOPVLF'
          TYPE*,IAM(),' '
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C
C  COPY VLF ---> VLC     (FROM VLF2VLC)
C
        WRITE(5,901) IAM(),(SCFSFN(K,VLF),K=1,5),(SCFSFN(K,VLC),K=1,5)
        BLOCK=0
10      CONTINUE
        BLOCK=BLOCK+1
        CALL READW(OFDB,BLOCK,COPBUF,ST)
        IF(ST.NE.0.AND.ST.NE.144) THEN
          WRITE(5,902) IAM(),(SCFSFN(K,VLF),K=1,5),ST,BLOCK
          CALL GPAUSE
        ENDIF
        CALL WRITEW(NFDB,BLOCK,COPBUF,ST)
        IF(ST.EQ.144) GOTO 15
        IF(ST.NE.0) THEN
          WRITE(5,903) IAM(),(SCFSFN(K,VLC),K=1,5),ST,BLOCK
          CALL GPAUSE
        ENDIF
        GOTO 10
15      CONTINUE
C
        VLCSTS=WCOP
C
        TYPE*,IAM(),'VLF --> VLC file copy completed.'
c
        CALL CLOSEFIL(NFDB)
        CALL CLOSEFIL(OFDB)
C
        CALL GSTOP(GEXIT_SUCCESS)
C
900     FORMAT(1X,A,1X,5A4,' open error > ',I4)
901     FORMAT(1X,A,1X,'Copying ',5A4,' to ',5A4)
902     FORMAT(1X,A,1X,5A4,' read error > ',I4,' Block > ',I5)
903     FORMAT(1X,A,1X,5A4,' write error > ',I4,' Block > ',I5)
        END
