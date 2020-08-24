C PROMOTION.FOR
C
C V02 28-FEB-2000 RXK Released for Finland
C V01 27-FEB-1996 SLK INITIAL RELEASE
C
C PROGRAM TO ENABLE/DISABLE PRIZE AND DISCOUNT PROMOTIONS
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
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM PROMOTION
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLVL.DEF'
        INCLUDE 'INCLIB:RECUSE.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECPRM.DEF'
C
        INTEGER*4 ST, OPT, EXT, SCFFDB(7), PRMFDB(7), I, K, GNUM, TER
        INTEGER*4 UID, SECLEV, SIND, COUNT
        INTEGER*4 SCFNAM(5)
        DATA      SCFNAM/'SCF.','FIL ',3*'    '/
        LOGICAL   ISPROMO
C
        CALL COPYRITE
C
5       CONTINUE
        CALL SGNON(VINDEX,SECLEV,UID,SIND,ST)
        CALL USRCLOS1(     2)
        IF (ST.EQ.1) THEN
           TYPE*,IAM(),' USER file does not exist '
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
        IF (SECLEV.NE.SUPERUSE) THEN
           TYPE*,IAM(),' Security level does not authorize access'
           CALL XWAIT(2,2,ST)
           GOTO 5
        ENDIF
C
C READ SCF RECORD
C
        CALL OPENW(1,SCFNAM,4,0,0,ST)
        CALL IOINIT(SCFFDB,1,SCFSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
        CALL READW(SCFFDB,1,SCFREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
        CALL CLOSEFIL(SCFFDB)
        DO I=1,MAXFIL
           IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
        ENDDO
C
10      CONTINUE
        CALL CLRSCR(5)
C
        TYPE *,IAM()
        TYPE *,IAM(),'*** PROMOTION manager ***'
        TYPE *,IAM()
        TYPE *,IAM(),' (1) Display actual promotions'
        TYPE *,IAM(),' (2) Set promotion'
        TYPE *,IAM(),' (3) Exit'
        TYPE *,IAM()
        CALL INPNUM('Choose option ',OPT,1,3,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_SUCCESS)
        IF(OPT.EQ.3) CALL GSTOP(GEXIT_SUCCESS)
C
        IF(OPT.EQ.1) THEN
C
C OPEN PROMO FILE
C
           CALL OPENW(1,SCFSFN(1,PRMO),4,0,0,ST)
           CALL IOINIT(PRMFDB,1,PRMSEC*256)
           IF(ST.NE.0) THEN
              CALL CLOSEFIL(PRMFDB)
              CALL FILERR(SCFSFN(1,PRMO),1,ST,0)
           ENDIF
C
C READ PROMO FILE
C
           CALL READW(PRMFDB,1,PRMREC,ST)
           IF(ST.NE.0) THEN
              CALL FILERR(SCFSFN(1,PRMO),2,ST,0)
           ENDIF
C
C DISPLAY PROMOTIONS 
C
           TYPE *,IAM()
           TYPE *,IAM(),'*** Actual promotions ***'
           ISPROMO=.FALSE.
           DO 20 GNUM=1,MAXGAM
              IF(PRMTAB(PRMTYP,GNUM).EQ.0) GOTO 20
              ISPROMO=.TRUE.
              COUNT=0
              DO TER=1,NUMAGT
                 IF(TSBIT(PRMAFL(1,TER),GNUM).AND.AGTTAB(AGTNUM,TER).GT.0) THEN
                    COUNT=COUNT+1
                 ENDIF
              ENDDO
              IF(PRMTAB(PRMTYP,GNUM).EQ.PRMFRW) THEN
                 WRITE(6,901) IAM(), IAM(), (SCFLGN(K,GNUM),K=1,4),
     *                               PRMTAB(PRMIFX,GNUM)
              ELSE
                 WRITE(6,903) IAM(), (SCFLGN(K,GNUM),K=1,4)
              ENDIF
C             WRITE(6,902) IAM(), COUNT
20         CONTINUE

           IF(.NOT.ISPROMO) TYPE*,IAM(),'No promotions set' 
           TYPE*,IAM()
           CALL WIMG(6,'Hit return to continue')
           READ(5,904) K

        ENDIF                   !end of opt=1 (display)

        IF(OPT.EQ.2) CALL RUNTSK(8HENAPRM  )
        GOTO 10
C
C
901     FORMAT(1X,A,/,
     *         1X,A,1X,'Game ',4(A4),' gets free week if ',I2,' weeks paid')
902     FORMAT(1X,A,1X,'activated for ',I4,' active agents')
903     FORMAT(1X,A,'Unknown promotion type defined for ',4A4)
904     FORMAT(A4)
C
        END
