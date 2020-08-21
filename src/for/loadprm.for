C
C SUBROUTINE LOADPRM
C
C V02 28-Feb-2000 RXK Cleaned up and implemented new promo type
C                     "if X weeks paid then add 1 free additional week"
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C SUBROUTINE TO LOAD PROMO/DISCOUNT FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE LOADPRM
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECPRM.DEF'

        INTEGER*4  FDB(7)           !
        INTEGER*4  ST               !
        INTEGER*4  I,J,K            !
        LOGICAL    ISPROMO 
        CHARACTER*1 BELL/Z07/ 
        CHARACTER*24 PROMOTXT(1)   ! so far one type only
        DATA PROMOTXT /'Add free week promo for '/


        TYPE*,IAM(),' Loading promo / discount file '
        CALL OPENW(3,SFNAMES(1,PRMO),4,0,0,ST)
        CALL IOINIT(FDB,3,PRMSEC*256)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,PRMO),1,ST,0)

        CALL READW(FDB,1,PRMREC,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,PRMO),2,ST,1)

        CALL CLOSEFIL(FDB)

        CALL FASTMOV(PRMTAB(1,1),PROMO(1,1),MAXGAM*3)
        ISPROMO=.FALSE.


        DO 10 I=1,MAXGAM
           IF(PROMO(PRMTYP,I).EQ.0) GOTO 10
           ISPROMO=.TRUE.     
           DO J=1,NUMAGT
              CALL BSET(AGTGAM(GFLAGS,I,J),AGTDIS)
              IF(TSBIT(PRMAFL(1,J),I)) THEN
                 CALL BCLR(AGTGAM(GFLAGS,I,J),AGTDIS)
              ENDIF
           ENDDO
           IF(PROMO(PRMTYP,I).EQ.PRMFRW) THEN 
              WRITE(6,900) IAM(),  PROMOTXT(PRMTYP),
     *                     (GLNAMES(K,I),K=1,4),
     *                     PROMO(PRMIFX,I)
           ELSE
              TYPE*,IAM(),BELL,BELL
              WRITE(6,902) IAM(),IAM(),(GLNAMES(K,I),K=1,4)
           ENDIF
10      CONTINUE

        IF(.NOT.ISPROMO) TYPE*,IAM(),'No game with promotions'


        TYPE*,IAM(),' Promo / discount load complete'
        RETURN

900     FORMAT(1X,A,A24,4A4,' if ',I2,' week ticket')
902     FORMAT(1X,A,' WARNING!!!',/
     *         1X,A,'    unknown promotion type defined for ',4A4)
        END
