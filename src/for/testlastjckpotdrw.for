C
C PROGRAM TESTLASTJCKPOTDRW
C
C V01 31-MAR-2017 HXK INITIAL RELEASE
C
C PROGRAM TO RESET DATA JOKER FOR LAST JACKPOT DRAW TESTS
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
        PROGRAM TESTLASTJCKPOTDRW
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
        INTEGER*4  PLAN
        INTEGER*4  FILE(5)
C
C
        CALL COPYRITE
C
C
        TYPE*,IAM(),
     *  '<<<<< TESTLASTJCKPOTDRW >>>>>'
        TYPE*,IAM()
C
C GET SYSTEM CONFIGURATION INFO.
C
        CALL GETSCONF(SCFREC,ST)
        IF(ST.NE.0) THEN
           TYPE*,IAM(),'Unable to get System Configuration info.'
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        GTYP=TKIK
        GIND=1
C
        GNUM=SCFGTN(GTYP,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
           TYPE*,IAM(),'Sorry, game selected is not active'
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL FASTMOV(SCFGFN(1,GNUM),FILE,5)
C
100     CONTINUE
        CALL INPNUM('Enter draw number ',DRAW,1,99999,EXT)
        IF(EXT.LT.0) GOTO 100
        WRITE(*,905) (SCFGFN(K,GNUM),K=1,5),GIND,DRAW
C
        CALL TST_KIK(FILE,DRAW)
C
        CALL GSTOP(GEXIT_SUCCESS)
C
905     FORMAT(1X,5A4,' Index ',I4,'    Draw ',I4)
C
        END
