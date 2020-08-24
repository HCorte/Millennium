C
C SUBROUTINE LPRIZE
C
C V07 29-NOV-2000 UXN TOTOGOLO ADDED.
C V06 13-OCT-1999 RXK World Tour added.
C V05 28-MAY-1999 UXN Nothing to load for ODDSET games.
C V04 31-OCT-1994 HXK Apply subdivisions to Bingo
C V03 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V02 18-AUG-1993 SXH Released for Finland
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C SUBROUTINES TO LOAD/GET DETAIL PRIZE VALUES
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE LPRIZE(PDRAWS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DTGREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DNBREC.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'

        ! argument
        INTEGER*4  PDRAWS(MAXGAM)               !

        ! variables
        INTEGER*4  I                            !
        INTEGER*4  IND                          !
        INTEGER*4  GAM                          !
        INTEGER*4  GTYP                         !
        INTEGER*4  GIND                         !
        INTEGER*4  ST                           !
        INTEGER*4  FDB(7)                       !

        INTEGER*4  LPRZ(LTGDIV,2,400,NUMLTO)    !
        INTEGER*4  SPRZ(SPGDIV,400,NUMSPT)      !
        INTEGER*4  TGPRZ(TGGDIV,400,NUMTGL)      !
        INTEGER*4  KPRZ(KIGDIV,400,NUMKIK)    !
        INTEGER*4  NBPRZ(NBGPOL,2,400,NUMNBR)   !
        INTEGER*4  BPRZ(BGODIV,BGOSUB,400,NUMBGO)      !

        COMMON/PRIZE/ LPRZ,SPRZ,TGPRZ,KPRZ,NBPRZ,BPRZ      !
C
C
C
        DO 1000 GAM=1,MAXGAM
            IF(PDRAWS(GAM).LT.1) GOTO 1000
            GTYP=GNTTAB(GAMTYP,GAM)
            GIND=GNTTAB(GAMIDX,GAM)
	    IF(GTYP.NE.TLTO.AND.GTYP.NE.TSPT.AND.
     *         GTYP.NE.TKIK.AND.GTYP.NE.TNBR.AND.
     *         GTYP.NE.TBNG.AND.GTYP.NE.TTGL) GOTO 1000
            WRITE(5,900) IAM(),GTNAMES(GTYP),GIND
            CALL OPENW(2,GFNAMES(1,GAM),4,0,0,ST)
            IF(ST.NE.0) CALL FILERR(GFNAMES(1,GAM),1,ST,0)

            IF(GTYP.EQ.TLTO) CALL IOINIT(FDB,2,DLTSEC*256)
            IF(GTYP.EQ.TSPT) CALL IOINIT(FDB,2,DSPSEC*256)
            IF(GTYP.EQ.TTGL) CALL IOINIT(FDB,2,DTGSEC*256)
            IF(GTYP.EQ.TKIK) CALL IOINIT(FDB,2,DKKSEC*256)
            IF(GTYP.EQ.TNBR) CALL IOINIT(FDB,2,DNBSEC*256)
            IF(GTYP.EQ.TBNG) CALL IOINIT(FDB,2,DBNSEC*256)
C
C
            DO 100 I=PDRAWS(GAM),PDRAWS(GAM)-399,-1
                IF(I.LT.1) THEN
                    CALL CLOSEFIL(FDB)
                    GOTO 1000
                ENDIF
                IND=PDRAWS(GAM)-I+1
C
C
                IF(GTYP.EQ.TLTO) THEN
                    CALL READW(FDB,I,DLTREC,ST)
                    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GAM),2,ST,I)
                    CALL FASTMOV(DLTSHV,LPRZ(1,1,IND,GIND),LTGDIV*2)
                    GOTO 100
                ENDIF
C
C
                IF(GTYP.EQ.TSPT) THEN
                    CALL READW(FDB,I,DSPREC,ST)
                    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GAM),2,ST,I)
                    CALL FASTMOV(DSPSHV,SPRZ(1,IND,GIND),SPGDIV)
                    GOTO 100
                ENDIF

                IF(GTYP.EQ.TTGL) THEN
                    CALL READW(FDB,I,DTGREC,ST)
                    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GAM),2,ST,I)
                    CALL FASTMOV(DTGSHV,TGPRZ(1,IND,GIND),TGGDIV)
                    GOTO 100
                ENDIF
C
C
                IF(GTYP.EQ.TKIK) THEN
                    CALL READW(FDB,I,DKKREC,ST)
                    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GAM),2,ST,I)
                    CALL FASTMOV(DKKSHV,KPRZ(1,IND,GIND),KIGDIV)
                    GOTO 100
                ENDIF
C
C
                IF(GTYP.EQ.TNBR) THEN
                    CALL READW(FDB,I,DNBREC,ST)
                    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GAM),2,ST,I)
                    CALL FASTMOV(DNBPRZ,NBPRZ(1,1,IND,GIND),NBGPOL*2)
                    GOTO 100
                ENDIF
C
C
                IF(GTYP.EQ.TBNG) THEN
                    CALL READW(FDB,I,DBNREC,ST)
                    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GAM),2,ST,I)
                    CALL FASTMOV(DBNSHV,BPRZ(1,1,IND,GIND),BGODIV*BGOSUB)
                    GOTO 100
                ENDIF
C
C
100         CONTINUE

            CALL CLOSEFIL(FDB)

1000    CONTINUE

        RETURN

900     FORMAT(1X,A,' Loading detail prize data for ',A8,I1,' game')

        END
