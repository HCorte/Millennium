C
C SUBROUTINE GETPRIZE
C 
C V11 29-NOV-2000 UXN TTGL ADDED.
C V10 02-MAR-2000 UXN PRZDRW/PPP_PRZDRW removed.
C V09 03-FEB-2000 OXK Remove hardcoding for number of draws (PRZDRW/PPP_PRZDRW)
C V08 13-OCT-1999 RXK World Tour added.
C V07 27-MAY-1999 RXK Modified for oddset games (fractions).
C V06 14-MAY-1999 UXN Super Triple added.
C V05 23-NOV-1995 HXK Merge of post 65 stuff; changes for Double/Couple
C V04 30-OCT-1995 RXK Minor fix
C V03 31-OCT-1994 HXK ADDED BINGO
C V02 18-AUG-1993 SXH Released for Finland (added Fractions)
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
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE GETPRIZE(PDRAWS,GAME,FRCS,DETAIL,AMOUNT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMVAL.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'

        ! arguments
        INTEGER*4  PDRAWS(MAXGAM)       !
        INTEGER*4  GAME                 !
        INTEGER*4  FRCS                 !
        INTEGER*4  DETAIL(VPLEN)        !
        INTEGER*4  AMOUNT               !

        ! variables
        INTEGER*4  DIV                  !
        INTEGER*4  SHR                  !
        INTEGER*4  DRW                  !
        INTEGER*4  BNS                  !
        INTEGER*4  GTYP                 !
        INTEGER*4  GIND                 !
        INTEGER*4  DIND                 !
        INTEGER*4  SUBGAME              !
        INTEGER*4  SUB

        INTEGER*4  LPRZ(LTGDIV,2,400,NUMLTO)    !
        INTEGER*4  SPRZ(SPGDIV,400,NUMSPT)      !
        INTEGER*4  TGPRZ(TGGDIV,400,NUMTGL)      !
        INTEGER*4  KPRZ(KIGDIV,400,NUMKIK)      !
        INTEGER*4  NBPRZ(NBGPOL,2,400,NUMNBR)   !
        INTEGER*4  BPRZ(BGODIV,BGOSUB,400,NUMBGO)


        COMMON/PRIZE/ LPRZ,SPRZ,TGPRZ,KPRZ,NBPRZ,BPRZ
C
C
        AMOUNT=0
        DIV = DETAIL(VDIV)
        SHR = DETAIL(VSHR)
        DRW = DETAIL(VDRW)
        BNS = DETAIL(VBDR)+1
        SUB = DETAIL(VSUB)

        GTYP = GNTTAB(GAMTYP,GAME)
        GIND = GNTTAB(GAMIDX,GAME)

        DIND=PDRAWS(GAME)-DRW+1
        IF((DIND.LT.1.OR.DIND.GT.400).AND.
     *     (GTYP.EQ.TLTO.OR.GTYP.EQ.TSPT.OR.GTYP.EQ.TNBR.OR.
     *      GTYP.EQ.TKIK.OR.GTYP.EQ.TBNG.OR.GTYP.EQ.TTGL)) THEN
            WRITE(6,900) IAM(),GTNAMES(GTYP),GIND,DIND
            CALL GPAUSE
            RETURN
        ENDIF

        IF(GTYP.EQ.TLTO) AMOUNT=LPRZ(DIV,BNS,DIND,GIND)*SHR
        IF(GTYP.EQ.TNBR) AMOUNT=NBPRZ(DIV,BNS,DIND,GIND)*SHR
        IF(GTYP.EQ.TSPT) AMOUNT=SPRZ(DIV,DIND,GIND)*SHR
        IF(GTYP.EQ.TTGL) AMOUNT=TGPRZ(DIV,DIND,GIND)*SHR
        IF(GTYP.EQ.TKIK) AMOUNT=KPRZ(DIV,DIND,GIND)*SHR
      
        IF(GTYP.EQ.TBNG) THEN
           SUBGAME = DETAIL(VSUB)
           AMOUNT=BPRZ(DIV,SUBGAME,DIND,GIND)*SHR
        ENDIF
        IF(GTYP.EQ.TSCR.OR.GTYP.EQ.TWIT.OR.GTYP.EQ.TTSL.OR.
     *     GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.GTYP.EQ.TSSC.OR.
     *     GTYP.EQ.TTRP.OR.GTYP.EQ.TSTR) THEN
           AMOUNT=SHR
           RETURN
        ENDIF

        IF(FRCS.EQ.0.OR.FRCS.GE.MAXFRC(GAME)) THEN
            AMOUNT = AMOUNT
        ELSE
            AMOUNT=(AMOUNT/MAXFRC(GAME))*FRCS
        ENDIF

        RETURN

900     FORMAT(1X,A,1X,A8,I1,' prize too old ',I4)

        END
