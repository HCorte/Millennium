C
C V02 29-NOV-2000 UXN TOTOGOLA ADDED.
C V01 01-MAR-2000 UXN Separated from MULSUBS.
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
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE CHKWINS
      IMPLICIT NONE
C
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:WINCOM.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
C
      INCLUDE 'INCLIB:DLTREC.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
      INCLUDE 'INCLIB:DTGREC.DEF'
      INCLUDE 'INCLIB:DKKREC.DEF'
      INCLUDE 'INCLIB:DNBREC.DEF'
      INCLUDE 'INCLIB:DWIREC.DEF'
      INCLUDE 'INCLIB:DSCREC.DEF'
      INCLUDE 'INCLIB:DTSREC.DEF'
      INCLUDE 'INCLIB:DBNREC.DEF'
      INCLUDE 'INCLIB:DDBREC.DEF'
      INCLUDE 'INCLIB:DCPREC.DEF'
      INCLUDE 'INCLIB:DSSREC.DEF'
      INCLUDE 'INCLIB:DTRREC.DEF'
      INCLUDE 'INCLIB:DSTREC.DEF'
C
      INTEGER*4 GNUM,GTYP,GIND
      INTEGER*4 DRAW,LUN,ROW
      INTEGER*4 FROM,UNTIL,STS
      INTEGER*4 PREVDRW
      PARAMETER(PREVDRW = 40)
C
C
      LUN=1
      CALL FASTSET(WINNOT, DRWSTS, MAX_WINSEL*MAXGAM)
      CALL FASTSET(     0, DRWGAM, MAX_WINSEL*MAXGAM)
C
      DO 2 GNUM=1,MAXGAM
           GTYP = GNTTAB(GAMTYP,GNUM)
           GIND = GNTTAB(GAMIDX,GNUM)
           IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) GOTO 2
           IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 2
C
           FROM=MAX(DAYHDR(GNUM),DAYDRW(GNUM))
           UNTIL=MAX(1,FROM-PREVDRW)
           DO DRAW=FROM, UNTIL, -1
C
             IF (GTYP .EQ. TLTO) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DLTSEC,DRAW,DLTREC)
                  STS = DLTSTS
                  IF(DLTDAT(CURDRW).EQ.DAYCDC) STS=GAMENV
                  IF(DLTDAT(CURDRW).NE.DAYCDC.AND.DLTSTS.GE.GAMENV) STS=GAMDON
                  CALL CHKSTS(STS, GNUM, DRAW, DLTDAT(CURDRW))
             ENDIF
C
             IF (GTYP .EQ. TSPT) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DSPSEC,DRAW,DSPREC)
                  STS = DSPSTS
                  IF (DSPDAT(CURDRW).EQ.DAYCDC) STS=GAMENV
                  IF(DSPDAT(CURDRW).NE.DAYCDC.AND.DSPSTS.GE.GAMENV) STS=GAMDON
                  CALL CHKSTS(STS, GNUM, DRAW, DSPDAT(CURDRW))
             ENDIF

             IF (GTYP .EQ. TTGL) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DTGSEC,DRAW,DTGREC)
                  STS = DTGSTS
                  IF (DTGDAT(CURDRW).EQ.DAYCDC) STS=GAMENV
                  IF(DTGDAT(CURDRW).NE.DAYCDC.AND.DTGSTS.GE.GAMENV) STS=GAMDON
                  CALL CHKSTS(STS, GNUM, DRAW, DTGDAT(CURDRW))
             ENDIF
C
             IF (GTYP .EQ. TBNG) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DBNSEC,DRAW,DBNREC)
                  CALL CHKSTS(DBNSTS, GNUM, DRAW, DBNDAT(CURDRW))
             ENDIF
C
             IF (GTYP .EQ. TKIK) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DKKSEC,DRAW,DKKREC)
                  STS = DKKSTS
                  IF(DKKDAT(CURDRW).NE.DAYCDC.AND.DKKSTS.GE.GAMENV) STS=GAMDON
                  CALL CHKSTS(STS, GNUM, DRAW, DKKDAT(CURDRW))
             ENDIF
C
             IF (GTYP .EQ. TWIT ) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DWISEC,DRAW,DWIREC)
                  CALL CHKSTS(DWISTS, GNUM, DRAW, DWIDAT)
             ENDIF
C
             IF (GTYP .EQ. TSCR) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DSCSEC,DRAW,DSCREC)
                  CALL CHKSTS(DSCSTS, GNUM, DRAW, DSCDAT)
             ENDIF
C
             IF (GTYP .EQ. TDBL ) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DDBSEC,DRAW,DDBREC)
                  CALL CHKSTS(DDBSTS, GNUM, DRAW, DDBDAT)
             ENDIF
C
             IF (GTYP .EQ. TCPL ) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DCPSEC,DRAW,DCPREC)
                  CALL CHKSTS(DCPSTS, GNUM, DRAW, DCPDAT)
             ENDIF
C
             IF (GTYP .EQ. TSSC) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DSSSEC,DRAW,DSSREC)
                  CALL CHKSTS(DSSSTS, GNUM, DRAW, DSSDAT)
             ENDIF
C
             IF (GTYP .EQ. TTRP) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DTRSEC,DRAW,DTRREC)
                  CALL CHKSTS(DTRSTS, GNUM, DRAW, DTRDAT)
             ENDIF
C
             IF (GTYP .EQ. TSTR) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DSTSEC,DRAW,DSTREC)
                  CALL CHKSTS(DSTSTS, GNUM, DRAW, DSTDAT)
             ENDIF
C
             IF(GTYP.EQ.TTSL) THEN
                  CALL READGFL(LUN,GFNAMES(1,GNUM),DTSSEC,DRAW,DTSREC)
                  STS = WINNOT
                  DO ROW=1,DTSRWS
                     IF ((DTSDAT(ROW).LE.DAYCDC).AND.(DTSDAT(ROW).NE.0)) THEN
                       IF ((DTSSTA(ROW).EQ.GAMENV.OR.
     *                      DTSSTA(ROW).EQ.GAMCAN)) THEN
                         STS=WINYES
                       ENDIF
                       IF ((DTSSTA(ROW).EQ.GAMBFD).OR.
     *                     (DTSSTA(ROW).EQ.GAMOPN)) THEN
                         STS=RESNOT
                       ENDIF
                     ENDIF
                  ENDDO
                  IF (STS.EQ.RESNOT) CALL CHKSTS(GAMBFD, GNUM, DRAW, DAYCDC-1)
                  IF (STS.EQ.WINYES) CALL CHKSTS(GAMENV, GNUM, DRAW, DAYCDC)
             ENDIF
C
           ENDDO
C
2	CONTINUE
C
      RETURN
      END
