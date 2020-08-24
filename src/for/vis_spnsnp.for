C SUBROUTINE SPNSNP
C  
C V10 30-MAR-2015 MTK Modified Super 14 game
C V09 03-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C V08 16-FEB-2000 OXK Only DSPMAX lines printed, not SPGNBR (Vakio changes)
C V07 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V06 18 Nov 1993 SXH Results 2 becomes X, 3 becomes 2
C V05 18 Aug 1993 HXK SHOW 14 CHARACTERS OF TEAM NAMES
C V04 13 Jun 1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V03 21 Jan 1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 24-OCT-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SPORTS GAME NAME SNAPSHOT
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
      SUBROUTINE SPNSNP(NUM,GIND,ROW)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:AGTINF.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:VISCOM.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:SPTCOM.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
      INCLUDE 'INCLIB:GTNAMES.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
C
C
      INTEGER * 2 BEGSAL(LDATE_LEN),ENDSAL(LDATE_LEN)
      INTEGER * 4 FDB(7),ROWRES(SPGNBR),RROWRES(2)
      INTEGER * 4 NUM,GIND,GNUM,DRAW,ROW,LNS,ST,BCNT
      INTEGER * 4 I,J,K,R
      INTEGER * 4 MAX_VIS_LINES
      CHARACTER RESULT(5),RRESULT(5)
      CHARACTER * 17 POLSTS(11)
      CHARACTER * 32 EVENT_CANCEL_MESSAGE

      DATA RESULT/'-','1','X','C','2'/
      DATA RRESULT/'-','0','1','C','M'/
      DATA POLSTS/'Not initialized  ','No drawing       ',
     *            'Info entered     ','Game open        ',
     *            'End of game      ','Results entered  ',
     *            'Results verified ','Drawing completed',
     *            'Results are final','Refund/cancelled ',
     *            'Refunds enabled  '/
C
C
      DRAW=NUM
      IF(GIND.LT.1.OR.GIND.GT.MAXIND) GIND=1
C
C
      GNUM=GTNTAB(TSPT,GIND)
      IF(GNUM.LT.1) THEN
         WRITE(CLIN23,3010) GTNAMES(TSPT),GIND
         RETURN
      ENDIF
      IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
      IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
      IF(ROW.LT.1.OR.ROW.GT.SPGNBR) ROW=1
      IF(ROW.GT.43) ROW=43
C
C GET DATA FROM COMMON OR DISK
C
      IF(DRAW.EQ.DAYDRW(GNUM)) THEN
         CALL GAMLOG(TSPT,GIND,DSPREC,SPTSTS)
         GOTO 100
      ENDIF
C
      SMODE=.TRUE.
      CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
      CALL IOINIT(FDB,1,DSPSEC*256)
      IF(ST.NE.0) THEN
         WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
         CALL USRCLOS1(     1)
         RETURN
      ENDIF
      CALL READW(FDB,DRAW,DSPREC,ST)
      IF(ST.NE.0) THEN
         WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
         CALL USRCLOS1(     1)
         RETURN
      ENDIF
      IF(DSPSTS.EQ.0) THEN
         WRITE(CLIN23,3040) GTNAMES(TSPT),GIND,DRAW
         CALL USRCLOS1(     1)
         RETURN
      ENDIF
      CALL USRCLOS1(     1)
C
100   CONTINUE
      IF(DSPSTS.GE.GAMENV) THEN
         CALL FASTMOV(DSPWIN,ROWRES,SPGNBR)
      ELSE
         CALL FASTSET(0,ROWRES,SPGNBR)
      ENDIF
C
C
      BEGSAL(VCDC)=DSPBSD
      ENDSAL(VCDC)=DSPESD
      CALL LCDATE(BEGSAL)
      CALL LCDATE(ENDSAL)
C
C
      BCNT = 0
      IF(DSPFRG.NE.0) BCNT = 1

      WRITE(CLIN1,1000) (GLNAMES(K,GNUM),K=1,4),
     *                  (BEGSAL(I),I=9,13),(ENDSAL(I),I=9,13)
      WRITE(CLIN2,1001) DSPDRW,POLSTS(DSPSTS+1)
      WRITE(CLIN3,1002) DSPMCE
      IF(DSPDCD .EQ. 0) THEN
         MAX_VIS_LINES = 17
      ELSE
         MAX_VIS_LINES = 15  ! MAX 15 LINES TO PRINT DRAW CANCEL MESSAGE
      ENDIF
      LNS=4
      DO 210 R = ROW, ROW + MAX_VIS_LINES
      IF(R.GT.DSPMAX) THEN
         WRITE(XNEW(  LNS),888)
      ELSE

         CALL FRMT_CANCEL_EVENT_DATE_TIME(DSPECD(R), DSPDAT(CURDRW), EVENT_CANCEL_MESSAGE)

         IF(R.LE.DSPMAX-BCNT) THEN
            WRITE (XNEW(  LNS),1003) R,
     *                              (DSPNMS(I,1,R), I = 1, TGNMS_LEN / 4),
     *                              (DSPNMS(I,2,R), I = 1, TGNMS_LEN / 4),
     *                              RESULT(ROWRES(R) + 1),
     *                              EVENT_CANCEL_MESSAGE
         ELSE
            WRITE(XNEW(  LNS),888)
            LNS=LNS+1
            IF(DSPFRG.EQ.1) THEN
              RROWRES(1)=ISHFT(ROWRES(R),-4)    !SUPER14 Home result
              RROWRES(2)=IAND(ROWRES(R),'0F'X)  !SUPER14 Away result
              WRITE (XNEW(  LNS),1004) R,
     *                                (DSPNMS(I,1,R), I = 1, TGNMS_LEN / 4),
     *                                (DSPNMS(I,2,R), I = 1, TGNMS_LEN / 4),
     *                                (RRESULT(RROWRES(J) + 1),J=1,2),
     *                                EVENT_CANCEL_MESSAGE
            ENDIF

            IF(DSPFRG.EQ.2) THEN
              WRITE (XNEW(  LNS),1003) R,
     *                                 (DSPNMS(I,1,R), I = 1, TGNMS_LEN / 4),
     *                                 (DSPNMS(I,2,R), I = 1, TGNMS_LEN / 4),
     *                                 (RESULT(ROWRES(R) + 1)),
     *                                 EVENT_CANCEL_MESSAGE
            ENDIF
         ENDIF
      ENDIF
      LNS=LNS+1
210   CONTINUE
C
C SHOW DRAW CANCEL MESSAGE IF THE DRAW HAS BEEN CANCELLED
C
      IF(DSPDCD .NE. 0) THEN
         CALL FRMT_CANCEL_EVENT_DATE_TIME(DSPDCD, DSPDAT(CURDRW), EVENT_CANCEL_MESSAGE)
         WRITE(XNEW(LNS), 1005) EVENT_CANCEL_MESSAGE
      ENDIF
C
      RETURN
C
C     FORMAT STATEMENTS
C
888   FORMAT(80(' '))
C999  FORMAT(1X,I2.2,77(' '))
1000  FORMAT(1X,4A4,2X,5A2,'-',5A2)
1001  FORMAT('  Event code- ',I4,20(' '),'* ',A17,' *')
1002  FORMAT('Row',2X,'Team names',25X,'Result', 2X, 'Cancel Events To Cancel Draw:', X, I2)
1003  FORMAT(1X,I2.2,2X,<TGNMS_LEN/4>A4,' - ',<TGNMS_LEN/4>A4,3X,A1,3X, A32)
1004  FORMAT('S',I2.2,2X,<TGNMS_LEN/4>A4,' - ',<TGNMS_LEN/4>A4,2X,A1,':',A1, 3X, A32)
1005  FORMAT(X, 'The Full Draw Has Been', X, A)

3010  FORMAT(A8,1X,I1,' game not active')
3020  FORMAT(5A4,' open error ',I4)
3030  FORMAT(5A4,' read error ',I4,' record > ',I4)
3040  FORMAT(A8,1X,I1,' game not initialized event > ',I4)
      END
