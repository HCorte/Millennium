C SUBROUTINE PTOSNP
C
C V03 21-SEP-1999 UXN Find function added.
C V02 17-MAY-1999 UXN TROSNP CHANGED TO PTOSNP
C V01 12-Jan-1998 RXK Initial release.  
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE PTOSNP(PAGE,DRAW,CMB,GIND)
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
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:TROCOM.DEF'
        INCLUDE 'INCLIB:DTRREC.DEF'
C
        INTEGER*4  PAGESIZE
        PARAMETER (PAGESIZE=16)

        INTEGER*4  PAGE                      !
        INTEGER*4  GIND
	INTEGER*4  CMB(3)
	INTEGER*4  FID

        INTEGER*2  DBUF(LDATE_LEN)
        INTEGER*2  BEGSAL(LDATE_LEN)
        INTEGER*2  ENDSAL(LDATE_LEN)
        INTEGER*4  GNUM               
        INTEGER*4  DRAW
        INTEGER*4  CNT
        INTEGER*4  PAGEBEG
        INTEGER*4  REMPAGE
        INTEGER*4  REMJ
        INTEGER*4  EVE(3)
        INTEGER*4  UCID
        INTEGER*4  AMT
        INTEGER*4  NETAMT,I1,I2,I3
        INTEGER*4  NUMEVE
        INTEGER*4  AMOUNT(MAXTRPRW,3)
        INTEGER*4  I,J,K,LIN,S
        INTEGER*4  TOT_PAGES,CUR_PAGE

        REAL*8     POOL
        REAL*8     ODDS
        INTEGER*4  SAV_GIND
        INTEGER*4  TOTSAL,NETPOL,TOTPOL
        BYTE       B_DTRENM1(TRPENM_LEN)
        BYTE       B_DTRENM2(TRPENM_LEN)
        BYTE       B_DTRENM3(TRPENM_LEN)
        EQUIVALENCE (B_DTRENM1,DTRENM(1,1))
        EQUIVALENCE (B_DTRENM2,DTRENM(1,2))
        EQUIVALENCE (B_DTRENM3,DTRENM(1,3))
        DATA SAV_GIND /1/ 

        CHARACTER*17 POLSTS(11)     !pool status
        DATA POLSTS/'Not initialized  ','No drawing       ',
     *              'Info entered     ','Game open        ',
     *              'End of game      ','Results entered  ',
     *              'Results verified ','Drawing completed',
     *              'Results are final','Cancelled/Ref dis',
     *              'Cancelled/Ref ena'/

        INTEGER*4   FRST_WIN,LAST_WIN,NUMWIN,ST,FDB(7)
        INTEGER*4   WEEK,YEAR
        INTEGER*4   VIS_BOLD
        COMMON/SCREEN/ VIS_BOLD(24)
        LOGICAL FOUND
C	    
C
	FOUND = .FALSE.
	SMODE = .FALSE.
	FID   = -1

        IF(PAGE.LT.1) PAGE=1

        IF(GIND.LE.0) GIND=SAV_GIND

        IF (GIND .LT. 1 .OR. GIND .GT. MAXIND) THEN
          WRITE(CLIN23,3000) GTNAMES(TTRP)
          RETURN
        END IF
          
        SAV_GIND=GIND

        GNUM=GTNTAB(TTRP,GIND)
        IF(GNUM.LE.0.OR.GNUM.GT.MAXGAM) RETURN
        IF(DRAW.LE.0) DRAW  = DAYHDR(GNUM)

        IF (DRAW .LT. 1) THEN
          WRITE(CLIN23,3010) GTNAMES(TTRP),GIND
          RETURN
        END IF
	
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
           CALL GAMLOG(TTRP,GIND,DTRREC,TRPSTS)
	   GOTO 120
	ENDIF
C---- Get game data from file.

        SMODE = .TRUE.
        CALL OPENW(1,GFNAMES(1,GNUM),0,0,0,ST)
        CALL IOINIT(FDB,1,DTRSEC*256)
        IF (ST .NE. 0) THEN
          WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
          CALL USRCLOS1(1)
          RETURN
        END IF
        CALL READW(FDB,DRAW,DTRREC,ST)
        IF (ST .NE. 0) THEN
          WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
          CALL USRCLOS1(1)
          RETURN
        ENDIF

        CALL USRCLOS1(1)

120     CONTINUE

        IF (DTRSTS .EQ. 0) THEN
          DO I = 3,22
             WRITE(XNEW,3050)
          ENDDO
          WRITE(CLIN23,3040) GTNAMES(TTRP),GIND,DRAW
          RETURN
        END IF
C
C COMBINATION
C
	IF(CMB(1).GT.0) THEN	
	   DO I=1,3
              IF(CMB(I).LE.0.OR.CMB(I).GT.MAXTRPRW) CMB(I) = 1
           ENDDO
           FID = CMB(1) + (CMB(2)-1)*MAXTRPRW + (CMB(3)-1)*MAXTRPRW*MAXTRPRW
        ENDIF
C HEADER

        BEGSAL(5) = DTRBSD
        ENDSAL(5) = DTRESD
        DBUF(5) = DTRDAT
        CALL LCDATE(DBUF)
        CALL LCDATE(BEGSAL)
        CALL LCDATE(ENDSAL)
        CALL LCDATE(DBUF)
        WRITE(CLIN1,901) GTNAMES(TTRP),GIND,
     *                   (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
        WRITE(CLIN2,902) (DTRMNM(I),I=1,TRPENM_LEN/4),DRAW,
     *                   POLSTS(DTRSTS+1),(DBUF(I),I=7,13)
        WRITE(CLIN3,3050)
C
	IF(DRAW.NE.DAYDRW(GNUM)) GOTO 1000 
C
        WRITE(CLIN4,903) (B_DTRENM1(I),I=1,18),
     *                   (B_DTRENM2(I),I=1,18),
     *                   (B_DTRENM3(I),I=1,18)
C
        WRITE(CLIN5,905) 

C GET NETTO FOR SHARE

        NUMEVE=0
        DO I=1,3
           IF(DTREST(I).EQ.GAMOPN) NUMEVE=NUMEVE+1
        ENDDO 
C
C CALCULATE NET AMOUNT
C
        CALL FASTSET(0,AMOUNT,3*MAXTRPRW)
        IF(NUMEVE.EQ.3) THEN
           DO 420 I1=1,MAXTRPRW
              IF(DTRSTA(I1,1).NE.GAMOPN) GOTO 420
              DO 410 I2=1,MAXTRPRW
                 IF(DTRSTA(I2,2).NE.GAMOPN) GOTO 410
                 DO 400 I3=1,MAXTRPRW
                    IF(DTRSTA(I3,3).NE.GAMOPN) GOTO 400
                    UCID= I1 + (I2-1)*MAXTRPRW + (I3-1)*MAXTRPRW*MAXTRPRW
                    AMOUNT(I1,1) = AMOUNT(I1,1) + TRODDS(TRGAMT,UCID,GIND)
                    AMOUNT(I2,2) = AMOUNT(I2,2) + TRODDS(TRGAMT,UCID,GIND)
                    AMOUNT(I3,3) = AMOUNT(I3,3) + TRODDS(TRGAMT,UCID,GIND)
400              CONTINUE
410           CONTINUE
420        CONTINUE
        ELSEIF(NUMEVE.EQ.2) THEN
           DO 440 I1=1,MAXTRPRW
              IF(DTRSTA(I1,1).NE.GAMOPN) GOTO 440
              DO 430 I2=1,MAXTRPRW
                 IF(DTRSTA(I2,2).NE.GAMOPN) GOTO 430
                 UCID= I1 + (I2-1)*MAXTRPRW 
                 AMOUNT(I1,1) = AMOUNT(I1,1) + TRODDS(TRGAMT,UCID,GIND)
                 AMOUNT(I2,2) = AMOUNT(I2,2) + TRODDS(TRGAMT,UCID,GIND)
430           CONTINUE
440        CONTINUE
        ELSEIF(NUMEVE.EQ.3) THEN
           DO 450 I1=1,MAXTRPRW
              IF(DTRSTA(I1,1).NE.GAMOPN) GOTO 450
              UCID= I1
              AMOUNT(I1,1) = AMOUNT(I1,1) + TRODDS(TRGAMT,UCID,GIND)
450        CONTINUE
        ENDIF
        NETAMT = 0
        DO I=1,MAXTRPRW
          DO J=1,3
            NETAMT = NETAMT + AMOUNT(I,J)
          ENDDO
        ENDDO
C
        POOL=DFLOAT(NETAMT)/DFLOAT(NUMEVE)*CALPER(DTRSPR)+DFLOAT(DTRPOL(1)) 
 
        J=TROFEL(GIND)
        IF(J.EQ.0) GOTO 999
C
C FIND OUT TOTAL NUMBER OF PAGES.
C
        CNT = 0
        DO WHILE(J.NE.0)
            CNT = CNT + 1 
	    IF(FID.GT.0) THEN
	       IF(J.EQ.FID) THEN
		    PAGE  = (CNT-1)/PAGESIZE + 1
		    FOUND = .TRUE.
	       ENDIF
            ENDIF
            J = TRODDS2(TRGNEL,J,GIND)
        ENDDO
C
	IF(FID.GT.0.AND..NOT.FOUND) THEN
	    WRITE(XNEW(23),942) (CMB(K),K=1,NUMEVE)
            CALL FASTSET(-1,CMB,3)
            RETURN
        ENDIF
        CALL FASTSET(-1,CMB,3)

        TOT_PAGES = 0
        IF(MOD(CNT,PAGESIZE).NE.0) TOT_PAGES = 1
        TOT_PAGES = TOT_PAGES+CNT/PAGESIZE
	IF(PAGE.GT.TOT_PAGES) PAGE = TOT_PAGES
C
        J=TROFEL(GIND)
        REMPAGE=1
        REMJ=J
        CNT=1
C
        PAGEBEG=MIN((PAGE-1)*PAGESIZE+1,(TOT_PAGES-1)*PAGESIZE+1)
C
C FIND BEGINNING OF ODDS FOR PARTICULAR PAGE 
C
100     CONTINUE
        IF(CNT.LT.PAGEBEG) THEN
           J=TRODDS2(TRGNEL,J,GIND)
           CNT=CNT+1
           IF(J.EQ.0) GOTO 200
           GOTO 100
        ENDIF

C DISPLAY ODDS

200     CONTINUE
             
        LIN=6
        CUR_PAGE=0
        IF(MOD(CNT,PAGESIZE).NE.0) CUR_PAGE=1
        CUR_PAGE=CUR_PAGE+CNT/PAGESIZE
C       
        WRITE(XNEW(3),939) CUR_PAGE,TOT_PAGES
C
        DO K=1,PAGESIZE

           UCID=J

           IF(DTREST(3).EQ.GAMOPN) THEN
              EVE(3)=(UCID-1)/(MAXTRPRW*MAXTRPRW)+1
              UCID=UCID-(EVE(3)-1)*MAXTRPRW*MAXTRPRW
           ELSE
              EVE(3)=0
           ENDIF
           IF(DTREST(2).EQ.GAMOPN) THEN
              EVE(2)=(UCID-1)/MAXTRPRW +1
              UCID=UCID-(EVE(2)-1)*MAXTRPRW
           ELSE
              EVE(2)=0
           ENDIF
           EVE(1)=UCID
           AMT=TRODDS(TRGAMT,J,GIND)

           IF(AMT.GT.0) THEN
              ODDS=POOL/DFLOAT(AMT)
           ELSE 
              ODDS=0.D0
           ENDIF
           IF(DTRSTA(EVE(1),1).EQ.GAMCAN .OR. 
     *        EVE(2).NE.0.AND.DTRSTA(EVE(2),2).EQ.GAMCAN .OR. 
     *        EVE(3).NE.0.AND.DTRSTA(EVE(3),3).EQ.GAMCAN) ODDS=1.D0
	   IF(J.EQ.FID) VIS_BOLD(LIN) = 1
           WRITE(XNEW(LIN),906) CNT,
     *           (EVE(S),(DTRNMS(I,EVE(S),S),I=1,4),S=1,NUMEVE),
     *           CMONY(AMT,8,BETUNIT),
     *           ODDS
           LIN=LIN+1  
           IF(J.EQ.TROLEL(GIND)) GOTO 999

           J=TRODDS2(TRGNEL,J,GIND)
           CNT=CNT+1  

        ENDDO
999     CONTINUE
        
        TOTSAL = IDNINT(DFLOAT(NETAMT)/DFLOAT(NUMEVE))  
        NETPOL = IDNINT(DFLOAT(NETAMT)/DFLOAT(NUMEVE)*CALPER(TRPSPR(GIND)))
        TOTPOL = NETPOL + TRPPOL(1,GIND)
        WRITE(XNEW(22),922) CSMONY(TOTSAL,12,BETUNIT),
     *                       CSMONY(NETPOL,11,BETUNIT),
     *                       CSMONY(TRPPOL(1,GIND),10,BETUNIT),
     *                       CSMONY(TOTPOL,13,BETUNIT)
        WRITE(XNEW(23),937) 
        RETURN
C
C Come here when draw is not currently active.
C
1000    CONTINUE
C
        IF(DTRSTS.LE.GAMINF) RETURN
        CALL FIGWEK(DTRESD - WEEK_OFFSET, WEEK, YEAR)
        WRITE(CLIN3,940) WEEK,YEAR
        IF (DTRSTS.LT.GAMDON .OR.
     *      DTRSTS.EQ.GAMCAN .OR. DTRSTS.EQ.GAMREF) GOTO 1210

        !---- If results are in display winners and some totals
        LIN = 7
        WRITE (XNEW(  LIN),930)
        LIN=LIN+2
        WRITE (XNEW(  LIN),931) (DTRENM(K,1),K=1,3),(DTRENM(K,3),K=1,3),
     *                          (DTRENM(K,3),K=1,3)
        LIN=LIN+2
C
C Prepare printout for winners
C
        IF(PAGE.LE.0) PAGE = 1
        IF((PAGE-1)*6.GE.DTRCMB) THEN
               FRST_WIN = 1
	       PAGE = 1
        ELSE
               FRST_WIN = (PAGE-1)*6+1
        ENDIF
        LAST_WIN = MIN(FRST_WIN+5,DTRCMB)

        NUMWIN = 0
        DO 1300 I=1,DTRCMB
              NUMWIN = NUMWIN + DTRWBT(TRACNT,I)
              IF(I.LT.FRST_WIN.OR.I.GT.LAST_WIN) GOTO 1300
              WRITE (XNEW(  LIN),932)  DTRWIN(1,I),
     *                                 (DTRNMS(K,DTRWIN(1,I),1),K=1,3),
     *                                 DTRWIN(2,I),
     *                                 (DTRNMS(K,DTRWIN(2,I),2),K=1,3),
     *                                 DTRWIN(3,I),
     *                                 (DTRNMS(K,DTRWIN(3,I),3),K=1,3),
     *                                 DTRWBT(TRACNT,I),
     *                                 CMONY(DTRWBT(DOLAMT,I),12,BETUNIT),
     *                                 DTRODS(I)/100,
     *                                 MOD(DTRODS(I),100)
              LIN = LIN + 1
1300    ENDDO

1210    CONTINUE

        LIN = 18
        WRITE (XNEW(  LIN),933) CSMONY(DTRSAL(DOLAMT),12,BETUNIT)
        LIN=LIN+1
        WRITE (XNEW(  LIN),934) CSMONY(DTRPOL(1),10,BETUNIT)
        LIN=LIN+1
        WRITE (XNEW(  LIN),935) CSMONY(DTRWON-DTRREF,12,BETUNIT)
        LIN=LIN+1
        WRITE (XNEW(  LIN),936) NUMWIN
        WRITE(XNEW(6),939) PAGE,DTRCMB/7+1

        RETURN

C

1       FORMAT(1X,3(Z3.2),3X,2(Z3.2),1X,Z3.2,2X,2(1X,Z3.2),5X,I8,I4)
3000    FORMAT ('Enter !',A8,' game index ')
3010    FORMAT (A8,1X,I1,' game not active')
3020    FORMAT (5A4,' open error ',I4)
3030    FORMAT (5A4,' read error ',I4,' record > ',I4)
3040    FORMAT (A8,1X,I1,' game not initialized event > ',I4)
3050    FORMAT (80(' '))
3060    FORMAT (80(' '))
901     FORMAT (A8,1X,I1,7A2,' -',7A2)
902     FORMAT (<TRPENM_LEN/4>A4,'*Event code-',I3,'*',A17,'*Draw',7A2)
903     FORMAT(5X,18A1,1X,18A1,1X,18A1)
905     FORMAT('  No',T6,'No Name',T25,'No Name',T44,'No Name',T62,
     *            '   Amount      Odds')
906     FORMAT(I4,1X,<NUMEVE>(I2.2,1X,4A4),A8,F10.2)
922     FORMAT ('Sales ',A12,1(' '),'Net pool ',A11,1(' '),
     *          'Extra ',A10,1(' '),'Tot pool  ',A13)
930     FORMAT(1X,'Winner(s):')
931     FORMAT(3A4,T17,3A4,T33,3A4,T52,'Count',
     *         T62,'Amount',T77,'Odds')
932     FORMAT(I2.2,1X,3A4,T17,I2.2,1X,3A4,T33,I2.2,1X,3A4,T49,
     *         I6,1X,A12,1X,I9,'.',I2.2)
933     FORMAT (10(' '),'Total Sales          ',A12,' mk.')
934     FORMAT (10(' '),'Extra Amount           ',A10,' mk.')
935     FORMAT (10(' '),'Winning Amount       ',A12,' mk.')
936     FORMAT (10(' '),'Number of winners       ',I7)
937     FORMAT('Enter !game index or /draw or page, L, N or ?XX-XX-XX')
938     FORMAT('Enter !game index or event number')
939     FORMAT(T65,'Page',1X,I3,' of ',I3)
940     FORMAT (1X,'Week  ',I2.2,'/',I4.4)
941     FORMAT (23X,'(',7A4,')')
942     FORMAT('Combination ',<NUMEVE>(I2.2,' '),' not found!!!')
        END

