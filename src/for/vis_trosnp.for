C SUBROUTINE TROSNP
C
C V02 21-SEP-1999 UXN Find function added.
C V01 26-MAY-1999 UXN Initial release.  
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE TROSNP(PAGE,DRAW,CMB,GIND)
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
        INCLUDE 'INCLIB:STRCOM.DEF'
        INCLUDE 'INCLIB:STROCOM.DEF'
        INCLUDE 'INCLIB:DSTREC.DEF'
C
        INTEGER*4  PAGESIZE
        PARAMETER (PAGESIZE=16)

        INTEGER*4  PAGE                      !
        INTEGER*4  GIND

        INTEGER*2  DBUF(LDATE_LEN)
        INTEGER*2  BEGSAL(LDATE_LEN)
        INTEGER*2  ENDSAL(LDATE_LEN)
        INTEGER*4  GNUM               
        INTEGER*4  DRAW
        INTEGER*4  CNT,CMB(3)
        INTEGER*4  PAGEBEG
        INTEGER*4  EVE(3)
        INTEGER*4  UCID
        INTEGER*4  AMT
        INTEGER*4  NETAMT,I1,I2,I3
        INTEGER*4  AMOUNT(MAXTRPRW)
        INTEGER*4  I,J,K,LIN
        INTEGER*4  TOT_PAGES,CUR_PAGE

        REAL*8     POOL
        REAL*8     ODDS
        INTEGER*4  SAV_GIND
        INTEGER*4  TOTSAL,NETPOL,TOTPOL
        DATA SAV_GIND /1/ 

        CHARACTER*17 POLSTS(11)     !pool status
        DATA POLSTS/'Not initialized  ','No drawing       ',
     *              'Info entered     ','Game open        ',
     *              'End of game      ','Results entered  ',
     *              'Results verified ','Drawing completed',
     *              'Results are final','Cancelled/Ref dis',
     *              'Cancelled/Ref ena'/

        INTEGER*4   FRST_WIN,LAST_WIN,NUMWIN,ST,FDB(7)
	INTEGER*4   WEEK,YEAR,FID
C
	INTEGER*4   VIS_BOLD
	COMMON/SCREEN/ VIS_BOLD(24)
	LOGICAL FOUND
C
C CONVERT CMB TO UCID IF NEEDED
C
	SMODE = .FALSE.
	FOUND = .FALSE.
	FID   = -1
C
	IF(CMB(1).GT.0) THEN
	   DO I=1,3
	      IF(CMB(I).LE.0.OR.CMB(I).GT.MAXSTRRW) CMB(I) = 1
           ENDDO
           FID = CMB(1) + (CMB(2)-1)*MAXSTRRW + (CMB(3)-1)*MAXSTRRW*MAXSTRRW
	ENDIF
	    
        IF(PAGE.LT.1) PAGE=1

        IF(GIND.LE.0) THEN 
          GIND=SAV_GIND
        ENDIF

        IF (GIND .LT. 1 .OR. GIND .GT. MAXIND) THEN
          WRITE(CLIN23,3000) GTNAMES(TSTR)
          RETURN
        END IF
          
        SAV_GIND=GIND

        GNUM=GTNTAB(TSTR,GIND)
        IF(GNUM.LE.0.OR.GNUM.GT.MAXGAM) RETURN
        IF(DRAW.LE.0) DRAW = DAYHDR(GNUM)
 
        IF(DRAW .LT. 1) THEN
          WRITE(CLIN23,3010) GTNAMES(TSTR),GIND
          RETURN
        END IF
C---- Get data from memory.

        IF(DRAW .EQ. DAYDRW(GNUM)) THEN
          CALL GAMLOG(TSTR,GIND,DSTREC,STRSTS)
          GOTO 120
        ENDIF

C---- Get game data from file.

        SMODE = .TRUE.
        CALL OPENW(1,GFNAMES(1,GNUM),0,0,0,ST)
        CALL IOINIT(FDB,1,DSTSEC*256)
        IF (ST .NE. 0) THEN
          WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
          CALL USRCLOS1(1)
          RETURN
        END IF

        CALL READW(FDB,DRAW,DSTREC,ST)
        IF (ST .NE. 0) THEN
          WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
          CALL USRCLOS1(1)
          RETURN
        ENDIF

        CALL USRCLOS1(1)

120	CONTINUE

        IF (DSTSTS .EQ. 0) THEN
          DO I = 3,22
             WRITE(XNEW,3050)
          ENDDO	  
          WRITE(CLIN23,3040) GTNAMES(TSTR),GIND,DRAW
          RETURN
        END IF

C HEADER

        BEGSAL(5) = DSTBSD
        ENDSAL(5) = DSTESD
        DBUF(5) = DSTDAT
        CALL LCDATE(DBUF)
        CALL LCDATE(BEGSAL)
        CALL LCDATE(ENDSAL)
        CALL LCDATE(DBUF)
        WRITE(CLIN1,901) GTNAMES(TSTR),GIND,
     *                   (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
        WRITE(CLIN2,902) (DSTENM(I),I=1,STRENM_LEN/4),DRAW,
     *                   POLSTS(DSTSTS+1),(DBUF(I),I=7,13)
        WRITE(CLIN3,3050)
C
	IF(DRAW.NE.DAYDRW(GNUM)) GOTO 1000
C
        WRITE(CLIN5,905) 
C
C GET NETTO FOR SHARE
C CALCULATE NET AMOUNT
C
        CALL FASTSET(0,AMOUNT,3*MAXSTRRW)
        DO 420 I1=1,MAXSTRRW
           IF(DSTSTA(I1).NE.GAMOPN) GOTO 420
           DO 410 I2=1,MAXSTRRW
              IF(DSTSTA(I2).NE.GAMOPN) GOTO 410
              DO 400 I3=1,MAXSTRRW
                 IF(DSTSTA(I3).NE.GAMOPN) GOTO 400
                 UCID= I1 + (I2-1)*MAXSTRRW + (I3-1)*MAXSTRRW*MAXSTRRW
                 AMOUNT(I1) = AMOUNT(I1) + STRODDS(STRGAMT,UCID,GIND)
                 AMOUNT(I2) = AMOUNT(I2) + STRODDS(STRGAMT,UCID,GIND)
                 AMOUNT(I3) = AMOUNT(I3) + STRODDS(STRGAMT,UCID,GIND)
400           CONTINUE
410        CONTINUE
420     CONTINUE
        NETAMT = 0
        DO I=1,MAXSTRRW
           NETAMT = NETAMT + AMOUNT(I)
        ENDDO
C
        POOL=DFLOAT(NETAMT)/3.0D0*CALPER(DSTSPR)+DFLOAT(DSTPOL(1)) 
 
        J=STROFEL(GIND)
        IF(J.EQ.0) GOTO 999
C
C FIND OUT TOTAL NUMBER OF PAGES.
C
        CNT = 0
        DO WHILE(J.NE.0)
            CNT = CNT + 1
	    IF(FID.GE.0) THEN
	       IF(J.EQ.FID) THEN
		    PAGE  = (CNT-1)/PAGESIZE + 1
		    FOUND = .TRUE.
	       ENDIF  
	    ENDIF
            J = STRODDS2(STRGNEL,J,GIND)
        ENDDO
C
	IF(FID.GE.0.AND..NOT.FOUND) THEN
            WRITE(XNEW(23),942) (CMB(K),K=1,3)
	    CALL FASTSET(-1,CMB,3)
	    RETURN
	ENDIF
	CALL FASTSET(-1,CMB,3)
C
        TOT_PAGES = 0
        IF(MOD(CNT,PAGESIZE).NE.0) TOT_PAGES = 1
        TOT_PAGES = TOT_PAGES+CNT/PAGESIZE
C
	IF(PAGE.GT.TOT_PAGES) PAGE = TOT_PAGES
C
        PAGEBEG=MIN((PAGE-1)*PAGESIZE+1,(TOT_PAGES-1)*PAGESIZE+1)
C
C FIND BEGINNING OF ODDS FOR PARTICULAR PAGE 
C
	CNT = 1
        J=STROFEL(GIND)
100     CONTINUE
        IF(CNT.LT.PAGEBEG) THEN
           IF(J.EQ.0) GOTO 200
           J=STRODDS2(STRGNEL,J,GIND)
           CNT=CNT+1
           GOTO 100
        ENDIF

C DISPLAY ODDS

200     CONTINUE
             
        LIN=6
C
        CUR_PAGE= (CNT-1)/PAGESIZE + 1
C       
        WRITE(XNEW(3),939) CUR_PAGE,TOT_PAGES
C
        DO K=1,PAGESIZE

           UCID=J
           EVE(3)=(UCID-1)/(MAXSTRRW*MAXSTRRW) +1
           UCID=UCID-(EVE(3)-1)*MAXSTRRW*MAXSTRRW
           EVE(2)=(UCID-1)/MAXSTRRW +1
           UCID=UCID-(EVE(2)-1)*MAXSTRRW
           EVE(1)=UCID
           AMT=STRODDS(STRGAMT,J,GIND)

           IF(AMT.GT.0) THEN
              ODDS=POOL/DFLOAT(AMT)
           ELSE 
              ODDS=0.D0
           ENDIF
           IF(DSTSTA(EVE(1)).EQ.GAMCAN .OR. 
     *        DSTSTA(EVE(2)).EQ.GAMCAN .OR. 
     *        DSTSTA(EVE(3)).EQ.GAMCAN) ODDS=1.D0
	   IF(J.EQ.FID)  VIS_BOLD(LIN) = 1
           WRITE(XNEW(LIN),906) CNT,
     *           EVE(1),(DSTNMS(I,EVE(1)),I=1,4),
     *           EVE(2),(DSTNMS(I,EVE(2)),I=1,4),
     *           EVE(3),(DSTNMS(I,EVE(3)),I=1,4),
     *           CMONY(AMT,8,BETUNIT),
     *           ODDS
           LIN=LIN+1  
           IF(J.EQ.STROLEL(GIND)) GOTO 999

           J=STRODDS2(STRGNEL,J,GIND)
           CNT=CNT+1  

        ENDDO
999     CONTINUE
        
        TOTSAL = IDNINT(DFLOAT(NETAMT)/3.0D0)  
        NETPOL = IDNINT(DFLOAT(NETAMT)/3.0D0*CALPER(STRSPR(GIND)))
        TOTPOL = NETPOL + STRPOL(1,GIND)
        WRITE(XNEW(22),922) CSMONY(TOTSAL,12,BETUNIT),
     *                       CSMONY(NETPOL,11,BETUNIT),
     *                       CSMONY(STRPOL(1,GIND),10,BETUNIT),
     *                       CSMONY(TOTPOL,13,BETUNIT)
	WRITE(XNEW(23),937)
        RETURN
C
C Come here when draw is not currently active.
C
1000	CONTINUE
C
	IF(DSTSTS.LE.GAMINF) RETURN
        CALL FIGWEK(DSTESD - WEEK_OFFSET, WEEK, YEAR)
        WRITE(CLIN3,940) WEEK,YEAR,(DSTDES(I),I=1,7)
        WRITE(CLIN4,941) (DSTDES(I),I=8,14)
        WRITE(CLIN5,941) (DSTDES(I),I=15,21)
        IF (DSTSTS.LT.GAMDON .OR.
     *      DSTSTS.EQ.GAMCAN .OR. DSTSTS.EQ.GAMREF) GOTO 1210
         
        !---- If results are in display winners and some totals
	LIN = 7
        WRITE (XNEW(  LIN),930)
        LIN=LIN+2
        WRITE (XNEW(  LIN),931)
        LIN=LIN+2
C
C Prepare printout for winners
C
        IF(PAGE.LE.0) PAGE = 1
        IF((PAGE-1)*6.GE.DSTCMB) THEN
               FRST_WIN = 1
	       PAGE = 1
        ELSE
               FRST_WIN = (PAGE-1)*6+1
        ENDIF
        LAST_WIN = MIN(FRST_WIN+5,DSTCMB)

	NUMWIN = 0
        DO 1300 I=1,DSTCMB
              NUMWIN = NUMWIN + DSTWBT(TRACNT,I)
              IF(I.LT.FRST_WIN.OR.I.GT.LAST_WIN) GOTO 1300
              WRITE (XNEW(  LIN),932)  DSTWIN(1,I),
     *                                 (DSTNMS(K,DSTWIN(1,I)),K=1,3),
     *                                 DSTWIN(2,I),
     *                                 (DSTNMS(K,DSTWIN(2,I)),K=1,3),
     *                                 DSTWIN(3,I),
     *                                 (DSTNMS(K,DSTWIN(3,I)),K=1,3),
     *                                 DSTWBT(TRACNT,I),
     *                                 CMONY(DSTWBT(DOLAMT,I),12,BETUNIT),
     *                                 DSTODS(I)/100,
     *                                 MOD(DSTODS(I),100)
              LIN = LIN + 1
1300    ENDDO

1210    CONTINUE

        LIN = 18
        WRITE (XNEW(  LIN),933) CSMONY(DSTSAL(DOLAMT),12,BETUNIT)
        LIN=LIN+1
        WRITE (XNEW(  LIN),934) CSMONY(DSTPOL(1),10,BETUNIT)
        LIN=LIN+1
        WRITE (XNEW(  LIN),935) CSMONY(DSTWON-DSTREF,12,BETUNIT)
        LIN=LIN+1
        WRITE (XNEW(  LIN),936) NUMWIN
        WRITE(XNEW(6),939) PAGE,DSTCMB/7+1

        RETURN


1       FORMAT(1X,3(Z3.2),3X,2(Z3.2),1X,Z3.2,2X,2(1X,Z3.2),5X,I8,I4)
3000    FORMAT ('Enter !',A8,' game index ')
3010    FORMAT (A8,1X,I1,' game not active')
3020    FORMAT (5A4,' open error ',I4)
3030    FORMAT (5A4,' read error ',I4,' record > ',I4)
3040    FORMAT (A8,1X,I1,' game not initialized event > ',I4)
3050    FORMAT (80(' '))
3060    FORMAT (80(' '))
901     FORMAT (A8,1X,I1,7A2,' -',7A2)
902     FORMAT (<STRENM_LEN/4>A4,'*Event code-',I3,'*',A17,'*Draw',7A2)
903     FORMAT(5X,18A1,1X,18A1,1X,18A1)
905     FORMAT('  No',T6,'No Name',T25,'No Name',T44,'No Name',T65,
     *            'Amount      Odds')
906     FORMAT(I4,1X,3(I2.2,1X,4A4),A8,F10.2)
922     FORMAT ('Sales ',A12,1(' '),'Net pool ',A11,1(' '),
     *          'Extra ',A10,1(' '),'Tot pool  ',A13)
930     FORMAT(1X,'Winner(s):')
931     FORMAT(T4,'First',T20,'Second',T36,'Third',T52,'Count',
     *         T62,'Amount',T77,'Odds')
932     FORMAT(I2.2,1X,3A4,T17,I2.2,1X,3A4,T33,I2.2,1X,3A4,T49,
     *         I6,1X,A12,1X,I9,'.',I2.2)
933     FORMAT (10(' '),'Total Sales          ',A12,' mk.')
934     FORMAT (10(' '),'Extra Amount           ',A10,' mk.')
935     FORMAT (10(' '),'Winning Amount       ',A12,' mk.')
936     FORMAT (10(' '),'Number of winners       ',I7)
937     FORMAT('Enter !game index or /draw or page, L, N or ?XX-XX-XX')
938     FORMAT('Enter !game index or event number')
939     FORMAT(1X,T65,'Page',1X,I3,' of ',I3)
940     FORMAT (1X,'Week  ',I2.2,'/',I4.4,9X,'(',7A4,')')
941     FORMAT (23X,'(',7A4,')')
942     FORMAT('Combination ',I2.2,'-',I2.2,'-',I2.2,' not found!!!')
        END

