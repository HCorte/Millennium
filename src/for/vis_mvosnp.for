C SUBROUTINE MVOSNP
C
C V02 31-MAY-2000 PXO Subroutine name SSOSNP -> MVOSNP
C V01 12-Jan-98   RXK Initial release.  
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE MVOSNP(PAGE,GIND)
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
	INCLUDE 'INCLIB:SSCCOM.DEF'
	INCLUDE 'INCLIB:SSPCOM.DEF'
	INCLUDE 'INCLIB:DSSREC.DEF'
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
	INTEGER*4  CNT
	INTEGER*4  PAGEBEG
	INTEGER*4  ROWNUM
	INTEGER*4  BUCKET
	INTEGER*4  ENTR
	INTEGER*4  AMT
	INTEGER*4  SCORE(6)
	INTEGER*4  I,J,K,LNS
	INTEGER*4  IROWS(10,2*PAGESIZE)
	INTEGER*4  SPACE/'20202020'X/
	INTEGER*4  TEMP
	BYTE	   BTEMP(4)
	EQUIVALENCE (TEMP,BTEMP)

        CHARACTER*40 ROWS(2*PAGESIZE)  
        CHARACTER*40 ONEROW
        INTEGER*4    IONEROW(10)
        CHARACTER    SROWS(40)
        CHARACTER*2  MUUT/'Mu'/
	CHARACTER*2  MUUT1,MUUT2,MUUT3,MUUT4,MUUT5,MUUT6
        EQUIVALENCE (ONEROW,SROWS,IONEROW)
	EQUIVALENCE (SROWS(5),MUUT1),(SROWS(8),MUUT2),(SROWS(11),MUUT3)
	EQUIVALENCE (SROWS(14),MUUT4),(SROWS(17),MUUT5),(SROWS(20),MUUT6)

 	EQUIVALENCE(ROWS(1),IROWS(1,1))
	INTEGER*4   TOT_PAGES

        REAL*8     POOL
 	REAL*8     ODDS
	INTEGER*4  NETPOL,TOTPOL
	INTEGER*4  SAV_GIND/1/

        CHARACTER*17 POLSTS(11)     !pool status
        DATA POLSTS/'Not initialized  ','No drawing       ',
     *              'Info entered     ','Game open        ',
     *              'End of game      ','Results entered  ',
     *              'Results verified ','Drawing completed',
     *              'Results are final','Cancelled/Ref dis',
     *              'Cancelled/Ref ena'/


        IF(PAGE.LT.1.OR.PAGE.GT.4) PAGE=1

        IF(GIND.LE.0) GIND=SAV_GIND

        IF (GIND .LT. 1 .OR. GIND .GT. MAXIND) THEN
          WRITE(CLIN23,3000) GTNAMES(TSSC)
          RETURN
        END IF
        SAV_GIND = GIND
        GNUM=GTNTAB(TSSC,GIND)
	IF(GNUM.LE.0.OR.GNUM.GT.MAXGAM) RETURN
	DRAW = DAYDRW(GNUM)
        IF (DRAW .LT. 1) THEN
          DO I=1,22
             WRITE(XNEW(I),3050)
          ENDDO
          WRITE(CLIN23,3010) GTNAMES(TSSC),GIND
          RETURN
        END IF

        CALL GAMLOG(TSSC,GIND,DSSREC,SSCSTS)

        IF (DSSSTS .EQ. 0) THEN
          DO I = 3,22
             WRITE(XNEW,3050)
          ENDDO
          WRITE(CLIN23,3040) GTNAMES(TSSC),GIND,DRAW
          RETURN
        END IF
C
C HEADER
C
        BEGSAL(5) = DSSBSD
        ENDSAL(5) = DSSESD
        DBUF(5) = DSSDAT
        CALL LCDATE(DBUF)
        CALL LCDATE(BEGSAL)
        CALL LCDATE(ENDSAL)
        CALL LCDATE(DBUF)
        WRITE(CLIN1,901) GTNAMES(TSSC),GIND,
     *                   (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
        WRITE(CLIN2,902) (DSSMNM(I),I=1,SSNMS_LEN/4),DRAW,
     *                   POLSTS(DSSSTS+1),(DBUF(I),I=7,13)
        WRITE(CLIN3,903) (DSSSNM(I,1),I=1,6),
     *			 (DSSSNM(I,2),I=1,6),
     *			 (DSSSNM(I,3),I=1,6) 
	WRITE(CLIN5,905) 

        POOL = DSSSAL * CALPER(DSSSPR) + DFLOAT(DSSPOL(1))

        J=SSPFEL(GIND)
        IF(J.LE.0.OR.J.GT.SSGTOP) GOTO 999
	TOT_PAGES = SSPTNUM(GIND)/(PAGESIZE*2)
	IF(MOD(SSPTNUM(GIND),PAGESIZE*2).NE.0) TOT_PAGES=TOT_PAGES+1
	PAGE=MIN(TOT_PAGES,PAGE)
        PAGEBEG=(PAGE-1)*2*PAGESIZE+1
	WRITE(CLIN4,4000) PAGE,TOT_PAGES
        CNT=1
C
100     CONTINUE
        IF(CNT.LT.PAGEBEG) THEN
           J=SSPTOPC(SSGNEL,J,GIND)
           CNT=CNT+1
           IF(J.EQ.0) GOTO 200
           GOTO 100
        ENDIF
200     CONTINUE             
        DO ROWNUM=1,2*PAGESIZE

           ENTR=SSPTOPC(SSGAME,J,GIND)
           CALL MOVBYT(SSPTOPC(SSGAMB,J,GIND),1,BUCKET,1,2)
           AMT=SSPCAMT(ENTR,BUCKET,GIND)
           IF(AMT.EQ.0) THEN
              ODDS=9999.99
           ELSE 
              ODDS=POOL/AMT
	   ENDIF		
	
	   TEMP = 0
           BTEMP(1) =SSPTOPC(SSGCID+2,J,GIND)
           SCORE(1)=ISHFT(TEMP,-4)
           SCORE(2)=IAND(TEMP,'0000000F'X)
           BTEMP(1)=SSPTOPC(SSGCID+1,J,GIND)
           SCORE(3)=ISHFT(TEMP,-4)
           SCORE(4)=IAND(TEMP,'0000000F'X)
           BTEMP(1) =SSPTOPC(SSGCID,J,GIND)
           SCORE(5)=ISHFT(TEMP,-4)
           SCORE(6)=IAND(TEMP,'0000000F'X)

	   IF(SSCEST(2,GIND).EQ.GAMOPN.AND.SSCEST(3,GIND).EQ.GAMOPN) THEN
              WRITE(ONEROW,906) CNT,
     *                    (SCORE(K),K=1,6),
     * 			  CMONY(AMT,9,BETUNIT),
     *                    ODDS
	   ELSEIF(SSCEST(2,GIND).EQ.GAMOPN.AND.SSCEST(3,GIND).NE.GAMOPN) THEN
              WRITE(ONEROW,9061) CNT,
     *                    (SCORE(K),K=1,4),
     * 			  CMONY(AMT,9,BETUNIT),
     *                    ODDS  
	   ELSEIF(SSCEST(2,GIND).NE.GAMOPN.AND.SSCEST(3,GIND).NE.GAMOPN) THEN
              WRITE(ONEROW,9062) CNT,
     *                    (SCORE(K),K=1,2),
     * 			  CMONY(AMT,9,BETUNIT),
     *                    ODDS  
           ENDIF
	   IF(SCORE(1).EQ.15) MUUT1=MUUT
	   IF(SCORE(2).EQ.15) MUUT2=MUUT
	   IF(SCORE(3).EQ.15) MUUT3=MUUT
	   IF(SCORE(4).EQ.15) MUUT4=MUUT
	   IF(SCORE(5).EQ.15) MUUT5=MUUT
	   IF(SCORE(6).EQ.15) MUUT6=MUUT
 	   CALL FASTMOV(IONEROW,IROWS(1,ROWNUM),10)  

           J=SSPTOPC(SSGNEL,J,GIND)
           IF(J.LE.0.OR.J.GT.SSGTOP) GOTO 300
	   CNT=CNT+1  

        ENDDO

300     CONTINUE
        IF(ROWNUM.LT.2*PAGESIZE) THEN
	   DO I=ROWNUM+1,2*PAGESIZE
	      CALL FASTSET(SPACE,IROWS(1,I),10)
	   ENDDO
        ENDIF

	LNS=6
	DO I=1,PAGESIZE
	   WRITE(XNEW(LNS),907) ROWS(I),ROWS(I+PAGESIZE)     
	   LNS=LNS+1
        ENDDO
999	CONTINUE
	NETPOL = IDNINT(DFLOAT(SSCSAL(GIND))*CALPER(SSCSPR(GIND)))
	TOTPOL = NETPOL + SSCPOL(1,GIND)
	WRITE(XNEW(22),922) CSMONY(SSCSAL(GIND),12,BETUNIT),
     *                       CSMONY(NETPOL,11,BETUNIT),
     *                       CSMONY(SSCPOL(1,GIND),10,BETUNIT),
     *                       CSMONY(TOTPOL,13,BETUNIT)
        RETURN

1       FORMAT(1X,3(Z3.2),3X,2(Z3.2),1X,Z3.2,2X,2(1X,Z3.2),5X,I8,I4)
3000    FORMAT ('Enter !',A8,' game index ')
3010    FORMAT (A8,1X,I1,' game not active')
3020    FORMAT (5A4,' open error ',I4)
3030    FORMAT (5A4,' read error ',I4,' record > ',I4)
3040    FORMAT (A8,1X,I1,' game not initialized event > ',I4)
3050    FORMAT (80(' '))
3060    FORMAT (80(' '))
901     FORMAT ('*',A8,1X,I1,7A2,'-',7A2,'* ')
902     FORMAT (<SSNMS_LEN/4>A4,'*Event code-',I3,'*',A17,'*Draw',7A2)
903     FORMAT(1X,6A4,3X,6A4,3X,6A4)
905     FORMAT(2(' No Scr 1 Scr 2 Scr 3    Amount    Odds '))
906     FORMAT(2(I3,' ',3(I2.2,'-',I2.2),A9,F9.2))
9061    FORMAT(2(I3,' ',2(I2.2,'-',I2.2),6X,A9,F9.2))
9062    FORMAT(2(I3,' ',1(I2.2,'-',I2.2),12X,A9,F9.2))
907     FORMAT(2A40)
922     FORMAT ('Sales ',A12,1(' '),'Net pool ',A11,1(' '),
     *          'Extra ',A10,1(' '),'Tot pool ',A13)
4000	FORMAT(1X,T65,'Page',1X,I3,' of ',I3)
	END
