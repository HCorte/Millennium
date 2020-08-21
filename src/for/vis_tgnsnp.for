C SUBROUTINE TGNSNP
C
C V02 03-NOV-2003 FRP Fix for BEGSAL and ENDSAL.
C V01 02-DEC-2000 UXN Initial release.
C
C TotoGolo GAME NAME SNAPSHOT
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
	SUBROUTINE TGNSNP(NUM,GIND)
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
	INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:DTGREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
C
	INTEGER*2 BEGSAL(LDATE_LEN),ENDSAL(LDATE_LEN)
        INTEGER*4 FDB(7),ROWRES(2,TGGNBR)
	INTEGER*4 NUM,GIND,GNUM,DRAW,LNS,ST
	INTEGER*4 I,K,R
	CHARACTER RESULT(0:3)
	CHARACTER RES(0:3)
	DATA RES/'0','1','2','M'/
	CHARACTER*17 POLSTS(11)
	INTEGER*4 NAME_LEN
	PARAMETER(NAME_LEN=TGNMS_LEN/4)

	DATA POLSTS/'Not initialized  ','No drawing       ',
     *	            'Info entered     ','Game open        ',
     *	            'End of game      ','Results entered  ',
     *	            'Results verified ','Drawing completed',
     *	            'Results are final','Refund/cancelled ',
     *	            'Refunds enabled  '/
C
C
	DRAW=NUM
	IF(GIND.LT.1.OR.GIND.GT.NUMTGL) GIND=1
C
	GNUM=GTNTAB(TTGL,GIND)
	IF(GNUM.LT.1) THEN
	  WRITE(CLIN23,3010) GTNAMES(TTGL),GIND
	  RETURN
	ENDIF
	IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
	IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
C
C GET DATA FROM COMMON OR DISK
C
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	  CALL GAMLOG(TTGL,GIND,DTGREC,TGLSTS)
	  GOTO 100
	ENDIF
C
C
	SMODE=.TRUE.
	CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,1,DTGSEC*256)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	  CALL USRCLOS1(     1)
	  RETURN
	ENDIF
	CALL READW(FDB,DRAW,DTGREC,ST)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	  CALL USRCLOS1(     1)
	  RETURN
	ENDIF
	IF(DTGSTS.EQ.0) THEN
	  WRITE(CLIN23,3040) GTNAMES(TTGL),GIND,DRAW
	  CALL USRCLOS1(     1)
	  RETURN
	ENDIF
	CALL USRCLOS1(     1)
C
100	CONTINUE
	IF(DTGSTS.GE.GAMENV) THEN
	   CALL FASTMOV(DTGWIN,ROWRES,2*TGGNBR)
	   DO I=0,3
	      RESULT(I) = RES(I)
	   ENDDO
	ELSE
	   CALL FASTSET(0,ROWRES,2*TGGNBR)
	   DO I=0,3
	      RESULT(I) = ' '
           ENDDO
	ENDIF
C
C
        BEGSAL(VCDC)=DTGBSD
        ENDSAL(VCDC)=DTGESD
        CALL LCDATE(BEGSAL)
        CALL LCDATE(ENDSAL)
C
C
	WRITE(CLIN1,1000) (GLNAMES(K,GNUM),K=1,4),
     *	                  (BEGSAL(I),I=9,13),(ENDSAL(I),I=9,13)
	WRITE(CLIN2,1001) DTGDRW,POLSTS(DTGSTS+1)
	WRITE(CLIN3,1002)
	LNS=4
	DO 210 R=1,DTGMAX
	  WRITE (XNEW(  LNS),1003) R,
     *          (DTGNMS(I,1,R),I=1,NAME_LEN),(DTGNMS(I,2,R),I=1,NAME_LEN),
     *          RESULT(ROWRES(1,R)),RESULT(ROWRES(2,R))
	LNS=LNS+1
210	CONTINUE
	RETURN
C
C     FORMAT STATEMENTS
C
888	FORMAT(80(' '))
C999    FORMAT(1X,I2.2,77(' '))
1000	FORMAT(1X,4A4,2X,5A2,'-',5A2)
1001	FORMAT('  Event code- ',I4,20(' '),'* ',A17,' *')
1002	FORMAT('Row',2X,'Team names',25X,'Result')
1003	FORMAT(1X,I2.2,2X,<NAME_LEN>A4,' - ',<NAME_LEN>A4,1X,A1,' : ',A1)
3010	FORMAT(A8,1X,I1,' game not active')
3020	FORMAT(5A4,' open error ',I4)
3030	FORMAT(5A4,' read error ',I4,' record > ',I4)
3040	FORMAT(A8,1X,I1,' game not initialized event > ',I4)
	END
