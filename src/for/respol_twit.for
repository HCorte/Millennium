C SUBROUTINE TO RECALCULATE POOLS FOR WINNERS TIP GAMES.
C
C V01 01-JUN-2000 UXN Initial release.
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
	SUBROUTINE RESPOL_TWIT(GNUM, CDC, TIME)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 GNUM
	INTEGER*4 CDC
	INTEGER*4 TIME
C
	CHARACTER*20 CFILES(100)
	INTEGER*4    FILES(5,100)
	EQUIVALENCE  (FILES, CFILES)
	INTEGER*4    FILCNT, LUN/7/
	INTEGER*4    ST, FILNUM
	LOGICAL*4    NEW,EOF
	INTEGER*4    FDB(7)
	INTEGER*4    LOGREC(LREC*3)
	INTEGER*4    VOL
	CHARACTER*4  CVOL
	EQUIVALENCE  (VOL,CVOL)
C
	VOL = P(ODD_DRWPCK)
        IF(VOL.EQ.0) THEN
           CALL PRMTEXT('Enter draw pack volume name: ',CVOL,ST)
        ENDIF
C
C Initialize pools
C	
	CALL FASTSET(0,DWISBR,MAXWRW)
C
C Set date & time
C
	DWILAT(LATCDC) = CDC
	DWILAT(LATTIM) = TIME
C
	FILCNT = 0
	CALL SETDFN(GNUM, DWIBSD, DWIESD, VOL, CFILES, FILCNT)
	CALL GETLUN(LUN)
C
        DO 10 FILNUM=1, FILCNT

           CALL OPENW(LUN,FILES(1,FILNUM),4,0,0,ST)
           IF(ST.NE.0) CALL FILERR(FILES(1,FILNUM),1,ST,0)
           CALL IOINIT(FDB,LUN,128*256)

           WRITE(6,9000) IAM(),CFILES(FILNUM)
           NEW = .TRUE.
           EOF = .FALSE.

20         CONTINUE

           CALL READDRWN(LOGREC,FDB,EOF,NEW)
           IF(EOF) THEN
              CALL CLOSEFIL(FDB)
              GOTO 10
           ENDIF
           CALL LOGTRA(TRABUF,LOGREC)
C
C UPDATE POOLS
C
	   IF(TRABUF(TCDC).GT.CDC) GOTO 20
	   IF(TRABUF(TCDC).EQ.CDC.AND.TRABUF(TTIM).GT.TIME) GOTO 20

	   IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TSTAT).NE.FRAC) GOTO 20
	   IF(TRABUF(TWFFLG).NE.0) GOTO 20 ! no fractions, only original tickets

	   CALL UPDPOL_TWIT(TRABUF)

           GOTO 20      ! GET NEXT WAGER FROM DRAW FILE

10      CONTINUE        ! GET NEXT DRAW FILE

9000    FORMAT(1X,A,'Scanning file ',A) 
	END
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SUBROUTINE to update VOITTAJA game pools
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
	SUBROUTINE UPDPOL_TWIT(TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	
	INTEGER*4 I,ROW, ROWAMT
C
        DO I = 0, TRABUF(TWNBET)-1
           ROW = TRABUF(TWWROW+I*TWWBLEN)
           ROWAMT = TRABUF(TWWAMT+I*TWWBLEN)
           DWISBR(ROW) = DWISBR(ROW) + ROWAMT
	ENDDO
C
	END
