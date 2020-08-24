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
	SUBROUTINE RESPOL_TSCR(GNUM, CDC, TIME)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:RECSPF.DEF'
C
	COMMON/RESULTS_SPFREC/ SPFREC
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
	CALL FASTSET(0,SPFPOL,SLEN*2)
C
C Set date&time
C
	DSCLAT(LATCDC) = CDC
	DSCLAT(LATTIM) = TIME
C
	FILCNT = 0
	CALL SETDFN(GNUM, DSCBSD, DSCESD, VOL, CFILES, FILCNT)
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

	   CALL UPDPOL_TSCR(TRABUF)

           GOTO 20      ! GET NEXT WAGER FROM DRAW FILE

10      CONTINUE        ! GET NEXT DRAW FILE

9000    FORMAT(1X,A,'Scanning file ',A) 
	END
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SUBROUTINE to update SCORE game pools
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
	SUBROUTINE UPDPOL_TSCR(TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:RECSPF.DEF'
C
	COMMON/RESULTS_SPFREC/ SPFREC
C	
	INTEGER*4 AMT, HOMSCR, AWYSCR
	INTEGER*4 I,J,INDEX
C
        IF(TRABUF(TWSYST).NE.1) GOTO 4000 ! SIMPLE BET
C
C SYSTEM BET
C         
        AMT=TRABUF(TWSAMT)
        DO 3030 I=0,TRABUF(TWNBET)-1
           HOMSCR=TRABUF(TWSSCR1+I*TWSBLEN)
           IF(HOMSCR.EQ.'FF'X) RETURN      
           DO J=0,TRABUF(TWNBET)-1
              AWYSCR=TRABUF(TWSSCR2+J*TWSBLEN)
              IF(AWYSCR.EQ.'FF'X) GOTO 3030
              CALL POLIND(HOMSCR,AWYSCR,INDEX)
              SPFPOL(INDEX,SPAMNT) = SPFPOL(INDEX,SPAMNT) + AMT
           ENDDO
3030    CONTINUE
	RETURN
C
4000	CONTINUE
C
C SIMPLE BET
C
        DO I=0,TRABUF(TWNBET)-1
           HOMSCR=TRABUF(TWSSCR1+I*TWSBLEN)
           AWYSCR=TRABUF(TWSSCR2+I*TWSBLEN)
           AMT=TRABUF(TWSAMT+I*TWSBLEN)
           CALL POLIND(HOMSCR,AWYSCR,INDEX)
           SPFPOL(INDEX,SPAMNT) = SPFPOL(INDEX,SPAMNT) + AMT
	ENDDO	
	END
