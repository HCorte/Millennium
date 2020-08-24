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
	SUBROUTINE RESPOL_TSTR(GNUM, CDC, TIME)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:STROCOM.DEF'
	INCLUDE 'INCLIB:STRFREC.DEF'
C
	COMMON/RESULTS_STRFREC/ STRFREC
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
	CALL FASTSET(0,STRFREC,STRFLEN)
C
C Set date&time
C
	DSTLAT(LATCDC) = CDC
	DSTLAT(LATTIM) = TIME
C
	FILCNT = 0
	CALL SETDFN(GNUM, DSTBSD, DSTESD, VOL, CFILES, FILCNT)
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

	   CALL UPDPOL_TSTR(TRABUF)

           GOTO 20      ! GET NEXT WAGER FROM DRAW FILE

10      CONTINUE        ! GET NEXT DRAW FILE

9000    FORMAT(1X,A,'Scanning file ',A) 
	END
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SUBROUTINE to update pools
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
	SUBROUTINE UPDPOL_TSTR(TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:STROCOM.DEF'
	INCLUDE 'INCLIB:STRFREC.DEF'
C
	COMMON/RESULTS_STRFREC/ STRFREC
	
	INTEGER*4 I1,I2,I3,MONY
	INTEGER*4 ROW1,ROW2,ROW3, UCID, OFF1, OFF2, OFF3
C
        MONY=TRABUF(TWAMT)
        IF(TRABUF(TWSYST).EQ.FULSYS) MONY=MONY/TRABUF(TWSYSN)
        OFF1=-1
        OFF2=OFF1+TRABUF(TWSTM1)
        OFF3=OFF2+TRABUF(TWSTM2)

        DO 1030 I1=1,TRABUF(TWSTM1)
           ROW1 = TRABUF(TWSTBET+OFF1+I1)
           DO 1020 I2=1,TRABUF(TWSTM2)
              ROW2 = TRABUF(TWSTBET+OFF2+I2)
              IF(ROW1.EQ.ROW2) GOTO 1020
              DO 1010 I3=1,TRABUF(TWSTM3)
                 ROW3 = TRABUF(TWSTBET+OFF3+I3)
                 IF(ROW1.EQ.ROW3) GOTO 1010
                 IF(ROW2.EQ.ROW3) GOTO 1010
                 UCID = ROW1+(ROW2-1)*MAXSTRRW+(ROW3-1)*MAXSTRRW*MAXSTRRW
                 STRFODDS(STRGAMT,UCID) = STRFODDS(STRGAMT,UCID) + MONY
1010	      CONTINUE
1020	   CONTINUE
1030	CONTINUE
	END
