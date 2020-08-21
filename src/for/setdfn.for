C
C V01 01-JUN-2000 UXN Initial release.
C
C Parameters:
C   Input: 
C       INTEGER*4 GNUM         - game number
C       INTEGER*4 SCDC         - starting CDC 
C       INTEGER*4 ECDC         - ending CDC
C   Output:
C       CHARACTER*20 CFILES(*) - draw file names
C       INTEGER*4 FILCNT       - draw file count in CFILES
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
	SUBROUTINE SETDFN(GNUM, SCDC, ECDC, VOL, CFILES, FILCNT)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
C
	INTEGER*4 GNUM
	INTEGER*4 SCDC	
	INTEGER*4 ECDC	
        INTEGER*4 VOL
	CHARACTER*20 CFILES(*)
	INTEGER*4 FILCNT
C
	INTEGER*4 FDB(7)
	INTEGER*4 ST, LUN/7/, CDC
	INTEGER*4 GTYP
C
	GTYP = GNTTAB(GAMTYP, GNUM)
	IF(ISODDSET2(GTYP)) THEN
	   VOL = P(ODD_DRWPCK)
	ELSE
	   VOL = P(REG_DRWPCK)
	ENDIF
	IF(VOL.EQ.0) VOL = 'DRAW'

	CALL GETLUN(LUN)

        CALL OPENW(LUN,SFNAMES(1,DAF),4,0,0,ST)
        CALL IOINIT(FDB,LUN,DAFSEC*256)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)

        DO 10 CDC=SCDC,ECDC
           CALL READW(FDB,CDC,DAFREC,ST)
           IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
           IF(DAFSTS.EQ.DNOSAL)  GOTO 10
           IF(DAFDRW(GNUM).EQ.0) GOTO 10
           FILCNT=FILCNT+1
           WRITE (CFILES(FILCNT),900) VOL,GSNAMES(GNUM),CDC
10      CONTINUE
        CALL CLOSEFIL(FDB)
900	FORMAT(A4,':',A4,I4.4,'.FIL')
	END
