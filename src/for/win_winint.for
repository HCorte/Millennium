C
C SUBROUTINE WIN_WININT
C
C SUBROUTINE TO INITIALIZE WINNER SELECTION COMMON FOR LOTTO, SPORTS, KICKER
C
C V05 11-APR-2011 RXK Split IF for TKIK
C V04 03-DEC-2000 UXN TOTOGOLO added
C V03 29-JUN-2000 UXN P(SPTTCF) removed
C V02 14-MAR-2000 UXN NEED_TCF added.
C V01 01-MAR-2000 UXN Initial release.
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
	SUBROUTINE WIN_WININT(FILES,FTYPE,FILCNT)
	IMPLICIT NONE

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

	CHARACTER*20 FILES(200)
	INTEGER*4    FTYPE(200)
	INTEGER*4    FILCNT

	LOGICAL*4   NEED_TCF
	INTEGER*4   GNUM, GIND,  GTYP
C
	FILCNT = 0
 	CALL WIN_WININT_TLTO(FILES,FTYPE,FILCNT)
	CALL WIN_WININT_TSPT(FILES,FTYPE,FILCNT)
	CALL WIN_WININT_TTGL(FILES,FTYPE,FILCNT)
	CALL WIN_WININT_TKIK(FILES,FTYPE,FILCNT)

	NEED_TCF =.FALSE.
	DO 5 GNUM=1,MAXGAM
	   GTYP = GNTTAB(GAMTYP,GNUM)
	   GIND = GNTTAB(GAMIDX,GNUM)
	   IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) GOTO 5
	   IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 5
	   IF(GTYP.NE.TLTO.AND.GTYP.NE.TSPT .AND.
     *        GTYP.NE.TKIK.AND.GTYP.NE.TTGL) GOTO 5
	   IF(GTYP.EQ.TLTO.AND.LLTDRW(GIND).LE.0) GOTO 5
	   IF(GTYP.EQ.TKIK) THEN
              IF(LKKDRW(GIND).LE.0) GOTO 5
           ENDIF
	   IF(GTYP.EQ.TSPT.AND.LSPDRW(GIND).LE.0) GOTO 5
	   IF(GTYP.EQ.TTGL.AND.LTGDRW(GIND).LE.0) GOTO 5

	   IF(GTYP.EQ.TSPT) THEN
	      IF(LSPMLT(GIND).GT.1) NEED_TCF = .TRUE.
	   ELSEIF(GTYP.EQ.TTGL) THEN
	      IF(LTGMLT(GIND).GT.1) NEED_TCF = .TRUE.
	   ELSE
	      NEED_TCF = .TRUE.
	   ENDIF

5	CONTINUE

	IF (NEED_TCF) THEN
	    MRGTYP(1) = MRGALL
	ELSE
	    MRGTYP(1) = MRGVL
	ENDIF

	END
