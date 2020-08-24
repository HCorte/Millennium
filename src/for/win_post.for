C
C SUBROUTINE WIN_POST
C  
C  
C V05 02-DEC-2000 UXN TOTOGOLO ADDED.
C V04 07-FEB-2000 UXN ADD_PENNY added.
C V03 27-APR-1999 RXK STOPSYS optimization (CARYSCAN is now an array).
C V02 15-SEP-1993 HXK Fix for Viking Winsel balancing
C V01 30-AUG-1993 HXK Added check for second kicker flag so that it second
C                     jokeri only wagers are caught
C  
C
C SUBROUTINE TO POST WINSEL SALES DATA
C
C CALLING SEQUENCE:
C     CALL WIN_POST(TRABUF)
C INPUT
C     TRABUF - LOTTO WAGER BODY
C OUTPUT
C     NONE
C
C
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
	SUBROUTINE WIN_POST(TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 KIND, GAM, DATIND, KGAM, GIND, GTYP
        INTEGER*4 AMT
C
        IF(TRABUF(TTYP).NE.TWAG) RETURN
        IF(CARYSCAN(TLTO).AND.TRABUF(TSTAT).EQ.EXCH) GOTO 100  !same for Vakio
        IF(TRABUF(TSTAT).NE.GOOD) RETURN                       !and Jokeri
C
C
100     CONTINUE
	GTYP=TRABUF(TGAMTYP)
	GIND=TRABUF(TGAMIND)
	KGAM=TRABUF(TWKGME)
	IF(TRABUF(TWKFLG).EQ.0.AND.TRABUF(TWKFLG2).EQ.0) KGAM=0
C
C
	IF(TRABUF(TGAMTYP).EQ.TLTO) THEN
	  DATIND=LLTDAT(CURDRW,GIND)-TRABUF(TCDC)+3
	  IF(CARYSCAN(TLTO)) THEN
             IF(TRABUF(TCDC_SOLD).GE.LLTBSD(GIND).AND.
     *          TRABUF(TCDC_SOLD).LE.LLTESD(GIND)) THEN
                  DATIND = LLTDAT(CURDRW,GIND) - TRABUF(TCDC_SOLD) + 3
             ELSE
                  DATIND=1
             ENDIF
          ENDIF
	  IF(MAILSCAN) DATIND=2
	  IF(TRABUF(TWBEG).GT.LLTDRW(GIND)) GOTO 200
	  IF(TRABUF(TWEND).LT.LLTDRW(GIND)) GOTO 200
          AMT = TRABUF(TWAMT)
	  LLTSAL(DATIND,GIND)=LLTSAL(DATIND,GIND)+AMT
	ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TSPT) THEN
	  DATIND=LSPDAT(CURDRW,GIND)-TRABUF(TCDC)+3
	  IF(CARYSCAN(TLTO)) DATIND=1
	  IF(MAILSCAN) DATIND=2
	  IF(TRABUF(TWBEG).GT.LSPDRW(GIND)) GOTO 200
	  IF(TRABUF(TWEND).LT.LSPDRW(GIND)) GOTO 200
          AMT = TRABUF(TWAMT)
	  LSPSAL(DATIND,GIND)=LSPSAL(DATIND,GIND)+AMT
	ENDIF
C
	IF(TRABUF(TGAMTYP).EQ.TTGL) THEN
	  DATIND=LTGDAT(CURDRW,GIND)-TRABUF(TCDC)+3
	  IF(CARYSCAN(TLTO)) DATIND=1
	  IF(MAILSCAN) DATIND=2
	  IF(TRABUF(TWBEG).GT.LTGDRW(GIND)) GOTO 200
	  IF(TRABUF(TWEND).LT.LTGDRW(GIND)) GOTO 200
          AMT = TRABUF(TWAMT)
	  LTGSAL(DATIND,GIND)=LTGSAL(DATIND,GIND)+AMT
	ENDIF
C
200	CONTINUE
	IF(KGAM.NE.0) THEN
C
	  GAM=TRABUF(TGAM)
          KIND=GNTTAB(GAMIDX,KGAM)
C
          DATIND = LKKDAT(CURDRW,KIND) - TRABUF(TCDC) + 3
	  IF(CARYSCAN(TLTO)) THEN
             IF(TRABUF(TCDC_SOLD).GE.LKKBSD(KIND).AND.
     *          TRABUF(TCDC_SOLD).LE.LKKESD(KIND)) THEN
                  DATIND = LKKDAT(CURDRW,KIND) - TRABUF(TCDC_SOLD) + 3
             ELSE
                  DATIND = 1
             ENDIF
          ENDIF
	  IF(TRABUF(TWKBEG).GT.LKKDRW(KIND)) RETURN
	  IF(TRABUF(TWKEND).LT.LKKDRW(KIND)) RETURN       
          AMT = TRABUF(TWKAMT)
	  LKKSAL(DATIND,GAM,KIND)=LKKSAL(DATIND,GAM,KIND)+AMT

	ENDIF
	RETURN
	END
