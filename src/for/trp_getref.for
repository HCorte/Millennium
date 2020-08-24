C
C SUBROUTINE TRP_GETREF
C  
C V03 03-JUL-2000 UXN Refund too late played tickets.
C V02 01-FEB-2000 UXN TNFRAC ADDED.
C V01 XX-XXX-XXXX RXK INITIAL RELEASE.
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE TRP_GETREF(TRABUF,WINTAB,REFTAB,WIN,REFROWS,LATEFLG)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

	LOGICAL*4 LATEFLG
        INTEGER*4 WINTAB(65)
        INTEGER*4 REFTAB(65)
        INTEGER*4 GIND
        INTEGER*4 ROW
        INTEGER*4 I
        INTEGER*4 OFF
        INTEGER*4 WIN
        INTEGER*4 REFROWS
        INTEGER*4 AMT
        INTEGER*4 REFAMT
        INTEGER*4 REF1,REF2,REF3
        INTEGER*4 MB,MC
C----------------------------- Start of Code --------------------------

        GIND = TRABUF(TGAMIND)
        AMT=TRABUF(TWAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT/TRABUF(TNFRAC)
        REFROWS = 0

C---- Check all tickets refunded

        IF (LTRSTS(GIND).EQ.GAMCAN .OR. LATEFLG) THEN
           WIN = WIN + 1
           WINTAB(WIN) = AMT
           REFTAB(WIN) = 1
           MB = MAX(TRABUF(TWTTMB),1)
           MC = MAX(TRABUF(TWTTMC),1)
           REFROWS = TRABUF(TWTTMA)*MB*MC
	   IF(LATEFLG) THEN
	      LTRLAT(LATCNT,GIND) = LTRLAT(LATCNT,GIND) + 1
	      LTRLAT(LATAMT,GIND) = LTRLAT(LATAMT,GIND) + AMT
	   ENDIF
           RETURN
        ENDIF

        IF(TRABUF(TWSYST).EQ.FULSYS) AMT=AMT/TRABUF(TWSYSN)

C---- Check if any row cancelled

        REF1=0
        REF2=0
        REF3=0
        REFAMT=0
        OFF=-1
        MB = MAX(TRABUF(TWTTMB),1)
        MC = MAX(TRABUF(TWTTMC),1)

        DO I = 1,TRABUF(TWTTMA)
           ROW = TRABUF(TWTTBET+OFF+I)
           IF(LTRSTA(ROW,1,GIND).EQ.GAMCAN) THEN
              REFROWS = REFROWS+MB*MC
              REF1=REF1+1       
           ENDIF
        ENDDO
        IF(REF1.EQ.TRABUF(TWTTMA)) GOTO 100

        OFF=OFF+TRABUF(TWTTMA)
        DO I = 1,TRABUF(TWTTMB)
           ROW = TRABUF(TWTTBET+OFF+I)
           IF(LTRSTA(ROW,2,GIND).EQ.GAMCAN) THEN
              REFROWS = REFROWS+(TRABUF(TWTTMA)-REF1)*MC
              REF2=REF2+1
           ENDIF
        ENDDO
        IF(REF2.EQ.TRABUF(TWTTMB)) GOTO 100

        OFF=OFF+TRABUF(TWTTMB)
        DO I = 1,TRABUF(TWTTMC)
           ROW = TRABUF(TWTTBET+OFF+I)
           IF(LTRSTA(ROW,3,GIND).EQ.GAMCAN) THEN
              REFROWS = REFROWS+(TRABUF(TWTTMA)-REF1)*(TRABUF(TWTTMB)-REF2)
              REF3=REF3+1       
           ENDIF
        ENDDO

100     CONTINUE

        IF(REF1.GT.0 .OR. REF2.GT.0 .OR. REF3.GT.0) THEN
           WIN = WIN + 1
           WINTAB(WIN) = REFROWS*AMT
           REFTAB(WIN) = 1
        ENDIF

        RETURN

        END
