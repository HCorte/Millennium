C
C V01 27-APR-2001 UXN Initial release.
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE GUI_CHKSTS_RESCOM(GIND,GTYP,DRAW,STS)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

	INTEGER*4 GIND,GTYP,DRAW,STS

	STS = 0

	IF(GIND.NE.CUR_GIND .OR. 
     *     GTYP.NE.CUR_GTYP .OR.
     *     DRAW.NE.CUR_DRAW) RETURN

	IF(GTYP.EQ.TLTO) THEN
	   STS = DLTSTS
	ELSEIF(GTYP.EQ.TSPT) THEN 
	   STS = DSPSTS
	ELSEIF(GTYP.EQ.TKIK) THEN
	   STS = DKKSTS 
	ELSEIF(GTYP.EQ.TTGL) THEN
	   STS = DTGSTS 
        ELSEIF(GTYP.EQ.TPAS) THEN
           STS = DPASTS
	ENDIF

	RETURN
	END
