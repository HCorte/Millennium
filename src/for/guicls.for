C
C SUBROUTINE TO CLOSE FILES FOR GUIMGR
C
C V02 29-APR-2011 RXK Closing of TCF and VLF moved to GUI_039.
C V01 08-NOV-2000 UXN Initial release.
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
	SUBROUTINE GUICLS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
C
	INTEGER*4 I,DUMMY,ST
C
	CALL CLOSEFIL(TMFFDB)
	CALL CLOSEFIL(ASFFDB)
	CALL CLOSEFIL(DAFFDB)
	CALL CLOSEFIL(X2STNFDB)
	CALL CLOSEFIL(X2SCLFDB)
	CALL CLOSEFIL(X2TERFDB)
C
C	CALL ICLOSE(VLFLUN,DUMMY,ST)
C	CALL ICLOSE(TCFLUN,DUMMY,ST)
C
	DO 10 I=1,MAXGAM
	   IF(DAYHDR(I).LT.1) GOTO 10
	   CALL CLOSEFIL(GAMFDB(1,I))
10	CONTINUE
	RETURN
	END
