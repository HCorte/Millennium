C
C PROGRAM LODIMGS3
C
C V02 27-MAY-1999 UXN AGTCOM split into 2 LODIMG
C V01 15-DEC-1995 HXK Initial revision.
C
C LODIMGS3.FOR
C
C This program exists solely to lock global pages in memory during
C STOPSYS processing.
C It is run by STOPSYS
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM LODIMGS3
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
C
	INTEGER*4   ST, WORKSET_SIZE
C
	CALL COPYRITE
C
	CALL GET_WSEXTENT(WORKSET_SIZE)
	CALL WRKSET(WORKSET_SIZE)
C
	CALL LKPMEM(FRST_AGTCOM, LAST_AGTCOM)

C	FIRST WAIT FOR THE STATUS TO BECOME 'DSCLOS'
C	--------------------------------------------
1000	CONTINUE
	CALL XWAIT(10,2,ST)		!WAIT 10 SECONDS
	IF(DAYSTS.NE.DSCLOS) GOTO 1000

C	AFTER THAT WAIT FOR THE STATUS TO BECOME SOMETHING ELSE...
C	----------------------------------------------------------
2000	CONTINUE
	CALL XWAIT(5,2,ST)		!WAIT 5 SECONDS
	IF(DAYSTS.EQ.DSCLOS) GOTO 2000

	CALL GSTOP(GEXIT_SUCCESS)
	END
