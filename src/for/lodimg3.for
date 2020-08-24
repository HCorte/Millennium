C PROGRAM LODIMG3
C
C V07 10-FEB-2000 UXN SNIF_AND_WRKSET added.
C V06 25-MAY-1999 UXN AGTCOM split into two LODIMG
C V05 20-NOV-1995 HXK Minor changes for Double, Couple release
C V04 13-NOV-1995 HXK Initial revision.
C V03 10-NOV-1995 HXK Further changes for Double, Couple
C V02 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V01 24-AUG-1993 GXA Initial revision.
C  
C  DEC Baseline
C
C
C This program exists solely to lock global pages in memory.
C It is run by reset.
C LODIMG3 exists so you can lock more than 65000 pages (Limit for one task).
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
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM LODIMG3
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:X2FCOM.DEF'
        INCLUDE 'INCLIB:X2NETCOM.DEF'

C
C
	INTEGER*4   ST
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
        CALL LOCKPAGES('X2FCOM', FRST_X2FCOM, LAST_X2FCOM)
        CALL LOCKPAGES('X2NETCOM', FRST_X2NETCOM, LAST_X2NETCOM)
        CALL LOCKPAGES('X2XCOM', FRST_X2XCOM, LAST_X2XCOM)

	CALL LOCKPAGES('???TOTAL???',0,0)
C
1000	CONTINUE
	CALL XWAIT(5,2,ST)		!WAIT 5 SECONDS
	IF(DAYSTS.NE.DSCLOS) GOTO 1000
C
	CALL GSTOP(GEXIT_SUCCESS)

	END
