C
C PROGRAM LODIMG5
C
C V05 10-FEB-2000 UXN SNIF_AND_WRKSET added.
C V04 13-JAN-2000 UXN From CHKTAB(2,3700) to CHKTAB(1,7300) locked here. 
C V03 26-NOV-1999 UXN From CHKTAB(2,3700) to CHKTAB(1,7500) locked here. 
C V02 25-MAY-1999 UXN CHKTAB(2,3000) changed to CHKTAB(2,3700) 
C V01 03-JUL-1998 UXN Initial release.  
C
C This program exists solely to lock global pages in memory.
C It is run by reset.
C LODIMG2 exists so you can lock more than 65000 pages (Limit for one task).
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
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM LODIMG5
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:CHKCOM.DEF'
C
C
	INTEGER*4   ST
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
        CALL LOCKPAGES('CHKCOM',FRST_CHKCOM, LAST_CHKCOM)
C
	CALL LOCKPAGES('???TOTAL???',0,0)
C
1000	CONTINUE
	CALL XWAIT(5,2,ST)		!WAIT 5 SECONDS
	IF(DAYSTS.NE.DSCLOS) GOTO 1000
C
	CALL GSTOP(GEXIT_SUCCESS)

	END
