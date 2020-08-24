C
C PROGRAM LODIMG4
C 
C V05 10-FEB-2000 UXN SNIF_AND_WRKSET added.
C V04 21-JUL-1999 UXN SERVCOM added.
C V03 08-JAN-1997 HXK Moved TSPCOM out to LODIMG4
C V02 07-JAN-1997 HXK Switched some .def's from LODIMG2 to LODIMG4
C V01 07-JAN-1997 HXK Initial revision.
C  
C  DEC Baseline
C
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM LODIMG4
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:ENCCOM.DEF'
        INCLUDE 'INCLIB:HASHMEM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:BNGCOM.DEF'
C
	INTEGER*4   ST
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
	CALL LOCKPAGES('ENCCOM', FRST_ENCCOM, LAST_ENCCOM)
        CALL LOCKPAGES('TSPCOM', FRST_TSPCOM, LAST_TSPCOM)
        CALL LOCKPAGES('X2XREL', FRST_X2XREL, LAST_X2XREL)
        CALL LOCKPAGES('BNGCOM', FRST_BNGCOM, LAST_BNGCOM)

	CALL LOCKPAGES('???TOTAL???',0,0)
C
1000	CONTINUE
	CALL XWAIT(5,2,ST)		!WAIT 5 SECONDS
	IF(DAYSTS.NE.DSCLOS) GOTO 1000
C
	CALL GSTOP(GEXIT_SUCCESS)

	END
