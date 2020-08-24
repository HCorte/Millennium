C PROGRAM SYSTOT
C  
C V09 05-APR-2000 OXK Non-multidraw games commented out.
C V08 13-OCT-1999 RXK World Tour added.
C V07 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V06 27-APR-1994 JXP COPY=0
C V05 26-AUG-1993 SXH Added IAM() etc
C V04 13-JUL-1993 SXH Released for Finland
C V03 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C     		      and Comm 1/93 update.  DEC Baseline
C V02 31-JAN-1992 GCAN ADDED JOKER.
C V01 03-APR-1991 MTK  INITIAL RELEASE FOR MARYLAND
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM SYSTOT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'

        ! variables
	INTEGER*4  GNUM                !
	INTEGER*4  GIND                !
	INTEGER*4  GTYP                !
	INTEGER*4  COPY                !
C
	CALL COPYRITE
C
C
C	CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
C	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	COPY=0

	CALL SYSSUB(0,0,0,COPY)
	DO 100 GNUM=1,MAXGAM
	    GTYP=GNTTAB(GAMTYP,GNUM)
	    GIND=GNTTAB(GAMIDX,GNUM)
            IF(GTYP .EQ. TPAS) GOTO 100
	    IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) GOTO 100
	    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 100
	    CALL SYSSUB(GNUM,GTYP,GIND,COPY)
100	CONTINUE

	CALL GSTOP(GEXIT_SUCCESS)

	END
