C
C SUBROUTINE ZAPCLERK
C
C ZAPCLERK.FOR
C
C V13 04-JUN-1999 UXN File size calculated.
C V12 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V11 08-APR-1994 HXK FASTER CLEAR OF CLERK FILE
C V10 08-JAN-1994 HXK HALVED INIT RECORD LENGTH.
C V09 08-JAN-1994 HXK speeded up by restoring record length * 4
C V08 10-NOV-1993 BCD FIX FOR INCORRECT ZAP LENGTH.
C V07 17-OCT-1993 HXK added IAM to type*'s.
C V06 29-JUN-1993 HXK REMOVED 'MULTIPY BY 4' (*4) FACTOR IN IOINIT
C V05 19-JUN-1993 HXK removed ctrl char (caused compiler error)
C V04 16-JUN-1993 SXH Released for Finland
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 12-OCT-1989 LOU R.   INITIAL RELEASE FOR FINLAND.
C
C INITIALIZE CLERK ACCOUNTING FILE
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE ZAPCLERK
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:CLERK.DEF'
C
	INTEGER*4  ST,K
C
C OPEN CLERK ACCOUNTING FILE
C
	TYPE*,IAM(),' Beginning initialization of CLERK.FIL'

        CALL CRTFIL(SFNAMES(1,CLK),NUMAGT*CLRKSEC/2,ST)
        IF(ST.NE.0) THEN
           WRITE(6,900) IAM(),(SFNAMES(K,CLK),K=1,5),ST
           CALL GPAUSE
        ELSE
           TYPE*,IAM(),' CLERK.FIL initialisation complete'
        ENDIF
	RETURN
900     FORMAT(1X,A,5A4,'   Allocation error ',I8)
	END
