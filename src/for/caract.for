C CARACT.FOR
C
C V08 07-JUL-2000 UXN Start using CARACTSUB entries.
C V07 13-NOV-1997 UXN THe interface for CARACT_REPRT changed.
C V06 23-SEP-1992 HJK ADDED SPEDEN GAME #!#
C V05 04-MAR-1992 HJK FIXED OVERFLOWS (GOOD TO 42 MILLION)
C V04 31-MAY-1991 PP  ADDED CALL TO SUBPROGRAM BALWRI (FOR BALANCE REPORT)
C V02 30-JAN-1991 HHE OTHER THAN CARTEL SPECIAL_CARTEL SUMS ON OWN PAGE
C V03 20-APR-1990 HHE ONLY CARTEL TOTALS, TAKEN FROM AGTACT
C
C CARTEL DAILY SUMMARY REPORT
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
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
        PROGRAM CARACT
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:ASFREP.DEF'

        INTEGER*4  SORT(NUMAGT)           ! SORT ARRAY

        INTEGER*4  ST                     !
        INTEGER*4  CNT                    !
        INTEGER*4  REC                    ! 
C
C CALL  COPYRITE  SUBROUTINE
C
        CALL COPYRITE
C
C Sort by cartel and agent number
C
        CALL SRTFLD(66,1,SORT,CNT)
C
C Open the agent sales file
C
        CALL OPENASF(ASF)
	CALL CARACT_BEGIN
C
C Loop through and read ASF by agent record number
C
        TYPE *, IAM(),'Processing...'
C
        DO 500 REC=1,CNT
            XREC = SORT(REC)
            CALL READASF(XREC,ASFREC,ST)
	    CALL CARACT_UPDATE
500     CONTINUE
C
        CALL CLOSASF
	CALL CARACT_END

        CALL GSTOP(GEXIT_SUCCESS)
C
        END
