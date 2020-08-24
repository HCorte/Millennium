C
C CARRPT.FOR                                                
C
C V06 07-JUL-2000 UXN CARRPTSUB entries added.
C V05 13-NOV-1997 UXN Interface in CARRPT_REPRT call changed.
C V04 02-MAR-1992 HJK FIXED OVERFLOW PROBLEM
C V03 13-JAN-1991 HHE OWN TOTTAL GAGE FOR CARTELS OTHER THAN 500
C V02 22-MAR-1990 MTK FIXED OVERFLOW PROBLEM (GOOD TO 42 MILLION)
C V01 20-APR-1990 HHE ONLY CARTEL TOTALS, TAKEN FROM INVRPT
C
C CARTEL WEEKLY SUMMARY REPORT
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        PROGRAM CARREPT
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:CARRPT.DEF'   
	INCLUDE 'INCLIB:ASFREP.DEF'
C
C
        INTEGER*4  SORT(NUMAGT)             !SORT ARRAY
        INTEGER*4  ST                       !
        INTEGER*4  CNT                      !
        INTEGER*4  REC                      !
C
C CALL  COPYRITE  SUBROUTINE
C
        CALL COPYRITE
C
C SORT BY CARTEL AND AGENT NUMBER
C
        CALL SRTFLD(66,1,SORT,CNT)
C
C OPEN THE AGENT SALES FILE
C
        CALL OPENASF(ASF)
	CALL CARRPT_BEGIN
C
C LOOP THROUGH AND READ ASF BY AGENT RECORD NUMBER
C
        TYPE *, IAM(),'Processing...'
C
        DO 500 REC = 1, CNT
            XREC = SORT(REC)
            CALL READASF(XREC,ASFREC,ST)
	    CALL CARRPT_UPDATE
500     CONTINUE

        CALL CLOSASF
	CALL CARRPT_END

        END
