C
C PROGRAM ADJRPT
C
C V08 07-JUL-2000 UXN ADJRPTSUB entries added.
C V07 27-APR-1994 JXP COPY=0
C V06 03-JAN-1994 HXK INCREMENT LINCNT ONLY WHEN LINE IS PRINTED
C V05 24-NOV-1993 SXH ONLY PRINT NON-ZERO ADJUSTMENTS
C V04 10-OCT-1993 GXA Changed for negative amounts accumulation.
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
C SUMMARY ADJUSTMENTS REPORT
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
	PROGRAM ADJRPT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:ASFREP.DEF'

C
	INTEGER*4 REC, AGT, ACTAGT, ST, I
	INTEGER*4 SORT(NUMAGT)
	CHARACTER CZERO /Z0/
C
	CALL COPYRITE
C
C SORT BY AGENT NUMBER
C
	CALL SRTFLD(1,1,SORT,ACTAGT)
C
C OPEN THE AGENT SALES FILE
C
	CALL OPENASF(ASF)
	CALL ADJRPT_BEGIN
C
C LOOP THROUGH AND READ ASF BY AGENT RECORD NUMBER IN SORT
C
	DO 500 AGT=1,ACTAGT
	   REC=SORT(AGT)
	   CALL READASF(REC,ASFREC,ST)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,ASF),2,ST,REC)
C
           DO 100 I=1,512
              IF(ASFBYT(I).EQ.CZERO) ASFBYT(I)=' '
100        CONTINUE
	   CALL ADJRPT_UPDATE	
500	CONTINUE

	CALL CLOSASF
	CALL ADJRPT_END

	CALL GSTOP(GEXIT_SUCCESS)
	END
