C
C PROGRAM INVRPT
C
C V12 07-JUL-2000 UXN  Start using INVRPTSUB entries.
C V11 27-APR-1994 JXP  COPY=0
C V10 08-OCT-1993 GXA  Corrected clearing of arrays and loop endings for 
C                      refunds.
C V09 08-OCT-1993 HXK  FURTHER CHANGES FOR RENT.
C V08 08-OCT-1993 HXK  ADDED RENT.
C V07 01-OCT-1993 GXA  Corrected dimensioning of cnt, amt ..... for REFIND.
C V06 26-AUG-1993 SXH  Added IAM() etc
C V05 27-JUN-1993 HXK  MOVED AGTINF.DEF INCLUDE
C V04 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V03 01-FEB-1992 GCAN ADDED TICKET CHARGE AND SEPARATE VALIDATION AND
C                      WINNERS COMMISSION.
C V02 12-NOV-1991 MTK  INITIAL RELEASE FOR NETHERLANDS
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
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM INVRPT
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:ASFREP.DEF'
C
        CHARACTER CZERO/Z0/
        INTEGER*4 I, ST,REC,SRTCNT
        INTEGER*4 SORT(NUMAGT)
C
	CALL COPYRITE
C
	CALL OPENASF(ASF)
C
C SORT INVOICE REPORT BY DISTRIBUTION LINE, RECEPTION CENTER AND AGENT NUMBER
C
        CALL SRTFLD2(SRTCNT, SORT)
	CALL INVRPT_BEGIN
C
C READ ALL AGENTS
C
        DO 100 REC = 1,SRTCNT
           XREC = SORT(REC)
	   CALL READASF(XREC,ASFREC,ST)
           DO I=1,ALENGTH
              IF(ASFBYT(I).EQ.CZERO) ASFBYT(I)=' '
	   ENDDO
	   CALL INVRPT_UPDATE
100     CONTINUE
	
	CALL CLOSASF
	CALL INVRPT_END

        CALL GSTOP(GEXIT_SUCCESS)
        END
