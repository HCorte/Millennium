C
C PROGRAM AGTACT
C
C AGTACT.FOR
C 
C V31 07-JUL-2000 UXN  Start using AGTACTSUB entries.
C V30 13-OCT-1999 RXK  World Tour added.
C V29 10-MAY-1999 UXN  Today's Triple changed to Today's Trio.
C                      Super Triple added.
C V28 18-DEC-1996 HXK  Update from TEBE project (MXP,WXW,PXN,MJF)
C V27 28-NOV-1996 WXW  Telebetting startup, changes MP/PXN/WXW.
C                      Fixes for non commission agents.
C V26 17-APR-1996 HXK  Release of Finland for X.25, Telephone Betting, 
C                      Instant Pass Thru Phase 1
C V25 15-DEC-1995 PXB  Changes for double and couple games
C V24 29-AUG-1995 PXB  Commission on RAVI V5 game for on track terminals
C V23 08-AUG-1995 HXK  Fix for adding refunds to validations for BALANS 
C                      report (RXK)
C V22 21-JUL-1995 PXB  Make ravi generic on totals page.
C V21 24-APR-1995 HXK  Merge of V5 development with March 10th 1995 bible
C V20 02-MAR-1995 HXK  Changed commision calculation for Ontrack agents
C V19 25-NOV-1994 PXB  Bug Fixes.
C V18 02-SEP-1994 HXK  Merge of May,June RFSS batch 
C V17 29-APR-1994 JXP  COPY=0
C V16 27-APR-1994 JXP  No change.
C V15 05-FEB-1994 HXK  CORRECTED SCR, WIT TOTALS; COSMETIC CHANGES.
C V14 28-JAN-1994 JXP  Corrected report format
C V13 26-JAN-1994 JXP  Total on tulus and voittaja games
C V12 04-NOV-1993 HXK  FIX FOR BALANS -SXH.
C V11 15-OCT-1993 HXK  Rmoved cancels from net amounts in totals.
C V10 15-OCT-1993 HXK  Fixed Cartel so that cartels other than first agent 
C                      is used.
C V09 16-SEP-1993 HXN  Replaced NET SALES data by GROSS SALES data when 
C                      printing report.
C V08 03-SEP-1993 SXH  HARD CODED COPY=1
C V07 17-JUN-1993 SXH  Released for Finland
C V06 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V05 04-AUG-1992 HDB  ADDED COMMENTS, CORRECTED WINNERSTIP TOTALS
C                      CHANGED THE WAY TO CHECK FOR ACTIVE GAME
C V04 14-APR-1992 HdB  CLEANED UP UNUSED VARIABLES
C V03 26-JAN-1992 GCAN CLEAR TOTAL AMOUNTS (WAS CARRIED OVER FROM GAME TO GAME)
C V02 12-NOV-1991 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 02-APR-1991 JPJ  INITIAL RELEASE FOR MARYLAND
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM AGTACT
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:ASFREP.DEF'
C
	INTEGER*4 ST,REC,SRTCNT,I
	CHARACTER*1 CZERO/Z0/
	INTEGER*4 SORT(NUMAGT)              !sort array

C
	CALL COPYRITE
C
C       READ AND SORT BY AGENT NUMBER
C
	CALL SRTFLD(1,1,SORT,SRTCNT)
C
C       OPEN ASF.FIL
C       
	CALL OPENASF(ASF)
	CALL AGTACT_BEGIN
C
C       READ ALL AGENTS BY AGENT NUMBER
C
	DO 100 REC=1,SRTCNT
	   XREC = SORT(REC)
	   CALL READASF(XREC,ASFREC,ST)

	   DO I=1,512
	      IF(ASFBYT(I).EQ.CZERO) ASFBYT(I)=' '
	   ENDDO

	   CALL AGTACT_UPDATE

100	CONTINUE    ! read next agent

	CALL CLOSASF
	CALL AGTACT_END

	CALL GSTOP(GEXIT_SUCCESS)

	END  
