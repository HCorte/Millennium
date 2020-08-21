C FUNCTION NETBYT
C  
C V04 08-jun-2000 OXK IMPLICIT NONE
C V03 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V02 18 Jun 1993 GXA Replaced comment '*' with 'C' for make_systemlist.com.
C V01 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C SWAP BYTES INTO NETWORK BYTE ORDER.
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
	INTEGER*2 FUNCTION NETBYT(PORT)
C
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	BYTE	  PORT(4)
	BYTE	  TMPBYT(2)
	INTEGER*2 TEMP
	EQUIVALENCE (TMPBYT,TEMP)
C
	TMPBYT(1)=PORT(2)
	TMPBYT(2)=PORT(1)
	NETBYT=TEMP
C
	RETURN
	END
