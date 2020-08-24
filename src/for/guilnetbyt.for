C FUNCTION GUITCPPNETBYT
C
C V02 31-OCT-2000 UXN GUI prefix added.
C V01 16-JUN-1993 MP  INITIAL RELEASE FOR VAX (Produced From GUITCPASST).
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
C Copyright 1991-1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This program swaps port bytes into network byte order.
C INPUT:
C	PORT - port number
C OUTPUT:
C	GUITCPPNETBYT - port number with bytes in network order.
C
C
	INTEGER*2 FUNCTION GUITCPPNETBYT(PORT)
	IMPLICIT NONE
C
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
	GUITCPPNETBYT=TEMP
C
	RETURN
	END
