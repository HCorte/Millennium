C
C FUNCTION REVNIB
C $Log:   GXAFXT:[GOLS]REVNIB.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:44:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   07 Nov 1994 12:14:28   HXK
C  Initial revision.
C  
C
C This function will reverse the nibble order within a byte
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS/CHECK=NOOVERFLOW
	BYTE FUNCTION REVNIB(BITE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INTEGER*4 WORK
        BYTE BITE

        WORK = BITE

        REVNIB  = IAND(ISHFT(WORK,4), '000000F0'X ) +
     *          IAND(ISHFT(WORK,-4),'0000000F'X)
 
        RETURN
        END
C
