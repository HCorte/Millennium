C
C $Log:   GXAFXT:[GOLS]DFRAC.FOV  
C  
C     Rev 1.0   17 Apr 1996 12:52:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   03 Aug 1993 14:14:00   GXA
C  Initial revision.
C
C Subroutine to Decode Fraction / Unfraction messages from the terminal.
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
        SUBROUTINE DFRAC(MESTAB,JUL,SER,CDIG,NUMTKT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	BYTE	  MESTAB(*)			!terminal Input Message
	INTEGER*4 JUL				!Julian Date
	INTEGER*4 SER				!Serial #
	INTEGER*4 CDIG                          !Check Digits.
	INTEGER*4 NUMTKT			!Number of Fractioned Tickets
C
	BYTE	  I1TEMP(4)			!Temp. Variables.
	INTEGER*4 I4TEMP
C
	EQUIVALENCE(I4TEMP,I1TEMP)
C
C
C GET JULIAN DATE
C
	I4TEMP = 0
	I1TEMP(2) = ZEXT(MESTAB(6))
	I1TEMP(1) = ZEXT(MESTAB(7))
	JUL = I4TEMP
C
C GET SERIAL #
C
	I4TEMP = 0
	I1TEMP(3) = ZEXT(MESTAB(8))
	I1TEMP(2) = ZEXT(MESTAB(9))
	I1TEMP(1) = ZEXT(MESTAB(10))
	SER = I4TEMP
C
C GET CHECK DIGITS
C
	CDIG = ZEXT(MESTAB(11))
C
C GET NUMBER OF TICKETS
C
	NUMTKT = ZEXT(MESTAB(12))
C
C
	RETURN
	END	
