C  GXSRC:SIMISON.FOR
C  
C  $Log:   GXAFXT:[GOLS]SIMISON.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:07:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   22 Sep 1994  8:08:24   MCM
C  START INDEX AT 2 (BYTE 1 IS RESERVED)
C  
C     Rev 1.2   08 Sep 1994 13:37:42   MCM
C  SWAPPING BYTES IS NO LONGER NECESSARY ON THE DEC LMS
C  
C     Rev 1.1   03 Jan 1994 22:46:50   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 18:30:52   SYSTEM
C  Initial revision.
C
C
C
C V02 28-MAY-92 TKO UPDATED FOR NEW MESSAGE FORMATS
C V01 13-NOV-91 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO FILL IN CROSS SYSTEM SUPPLY ORDER MESSAGE.
C
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SIMISON(OUTTAB,TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 IND
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C FILL IN LENGTH
C
	IND=2
	I4TEMP=66
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
C
C FILL IN ERROR CODE
C
	IND=14
	I4TEMP=99
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C FILL IN START TIME
C
	I4TEMP=01
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
C FILL IN END TIME
C
	I4TEMP=82800
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
C FILL IN FLUSH TIME
C
	I4TEMP=03
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
C FILL IN DELAY BETWEEN SCROLL
C
	I4TEMP=04
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C FILL IN DELAY BEFORE SCROLL
C
	I4TEMP=05
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C FILL IN RETAILER STATUS
C
	I4TEMP=01
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C FILL IN OPTION FLAG
C
	I4TEMP=0
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C CHECKSUM GAME TABLES
C
	I4TEMP=10
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
C 8 DIGIT ACTIVATION CODE
C
	I4TEMP=11
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
C AGENT NAME
C
	I4TEMP='SUE '
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
	I4TEMP='SUE '
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
	I4TEMP='SUE '
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
	I4TEMP='SUE '
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
	I4TEMP='SUE '
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
C HOTLINE NUMBER
C
	I4TEMP=3979827
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
C
C
	RETURN
	END
