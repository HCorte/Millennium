C  GXSRC:CRSON.FOR
C  
C  $Log:   GXAFXT:[GOLS]CRSON.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:44:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   22 Sep 1994  7:50:46   MCM
C  START INDEX AT 2 (BYTE 1 IS RESERVED)
C  
C     Rev 1.2   08 Sep 1994 11:40:22   MCM
C  SWAPPING BYTES IS NO LONGER NECESSARY ON THE DEC LMS
C  
C     Rev 1.1   03 Jan 1994 20:03:50   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:34:06   SYSTEM
C  Initial revision.
C
C
C
C V02 11-FEB-92 JPJ ADDED (GVT)
C V01 13-NOV-91 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO BUILD CROSS SYSTEM SIGN-ON MESSAGE.
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
	SUBROUTINE CRSON(OUTTAB,TRABUF,BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 IND, BUF, CROSS
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP, I
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C SET LENGTH
C
	IND=2
	I4TEMP=14
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C SET GTECH BUFFER NUMBER
C
	I4TEMP=BUF
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C SET GTECH CROSS REFERENCE NUMBER
C
	CALL GETXRF(CROSS)
	TRABUF(TIXRF)=CROSS
	I4TEMP=CROSS
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
C SET TRANSACTION TYPE CODE
C
	I4TEMP=9
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C SET RETAILER NUMBER
C
	I4TEMP=TRABUF(TAGT)
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
