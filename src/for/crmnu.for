C  GXSRC:CRMNU.FOR
C  
C V02 14-JUN-2005 FRP Modify for IPS Distribution.
C
C  $Log:   GXAFIP:[GOLS]CRMNU.FOV  $
C  
C     Rev 1.2   14 Feb 1997 15:47:52   RXK
C  Things revbyted (to get correct value for trabuf) 
C  
C     Rev 1.1   28 Jan 1997 19:33:22   HXK
C  Changes for LOTGEN (IPS=
C  
C     Rev 1.0   18 Dec 1996  9:50:24   HXK
C  Initial revision.
C
C SUBROUTINE TO BUILD SUPPLY MENU MESSAGE.
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE CRMNU(OUTTAB,TRABUF,BUF)
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
	I4TEMP=16+TRABUF(TIBCH)*4
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
	I4TEMP=6
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
C SET NUMBER OF ITEMS IN BATCH
C
	I4TEMP=TRABUF(TIBCH)
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C SET GAME (Last nibble: game type; First 3 nibbles: game number)
C
	DO 100 I=0,TRABUF(TIBCH)-1
C
	   I4TEMP=TRABUF(TSGAM+I)
	   OUTTAB(IND+0) = I1TEMP(1)
	   OUTTAB(IND+1) = I1TEMP(2)
	   IND=IND+2
C
C SET QUANTITY
C
	   I4TEMP=TRABUF(TSQTY+I)
	   OUTTAB(IND+0) = I1TEMP(1)
	   OUTTAB(IND+1) = I1TEMP(2)
	   IND=IND+2
C
100	CONTINUE
C
C
C
	RETURN
	END
