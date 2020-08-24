C  GXSRC:SIMIVAL.FOR
C  
C V03 08-JUL-2005 FRP Modify for IPS Distribution.
C
C  $Log:   GXAFXT:[GOLS]SIMIVAL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:07:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   22 Sep 1994  8:09:06   MCM
C  START INDEX AT 2 (BYTE 1 IS RESERVED)
C  
C     Rev 1.2   08 Sep 1994 13:38:56   MCM
C  SWAPPING BYTES IS NO LONGER NECESSARY ON THE DEC LMS
C  
C     Rev 1.1   03 Jan 1994 22:47:06   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 18:30:58   SYSTEM
C  Initial revision.
C
C
C
C V02 28-MAY-92 TKO UPDATED FOR NEW MESSAGE FORMATS
C V01 13-NOV-91 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO FILL IN CROSS SYSTEM VALIDATION MESSAGE.
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
	SUBROUTINE SIMIVAL(OUTTAB,TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 IND, I
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
	CHARACTER*20 DESCR/'SUCCESSFUL TRANSACT '/
C
C FILL IN LENGTH
C
	IND=2
	IF(TRABUF(TIVALM).EQ.IVTP_NCP) THEN
	  I4TEMP=14+TRABUF(TIBCH)*24
	ELSE
	  I4TEMP=14+TRABUF(TIBCH)*8
	ENDIF
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
C
C FILL IN VALIDATION TYPE
C
	IND=14
	I4TEMP=TRABUF(TIVALM)
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C NON-CASH PRIZE
C
	IF(TRABUF(TIVALM).EQ.IVTP_NCP) THEN
C
C FILL IN TICKET RESULT CODE
C
	  I4TEMP=99
	  OUTTAB(IND+0) = I1TEMP(1)
	  OUTTAB(IND+1) = I1TEMP(2)
	  IND=IND+2
C
C FILL IN PACK STATUS FOR PRIV TERMINAL
C
	  I4TEMP=0
	  OUTTAB(IND+0) = I1TEMP(1)
	  OUTTAB(IND+1) = I1TEMP(2)
	  IND=IND+2
C
C FILL IN PRIZE DESCRIPTION
C
	  CALL MOVBYT(DESCR,1,OUTTAB,IND,20)
	  IND=IND+20
	ELSE
C
C CASH PRIZE
C
	  DO 100 I=1,TRABUF(TIBCH)
C
C FILL IN TICKET RESULT CODE
C
	    I4TEMP=99
	    OUTTAB(IND+0) = I1TEMP(1)
	    OUTTAB(IND+1) = I1TEMP(2)
	    IND=IND+2
C
C FILL IN PRIZE AMOUNT
C
	    I4TEMP=49+I
	    OUTTAB(IND+0) = I1TEMP(1)
	    OUTTAB(IND+1) = I1TEMP(2)
	    OUTTAB(IND+2) = I1TEMP(3)
	    OUTTAB(IND+3) = I1TEMP(4)
	    IND=IND+4
C
C FILL IN PACK STATUS FOR PRIV TERMINAL
C
	    I4TEMP=0
	    OUTTAB(IND+0) = I1TEMP(1)
	    OUTTAB(IND+1) = I1TEMP(2)
	    IND=IND+2
C
100	  CONTINUE
C
	ENDIF
C
C
C
	RETURN
	END
