C
C OUTUNF.FOR
C
C  V03 29-09-1999 UXN Retry added.
C  V02 04-08-1993 GXA Corrected starting point of output message.
C  V01 03-08-1993 GXA Initial revision.
C
C Subroutine to build UnFractioning Output message.
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
        SUBROUTINE OUTUNF(TRABUF,OUTTAB,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
C
	BYTE	  OUTTAB(*)			!Message Output Table.
C
	INTEGER*2 OUTLEN			!Message Output length.
C
	INTEGER*4 UNFTYP/ZFF/			!UnFraction Wager Type/Subtype.
	INTEGER*4 ERRTYP/Z90/			!Error Type/Subtype.
	INTEGER*4 CONTRL/Z20/			!Control/Sequence.
	INTEGER*4 IND				!Index Into Outtab.
	INTEGER*4 NUMTKT			!# Tickets to Output.
	INTEGER*4 CDIG				!Check digits for Wager.
	INTEGER*4 TIME				!Transaction Time
	INTEGER*4 HOURS				!
	INTEGER*4 MINS				!
	INTEGER*4 SECS				!
	INTEGER*4 CHKLEN			!Length to Checksum.
	INTEGER*4 MYCHKSUM			!Checksum of message.
	INTEGER*4 I				!Loop Variable
C
	BYTE	  I1TEMP(4)			!Temp Variable.
	INTEGER*2 I2TEMP(2)			!Temp Variable.
	INTEGER*4 I4TEMP			!Temp Variable.
C
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
C SET / CLEAR VARIABLES
C
	IND = 1
	NUMTKT = TRABUF(TSDT4)
C
C SET CONTROL/SEQUENCE 
C
	OUTTAB(IND+0) = CONTRL + TRABUF(TTRN)
	IND = IND + 1
C
C IF TRANSACTION STATUS IS GOOD OR RETRY BUILD UNFRACTION MESSAGE 
C ELSE BUILD ERROR MESSAGE.
C
	IF(TRABUF(TSTAT).EQ.GOOD.OR.
     *     TRABUF(TSTAT).EQ.REJT.AND.TRABUF(TERR).EQ.RETY) THEN
	   OUTTAB(IND+0) = UNFTYP
	   IND = IND + 1
	   IND = IND + 2			!Skip Checksum field.
	   OUTTAB(IND+0) = NUMTKT
	   IND = IND + 1
C
	   TIME  = TRABUF(TTIM)
	   HOURS = TIME / 3600
	   MINS  = (TIME-HOURS*3600) / 60
	   SECS  = TIME - HOURS*3600 - MINS*60
C
	   OUTTAB(IND+0) = HOURS
	   OUTTAB(IND+1) = MINS
	   OUTTAB(IND+2) = SECS
	   IND = IND + 3
C
C LOOP FOR # TICKETS AND STORE EACH SERIAL # AND CHECK DIGITS.
C
	   DO I = 0,NUMTKT - 1
	      CALL OUTGEN(TRABUF(TCDC),TRABUF(TSDT5+I),I4TEMP,CDIG)
	      OUTTAB(IND+0) = I1TEMP(3)
	      OUTTAB(IND+1) = I1TEMP(2)
	      OUTTAB(IND+2) = I1TEMP(1)
	      OUTTAB(IND+3) = CDIG
	      IND = IND + 4
	   END DO
	ELSE
	   OUTTAB(IND+0) = ERRTYP
	   IND = IND + 1
	   IND = IND + 2			!Skip Checksum field.
	   OUTTAB(IND+0) = TRABUF(TERR)
	   OUTTAB(IND+1) = TRABUF(TSUBERR)
	   IND = IND + 2
	ENDIF
C
	OUTLEN = IND - 1
C
C CALCULATE CHECKSUM AND RETURN
C
	I4CCITT = TRABUF(TCHK)
	OUTTAB(3) = I1CCITT(2)
	OUTTAB(4) = I1CCITT(1)
	CHKLEN = OUTLEN - 1
	CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
	I4CCITT = MYCHKSUM
	OUTTAB(3) = I1CCITT(2)
	OUTTAB(4) = I1CCITT(1)
C
	RETURN
	END	   
