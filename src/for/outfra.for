C
C OUTFRA.FOR
C
C V14 06-JUL-1999 UXN Super Triple added. Wager output message is now
C                     built in WAGER_BODY(), that is also called in REPRINT.
C V13 24-MAY-1999 RXK Bet details of oddset games added.
C V12 18-MAR-1999 RXK Game type/game index change. Hack for V5 removed.
C V11 01-FEB-1999 UXN Retry and error message added.
C V10 04-AUG-1998 RXK the case of New Kicker game only added 
C V09 18-JUL-1995 HXK Changed for RAVI batch (HXK,PXB)
C V08 24-APR-1995 HXK Merge of V5 development with March 10th 1995 bible 
C V07 22-FEB-1995 HXK TWAS BRILLIG AND THE BOROGOVES DID
C V06 17-AUG-1993 HXK sports twsysn is stored as 1 for all full systems 
C                     (for pools) terminal sees twsimp (row equivalent) 
C                     as sys number
C V05 12-AUG-1993 CXK fixed typo in (TGAMTYP for GAMTYP)
C V04 12-AUG-1993 CXK FIXED FRACTION OF SPORTS REDUCED SYSTEM 
C V03 09-AUG-1993 SXH Fixed MOVBYT buf with RAVI V65
C V02 04-AUG-1993 GXA Corrected starting point of output message.
C V01 03-AUG-1993 GXA Initial revision.
C
C Subroutine to build Fractioning Output message.
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE OUTFRA(TRABUF,FRABUF,OUTTAB,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
C
	BYTE	  OUTTAB(*)			!Message Output Table.
C
	INTEGER*2 OUTLEN			!Message Output length.
C
	INTEGER*4 FRABUF(TRALEN)                !Fraction Wager Trans Record.
C
	INTEGER*4 FRATYP/ZF1/			!Fraction Wager Type/Subtype.
	INTEGER*4 ERRTYP/Z90/			!Error Type/Subtype.
	INTEGER*4 CONTRL/Z20/			!Control/Sequence.
	INTEGER*4 IND				!Index Into Outtab.
	INTEGER*4 NUMTKT			!# Tickets to Output.
	INTEGER*4 CDIG				!Check digits for Wager.
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
C
C SET CONTROL/SEQUENCE , TYPE/SUBTYPE
C
	IND = 1
	OUTTAB(IND+0) = CONTRL + TRABUF(TTRN)
	IND = IND + 1
C
C IF TRANSACTION STATUS IS GOOD OR RETRY BUILD FRACTION MESSAGE 
C ELSE BUILD ERROR MESSAGE
C
	IF(TRABUF(TSTAT).EQ.GOOD.OR.
     *     TRABUF(TSTAT).EQ.REJT.AND.TRABUF(TERR).EQ.RETY) GOTO 10
C
	OUTTAB(IND+0) = ERRTYP
        IND = IND + 1
        IND = IND + 2                        !Skip Checksum field.
        OUTTAB(IND+0) = TRABUF(TERR)
        OUTTAB(IND+1) = TRABUF(TSUBERR)
        IND = IND + 2
	GOTO 1000 
C
10	CONTINUE
C
	OUTTAB(IND) = FRATYP
	IND = IND + 1
C
	IND = IND + 2				!Skip Checksum field.
C
C GAME TYPE / GAME INDEX
C
        OUTTAB(IND) = FRABUF(TGAMTYP)
        OUTTAB(IND+1) = FRABUF(TGAMIND)
	IND = IND + 2
C
C NUMBER OF TICKETS
C
	NUMTKT = TRABUF(TSDT4)
	OUTTAB(IND) = NUMTKT
	IND = IND + 1
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
C
C BUILD WAGER BODY MESSAGE
C
	CALL WAGER_BODY(FRABUF,OUTTAB,IND)
C
1000    CONTINUE
	OUTLEN = IND - 1
        I4CCITT   = TRABUF(TCHK)
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
        CHKLEN = OUTLEN - 1
        CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT   = MYCHKSUM
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
	RETURN
	END
