C
C SUBROUTINE TOTALKEY
C  
C     Rev 1.0   17 Apr 1996 15:36:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   11 Dec 1994 18:29:38   HXK
C  Initial revision.
C  
C
C TOTALKEY.FOR
C
C
C SUBROUTINE TO PROCESS TERMINAL ORDER TRANSACTIONS.
C
C CALLING SEQUENCE:
C      CALL TOTALKEY(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TOTALKEY(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'

	BYTE	    MESTAB(*)
	BYTE	    ERRTYP, TOTKEYTYP

	INTEGER*4   MYCHKSUM, CHKLEN, IND
	INTEGER*2   OUTLEN,I2OPTIONS,I2ITEM_CNT
        INTEGER*4   HH, MM, SS  !hours, minutes, seconds
        INTEGER*4   I4TEMP
        BYTE        I1TEMP(4), I1OPTIONS(2), I1ITEM_CNT(2)

        EQUIVALENCE(I4TEMP,I1TEMP)
        EQUIVALENCE(I2OPTIONS,I1OPTIONS)
        EQUIVALENCE(I2ITEM_CNT,I1ITEM_CNT)

	DATA	    ERRTYP/Z90/
	DATA	    TOTKEYTYP/ZDE/

C
C CHECK FOR INVALID TRANS
C
	IF(TRABUF(TERR).NE.NOER) THEN
	  TRABUF(TSTAT)=REJT
	  GOTO 1000
	ENDIF
C
C DECODE TRANSACTION
C
C SAVE TOTAL DATA IN 4 BYTES OF DATA AREA
C

        IND=7
        I4TEMP=0
        I1TEMP(4)=MESTAB(IND+0)
        I1TEMP(3)=MESTAB(IND+1)
        I1TEMP(2)=MESTAB(IND+2)
        I1TEMP(1)=MESTAB(IND+3)
        TRABUF(TSOLD)=I4TEMP
        IND=IND+4

C
C BUILD OUTPUT MESSAGE BACK TO TERMINAL
C
	IND = 2
	MESTAB(IND) = TOTKEYTYP
	IND = IND + 3			!SAVE SPACE FOR THE CHECKSUM
        I4TEMP = TRABUF(TTIM)
        HH=I4TEMP/3600
        MM=(I4TEMP-HH*3600)/60
        SS=I4TEMP-(HH*3600+MM*60)
        MESTAB(IND+0) = HH
        MESTAB(IND+1) = MM
        MESTAB(IND+2) = SS
        IND=IND+3
	OUTLEN = IND
	GOTO 9000

C
C RETURN ERROR
C
1000	CONTINUE
	MESTAB(2) = ERRTYP
	CALL MOVBYT(TRABUF(TERR),4,MESTAB,5,1)
        MESTAB(6) = 0
	OUTLEN=6
C
C CALCULATE CHECKSUM AND RETURN
C
9000	CONTINUE
	I4CCITT = TRABUF(TCHK)
	MESTAB(3) = I1CCITT(2)
	MESTAB(4) = I1CCITT(1)
	CHKLEN = OUTLEN - 1
	CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
	I4CCITT = MYCHKSUM
	MESTAB(3) = I1CCITT(2)
	MESTAB(4) = I1CCITT(1)
	RETURN
	END
