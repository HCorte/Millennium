C
C SUBROUTINE TERORDER
C $Log:   GXAFXT:[GOLS]TERORDER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:32:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.4   19 Aug 1993 18:07:54   HXK
C  reorganised message format, hhmmss
C  
C     Rev 1.3   05 Aug 1993 12:32:10   HXK
C  fixed bug with offsets into message table.
C  
C     Rev 1.2   28 Jun 1993 17:27:36   HXK
C  changed err message length from 5 to 6
C  
C     Rev 1.1   22 Jun 1993 11:11:58   HXK
C  CHANGED FOR FINLAND VAX CONVERSION RELEASE
C  
C     Rev 1.0   21 Jan 1993 17:50:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - spe_terorder.for **
C
C TERORDER.FOR
C
C V01 19-NOV-91 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C
C SUBROUTINE TO PROCESS TERMINAL ORDER TRANSACTIONS.
C
C CALLING SEQUENCE:
C      CALL TERORDER(TRABUF,MESTAB,OUTLEN)
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TERORDER(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'

	BYTE	    MESTAB(*)
	BYTE	    ERRTYP, ORDTYP

	INTEGER*4   MYCHKSUM, CHKLEN, IND, I
	INTEGER*2   OUTLEN,I2OPTIONS,I2ITEM_CNT
        INTEGER*4   HH, MM, SS  !hours, minutes, seconds
        INTEGER*4   I4TEMP
        BYTE        I1TEMP(4), I1OPTIONS(2), I1ITEM_CNT(2)

        EQUIVALENCE(I4TEMP,I1TEMP)
        EQUIVALENCE(I2OPTIONS,I1OPTIONS)
        EQUIVALENCE(I2ITEM_CNT,I1ITEM_CNT)

	DATA	    ERRTYP/Z90/
	DATA	    ORDTYP/ZD1/		    

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
C SAVE ORDER DATA IN 68 BYTES OF DATA AREA (SEE MESSAGE FORMATS).
C

        IND=7
        I4TEMP=0
        I1TEMP(2)=MESTAB(IND+0)
        I1TEMP(1)=MESTAB(IND+1)
        TRABUF(TSOLD)=I4TEMP
        IND=IND+2

        DO I=0,15
           IF(TSBIT(I4TEMP,I).NE.0) THEN
	      I1ITEM_CNT(2)=MESTAB(IND+0)
              I1ITEM_CNT(1)=MESTAB(IND+1)
              IND=IND+2
           ELSE
              I2ITEM_CNT=0
           ENDIF
	   TRABUF(TSDT1+I) = I2ITEM_CNT
        ENDDO


C
C BUILD OUTPUT MESSAGE BACK TO TERMINAL
C
	IND = 2
	MESTAB(IND) = ORDTYP
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
