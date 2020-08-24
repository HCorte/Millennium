C
C SUBROUTINE SOFF
C $Log:   GXAFXT:[GOLS]SOFF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:10:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:39:46   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - spe_soff.for **
C
C SOFF.FOR
C
C V02 05-NOV-91 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS TERMINAL SIGN-OFF.
C
C CALLING SEQUENCE:
C     CALL SOFF(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SOFF(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
	INTEGER*2 OUTLEN
	INTEGER*4  TER, CHKLEN, MYCHKSUM
	BYTE MESTAB(*)
C
C SIGN-OFF TERMINAL
C
	TER=TRABUF(TTER)
	TRABUF(TSOLD)=AGTHTB(AOPSTS,TER)
	TRABUF(TSNEW)=SIGNOF
	AGTHTB(AOPSTS,TER)=SIGNOF
C
C CALCULATE CHECKSUM AND RETURN
C
	OUTLEN=5
        I4CCITT = TRABUF(TCHK)
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
        RETURN
	END
