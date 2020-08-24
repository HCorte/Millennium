C
C SUBROUTINE OUTDEL
C $Log:   GXAFXT:[GOLS]OUTDEL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:20:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   21 Jun 1993 10:16:20   SXH
C  Released for Finland
C  
C     Rev 1.0   21 Jan 1993 17:14:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - outdel.for **
C
C OUTDEL.FOR
C
C V02 12-FEB-92 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-FEB-89 MTK  INITIAL RELEASE FOR SWEDEN
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OUTDEL(TRABUF,OUTTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        ! arguments
	INTEGER*4  OUTTAB(*)       !

	INTEGER*2  OUTLEN          !

        ! variables
        INTEGER*4  MYCHKSUM        !
        INTEGER*4  CHKLEN          !
        INTEGER*4  TEMP            !
        INTEGER*4  CONTRL          !

	DATA CONTRL/Z20/
C
C
	TEMP = CONTRL + TRABUF(TTRN)
	IF (TRABUF(TERR).NE.0) TEMP=IAND(TEMP,'FFFFFFF7'X)
	CALL NMOVBYT(TEMP,4,OUTTAB,1,1)

	OUTLEN=4

	I4CCITT = TRABUF(TCHK)
	OUTTAB(3) = I1CCITT(2)
	OUTTAB(4) = I1CCITT(1)

	CHKLEN=OUTLEN-1

	CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
	I4CCITT = MYCHKSUM
	OUTTAB(3) = I1CCITT(2)
	OUTTAB(4) = I1CCITT(1)

	RETURN

	END
