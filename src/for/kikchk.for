C
C SUBROUTINE KIKCHK
C $Log:   GXAFXT:[GOLS]KIKCHK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:42:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   10 Jun 1993 15:52:04   SXH
C  Replace TWKFLG1 by TWKFLG
C  
C     Rev 1.1   10 May 1993 12:02:04   SXH
C  Released for Finland VAX
C  
C     Rev 1.0   21 Jan 1993 16:44:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - kikchk.for **
C
C KIKCHK.FOR
C
C V04 13-APR-92 GCAN REMOVED TWBEG AND TWEND, IT IS SET IN DKICK.
C V03 10-FEB-92 GCAN CHECK FOR KICKER ADVANCE DRAW
C V02 01-NOV-91 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
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
	SUBROUTINE KIKCHK(TRABUF,ADVANCE,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        ! arguments
        INTEGER*4  ST           ! status return

	LOGICAL ADVANCE         ! not used

        ! variables
	INTEGER*4  GIND         ! game index
	INTEGER*4  GAM          ! game number
	INTEGER*4  KGAM         ! joker game number

        LOGICAL    KICKER       ! joker participation
C
	ST     = -1
        KICKER = .FALSE.

	GAM  = TRABUF(TGAM)
	KGAM = KGNTAB(GAM)
	IF(KGAM.LT.1 .OR. KGAM.GT.MAXGAM) THEN
	    TRABUF(TERR)=SYNT
	    SYNTERRCOD=700
	    RETURN
	ENDIF
C
C
	GIND = GNTTAB(GAMIDX,KGAM)
	IF(GIND.LT.1.OR.GIND.GT.NUMKIK) THEN
	    TRABUF(TERR)=SYNT
	    SYNTERRCOD=710
	    RETURN
	ENDIF
C
C
        IF(TRABUF(TWKFLG).NE.0 .OR. TRABUF(TWKFLG2).NE.0) THEN                
            KICKER = .TRUE.                                                    
        END IF

	TRABUF(TWKGME) = KGAM
	TRABUF(TWKDUR) = TRABUF(TWDUR)
	TRABUF(TWKBEG) = KIKDRW(GIND)
	IF(ADVANCE) THEN
	    TRABUF(TWKBEG) = TRABUF(TWKBEG) + 1
	ENDIF
	TRABUF(TWKEND) = TRABUF(TWKBEG)+TRABUF(TWKDUR)-1

        TRABUF(TWKAMT) = 0
        IF (KICKER) THEN
            IF (TRABUF(TWKFLG) .NE. 0) THEN
                TRABUF(TWKAMT) = IDNINT ( DFLOAT(KIKPRC(GIND)) / P(PRFACTOR) )
            END IF
            IF (TRABUF(TWKFLG2) .NE. 0) THEN
                TRABUF(TWKAMT) = TRABUF(TWKAMT) + IDNINT ( DFLOAT(KIKPRC(GIND)) / P(PRFACTOR) )
            END IF

            IF(KIKDFF(GIND).EQ.1.AND.TRABUF(TWKFLG) .NE.0.AND.   
     *                               TRABUF(TWKFLG2).NE.0) THEN
                TRABUF(TWKAMT) = TRABUF(TWKAMT) - IDNINT ( DFLOAT(KIKPRC(GIND)) / P(PRFACTOR) )     
            END IF
        END IF

	IF(KIKSTS(GIND).GE.GAMBFD.AND..NOT.ADVANCE) THEN
	    TRABUF(TERR) = SDRW
	    RETURN
	ENDIF
C
C
	ST=0

	RETURN

	END
