C
C SUBROUTINE HOURSALE
C $Log:   GXAFXT:[GOLS]HOURSALE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:32:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   25 Jun 1993 10:16:12   SXH
C  Code assumed that DAYTYP(WAG) contains GROSS sales, ie NET + CANCELS
C  It doesn't, it contains NET sales. So altered code to reflect this.
C  
C     Rev 1.0   21 Jan 1993 16:35:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - hoursale.for **
C
C HOURSALE.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C SUBROUTINE TO MEASURE HOULRY SALES     GROSS/NET
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
	SUBROUTINE HOURSALE(HOUR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
        ! argument
        INTEGER*4  HOUR           !

        ! variables
	INTEGER*4  NET            !
        INTEGER*4  CANCELS        !
	INTEGER*4  GROSS          !
	INTEGER*4  GNUM           !
C
	IF(HOUR.LT.1.OR.HOUR.GT.24) GOTO 9000    !IF INVALID INDEX

	DO GNUM = 1, MAXGAM
	    NET     = DAYTYP(DOLAMT,TWAG,GNUM)    ! NET SALES
	    CANCELS = DAYTYP(DOLAMT,TCAN,GNUM)    ! CANCELS
	    GROSS   = NET + CANCELS

	    HOURSAL(GNUM,1,HOUR) = GROSS       !GROSS SALES MEASUREMENT
	    HOURSAL(GNUM,2,HOUR) = NET         !NET   SALES MEASUREMENT
        END DO
C
C COMMON RETURN
C
9000	CONTINUE

	RETURN

	END
