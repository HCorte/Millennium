C
C SUBROUTINE UPVREC
C
C V06 12-OCT-2013 SCML New Validation Messages
C V05 12-MAR-2010 RXK TCLM part commented out
C V04 01-OCT-1999 UXN Cashing terminal number field updated for BANK winners.
C V03 13-AUG-1993 GXA Removed unused variables detected by Flint.
C V02 13-AUG-1993 GXA Released for Finland Dec Conversion / Oddset.
C V01 21-JAN-1993 DAB Initial Release
C                 Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                 DEC Baseline
C
C
C SUBROUTINE TO UPDATE VALIDATION RECORD
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE UPVREC(TRABUF,VALREC,EFLAG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	LOGICAL EFLAG
C
C
	TRABUF(TERR)=NOER
	TRABUF(TSTAT)=GOOD
C

	IF(TRABUF(TVTYPE).EQ.VPTB) THEN
	   TRABUF(TVCODE) = SETBNK
	   TRABUF(TVSTS)  = VSBNK
	   IF(EFLAG) TRABUF(TVSTS) = VSBNKM
	   VALREC(VBNKID) = TRABUF(TVBNKID)
	   VALREC(VBNKNUM)= TRABUF(TVBNKNUM)
	   VALREC(VCTER)  = TRABUF(TTER)
	   RETURN				!It's not really cashed.
	ENDIF
C
C
	IF(TRABUF(TTYP).EQ.TVAL.OR.TRABUF(TTYP).EQ.TREF) THEN
	  TRABUF(TVCODE)=RCASH
	  TRABUF(TVSTS )=VCASH
	  IF(EFLAG) THEN
	    TRABUF(TVCODE)=ECASH
	    TRABUF(TVSTS )=VCASHX
	  ENDIF
C----+------------------------------------------------------------------
C V06| Adding New Validation Messages
C----+------------------------------------------------------------------
        IF(TRABUF(TVTYPE).EQ.VNBNK) THEN
          TRABUF(TVSTS ) = VBANK                               
        ELSE                      
          TRABUF(TVSTS ) = VCASH                               
        ENDIF
C----+------------------------------------------------------------------
C V06| Adding New Validation Messages
C----+------------------------------------------------------------------
	  VALREC(VCCDC)=TRABUF(TCDC)
	  VALREC(VCSER)=TRABUF(TSER)
	  VALREC(VCTER)=TRABUF(TTER)
	ENDIF
C
C
C NOTE!! VALUNITS AND BETUNITS MUST BE THE SAME FOR CLAIM PROCESSING
C        CLAIM AMOUNT = AMOUNT WON IN VALUNITS + AMT REFUNDED IN BETUNITS
C
C
C	IF(TRABUF(TTYP).EQ.TCLM) THEN
C	  TRABUF(TVPAY)=TRABUF(TVPAY)+TRABUF(TVREF)
C	  TRABUF(TVREF)=0
C	  TRABUF(TVCODE)=RCLAM
C	  TRABUF(TVSTS )=VCLAM
C	  IF(EFLAG) THEN
C	    TRABUF(TVCODE)=ECLAM
C	    TRABUF(TVSTS )=VCLAMX
C	  ENDIF
C	  VALREC(VLCDC)=TRABUF(TCDC)
C	  VALREC(VLSER)=TRABUF(TSER)
C	  VALREC(VLTER)=TRABUF(TTER)
C	ENDIF
C
C

	VALREC(VSTAT)=TRABUF(TVSTS)

	RETURN
	END
