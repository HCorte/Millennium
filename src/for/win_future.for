C
C SUBROUTINE WIN_FUTURE
C
C V10 28-JUN-2009 FRP If it's not a Kicker draw day, for Loto single-draw
C                     tickets having Kicker as add-on and being purchased
C                     during the current draw (scanned in draw files),
C                     make sure they are not placed into the carryover file.
C                     If it's a Kicker draw day, for Loto multidraw tickets 
C                     having Kicker as add-on and expiring the current draw
C                     (scanned in carryover file), make sure they are purged
C                     from the carryover file.
C             FGR/FRP If it's a Kicker draw day, for Loto multidraw tickets
C                     having Kicker as add-on and being purchased during
C                     the current draw (scanned in draw files), make sure
C                     they are not placed again into the carryover file (they
C                     were already placed the day before during the Loto draw).
C V09 20-MAR-2001 ANG/JHR Test if draw number if GT 0 before save the ticket
C V08 12-DEC-2000 UXN Fix for tickets played with Joker
C V07 03-DEC-2000 UXN TOTOGOLO ADDED.
C V06 28-FEB-2000 RXK Promotion ("add 1 free week") added.
C V05 08-FEB-2000 UXN TFAMTFLG added.
C V04 27-APR-1999 RXK STOPSYS optimization (CARYSCAN is now an array).
C V03 06-OCT-1993 HXK Prevent postponed sports wagers with kickers being posted
C                     unnecessarily.
C V02 23-SEP-1993 HXK Save all wagers that are not Lotto, Sports or Kicker.
C V01 20-SEP-1993 GXA Re-Released for Finland Dec Conversion / Oddset.
C                     Viking Lotto was removed and a separate WIN_VIK_FUTURE
C                     was made to keep it separated from the other games, since 
C                     it was such a pain to make it work nicely in the Future.
C  
C
C SUBROUTINE TO DETERMINE IF A TRANSACTION EXPIRES THIS DRAW.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WIN_FUTURE(TRABUF,SAVE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
C
	INTEGER*4 KIND, ST, GIND, KGAM, GAME, SAVE, GTYP
	INTEGER*4 AMT
C
	SAVE=0
	GAME=TRABUF(TGAM)
	KGAM=TRABUF(TWKGME)
	GTYP=TRABUF(TGAMTYP)
	GIND=TRABUF(TGAMIND)
	IF(MAILSCAN) RETURN
	IF(TRABUF(TTYP).NE.TWAG) RETURN
C
        IF(TRABUF(TGAMTYP).NE.TLTO.AND.
     *     TRABUF(TGAMTYP).NE.TSPT.AND.
     *     TRABUF(TGAMTYP).NE.TTGL.AND.
     *     TRABUF(TGAMTYP).NE.TKIK) THEN
          SAVE=1
          RETURN
        ENDIF

	IF(TRABUF(TWKFLG).EQ.0.AND.TRABUF(TWKFLG2).EQ.0) KGAM=0
	IF(KGAM.GT.0) KIND = GNTTAB(GAMIDX,KGAM)
	ST=TRABUF(TSTAT)
C
C
	IF(TRABUF(TGAMTYP).EQ.TLTO) THEN
	  IF( (LLTDRW(GIND).LT.1.OR.LTDELAY(GIND).EQ.2) .AND. CARYSCAN(TLTO) ) THEN
	    SAVE = 1
	    GOTO 1000
	  ENDIF
	  IF(ST.NE.GOOD.AND.ST.NE.VOID.AND.
     *	     ST.NE.INCA.AND.ST.NE.EXCH.AND.
     *       ST.NE.FRAC) RETURN
          IF( (TRABUF(TWEND)+TRABUF(TWADDFW).GT.LLTDRW(GIND)) .AND. LLTDRW(GIND).GT.0 ) THEN
	    SAVE=1
	    IF((ST.EQ.GOOD.OR.ST.EQ.EXCH) .AND.
     *	      LTDELAY(GIND).NE.2 .AND.
     *	      TRABUF(TFIL).NE.CPOST .AND.
     *	      TRABUF(TWBEG).LE.LLTDRW(GIND)+1 .AND.
     *	      TRABUF(TWEND).GT.LLTDRW(GIND) ) THEN     !exlude promotion week
	      AMT = TRABUF(TWAMT)
	      LADVSAL(GIND)=LADVSAL(GIND)+AMT
	    ENDIF
	    IF(LTDELAY(GIND).EQ.2) TRABUF(TFIL)=CPOST
	    IF(LTDELAY(GIND).EQ.1) TRABUF(TFIL)=CARRY
C	    IF(LTDELAY(GIND).EQ.1.AND..NOT.CARYSCAN(TLTO)) SAVE=0
	  ENDIF
	ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TSPT) THEN
	  IF( (LSPDRW(GIND).LT.1.OR.SPDELAY(GIND).EQ.2) .AND. CARYSCAN(TLTO) ) THEN
	    SAVE=1
	    GOTO 1000
	  ENDIF
	  IF(ST.NE.GOOD.AND.ST.NE.VOID.AND.
     *	     ST.NE.INCA.AND.ST.NE.EXCH.AND.
     *       ST.NE.FRAC) RETURN
	  IF( (TRABUF(TWEND).GT.LSPDRW(GIND)) .AND. LSPDRW(GIND).GT.0 ) THEN
	    SAVE=1
	    IF((ST.EQ.GOOD.OR.ST.EQ.EXCH).AND.
     *	      SPDELAY(GIND).NE.2.AND.
     *	      TRABUF(TFIL).NE.CPOST.AND.
     *	      TRABUF(TWBEG).LE.LSPDRW(GIND)+1) THEN
	      AMT = TRABUF(TWAMT)
	      SADVSAL(GIND)=SADVSAL(GIND)+AMT
	    ENDIF
	    IF(SPDELAY(GIND).EQ.2) TRABUF(TFIL)=CPOST
	    IF(SPDELAY(GIND).EQ.1) TRABUF(TFIL)=CARRY
C	    IF(SPDELAY(GIND).EQ.1.AND..NOT.CARYSCAN(TLTO)) SAVE=0
	  ENDIF
	ENDIF

	IF(TRABUF(TGAMTYP).EQ.TTGL) THEN
	  IF( (LTGDRW(GIND).LT.1.OR.TGDELAY(GIND).EQ.2) .AND. CARYSCAN(TLTO) ) THEN
	    SAVE=1
	    GOTO 1000
	  ENDIF
	  IF(ST.NE.GOOD.AND.ST.NE.VOID.AND.
     *	     ST.NE.INCA.AND.ST.NE.EXCH.AND.
     *       ST.NE.FRAC) RETURN
	  IF( (TRABUF(TWEND).GT.LTGDRW(GIND)) .AND. LTGDRW(GIND).GT.0 ) THEN
	    SAVE=1
	    IF((ST.EQ.GOOD.OR.ST.EQ.EXCH).AND.
     *	      TGDELAY(GIND).NE.2.AND.
     *	      TRABUF(TFIL).NE.CPOST.AND.
     *	      TRABUF(TWBEG).LE.LTGDRW(GIND)+1) THEN
	      AMT = TRABUF(TWAMT)
	      TGADVSAL(GIND)=TGADVSAL(GIND)+AMT
	    ENDIF
	    IF(TGDELAY(GIND).EQ.2) TRABUF(TFIL)=CPOST
	    IF(TGDELAY(GIND).EQ.1) TRABUF(TFIL)=CARRY
C	    IF(TGDELAY(GIND).EQ.1.AND..NOT.CARYSCAN(TLTO)) SAVE=0
	  ENDIF
	ENDIF
C
C
1000	CONTINUE
	IF(KGAM.NE.0) THEN
	  KIND=GNTTAB(GAMIDX,KGAM)
	  IF(LKKDRW(KIND).LT.1) THEN            !! SMELLS OF KLUDGE !!
	    IF(TRABUF(TGAMTYP).NE.TSPT.AND.
     *         TRABUF(TGAMTYP).NE.TTGL) SAVE=1  !this kludge is to stop post-
C
  	    IF(TRABUF(TGAMTYP).EQ.TLTO .AND.
     *         LLTDRW(GIND).GT.0 .AND.
     *         CARYSCAN(TLTO).EQ. .FALSE. .AND.
     *         TRABUF(TWBEG).EQ.LLTDRW(GIND) .AND.
     *         TRABUF(TWEND).EQ.LLTDRW(GIND)) SAVE=0
C
	    RETURN                              !-poned sports that are single
	  ENDIF                                 !draws from being posted to the
	  IF(ST.NE.GOOD.AND.ST.NE.VOID.AND.     !TCF when played with kicker.
     *	     ST.NE.INCA.AND.ST.NE.EXCH.AND.     !(assume only sports are
     *       ST.NE.FRAC) RETURN                 !postponed)
          IF(TRABUF(TWKEND)+TRABUF(TWADDFW).GT.LKKDRW(KIND)) THEN
C
  	    IF(TRABUF(TGAMTYP).EQ.TLTO .AND.
     *         CARYSCAN(TLTO).EQ. .FALSE. .AND.
     *         TRABUF(TWKBEG).EQ.LKKDRW(GIND)) THEN
              SAVE=0
              RETURN
            ENDIF
C
	    SAVE=1
	    IF((ST.EQ.GOOD.OR.ST.EQ.EXCH) .AND.
     *	      TRABUF(TWKBEG).LE.LKKDRW(KIND)+1 .AND.
     *        TRABUF(TWKEND).GT.LKKDRW(KIND) ) THEN      !exlude promotion week
              AMT = TRABUF(TWKAMT)
     	      KADVSAL(GAME,KIND) = KADVSAL(GAME,KIND)+AMT
	    ENDIF
	    RETURN
	  ELSE
            SAVE=0
	  ENDIF
C
	ENDIF
	RETURN
	END
