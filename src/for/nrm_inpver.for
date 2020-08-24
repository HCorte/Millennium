C
C FUNCTION INPVER
C $Log:   GXAFXT:[GOLS]INPVER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:38:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:40:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_scram.for **
C
C
C
C *** INPVER
C
C This function will convert an external serial number into an internal
C serial number and check the checksum as it does so.  If a checksum error
C occurs, this function returns a non-zero value.  Note, however, that the
C internal serial number will be changed in any case.
C
C CALLING SEQUENCE:
C        RETURN_CODE = INPVER( CDC, EXTSER, INTSER, CHKDIG)
C INPUT:
C	 CDC	    CDC date of ticket
C	 EXTSER	    External serial number
C	 CHKDIG	    Check digits
C
C OUTPUT:
C	 INTSER	    Internal serial number
C	 INPVER	    Status (0 = ok, 15 = error in checksum)
C	
C SUBROUTINES CALLED:
C        GENKEY, GENCHK
C
C
	INTEGER*4 FUNCTION INPVER(XCDC, XEXTSER, XINTSER, XCHKDIG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4      XCDC       ! INPUT: CDC OF TICKET
	INTEGER*4      XEXTSER    ! INPUT: EXTERNAL SRL #
	INTEGER*4      XINTSER    !OUTPUT: INTERNAL SRL #
	INTEGER*4      XCHKDIG    ! INPUT: CHECK DIGITS OF EXT SRL #
C
	INTEGER*4      CDC,EXTSER,CHKDIG !INTERNAL COPIES
C
	INTEGER*4      CDCH      !HIGH PORTION OF CDC
	INTEGER*4      CDCL      ! LOW PORTION OF CDC
C
	INTEGER*4      HTIC      !HIGH PORTION OF SERIAL #
	INTEGER*4      LTIC      ! LOW PORTION OF SERIAL #
C
	INTEGER*4      SYSIDX    ! VARIANT FOR THE SITE
C
	INTEGER*4      TIC2,TIC3,TIC4,X
C
C
C
	CDC     = XCDC
	EXTSER  = XEXTSER
	CHKDIG  = XCHKDIG
C
C
C
	CALL MIXCHK( EXTSER, CHKDIG )    !UNMIX CHKDIGIT
	CALL GENKEY( CDC, CDCH, CDCL, SYSIDX)   !GET CDCH, CDCL
C
C Assume at this point that the external srl # is as follows:
C
C 0000 0000 abcd efgh ijkl mnop qrst uvwx
C
	LTIC = IAND ( EXTSER, '0000FFFF'X)  !LTIC=ijkl mnop qrst uvwx
	HTIC = ISHFT( EXTSER, -16)
	HTIC = IAND ( HTIC, '0000007F'X)    !HTIC=0bcd efgh
	TIC4 = IAND ( LTIC, '0000007F'X)    !TIC4=0rst uvwx
	TIC3 = ISHFT( LTIC, -8)
	TIC3 = IAND ( TIC3, '0000007F'X)    !TIC3=0jkl mnop
	TIC2 = IAND ( HTIC, '0000007F'X)    !TIC2=0bcd efgh
C
	TIC4 = ISHFT( TIC4, 6)
	TIC2 = IEOR ( TIC2, TIC4)
	TIC4 = ISHFT( TIC4, -1)
	TIC2 = IEOR ( TIC2, TIC4)
	TIC4 = ISHFT( TIC4, -1)
	TIC2 = IEOR ( TIC2, TIC4)
	TIC4 = ISHFT( TIC4, -4)
	TIC2 = IEOR ( TIC2, TIC4)
	TIC4 = IEOR ( TIC4, TIC3)
	TIC3 = IEOR ( TIC3, TIC2)
	TIC2 = IAND ( TIC2, '0000007F'X)
	TIC3 = IAND ( TIC3, '0000007F'X)
	TIC4 = IAND ( TIC4, '0000007F'X)
C
	HTIC = IAND ( HTIC, '0000FF80'X)    !HTIC=0000 0000
	LTIC = IAND ( LTIC, '00008080'X)    !LTIC=i000 0000 q000 0000
	LTIC = LTIC + TIC4
	TIC3 = ISHFT( TIC3, 8)
	LTIC = LTIC + TIC3
	HTIC = HTIC + TIC2
C
C Now subtract CDCL and CDCH from LTIC, HTIC with carry
C
	LTIC = LTIC - CDCL
	IF(LTIC.LT.0) HTIC = HTIC - 1
	HTIC = HTIC - CDCH
	IF(HTIC.LT.0) HTIC = HTIC + 128
C
	LTIC = IAND ( LTIC, '0000FFFF'X)
C
C Get the 24th bit from the original serial # and put into result
C
	X    = ISHFT( XEXTSER, -16)
	X    = IAND ( X, '00000080'X)
	HTIC = IOR  ( HTIC, X)
C
C Put in high 3 bits of check digit
C
	X   = CHKDIG - SYSIDX              !(SUBTRACT OUT SITE CODE)
	X    = IAND ( X, '00000007'X)       !3 BITS ONLY
	X    = ISHFT( X, 8)
	HTIC = IOR  ( HTIC, X)
C
C Set internal serial #
C
	XINTSER = ISHFT(HTIC, 16) + LTIC
C
C
C Compare check digits
C
	CALL GENCHK(XCDC, XINTSER, XEXTSER, CHKDIG, SYSIDX)
	IF(CHKDIG.EQ.XCHKDIG)THEN
	  INPVER = 0
	ELSE
	  INPVER = 15		    !FOR COMPATABILITY
	ENDIF
C
	RETURN
	END
