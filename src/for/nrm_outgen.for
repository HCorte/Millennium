C
C SUBROUTINE OUTGEN
C $Log:   GXAFXT:[GOLS]OUTGEN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:20:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:14:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_scram.for **
C
C
C
C
C
C *** OUTGEN
C This subroutine will convert from an internal serial number to an external
C serial # and checkdigits
C
C CALLING SEQUENCE:
C        CALL OUTGEN ( CDC, INTSER, EXTSER, CHKDIG)
C
C INPUT:
C	 CDC	    Date of ticket
C	 INTSER	    Internal serial #
C
C OUTPUT:
C	 EXTSER	    External serial #
C	 CHKDIG	    Check digits
C
C SUBROUTINES CALLED:
C        GENKEY,GENCHK
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OUTGEN(XCDC, XINTSER, XEXTSER, XCHKDIG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4      XCDC       ! INPUT: CDC OF TICKET
	INTEGER*4      XINTSER    !OUTPUT: INTERNAL SRL #
	INTEGER*4      XEXTSER    ! INPUT: EXTERNAL SRL #
	INTEGER*4      XCHKDIG    ! INPUT: CHECK DIGITS OF EXT SRL #
C
	INTEGER*4      CDC,INTSER,CHKDIG !INTERNAL COPIES
C
	INTEGER*4      CDCH      !HIGH PORTION OF CDC
	INTEGER*4      CDCL      ! LOW PORTION OF CDC
C
	INTEGER*4      HINT      !HIGH PORTION OF SERIAL #
	INTEGER*4      LINT      ! LOW PORTION OF SERIAL #
C
	INTEGER*4      SYSIDX    ! SITE VARIANT
C
	INTEGER*4      INT4, INT43, X
C
C
C
C
	CDC     = XCDC
	INTSER  = XINTSER
C
C
C
	CALL GENKEY( CDC, CDCH, CDCL, SYSIDX)   !GET CDCH, CDCL
C
	HINT = ISHFT( INTSER, -16) !filtra os 16 bits menos sigificativos
	HINT = IAND ( HINT, '0000007F'X)!filtra o 8º bit que na realidade é o 24º bit original do serial interno
	LINT = IAND ( INTSER, '0000FFFF'X)!filtra 2 bytes ou seja 16 bits
	LINT = LINT + CDCL !fica com os 16 bits menos significativos da data CDC
	IF( LINT.GT.'0000FFFF'X)THEN        !IF CARRY
	  HINT = HINT + 1
	  LINT = IAND (LINT,'0000FFFF'X)
	ENDIF
	HINT = HINT + CDCH
	HINT = IAND ( HINT, '0000007F'X)
C
	INT4 = IAND ( LINT, '000000FF'X)
	INT43= ISHFT( INT4,8) + ISHFT( LINT, -8)
C
	INT43= IEOR ( INT43, HINT)
	INT4 = IEOR ( INT4,  INT43)
	HINT = IEOR ( HINT,  INT4)
C
	INT4 = ISHFT( INT4, 6)
	HINT = IEOR ( HINT, INT4)
	INT4 = ISHFT( INT4, -1)
	HINT = IEOR ( HINT, INT4)
	INT4 = ISHFT( INT4, -1)
	HINT = IEOR ( HINT, INT4)
	INT4 = ISHFT( INT4, -4)
C
	HINT = IAND ( HINT, '0000007F'X)
	INT43= IAND ( INT43,'0000007F'X)
	INT4 = IAND ( INT4, '0000007F'X)
	LINT = IAND ( LINT, '00008080'X)
	LINT = LINT + INT4
	INT43= ISHFT( INT43,8)
	LINT = LINT + INT43
C
	LINT = IAND ( LINT, '0000FFFF'X)
C
C Get the 24th bit from the original serial # and put into result
C
	X    = ISHFT( XINTSER, -16) !filtra os 2 bytes menos significativos X fica com os 2 bytes mais significativos
	X    = IAND ( X, '00000080'X) !8º bit mais significativo com é um and filtra só este bit (16+8=24º bit original)
	HINT = IOR  ( HINT, X) !mete este 24º bit no resultado
C
	XEXTSER = ISHFT(HINT,16) + LINT !faz um left shift de 2 bytes passando o LINT a ser esses 2 bytes menos significativos
C
C Set check digits
C
	CALL GENCHK(XCDC, XINTSER, XEXTSER, CHKDIG, SYSIDX)
C
	XCHKDIG = CHKDIG
C
	RETURN
	END
