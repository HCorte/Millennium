C
C SUBROUTINE BCDASC
C $Log:   GXAFXT:[GOLS]BCDASC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:15:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   05 Sep 1993 23:47:12   WXS
C  
C     Rev 1.0   21 Jan 1993 15:40:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - ascbcd.for **
C
C
C ===============================================================
C BCDASC.FTN
C
C Calling sequence:
C
C     CALL BCDASC(ASCSTR,START,LENGTH,INTSTR,ERR)
C
C Input parameters:
C
C     INTSTR      Int*4(2)    Binary Coded Decimal string
C     START       Int*4       Starting BCD digit (2 digits per byte)
C     LENGTH      Int*4       Number of BCD digits to convert
C
C Output parameters:
C
C     ASCSTR      Char*1(*)   ASC string of converted BCD
C     ERR         Int*4       Return error
C                             -1 = invalid string
C                             -2 = invalid length
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BCDASC(ASCSTR,START,LENGTH,INTSTR,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER   ASCSTR(*)*1             !ASCII string
	INTEGER*4   START                   !Start position into string
	INTEGER*4   LENGTH                  !Length to convert
	INTEGER*4   INTSTR(2)               !Output BCD value
	INTEGER*4   ERR                     !Conversion error
	INTEGER*4   I,K                     !Work variables
	INTEGER*4   VALUE                   !Conversion variable
	INTEGER*4   BYTE,POS,IDX            !Byte/conversion position
	CHARACTER   VAL1,VAL2               !Work variables
C
C INITIALIZE VARIABLES.
C
	ERR=0
C
C ENSURE A LENGTH WHICH EXCEEDS OUR TABLE HAS NOT BEEN INPUT.
C
	IF(LENGTH.GT.16) THEN
	  ERR=-2
	  GOTO 8000
	ENDIF
C
C CALCULATE THE TOTAL NUMBER OF BYTES REQUIRED
C AND THE BYTE OFFSET.
C
	BYTE=START/2
	IF(MOD(START,2).EQ.0) BYTE=BYTE-1
	IDX=0
C
100	CONTINUE
	IF(IDX.LT.LENGTH) THEN
C
C EXTRACT THE BYTE INFORMATION.
C
C****	  CALL NLBYTE(VALUE,INTSTR,BYTE)
	  CALL ILBYTE(VALUE,INTSTR,BYTE)
	  VAL2=CHAR(IAND(VALUE,'0F'X)+48)
	  VAL1=CHAR(ISHFT(VALUE,-4)+48)
C
C DETERMINE IF THIS DIGIT SHOULD BE INCLUDED INTO
C THE CONVERTED OUTPUT ASCII STRING.
C
	  POS=BYTE*2+1
	  IF(POS.GE.START .AND. IDX.LT.LENGTH) THEN
	    IF(ICHAR(VAL1).LT.48 .OR.
     *	       ICHAR(VAL1).GT.63) THEN
	      ERR=-1
	      GOTO 8000
	    ENDIF
	    IDX=IDX+1
	    ASCSTR(IDX)=VAL1
	  ENDIF
C
	  POS=POS+1
	  IF(POS.GE.START .AND. IDX.LT.LENGTH) THEN
	    IF(ICHAR(VAL2).LT.48 .OR.
     *	       ICHAR(VAL2).GT.63) THEN
	      ERR=-1
	      GOTO 8000
	    ENDIF
	    IDX=IDX+1
	    ASCSTR(IDX)=VAL2
	  ENDIF
	  BYTE=BYTE+1
	  GOTO 100
	ENDIF
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
