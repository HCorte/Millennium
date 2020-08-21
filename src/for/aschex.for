C
C SUBROUTINE ASCHEX
C $Log:   GXAFXT:[GOLS]ASCHEX.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:13:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:40:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - aschex.for **
C
C ASCHEX.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C NOTE:  This source file includes ASCHEX and HEXASC.
C
C This subroutine will convert a string from ASCII to
C Hexadecimal.
C
C Calling sequence:
C
C     CALL ASCHEX(ASCSTR,START,LENGTH,INTSTR,ERR)
C
C Input parameters:
C
C     ASCSTR      Char*1(*)   String to be converted
C     START       Int*4       Start position of ASCSTR
C     LENGTH      Int*4       Length of string to convert
C
C Output parameters:
C
C     INTSTR      Int*4(2)    Hexadecimal number
C     ERR         Int*4       Return error
C                             -1 = invalid string
C                             -2 = string too long to convert
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
C Copyright 1990 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ASCHEX(ASCSTR,START,LENGTH,INTSTR,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER   ASCSTR(*)*1             !ASCII string
	INTEGER*4   START                   !Start position into string
	INTEGER*4   LENGTH                  !Length to convert
	INTEGER*4   INTSTR(2)               !Output HEX value
	INTEGER*4   ERR                     !Conversion error
	INTEGER*4   VAL1,VAL2,VALUE         !Work variables
	INTEGER*4   I                       !Work variables
	INTEGER*4   BYTE,POS                !Byte/conversion position
	INTEGER*4   BYTES                   !Words/bytes required
C
C INITIALIZE VARIABLES.
C
	DO 50 I=1,2
	  INTSTR(I)=0
50	CONTINUE
	ERR=0
C
C ENSURE A LENGTH WHICH EXCEEDS OUR TABLE HAS NOT BEEN INPUT.
C
	IF(LENGTH.GT.16) THEN
	  ERR=-2
	  GOTO 8000
	ENDIF
C
C CHECK FOR A VALID HEX DIGIT.
C
	DO 60 I=START,START+LENGTH-1
	  IF(ICHAR(ASCSTR(I)).LT.48 .OR.
     *	     ICHAR(ASCSTR(I)).GT.57) THEN
	    IF(ICHAR(ASCSTR(I)).LT.65.OR.
     *	       ICHAR(ASCSTR(I)).GT.70) THEN
	      ERR=-1
	      GOTO 8000
	    ENDIF
	  ENDIF
60	CONTINUE
C
C CALCULATE THE TOTAL NUMBER OF BYTES REQUIRED
C AND THE BYTE OFFSET.
C
	BYTES=(LENGTH+1)/2
	BYTE=MAX0(4-BYTES,0)
	POS=START
C
C CONVERT THE REMAINING CHARACTERS INTO HEX, STARTING
C FROM LEFT TO RIGHT.
C
100	CONTINUE
	IF(BYTES.GT.0) THEN
	  VAL1=0
	  VAL2=0
	  IF(POS.LE.START+LENGTH-1) THEN
	    IF(ICHAR(ASCSTR(POS)).LE.57) THEN
	       VAL1=ICHAR(ASCSTR(POS))-48
	    ELSE
	       VAL1=ICHAR(ASCSTR(POS))-55
	    ENDIF
	    POS=POS+1
	  ENDIF
	  IF(POS.LE.START+LENGTH-1) THEN
	    IF(ICHAR(ASCSTR(POS)).LE.57) THEN
	       VAL2=ICHAR(ASCSTR(POS))-48
	    ELSE
	       VAL2=ICHAR(ASCSTR(POS))-55
	    ENDIF
	    POS=POS+1
	  ENDIF
C
	  VALUE=ISHFT(VAL1,4)+VAL2
	  CALL NSBYTE(VALUE,INTSTR,BYTE)
	  BYTE=BYTE+1
	  BYTES=BYTES-1
	  GOTO 100
	ENDIF
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
