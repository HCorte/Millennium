C
C SUBROUTINE SCAN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]SCAN.FOV                                     $
C  $Date::   17 Apr 1996 14:51:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vax_scan.for;1 **
C
C VAX_SCAN .FOR
C
C V01 27-AUG-90 MP RELEASED FOR VAX
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C CALLING SEQUENCE:
C CALL SCAN(COMMAND,                 ;INPUT LINE OF ASCII CHARS
C           POINTER,                 ;POINTER TO BEGIN WITH
C           MNEMONIC,                ;MNEMONIC TABLE, KEYWORDS
C                                    ;SEPARATED WITH BIN Z0N,
C                                    ;WHERE N IS THE DEFAULT PART LEN
C                                    ;EOT MARKED AS TWO BIN ZEROS
C           KEY)                     ;NUMBER OF KEYWORD FOUND OR 0
C
C ACTION:
C ROUTINE WILL SKIP TO FIRST NONBLANK CHAR,
C              SCAN THE TABLE AND FIND FIRST MATCH,
C              MOVE THE POINTER IN "COMMAND LINE" TO NXT AFTER FOUND
C              SET THE KEY FOR THE USER
C
C NOTES:
C 1. COMMAND CHARACTER *(*)
C    POINTER INTEGER   *4        FROM 1-
C    MNEMONIC CHARACTER*(*)
C    KEY      INTEGER  *4        FROM 1- OR 0
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SCAN (COMMAND, POINTER, MNEM, KEY)
	IMPLICIT NONE
C
	CHARACTER   COMMAND*(*)
	INTEGER*4   POINTER
	CHARACTER   MNEM*(*)
	INTEGER*4   KEY
C
	INTEGER*4   I,J,K,L,M
	INTEGER*4   CLENGTH
	INTEGER*4   MLENGTH
	CHARACTER*1 COMC, MNEMC
C
C
C	GET STRING LENGTH
C
	CLENGTH = LEN(COMMAND)
	MLENGTH = LEN(MNEM)
C
	KEY = 0
C
C	SKIP BLANK CHARACTERS
C
	DO 10, I=POINTER,CLENGTH
	    IF (COMMAND(I:I) .NE. ' ') GOTO 20
10	CONTINUE
C
C	NO CHARACTERS FOUND
C
	GOTO 800		    ! ERROR
C
C	FOUND NON-ZERO CHARACTER
C
20	CONTINUE
C
C	STEP THROUGH KEYWORDS IN THE TABLE OF MNEMONICS
C
	J = 1
50	CONTINUE
C	    FOLLOWING IS A SIMULATED DO-LOOP
	    IF (J .GE. MLENGTH) GOTO 800	! ERROR
	    K = ZEXT(ICHAR(MNEM(J:J)))
	    IF (K .GT. 15) THEN
C		ERROR - SHOULD START WITH THE LEHGTH OF THE KEYWORD
		TYPE *,'VAX_SCAN: KEYWORD TABLE IS NOT CORRECT AT ', J
C		USE ERROR CODE 10 FOR DISPLAYING THE STACK
		CALL LIB$STOP(%VAL(10))
		GOTO 800	    ! ERROR
	    ENDIF
	    IF (K .EQ. 0) THEN
C		END OF THE TABLE
		GOTO 800	    ! NOT FOUND
	    ENDIF
C
C	COMPARE STRINGS
C
	    J = J + 1
	    DO 100, L=0,(K-1)
C		CONVERT INTO UPPER CASE
		COMC = COMMAND(I+L:I+L)
		IF(COMC .GE. 'a' .AND. COMC .LE. 'z') THEN
		    COMC = CHAR(ICHAR('A') + ICHAR(COMC) - ICHAR('a'))
		ENDIF
		MNEMC = MNEM(J+L:J+L)
		IF(MNEMC .GE. 'a' .AND. MNEMC .LE. 'z') THEN
		    MNEMC = CHAR(ICHAR('A') + ICHAR(MNEMC) - ICHAR('a'))
		ENDIF
		IF (COMC .NE. MNEMC) THEN
C		    STRINGS ARE NOT EQUAL, GO TO THE NEXT KEYWORD
		    DO 80 M=L+1,MLENGTH-J-L
			MNEMC = MNEM(J+M:J+M)
			K = ZEXT(ICHAR(MNEMC))
			IF (K .LE. 15) GOTO 90
80		    CONTINUE
C		    ERROR - SHOULD START WITH THE LEHGTH OF THE KEYWORD
		    TYPE *,'VAX_SCAN: KEYWORD IS TOO LONG AT ', J
C		    USE ERROR CODE 10 FOR DISPLAYING THE STACK
		    CALL LIB$STOP(%VAL(10))
		    GOTO 800
90		    CONTINUE
		    J = J + M
		    KEY = KEY + 1
		    GOTO 50
		ENDIF
100	    CONTINUE
C	    FOUND KEYWORD
	    POINTER = I + K
C
	    KEY = KEY + 1
	    GOTO 900
C
C	END OF SIMULATED DO-LOOP
C
C	ERROR
800	CONTINUE
	KEY = 0
900	CONTINUE
C
	RETURN
	END
