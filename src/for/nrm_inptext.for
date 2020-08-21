C
C SUBROUTINE INPTEXT
C
C INPTEXT.FOR
C
C V03 24-MAY-1999 UXN OUTPUT LUN CHANGED TO 6.
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 02-APR-1991 TKO INITIAL RELEASE
C
C This program is used to output a message and wait for a CHARACTER STRING
C response.
C
C Calling sequence:
C
C	CALL INPTEXT(OUTPUTSTRING, INPUTSTRING, INPUTLEN)
C
C INPUT:
C	OUTPUTSTRING	This is the text string to output as a prompt.  It
C			must be a character string whose length can be
C			determined by the LEN(X) function.
C
C OUTPUT:
C	INPUTSTRING	This is the string as typed by the user.  It must be
C			a character string whose length can be determined by
C			the LEN(X) function (which will be the maximum length
C			of input).
C	INPUTLEN	This is the actual length of the input string.
C			(INPUTSTRING will be padded with blanks at the end
C		        to fill it up).  Note that if
C			the user types return only, INPUTLEN will be set to
C			ZERO and INPUTSTRING will contain all blanks.
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
	SUBROUTINE INPTEXT(OUTPUTSTRING, INPUTSTRING, INPUTLEN)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER   OUTPUTSTRING*(*)
	CHARACTER   INPUTSTRING* (*)
	INTEGER*4   INPUTLEN
C
	INTEGER*4   POS,I,INLEN
C
C SKIP SPACES FROM THE BEGINNING OF THE PROMPT.
C
C
        INLEN = LEN(OUTPUTSTRING)
        POS   = 1
        DO 5 I=1,INLEN
           IF(OUTPUTSTRING(I:I).EQ.' ') THEN
              POS = POS + 1
           ELSE
              GOTO 6
           ENDIF
5       CONTINUE
6       CONTINUE

	WRITE(6, 1001) IAM(),OUTPUTSTRING(POS:)
1001	FORMAT(' ',A,A,' >',$)
C
	READ (5, 1002) INPUTLEN, INPUTSTRING
1002	FORMAT(Q,A)
C
	IF(INPUTLEN.LT.LEN(INPUTSTRING))THEN
	  INPUTSTRING(INPUTLEN+1: ) = ' '
	ENDIF
C
	RETURN
	END
