C
C SUBROUTINE GETKEY
C $Log:   GXAFXT:[GOLS]GETKEY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:20:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:26:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_getkey.for **
C
C VAX_GETKEY.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C GETKEY.FOR
C
C V01 10-AUG-90 TKO  RELEASED FOR VAX
C
C This is the get key routine for VISION
C
C CALL GETKEY(KEYLST,BUFFER,INLEN,KEYNUM,INDEX,KLIMIT)
C
C KEYLST  LIST OF KEY WORDS
C BUFFER  INPUT BUFFER
C INLEN	  INPUT LENGTH
C INDEX	  INDEX INTO BUFFER
C KLIMIT  NUMBER OF KEYWORDS IN KEYLST
C
C OUTPUT:
C
C KEYNUM  NUMBER OF KEY THAT MATCHED (0 IF NO MATCH)
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
	SUBROUTINE GETKEY(KEYLST,BUFFER,INLEN,KEYNUM,INDEX,KLIMIT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	BYTE	    KEYLST(8,*)
	BYTE	    BUFFER(*)
	INTEGER*4   INLEN
	INTEGER*4   KEYNUM
	INTEGER*4   INDEX
	INTEGER*4   KLIMIT
	INTEGER*4   K, X, KEY
C
	BYTE	    LOCBUF(8)
C
C
C All entered keys must be at least 2 chars and at most 8
C
	KEYNUM = 0
	IF(INLEN.LT.2 .OR. INLEN.GT.8)GOTO 9000
C
C If first symbol is number then do not search for match
C
        IF(BUFFER(INDEX).GE.ICHAR('0').AND.
     *    BUFFER(INDEX).LE.ICHAR('9')) GOTO 9000
        IF(BUFFER(INDEX).EQ.ICHAR('!')) GOTO 9000
C
C Move entered key into local storage and make upper case while
C doing so.
C
	DO 1100 K = 1, INLEN
	  X = BUFFER(INDEX+K-1)
	  IF(X.GE.ICHAR('a') .AND. X.LE.ICHAR('z'))THEN
	    X = X - '20'X
	  ENDIF
	  LOCBUF(K) = X
1100	CONTINUE
C
C Now compare byte by byte - always using upper case
C
	DO 1900 KEY = 1, KLIMIT
	  DO 1800 K = 1, INLEN
	    IF(LOCBUF(K).NE.KEYLST(K,KEY) .AND.
     *	         LOCBUF(K).NE.KEYLST(K,KEY) - '20'X)GOTO 1900
1800	  CONTINUE
	  KEYNUM = KEY
	  GOTO 9000
1900	CONTINUE
C
9000	CONTINUE
	RETURN
	END
