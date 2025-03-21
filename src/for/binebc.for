C
C SUBROUTINE BINEBC
C $Log:   GXAFXT:[GOLS]BINEBC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:16:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:42:04   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lot_binebc.for **
C
C BINEBC.FOR
C
C V01 29-APR-91 JPJ INITIAL RELEASE FOR  MARYLAND
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
C
C INPUT:
C
C LENGTH - LENGTH OF EBCDIC STRING TO CREATE
C NUMBER - I4 NUMBER OR I8 NUMBER TO CONVERT TO EBCDIC
C UNITS  - BET UNITS
C TYPE   - 1 IS I8_TYPE ANYTHING ELSE IS I4 TYPE
C
C OUTPUT:
C
C STRING - EBCDIC STRING
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BINEBC(STRING,LENGTH,NUMBER,UNITS,TYPE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4 LENGTH,NUMBER(2),I,SIGN,TEMP,UNITS,TYPE
	INTEGER*4 LEFTOVER,TMP_NUMBER(2),I8_TYPE/1/,TMP_LENGTH
	BYTE	  STRING(LENGTH)
C
C MOVE NUMBER AND LENGTH INTO A SAFE PLACE
C
	TMP_NUMBER(1)=NUMBER(1)
	TMP_NUMBER(2)=NUMBER(2)
	TMP_LENGTH=LENGTH
C
C GET BETUNIT
C
	LEFTOVER=0
	IF(UNITS.EQ.VALUNIT) THEN
          LEFTOVER=DYN_VALUNIT/10
	ENDIF
C
	IF(UNITS.EQ.BETUNIT) THEN
          LEFTOVER=DYN_BETUNIT/10
	ENDIF
	TMP_LENGTH=TMP_LENGTH-LEFTOVER
C
C PUT NUMBER INTO STRING
C
	SIGN=1
	IF(TMP_NUMBER(1).LT.0) THEN
          SIGN=-1
	  TMP_NUMBER(1)=TMP_NUMBER(1)*-1
 	  TMP_NUMBER(2)=TMP_NUMBER(2)*-1
	ENDIF
	ENCODE(TMP_LENGTH,900,STRING) TMP_NUMBER(1)
	DO 100 I=1,TMP_LENGTH
	   TEMP=STRING(I)
	   STRING(I)=IOR(TEMP,'F0'X)
100	CONTINUE
C
C GET REST OF NUMBER
C
	IF(LEFTOVER.NE.0) THEN
	  IF(TYPE.EQ.I8_TYPE) THEN
	    ENCODE(LEFTOVER,901,STRING(TMP_LENGTH+1)) TMP_NUMBER(2)
          ELSE
	    ENCODE(LEFTOVER,901,STRING(TMP_LENGTH+1)) 0
	  ENDIF
	  DO 200 I=TMP_LENGTH+1,TMP_LENGTH+LEFTOVER
	     TEMP=STRING(I)
	     STRING(I)=IOR(TEMP,'F0'X)
200	  CONTINUE
	  TMP_LENGTH=TMP_LENGTH+LEFTOVER
        ENDIF
C
C UPDATE SIGN NIBBLE
C
	TEMP=STRING(TMP_LENGTH)
	TEMP=IAND(TEMP,'0F'X)
	IF(SIGN.EQ.1) THEN
	  STRING(TMP_LENGTH)=IOR(TEMP,'C0'X)
	ELSE
	  STRING(TMP_LENGTH)=IOR(TEMP,'D0'X)
	ENDIF
	RETURN
C
C FORMAT SECTION
C
900	FORMAT(I<TMP_LENGTH>.<TMP_LENGTH>)
901	FORMAT(I<LEFTOVER>.<LEFTOVER>)
	END
