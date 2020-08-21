C
C SUBROUTINE X2CHKFLD.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKFLD.FOV                                 $
C  $Date::   17 Apr 1996 16:12:12                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C CHECK THE FIELD NAME
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2CHKFLD(FIELD_FROM_DEF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER FIELD_FROM_DEF*15
	INTEGER*4 STATUS

	CHARACTER  PROMPT*50
     *  /'          Enter description (5 non blank chars) - '/

	CHARACTER CHECKED_FIELD*15
	INTEGER*4   MAX_FIELD_LEN
	PARAMETER   (MAX_FIELD_LEN=15)
	CHARACTER   FIELD_DESCRIPTION*15, SPACES/'               '/
	INTEGER*4   OFFSET_ENTERED, OFFSET_FIELD
	INTEGER*4   MIN_MATCH, MATCHED,NEXT, NON_EMPTY
	BYTE	    BFIELD_DESCRIPTION(MAX_FIELD_LEN)
	BYTE	    BCHECKED_FIELD(MAX_FIELD_LEN)
	BYTE	    FIELD_CHAR, CHECKED_CHAR
	INTEGER*4   LENGTH

	EQUIVALENCE (BFIELD_DESCRIPTION,FIELD_DESCRIPTION),
     *		    (BCHECKED_FIELD,CHECKED_FIELD)

C
C PROMPT FOR THE FIELD DESCRIPTION
C
	STATUS=0

	CHECKED_FIELD=FIELD_FROM_DEF
	FIELD_DESCRIPTION=SPACES
	CALL INPTEXT(PROMPT,FIELD_DESCRIPTION,LENGTH)
	OFFSET_ENTERED=1
	OFFSET_FIELD=1
	MATCHED=0
	DO 200, NEXT=1,MAX_FIELD_LEN
100	    CONTINUE
	    IF (BFIELD_DESCRIPTION(OFFSET_ENTERED).EQ.0  .OR.
     *		BFIELD_DESCRIPTION(OFFSET_ENTERED).EQ.33 .OR.
     *		BFIELD_DESCRIPTION(OFFSET_ENTERED).EQ.36 .OR.
     *		BFIELD_DESCRIPTION(OFFSET_ENTERED).EQ.32) THEN
		OFFSET_ENTERED=OFFSET_ENTERED+1
		IF (OFFSET_ENTERED.GE.MAX_FIELD_LEN) GOTO 220
		GOTO 100
	    ENDIF
110	    CONTINUE
	    IF (BCHECKED_FIELD(OFFSET_FIELD).EQ.0  .OR.
     *	        BCHECKED_FIELD(OFFSET_FIELD).EQ.33 .OR.
     *	        BCHECKED_FIELD(OFFSET_FIELD).EQ.36 .OR.
     *	        BCHECKED_FIELD(OFFSET_FIELD).EQ.32 ) THEN
		OFFSET_FIELD=OFFSET_FIELD+1
		IF (OFFSET_FIELD.GE.MAX_FIELD_LEN) GOTO 220
		GOTO 110
	    ENDIF

C EXIT IF NO MATCH
	    FIELD_CHAR=BFIELD_DESCRIPTION(OFFSET_ENTERED)
	    CHECKED_CHAR=BCHECKED_FIELD(OFFSET_FIELD)
	    IF (FIELD_CHAR.GT.96) FIELD_CHAR=FIELD_CHAR-32
	    IF (CHECKED_CHAR.GT.96) CHECKED_CHAR=CHECKED_CHAR-32
	    IF (FIELD_CHAR.NE.CHECKED_CHAR) THEN
		TYPE *,'          Invalid description '
		STATUS=-1
		GOTO 9000	
	    ENDIF
	    MATCHED=MATCHED+1
	    OFFSET_ENTERED=OFFSET_ENTERED+1
	    OFFSET_FIELD=OFFSET_FIELD+1
	    IF (OFFSET_FIELD.GE.MAX_FIELD_LEN) GOTO 220
	    IF (OFFSET_ENTERED.GE.MAX_FIELD_LEN) GOTO 220
200	CONTINUE
C
220	CONTINUE

	MIN_MATCH=5
	NON_EMPTY=0
	DO 300, NEXT=1,MAX_FIELD_LEN
	    IF (BCHECKED_FIELD(NEXT).EQ.0  .OR.
     *		BCHECKED_FIELD(NEXT).EQ.33 .OR.
     *		BCHECKED_FIELD(NEXT).EQ.36 .OR.
     *		BCHECKED_FIELD(NEXT).EQ.32) GOTO 300
	    NON_EMPTY=NON_EMPTY+1	
300	CONTINUE
	IF (NON_EMPTY.LT.MIN_MATCH) MIN_MATCH=NON_EMPTY

	IF (MATCHED.LT.MIN_MATCH) THEN
	    TYPE *,'          Description too short'
	    STATUS=-1
	ENDIF

9000	CONTINUE
	RETURN
	END
