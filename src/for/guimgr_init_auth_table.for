C  GUIMGR_INIT_AUTH_TABLE.FOR
C
C V02 08-NOV-2000 UXN GUI prefix added.
C V01 08-JUL-1993 MP  RELEASE FOR VAX 
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This routine loads Passthrough Server authorization file.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUIMGR_INIT_AUTH_TABLE
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	INTEGER*4   ST              !Work variables
	INTEGER*4   LUN
	INTEGER*4   AUTH_INX
	INTEGER*4   USER_COUNT
C
	CHARACTER*80	AUTH_FNAME/'GUI_AUTHORIZE.FIL'/
	CHARACTER*80	INPUT_LINE
	CHARACTER*32	SPOOL_DATA_CLASSES
	CHARACTER*32	CRITICAL_DATA_CLASSES
	CHARACTER*32	USER_DATA_CLASSES
C
	USER_COUNT = 0
C
C Open the file
C
	CALL GETLUN(LUN)
	OPEN(UNIT=LUN,FILE=AUTH_FNAME,STATUS='OLD',IOSTAT=ST)
	IF(ST.NE.0) THEN
	    CALL OPS('Failed to open '//AUTH_FNAME,ST,ST)
	    GOTO 9000
	ENDIF
C
	DO 1000 AUTH_INX=1, GUI_MAX_AUTH_USERS
C
	    GUI_AUTH_NAME(AUTH_INX) = ' '
	    GUI_AUTH_PASSWRD(AUTH_INX) = ' '
	    GUI_AUTH_DATA_CLASS_BITS(AUTH_INX) = 0
C
100	    CONTINUE
C
C Read the file
C
	    READ(LUN, 9010, IOSTAT=ST) INPUT_LINE
9010	FORMAT(A)
	    IF(ST.LT.0) GOTO 1000			    ! NEXT USER
C
	    CALL STR$UPCASE(INPUT_LINE,INPUT_LINE)
	    IF(GUI_DBG_UNIT.GT.0) THEN
		WRITE(GUI_DBG_UNIT, 9020) IAM(),'Input: ',
     *		  INPUT_LINE
9020		FORMAT(1X,A,A,A)
	    ENDIF
C
	    IF(INPUT_LINE(1:30) .EQ. 
     *		'SPOOL_DATA_CLASSES:...........') THEN
		SPOOL_DATA_CLASSES = INPUT_LINE(31:62)
		CALL GUIMGR_CHAR_TO_BITS(SPOOL_DATA_CLASSES,
     *		     GUI_SPOOL_DATA_CLASS_BITS)
		IF(GUI_DBG_UNIT.GT.0) THEN
		    WRITE(GUI_DBG_UNIT, 9030) IAM(),'Spooled classes:',
     *			  GUI_SPOOL_DATA_CLASS_BITS
9030		    FORMAT(1X,A,A,Z8.8)
		ENDIF
C
	    ELSE IF(INPUT_LINE(1:30) .EQ. 
     *		'CRITICAL_DATA_CLASSES:........') THEN
		CRITICAL_DATA_CLASSES = INPUT_LINE(31:62)
		CALL GUIMGR_CHAR_TO_BITS(CRITICAL_DATA_CLASSES,
     *		     GUI_CRITICAL_DATA_CLASS_BITS)
		IF(GUI_DBG_UNIT.GT.0) THEN
		    WRITE(GUI_DBG_UNIT, 9040) IAM(),'Critical classes:',
     *			  GUI_CRITICAL_DATA_CLASS_BITS
9040		    FORMAT(1X,A,A,Z8.8)
		ENDIF
C
	    ELSE IF(INPUT_LINE(1:30) .EQ. 
     *		'CLIENT_NAME:..................') THEN
		GUI_AUTH_NAME(AUTH_INX) = INPUT_LINE(31:62)
C
	    ELSE IF(INPUT_LINE(1:30) .EQ. 
     *		'CLIENT_PASSWORD:..............') THEN
		GUI_AUTH_PASSWRD(AUTH_INX) = INPUT_LINE(31:62)
C
	    ELSE IF(INPUT_LINE(1:30) .EQ. 
     *		'CLIENT_DATA_CLASSES:..........') THEN
		USER_DATA_CLASSES = INPUT_LINE(31:62)
		CALL GUIMGR_CHAR_TO_BITS(USER_DATA_CLASSES,
     *		     GUI_AUTH_DATA_CLASS_BITS(AUTH_INX))
C
		IF(GUI_DBG_UNIT.GT.0) THEN
		    WRITE(GUI_DBG_UNIT, *) IAM(),'User loaded.'
		    WRITE(GUI_DBG_UNIT, *) IAM(),'Name: ',
     *			  GUI_AUTH_NAME(AUTH_INX)
		    WRITE(GUI_DBG_UNIT, *) IAM(),'Pass: ',
     *			  GUI_AUTH_PASSWRD(AUTH_INX)
		    WRITE(GUI_DBG_UNIT, 9050)IAM(),'Data: ',
     *			  GUI_AUTH_DATA_CLASS_BITS(AUTH_INX)
9050		    FORMAT(1X,A,A,Z8.8)
		ENDIF
	    ELSE IF(INPUT_LINE(1:30) .EQ. 
     *		'CLIENT_END:...................') THEN
		USER_COUNT = USER_COUNT + 1
		GOTO 1000				    ! NEXT USER
	    ENDIF
C
	    GOTO 100				    ! NEXT RECORD
C
1000	CONTINUE
C
	READ(LUN, 9060, IOSTAT=ST) INPUT_LINE
9060	FORMAT(A)
	IF(ST.GE.0) THEN
	    CALL OPSTXT('WARNING: data passed last user found in '//AUTH_FNAME)
	ENDIF
C
	CLOSE(LUN)
C
9000	CONTINUE

	CALL OPS('# of PTS users loaded',USER_COUNT, USER_COUNT)
C
	RETURN
	END
