C
C *** SUBROUTINE GETCTRL ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_GETCTRL.FOV                              $
C  $Date::   17 Apr 1996 14:07:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_getctrl.for ***
C
C V03 06-JAN-94 PJS ADDED SWITCH COUNT KEYWORD VERIFICATION & CLEANED UP.
C V02 12-JAN-93 RRB ADDED CONFIGURATION WAIT LIMIT AND
C                   MODIFIED CHECK FOR AUXILIARY PORT IDs.
C V01 12-MAR-91 XXX RELEASED FOR VAX
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	GET CONNECTION CONTROL INFORMATION FOR TELENEX MATRIX SWITCH MANAGEMENT.
C
C Calling Sequence:
C	CALL GETCTRL(REPLY)
C
C Output:
C	REPLY - 0 = OK
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE GETCTRL(REPLY)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	AUX_CNT,
     *			DIG,
     *			I,
     *			IND1,
     *			IND2,
     *			KEY,
     *			REPLY,
     *			ST,
     *			VALUE
C
	CHARACTER*12	CFG_FILE_NAME /'MSCINFO.FIL '/	! CONFIG FILE NAME
	INTEGER*4	MSC_FILE_NAME(3)
	EQUIVALENCE	(CFG_FILE_NAME, MSC_FILE_NAME)
C
C NOTE: AUXILIARY PORT ID'S HANDLED AS EXCEPTION SINCE WE LOAD DIFFERENT TABLE.
C
	CHARACTER*25	MSC_KEYWORD(MSC_CTRL_PARMS + 1)
     *			/'SWITCH CNT:',
     *			 'SWITCH 1 NAME:',		!!!!!!!!!!!!!!!!!!!!!!!!
     *			 'SWITCH 2 NAME:',		!!!      IMPORTANT
     *			 'SWITCH 3 NAME:',		!!! Ensure that the # of
     *			 'SWITCH 4 NAME:',		!!! switch name keywords
     *			 'SWITCH 5 NAME:',		!!! corresponds with the
     *			 'SWITCH 6 NAME:',		!!! MAX_SWITCHES para-
     *			 'SWITCH 7 NAME:',		!!! meter in MSCCOM.DEF.
     *			 'SWITCH 8 NAME:',		!!!      IMPORTANT
     *			 'SWITCH 9 NAME:',		!!!!!!!!!!!!!!!!!!!!!!!!
     *			 'LOGON USER ID:',
     *			 'LOGON PASSWORD:',
     *			 'NETWORK PORT PREFIX:',
     *			 'LOCAL PORT PREFIX:',
     *			 'AUXILIARY PORT CNT:',		
     *			 'CONFIGURATION WAIT LIMIT:',	!!!!!!!!!!!!!!!!!!!!!!!!
     *			 'RESPONSE PAGE LENGTH:',	!!!      IMPORTANT
     *			 'SYSA LAT DEVICE NAME:',	!!! Ensure that the # of
     *			 'SYSB LAT DEVICE NAME:',	!!! LAT device keywords
     *			 'SYSC LAT DEVICE NAME:',	!!! corresponds with the
     *			 'SYSD LAT DEVICE NAME:',	!!! MAX_LAT_PORTS para-
     *			 'SYSE LAT DEVICE NAME:',       !!! meter in MSCCOM.DEF.
     *			 'AUXILIARY PORT ID:'/		!!!      IMPORTANT
C		!!! AUX PORT ID SHOULD ALWAYS BE LAST !!!!!!!!!!!!!!!!!!!!!!!!!!
C
C MAKE SURE TO UPDATE THE KEYLEN TABLE IF YOU MODIFY THE KEY WORDS.
C (KNOWING HOW MANY CHARACTERS TO MATCH PER STRING MAKES LIFE A LITTLE EASIER)
C
	INTEGER*4	MSC_KEYLEN(MSC_CTRL_PARMS + 1)
     *			/11, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
     *			 15, 20, 18, 19, 25, 21, 21, 21, 21, 21, 21, 18/
C
	REAL*8		TEMP_PARMS(MSC_CTRL_PARMS  + MAX_AUX_PORTS)
	CHARACTER*8	CTEMP_PARMS(MSC_CTRL_PARMS + MAX_AUX_PORTS)
	EQUIVALENCE	(TEMP_PARMS, CTEMP_PARMS)
C
	CHARACTER*80	LINE
C
	CHARACTER*25	SUBSTRING
C
	CHARACTER*8	TPARM,
     *			UNDEFINED	/'        '/
C
	CHARACTER*8	PARM
	BYTE		BPARM(8)
	EQUIVALENCE	(PARM, BPARM)
C
	CHARACTER*1	CHAR
	BYTE		ICHAR
	EQUIVALENCE	(CHAR, ICHAR)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	AUX_CNT = 0
	REPLY   = 0
C
	DO 100 I = 1, MSC_CTRL_PARMS + MAX_AUX_PORTS
	  CTEMP_PARMS(I) = UNDEFINED
100	CONTINUE
C
C OPEN CONTROL FILE
C
	CALL OPENW(1, MSC_FILE_NAME, 4, 0, 0, REPLY)
	IF (REPLY .NE. 0) GOTO 9999
C
C READ THRU THE CONFIGURATION FILE LOOKING FOR KEYWORDS.
C
200	CONTINUE
	LINE = ' '
	READ(1, 9000, END = 500) LINE
	IF (LINE .EQ. ' ') GOTO 200
C
C SEARCH FOR KEY WORD AND DECODE THE APPROPRIATE PARAMETER.
C
	DO 400 KEY = 1, MSC_CTRL_PARMS + 1
	  SUBSTRING = MSC_KEYWORD(KEY)
	  IND1 = INDEX(LINE, SUBSTRING(1:MSC_KEYLEN(KEY)))
C
C FOUND KEYWORD ...
C NOW FIND NEXT NON-BLANK CHARACTER AFTER KEYWORD
C TO GET PARAMETER WHICH IS LIMITED TO THE NEXT EIGHT CHARACTERS
C FOLLOWING THE KEYWORD EXCLUDING LEADING SPACES.
C
	  IF (IND1 .NE. 0) THEN
	    TPARM = UNDEFINED
	    DO 300 I = IND1 + MSC_KEYLEN(KEY), LEN(LINE)
	      IF (LINE(I:I) .NE. ' ') THEN
		TPARM = LINE(I:I+7)
		IND2  = INDEX(TPARM,'!')
C
		IF (IND2 .EQ. 0) THEN
		  PARM = TPARM
		ELSE
		  PARM = TPARM(1:IND2-1)
		ENDIF
C
		IF (KEY .EQ. MSC_CTRL_PARMS + 1) THEN	! AUXILIARY PORT ID
		  CTEMP_PARMS(KEY + AUX_CNT) = PARM
		  AUX_CNT = AUX_CNT + 1	
		ELSE
		  CTEMP_PARMS(KEY) = PARM
		ENDIF
C
		GOTO 200
	      ENDIF
300	    CONTINUE
	  ENDIF
400	CONTINUE
	GOTO 200
C
C GET AND VERIFY THE SWITCH COUNT.
C
500	CONTINUE
	VALUE = 0
	DO 600 I = 1, 8
	  CHAR = CTEMP_PARMS(SWITCH_CNT)(I:I)
	  IF (CHAR .NE. ' ') THEN
	    CALL ASCBIN(ICHAR, 1, 1, DIG, ST)
	    IF (ST .EQ. 0) VALUE = VALUE * 10 + DIG
	  ENDIF
600	CONTINUE
C
	IF (VALUE .LT. 1 .OR. VALUE .GT. MAX_SWITCHES) THEN
	  TYPE *, IAM(), 'INVALID NUMBER OF SWITCHES SPECIFIED IN ',
     *            CFG_FILE_NAME, VALUE
	  REPLY = -1
	  GOTO 9999
	ELSE
	  MSC_CONF_INFO(SWITCH_CNT) = VALUE
	ENDIF
C
C MAKE SURE EACH OF THE REQUIRED SWITCH NAMES WAS FOUND. 
C (CAN'T CHECK VALIDITY BUT CAN MAKE SURE SOMETHING WAS ENTERED)
C
	DO 700 I = SWITCH_NAME, SWITCH_NAME + VALUE - 1
	  IF (CTEMP_PARMS(I) .EQ. UNDEFINED) THEN
	    TYPE *, IAM(), MSC_KEYWORD(I),
     *              ' NOT FOUND IN ', CFG_FILE_NAME
	    REPLY = -1
  	  ELSE
	    CMSC_CONF_INFO(I) = CTEMP_PARMS(I)
	  ENDIF
700	CONTINUE
C
C MAKE SURE EACH OF THE REQUIRED REMAINING PARAMETERS WAS FOUND. 
C (CAN'T CHECK VALIDITY BUT CAN MAKE SURE SOMETHING WAS ENTERED)
C
	DO 800 I = USER_ID, MSC_CTRL_PARMS
	  IF (CTEMP_PARMS(I) .EQ. UNDEFINED) THEN
	    TYPE *, IAM(), MSC_KEYWORD(I),
     *              ' NOT FOUND IN ', CFG_FILE_NAME
	    REPLY = -1
  	  ELSE
	    CMSC_CONF_INFO(I) = CTEMP_PARMS(I)
	  ENDIF
800	CONTINUE
C
C GET AND VERIFY THE AUXILLIARY PORT COUNT.
C
	VALUE = 0
	DO 900 I = 1, 8
	  CHAR = CMSC_CONF_INFO(AUX_PORT_CNT)(I:I)
	  IF (CHAR .NE. ' ') THEN
	    CALL ASCBIN(ICHAR, 1, 1, DIG, ST)
	    IF (ST .EQ. 0) VALUE = VALUE * 10 + DIG
	  ENDIF
900	CONTINUE
C	
	IF (AUX_CNT .NE. VALUE .OR.
     *      AUX_CNT .GT. MAX_AUX_PORTS) THEN
	  TYPE *, IAM(), 'INVALID NUMBER OF AUX PORTS SPECIFIED IN ',
     *            CFG_FILE_NAME, VALUE, AUX_CNT
	  REPLY = -1
	ELSE
	  MSC_CONF_INFO(AUX_PORT_CNT) = AUX_CNT
	  DO 1000 I = 1, AUX_CNT
	    AUX_PORT_ID(I) = CTEMP_PARMS(MSC_CTRL_PARMS + I)
1000	  CONTINUE
	ENDIF
C
C CLOSE UP SHOP.
C
	CALL USRCLOS1(1)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT(A80)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
