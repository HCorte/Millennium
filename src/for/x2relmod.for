C
C SUBROUTINE X2RELMOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RELMOD.FOV                                 $
C  $Date::   17 Apr 1996 16:30:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C X2RELMOD.FOR
C
C V02 22-FEB-96 XXX INITIAL RELEASE 
C
C This program will modify a record in the reload configuration file
C Calling sequence:
C
C     CALL X2RELMOD(REC,UPDATE)
C
C Input parameters:
C
C     REC     Int*4   Record to be modified
C     UPDATE  LOGICAL INDICATES IF ANY FIELD IS UPDATED
C
C Output parameters:
C
C     NONE
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2RELMOD(REC,UPDATE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XRELOAD.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   ST,I,J                  !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT                     !Program exit/error
	INTEGER*4   FIELD                   !Field to update
	INTEGER*4   TEMP, ENDOFF
	INTEGER*4   MESNUM
	LOGICAL     UPDATE                  !Field update flag
	LOGICAL     VALID                   !Valid data flag
	LOGICAL     PRTFLG                  !Print flag
	CHARACTER   PROMPT*50               !Input prompt
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   CHRSTR(160)*1            !Output string
	INTEGER*4   ICHRSTR(40)		     !ARRAY TO BE EQUIVALENCED TO CHRSTR
	EQUIVALENCE (ICHRSTR,CHRSTR)
	CHARACTER*30 CRESET(5)
	DATA CRESET /'Terminal hard reset',
     *	             'News message',
     *	             'Terminal Ultimate reset',
     *		     'Terminal soft reset',
     *		     'Restart reset'/
C
        ST = 0
	UPDATE=.FALSE.
	PRTFLG=.FALSE.
	WRITE (PROMPT,9020) X2XRELOAD_ENTRIES-1
C
C DETERMINE THE CDC DATE FROM THE SYSTEM DATE.
C
	CALL XDAT(SYSDATE)
	DATBUF(VYEAR)=SYSDATE(1)
	DATBUF(VMON)=SYSDATE(2)
	DATBUF(VDAY)=SYSDATE(3)
	CALL BDATE(DATBUF)
C
C DISPLAY ALL GLOBAL INFORMATION PRINTING THREE VARIABLES
C PER LINE.
C
50	CONTINUE
C
	CALL CLRSCR(5)
C
	WRITE(5,9000) REC
c
	DO 100 J=1,X2XRELOAD_ENTRIES-1	
	   IF (X2XRELOAD_RANGE(1,J).EQ.-2) THEN
	      CALL FASTMOV(X2XRELOAD_REC(X2XRELOAD_INDEX(J)),ICHRSTR,160/4)
	      WRITE(5,9040) J,X2XRELOAD_FIELD(J),
     *	                  (CHRSTR(I),I=1,40)
	   ELSE IF (J.EQ.8.AND.
     *		   X2XRELOAD_REC(X2XRELOAD_INDEX(J)).LT.6.AND.
     *		   X2XRELOAD_REC(X2XRELOAD_INDEX(J)).GT.0) THEN
	      WRITE(5,9090) J,X2XRELOAD_FIELD(J),
     *	                  CRESET(X2XRELOAD_REC(X2XRELOAD_INDEX(J)))
	   ELSE
	      WRITE(5,9010) J,X2XRELOAD_FIELD(J),
     *	                  X2XRELOAD_REC(X2XRELOAD_INDEX(J))
	   ENDIF

100	CONTINUE

C
C PROMPT FOR FIELD TO BE MODIFIED.
C
	WRITE(5,*)
	CALL INPNUM(PROMPT,FIELD,1,X2XRELOAD_ENTRIES-1,EXT)
C
	IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C FIELD UPDATE.
C NOTE: FOR CHARACTER VARIABLES THE BOTTOM RANGE WILL CONTAIN
C       A -1 AND THE TOP RANGE CONTAINS THE NUMBER OF BYTES.
C
110	CONTINUE
	IF (FIELD.EQ.8) THEN
	    CALL CLRSCR(5)
	    WRITE(5,9000) REC
	    CALL SET_CURSOR(10)
C
	    TYPE 9100,'1. - Terminal hard reset'
	    TYPE 9100,'2. - News message'
	    TYPE 9100,'3. - Terminal Ultimate reset'
	    TYPE 9100,'4. - Terminal soft reset'
	    TYPE 9100,'5. - Restart reset'
	    TYPE 9100,'6. - Numeric message no.'
C
	    WRITE(5,*)
	    CALL INPNUM('         Enter option',MESNUM,1,6,EXT)
	    IF (MESNUM.EQ.6) THEN
		CALL INPNUM('         Enter message no.',MESNUM,0,500,EXT)
	    ENDIF
C
	    X2XRELOAD_MESSAGE_NO = MESNUM
	    GOTO 50
	ENDIF
C

	BOT=X2XRELOAD_RANGE(1,FIELD)
	TOP=X2XRELOAD_RANGE(2,FIELD)
	IF(BOT.GE.0) THEN
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	  ENDOFF=0
	  IF(X2XRELOAD_VALUE(1,FIELD).NE.-1) THEN
	    DO 112 I=1,15
	      IF(X2XRELOAD_VALUE(I,FIELD).NE.-1) THEN
	        ENDOFF=I
	      ENDIF
112	    CONTINUE
	    WRITE(5,9025) (X2XRELOAD_VALUE(I,FIELD),I=1,ENDOFF)
	  ENDIF
C
C PROMPT FOR INPUT DATA.
C
	  WRITE (PROMPT2,9030) X2XRELOAD_FIELD(FIELD),BOT,TOP
	  CALL INPNUM(PROMPT2,TEMP,BOT,TOP,EXT)
	  IF(EXT.LT.0) THEN
	    GOTO 50
	  ENDIF
C
C CHECK TO ENSURE THE INPUT INFORMATION FALLS WITHIN ANY
C SPECIFIC FIELD VALUES.
C
	  IF(X2XRELOAD_VALUE(1,FIELD).NE.-1) THEN
	    VALID=.FALSE.
	    DO 120 J=1,15
	      IF(TEMP.EQ.X2XRELOAD_VALUE(J,FIELD)) VALID=.TRUE.
120	    CONTINUE
	    IF(.NOT.VALID) THEN
	      WRITE(5,9070) CHAR(7)
	      GOTO 110
	    ENDIF
	  ENDIF
	  X2XRELOAD_REC(X2XRELOAD_INDEX(FIELD))=TEMP
C
C BCD FIELD.
C
	ELSE IF(BOT.EQ.-2.AND.X2XRELOAD_REC(FIELD-1).NE.0) THEN
	  WRITE (PROMPT2,9032) X2XRELOAD_FIELD(FIELD), 
     *			       X2XRELOAD_REC(X2XRELOAD_INDEX(FIELD-1))
	  CALL WIMG(5,PROMPT2)
	  READ(5,9060) CHRSTR

	  CALL FASTMOV(ICHRSTR,X2XRELOAD_REC(X2XRELOAD_INDEX(FIELD)),
     *		       BOT/4)
	ENDIF
C
C UPDATE THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XRELOAD_UPDATE=DATBUF(VCDC)
	UPDATE=.TRUE.
	GOTO 50
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T25,'Modify X2RELOAD configuration',//,
     *		  T32,'Subnet ',I2)
9010	FORMAT(T15,I2.2,'.',1X,A30,1X,T55,I10,2X)
9020	FORMAT(10(' '),'Enter number of field to update [2..',I2.2,
     *	               '] ')
9025	FORMAT(T12,'Specific values: ',13(I3,1X))
9030	FORMAT(3(' '),'Enter ',A30,' range [',I8,'-',I8,'] ')
9032	FORMAT(3(' '),'Enter ',A,' char [',I3,' bytes] ')
9040    FORMAT(T15,I2.2,'.',1X,A30,40A1)
9060	FORMAT(160A1)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
9080	FORMAT(1X,'Invalid Entry - try again',A)
9090    FORMAT(T15,I2.2,'.',1X,A30,A)
9100	FORMAT(T15,A)
	END
