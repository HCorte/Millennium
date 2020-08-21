C
C SUBROUTINE X2SPCADD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SPCADD.FOV                                 $
C  $Date::   17 Apr 1996 16:35:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2spcadd.for;1 **
C
C X2SPCADD.FOR
C
C V03 31-JUL-95 DAS ADDED CALL TO X2CNVDRP                                   
C V02 21-OCT-94 GPR SET THE FIRST BIT OF THE BITMAP NOT THE LAST - Integrate 
C		    UK changes into X2X Baseline
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program will add a port to a station.
C
C Calling sequence:
C
C     CALL X2SPCADD(STATION,PORT,REC)
C
C Input parameters:
C
C     STATION Int*4   Station number
C     PORT    Int*4   Port number
C     REC     Int*4   Record to be modified
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
	SUBROUTINE X2SPCADD(STATION,PORT,REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   STATION                 !Station number
	INTEGER*4   PORT                    !Port number
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   ST,I,J                  !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT                     !Program exit/error
	INTEGER*4   INDX                    !String index
	INTEGER*4   ENDOFF
	INTEGER*4   DROP_INDEX
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   CHRSTR(20)*1            !Output string
	LOGICAL     VALID                   !Valid data flag
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
	CALL CLRSCR(5)
	WRITE(5,9000)
C
C DETERMINE THE CDC DATE FROM THE SYSTEM DATE.
C
	CALL XDAT(SYSDATE)
	DATBUF(VYEAR)=SYSDATE(1)
	DATBUF(VMON)=SYSDATE(2)
	DATBUF(VDAY)=SYSDATE(3)
	CALL BDATE(DATBUF)
C
C BLANK OUT THE DROP ARRAY.
C
	DO 50 I=1,X2X_MAXTERMS
	  X2XSPC_DROPS(I)='  '
50	CONTINUE
C
	DO 100 I=3,X2XSPC_ENTRIES-1
110	  CONTINUE
	  BOT=X2XSPC_RANGE(1,I)
	  TOP=X2XSPC_RANGE(2,I)
	  IF(BOT.GE.0) THEN
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	    ENDOFF=0
	    IF(X2XSPC_VALUE(1,I).NE.-1) THEN
	      DO 112 J=1,15
	        IF(X2XSPC_VALUE(J,I).NE.-1) THEN
	          ENDOFF=J
	        ENDIF
112	      CONTINUE
	      WRITE(5,9025) (X2XSPC_VALUE(J,I),J=1,ENDOFF)
	    ENDIF
C
C PROMPT FOR INPUT DATA.
C
	    WRITE (PROMPT2,9030) X2XSPC_FIELD(I),BOT,TOP
	    CALL INPNUM(PROMPT2,X2XSPC_REC(X2XSPC_INDEX(I)),
     *	                BOT,TOP,EXT)
	    IF(EXT.EQ.-9) THEN
	      CALL X2XHLP('X2SPCFLD.HLP')
	      CALL CLRSCR(5)
	      WRITE(5,9000)
	      GOTO 110
	    ELSE IF(EXT.LT.0) THEN
	      GOTO 8000
	    ENDIF
C
C CHECK TO ENSURE THE INPUT INFORMATION FAILS WITHIN ANY
C SPECIFIC FIELD VALUES.
C
	    IF(X2XSPC_VALUE(1,I).NE.-1) THEN
	      VALID=.FALSE.
	      DO 120 J=1,15
	        IF(X2XSPC_REC(X2XSPC_INDEX(I)).EQ.X2XSPC_VALUE(J,I))
     *	          VALID=.TRUE.
120	      CONTINUE
	      IF(.NOT.VALID) THEN
	        WRITE(5,9070) CHAR(7)
	        GOTO 110
	      ENDIF
	    ENDIF
C
C CHARACTER FIELD VARIABLE.
C
	  ELSE IF(BOT.EQ.-1) THEN
	    WRITE (PROMPT2,9032) X2XSPC_FIELD(I), TOP
	    CALL WIMG(5,PROMPT2)
	    READ(5,9060) CHRSTR
C
C IF DROP ADDRESS CHECK FOR VALID RANGE.
C
	    IF(I.GE.4 .AND. I.LE.39) THEN
 	      IF(CHRSTR(1).EQ.' ' .AND. CHRSTR(2).EQ.' ') GOTO 100
C
C CHECK FOR NORMAL ADDRESSING.
C
	      CALL X2CNVDRP(CHRSTR,DROP_INDEX)
              IF(DROP_INDEX.LT.0) THEN
                  WRITE(5,9070) CHAR(7)
                  GOTO 110
              ENDIF
C
C VALID DROP HAS BEEN ENTERED, NOW UPDATE RECORD.
C
	      IF(X2XSPC_DROPS(DROP_INDEX).EQ.CHRSTR(1)//CHRSTR(2)) THEN
	        X2XSPC_DROPS(DROP_INDEX)='  '
	      ELSE
	        X2XSPC_DROPS(DROP_INDEX)=CHRSTR(1)//CHRSTR(2)
	      ENDIF
C
C NORMAL CHARACTER FIELD.
C
	    ELSE
	      IF(CHRSTR(1).EQ.'E' .OR. CHRSTR(1).EQ.'e') GOTO 8000
	      INDX=(X2XSPC_INDEX(I)-1)*4+1
	      DO 130 J=1,TOP
	        X2XSPC_CREC(INDX)=CHRSTR(J)
	        INDX=INDX+1
130	      CONTINUE
	    ENDIF
	  ENDIF
100	CONTINUE
C
C UPDATE THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XSPC_UPDATE=DATBUF(VCDC)
	X2XSPC_STN=STATION
	X2XSPC_PORT=PORT
CV02	X2XSPC_BITMAP=-1
	CALL X2BSET(X2XSPC_BITMAP,0,XSPC,REC)				!V02
C
C UPDATE THE FILE.
C
	CALL WRITX2X(4,REC,X2XSPC_REC,ST)
	CALL X2CHKMOD(XSPC,1)
C
C PROGRAM EXIT
C
8000	CONTINUE
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T26,'Add a Port to a Station',//)
9025	FORMAT(T12,'Specific values: ',13(I3,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9032	FORMAT(10(' '),'Enter ',A15,' char [',I2,' bytes] ',10(' '))
9060	FORMAT(20A1)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
	END
