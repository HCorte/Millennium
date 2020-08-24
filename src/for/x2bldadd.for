C
C SUBROUTINE X2BLDADD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BLDADD.FOV                                 $
C  $Date::   17 Apr 1996 16:08:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2bldadd.for **
C
C X2BLDADD.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This program will add a Station record to the
C build file.
C
C Calling sequence:
C
C     CALL X2BLDADD(REC)
C
C Input parameters:
C
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
	SUBROUTINE X2BLDADD(REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XBLD.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(64)  /64*0/      !Dummy record buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   ST,I,J                  !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT,ERR                 !Program exit/error
	INTEGER*4   INDX                    !String index
	INTEGER*4   ENDOFF
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   CHRSTR(20)*1            !Output string
	CHARACTER   X2FILNAM*20             !File name function
	LOGICAL     VALID                   !Valid data flag
	LOGICAL     EXITUPD                 !Exit record update
	LOGICAL     X25STN                  !X25 station type
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
	CALL CLRSCR(5)
	EXITUPD=.FALSE.
	WRITE(5,9000)
	X25STN=.FALSE.
C
C DETERMINE THE CDC DATE FROM THE SYSTEM DATE.
C
	CALL XDAT(SYSDATE)
	DATBUF(VYEAR)=SYSDATE(1)
	DATBUF(VMON)=SYSDATE(2)
	DATBUF(VDAY)=SYSDATE(3)
	CALL BDATE(DATBUF)
C
C OPEN THE STATION CLASS FILE.
C
	CALL OPENX(2,X2FILNAM(XSCL),4,0,0,ST)
	CALL IOINIT(X2XSCL_FDB,2,X2XSCL_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XSCL),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C READ ALL REQUIRED FIELDS.
C
	DO 100 I=2,X2XBLD_ENTRIES-1
C
110	  CONTINUE
C
C IF THE STATION CLASS RECORD CONTAINS DIALUP PORTS, DO
C NOT PROMPT FOR THE DIAL/X.32 PORT.
C
          IF(I.EQ.8 .OR. I.EQ.9) THEN
            IF(X2XSCL_DIALENA.EQ.1) GOTO 100
          ENDIF
          IF(I.EQ.8 .AND. X2XSCL_DIAL_PORT1.NE.0) GOTO 100
          IF(I.EQ.9 .AND. X2XSCL_DIAL_PORT2.NE.0) GOTO 100
C
	  BOT=X2XBLD_RANGE(1,I)
	  TOP=X2XBLD_RANGE(2,I)
	  IF(BOT.GE.0) THEN
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	    ENDOFF=0
	    IF(X2XBLD_VALUE(1,I).NE.-1) THEN
	      DO 112 J=1,15
	        IF(X2XBLD_VALUE(J,I).NE.-1) THEN
	          ENDOFF=J
	        ENDIF
112	      CONTINUE
	      WRITE(5,9025) (X2XBLD_VALUE(J,I),J=1,ENDOFF)
	    ENDIF
C
C PROMPT FOR INPUT DATA.
C
	    WRITE (PROMPT2,9030) X2XBLD_FIELD(I),BOT,TOP
	    CALL INPNUM(PROMPT2,X2XBLD_REC(X2XBLD_INDEX(I)),
     *	                BOT,TOP,EXT)
	    IF(EXT.EQ.-9) THEN
	      CALL X2XHLP('X2BLDFLD.HLP')
	      CALL CLRSCR(5)
	      WRITE(5,9000)
	      GOTO 110
	    ELSE IF(EXT.LT.0) THEN
	      EXITUPD=.TRUE.
	      GOTO 8000
	    ENDIF
C
C CHECK TO ENSURE THE INPUT INFORMATION FAILS WITHIN ANY
C SPECIFIC FIELD VALUES.
C
	    IF(X2XBLD_VALUE(1,I).NE.-1) THEN
	      VALID=.FALSE.
	      DO 120 J=1,15
	        IF(X2XBLD_REC(X2XBLD_INDEX(I)).EQ.X2XBLD_VALUE(J,I))
     *	          VALID=.TRUE.
120	      CONTINUE
	      IF(.NOT.VALID) THEN
	        WRITE(5,9070) CHAR(7)
	        GOTO 110
	      ENDIF
	    ENDIF
C
C IF FIELD IS THE STATION CLASS, READ THE STATION CLASS RECORD,
C TO ALLOW DEFAULT FIELDS TO BE FILLED.
C
	    IF(I.EQ.5) THEN
	      CALL READW(X2XSCL_FDB,X2XBLD_STNCLS,X2XSCL_REC,ST)
	      IF(ST.NE.0) THEN
	        CALL OS32ER(5,X2FILNAM(XSCL),'READW',ST,X2XBLD_STNCLS)
	        CALL GPAUSE
	      ENDIF
	      IF(X2XSCL_REC(1).LE.0) THEN
	        WRITE(5,9090) X2XBLD_STNCLS, CHAR(7)
	        GOTO 110
	      ENDIF
	    ENDIF
	    IF(X2XSCL_TYPE.EQ.2) X25STN=.TRUE.
C
C CHARACTER FIELD VARIABLE.
C
	  ELSE IF(BOT.EQ.-1) THEN
	    WRITE (PROMPT2,9032) X2XBLD_FIELD(I), TOP
	    CALL WIMG(5,PROMPT2)
	    READ(5,9060) CHRSTR
	    INDX=(X2XBLD_INDEX(I)-1)*4+1
	    DO 130 J=1,TOP
	      X2XBLD_CREC(INDX)=CHRSTR(J)
	      INDX=INDX+1
130	    CONTINUE
C
C BCD FIELD VARIABLE.
C
	  ELSE IF(BOT.EQ.-2) THEN
	    WRITE (PROMPT2,9032) X2XBLD_FIELD(I), TOP
	    CALL WIMG(5,PROMPT2)
	    READ(5,9060) CHRSTR
	    CALL ATOH(CHRSTR,1,TOP,X2XBLD_REC(X2XBLD_INDEX(I)),ERR)
	    IF(ERR.NE.0) THEN
	      WRITE(5,9080) CHAR(7)
	      GOTO 110
	    ENDIF
	  ENDIF
100	CONTINUE
C
C IF X25 STATION UPDATE THE STATION AND PORT NUMBER.
C
C >>>> NOTE: THE VAR. X2XBLD_TERM1 AND _PORT1 ARE DECLARED IN X2XBLD.DEF
C            AND EQUIVALENCED TO OTHER VARIABLES. THEY DO NOT APPEAR TO
C            BE USED ANYWHERE ELSE IN X2X AND ARE NOT REFERENCED IN THIS
C            ROUTINE. TO ELIMINATE FLINT WARNING MESSAGES THE ARE HEREBY
C            DECLARED USELESS (DAS 11/26/91)
C
C.......IF(X25STN) THEN
C........ X2XBLD_TERM1 = REC
C.......  X2XBLD_PORT1 = 1
C.......ENDIF
C
C UPDATE THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XBLD_UPDATE=DATBUF(VCDC)
	X2XBLD_STN=REC
C
C UPDATE THE FILE.
C
8000	CONTINUE
	IF(EXITUPD) THEN
	  CALL WRITEW(X2XBLD_FDB,REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBLD),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	ELSE
	  CALL WRITEW(X2XBLD_FDB,REC,X2XBLD_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBLD),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  CALL X2CHKMOD(XBLD,1)
	ENDIF
C
	CALL CLOSEFILE(X2XSCL_FDB)
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(T26,'GTECH Distributed Network',/,
     *	       T29,'Auto Network Build',/)
9025	FORMAT(T12,'Specific values: ',13(I3,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9032	FORMAT(10(' '),'Enter ',A15,' char [',I2,' bytes] ',10(' '))
9060	FORMAT(20A1)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
9080	FORMAT(1X,'Invalid Entry - try again',A)
9090	FORMAT(1X,'Station class ',I3,' does not exist ',A)
	END
