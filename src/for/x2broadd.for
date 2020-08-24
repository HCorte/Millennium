C
C SUBROUTINE X2BROADD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BROADD.FOV                                 $
C  $Date::   17 Apr 1996 16:10:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2broadd.for **
C
C X2BROADD.FOR
C
C V03 30-DEC-94 GPR Don't use -1, -2 from the range to determine
C		    if character or BCD, no longer required
C V02 21-OCT-94 GPR SET THE FIRST BIT OF THE BITMAP NOT THE LAST - Integrate
C		    UK changes into X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This program will add a relay application record.
C
C Calling sequence:
C
C     CALL X2BROADD(REC)
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
	SUBROUTINE X2BROADD(REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XBRO.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(64)  /64*0/      !Dummy record buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   ST,I,J                  !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT                     !Program exit/error
	INTEGER*4   ENDOFF
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   X2FILNAM*20             !File name function
	LOGICAL     VALID                   !Valid data flag
	LOGICAL     EXITUPD                 !Exit record update
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
	CALL CLRSCR(5)
	EXITUPD=.FALSE.
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
C READ ALL REQUIRED FIELDS.
C
	DO 100 I=2,X2XBRO_ENTRIES-1
C
110	  CONTINUE
	  BOT=X2XBRO_RANGE(1,I)
	  TOP=X2XBRO_RANGE(2,I)
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	    ENDOFF=0
	    IF(X2XBRO_VALUE(1,I).NE.-1) THEN
	      DO 112 J=1,15
	        IF(X2XBRO_VALUE(J,I).NE.-1) THEN
	          ENDOFF=J
	        ENDIF
112	      CONTINUE
	      WRITE(5,9025) (X2XBRO_VALUE(J,I),J=1,ENDOFF)
	    ENDIF
C
C PROMPT FOR INPUT DATA.
C
	    WRITE (PROMPT2,9030) X2XBRO_FIELD(I),BOT,TOP
	    CALL INPNUM(PROMPT2,X2XBRO_REC(X2XBRO_INDEX(I)),
     *	                BOT,TOP,EXT)
	    IF(EXT.EQ.-9) THEN
	      CALL X2XHLP('X2BROFLD.HLP')
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
	    IF(X2XBRO_VALUE(1,I).NE.-1) THEN
	      VALID=.FALSE.
	      DO 120 J=1,15
	        IF(X2XBRO_REC(X2XBRO_INDEX(I)).EQ.X2XBRO_VALUE(J,I))
     *	          VALID=.TRUE.
120	      CONTINUE
	      IF(.NOT.VALID) THEN
	        WRITE(5,9070) CHAR(7)
	        GOTO 110
	      ENDIF
	    ENDIF
100	CONTINUE
C
C UPDATE THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XBRO_UPDATE=DATBUF(VCDC)
	X2XBRO_REC(1)=REC
CV02	X2XBRO_BITMAP=-1
	CALL X2BSET(X2XBRO_BITMAP,0,XBRO,REC)				 !V02
C
C UPDATE THE FILE.
C
8000	CONTINUE
	IF(EXITUPD) THEN
	  CALL WRITEW(X2XBRO_FDB,REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBRO),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	ELSE
	  CALL WRITEW(X2XBRO_FDB,REC,X2XBRO_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBRO),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  CALL X2CHKMOD(XBRO,1)
	ENDIF
C
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(T26,'GTECH Distributed Network',/,
     *	       T29,'Relay Applications',/)
9025	FORMAT(T12,'Specific values: ',13(I3,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9032	FORMAT(10(' '),'Enter ',A15,' char [',I2,' bytes] ',10(' '))
9060	FORMAT(20A1)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
9080	FORMAT(1X,'Invalid Entry - try again',A)
	END
