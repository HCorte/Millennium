C
C SUBROUTINE X2STNADD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STNADD.FOV                                 $
C  $Date::   17 Apr 1996 16:35:50                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2stnadd.for **
C
C X2STNADD.FOR
C
C V03 21-OCT-94 GPR SET THE FIRST BIT OF THE BITMAP NOT THE LAST - 
C		    Integrate UK changes into X2X Baseline
C V02 22-AUG-94 GPR NO LONGER PROCESS PRTCNT AND POLTIM FOR STATION - 
C		    Integrate UK changes into X2X Baseline
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program will add a Station record to the
C station file.
C
C Calling sequence:
C
C     CALL X2STNADD(REC)
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
	SUBROUTINE X2STNADD(REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(64)  /64*0/      !Dummy record buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   ST,I,J                  !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT,ERR                 !Program exit/error
	INTEGER*4   INDX                    !String index
	INTEGER*4   NUMCHR, ENDOFF
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   CHRSTR(20)*1            !Output string
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
C OPEN THE STATION CLASS FILE.
C
	CALL OPENX(2,X2FILNAM(XSCL),4,0,0,ST)
	CALL IOINIT(X2XSCL_FDB,2,X2XSCL_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XSCL),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
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
	DO 100 I=2,X2XSTN_ENTRIES-1
110	  CONTINUE
	  IF(X2XSTN_FILFLDS(I).NE.0) GOTO 100
	  BOT=X2XSTN_RANGE(1,I)
	  TOP=X2XSTN_RANGE(2,I)
	  IF(BOT.GE.0) THEN
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	    ENDOFF=0
	    IF(X2XSTN_VALUE(1,I).NE.-1) THEN
	      DO 112 J=1,15
	        IF(X2XSTN_VALUE(J,I).NE.-1) THEN
	          ENDOFF=J
	        ENDIF
112	      CONTINUE
	      WRITE(5,9025) (X2XSTN_VALUE(J,I),J=1,ENDOFF)
	    ENDIF
C
C PROMPT FOR INPUT DATA.
C
	    WRITE (PROMPT2,9030) X2XSTN_FIELD(I),BOT,TOP
	    CALL INPNUM(PROMPT2,X2XSTN_REC(X2XSTN_INDEX(I)),
     *	                BOT,TOP,EXT)
	    IF(EXT.EQ.-9) THEN
	      CALL X2XHLP('X2STNFLD.HLP')
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
	    IF(X2XSTN_VALUE(1,I).NE.-1) THEN
	      VALID=.FALSE.
	      DO 120 J=1,15
	        IF(X2XSTN_REC(X2XSTN_INDEX(I)).EQ.X2XSTN_VALUE(J,I))
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
	    IF(I.EQ.13) THEN
	      CALL READW(X2XSCL_FDB,X2XSTN_STNCLS,X2XSCL_REC,ST)
	      IF(ST.NE.0) THEN
	        CALL OS32ER(5,X2FILNAM(XSCL),'READW',ST,X2XSTN_STNCLS)
	        CALL GPAUSE
	      ENDIF
	      IF(X2XSCL_REC(1).LE.0) THEN
	        WRITE(5,9090) X2XSTN_STNCLS, CHAR(7)
	        GOTO 110
	      ENDIF
	    ENDIF
C
C CHARACTER FIELD VARIABLE.
C
	  ELSE IF(BOT.EQ.-1) THEN
	    WRITE (PROMPT2,9032) X2XSTN_FIELD(I), TOP
	    CALL WIMG(5,PROMPT2)
	    READ(5,9060) CHRSTR
	    INDX=(X2XSTN_INDEX(I)-1)*4+1
	    DO 130 J=1,TOP
	      X2XSTN_CREC(INDX)=CHRSTR(J)
	      INDX=INDX+1
130	    CONTINUE
C
C BCD FIELD VARIABLE.
C
	  ELSE IF(BOT.EQ.-2) THEN
	    WRITE (PROMPT2,9032) X2XSTN_FIELD(I), TOP
	    CALL WIMG(5,PROMPT2)
	    READ(5,9060) CHRSTR
	    NUMCHR=0
	    DO 132 J=1,TOP
	      IF(CHRSTR(J).GT.' ') NUMCHR=NUMCHR+1
132	    CONTINUE
          CALL ATOH(CHRSTR,1,NUMCHR,X2XSTN_REC(X2XSTN_INDEX(I)),ERR)
	    IF(ERR.NE.0) THEN
	      WRITE(5,9080) CHAR(7)
	      GOTO 110
	    ENDIF
C
C HEX FIELD VALUE
C
          ELSE IF(BOT.EQ.-3) THEN
            WRITE (PROMPT2,9040) X2XSTN_FIELD(I), TOP
            CALL WIMG(5,PROMPT2)
            READ(5,9060) CHRSTR
            NUMCHR=0
            DO 232 J=1,TOP
              IF(CHRSTR(J).GT.' ') NUMCHR=NUMCHR+1
232         CONTINUE
          CALL ATOH(CHRSTR,1,NUMCHR,X2XSTN_REC(X2XSTN_INDEX(I)),ERR)
            IF(ERR.NE.0) THEN
              WRITE(5,9080) CHAR(7)
              GOTO 110
            ENDIF
	  ENDIF
100	CONTINUE
C
C FILL IN ALL DEFAULT FIELDS.
C
	X2XSTN_ADDLEN = X2XSCL_ADDLEN
CV02	X2XSTN_PROTO  = X2XSCL_PROTO
CV02	X2XSTN_PRTCNT = X2XSCL_PRTCNT
	X2XSTN_TYPE   = X2XSCL_TYPE
	X2XSTN_DELACK = 1		    !!!!X2XSCL_DELACK
	X2XSTN_ERRREP = 1		    !!!!X2XSCL_ERRREP
CV02	X2XSTN_STNDIS = X2XSCL_STNDIS
CV02	X2XSTN_FEDIS  = X2XSCL_FEDIS
CV02	X2XSTN_NETPT1 = X2XSCL_NETPT1
CV02	X2XSTN_NETPT2 = X2XSCL_NETPT2
CV02	X2XSTN_NETPT3 = X2XSCL_NETPT3
CV02	X2XSTN_NETPT4 = X2XSCL_NETPT4
CV02	X2XSTN_NETPT5 = X2XSCL_NETPT5
CV02	X2XSTN_NETPT6 = X2XSCL_NETPT6
CV02	X2XSTN_NETPT7 = X2XSCL_NETPT7
CV02	X2XSTN_POLTIM = X2XSCL_POLTIM
        X2XSTN_X32_PORT1 = X2XSCL_X32_PORT1
        X2XSTN_X32_PORT2 = X2XSCL_X32_PORT2
        X2XSTN_EVSN_LEN  = X2XSCL_EVSN_LEN
C
C UPDATE THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XSTN_UPDATE=DATBUF(VCDC)
	X2XSTN_STN=REC
CV03	X2XSTN_BITMAP=-1
	CALL X2BSET(X2XSTN_BITMAP,0,XSTN,REC)				!V03
C
C UPDATE THE FILE.
C
8000	CONTINUE
	IF(EXITUPD) THEN
	  CALL WRITEW(X2XSTN_FDB,REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XSTN),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	ELSE
	  CALL WRITEW(X2XSTN_FDB,REC,X2XSTN_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XSTN),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  CALL X2CHKMOD(XSTN,1)
	ENDIF
C
	CALL CLOSEFILE(X2XSCL_FDB)
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(T26,'GTECH Distributed Network',/,
     *	       T32,'Add Station',/)
9025	FORMAT(T12,'Specific values: ',13(I5,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9032	FORMAT(10(' '),'Enter ',A15,' char [',I2,' bytes] ',10(' '))
9040    FORMAT(10(' '),'Enter ',A15,' Hex Digits [',I2,'] ',10(' '))
9060	FORMAT(20A1)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
9080	FORMAT(1X,'Invalid Entry - try again',A)
9090	FORMAT(1X,'Station class ',I3,' does not exist ',A)
	END
