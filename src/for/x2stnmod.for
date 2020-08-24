C
C SUBROUTINE X2STNMOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STNMOD.FOV                                 $
C  $Date::   17 Apr 1996 16:36:28                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2stnmod.for **
C
C X2STNMOD.FOR
C
C V06 22-DEC-94 DAS Fixed back and next commands
C V05 12-DEC-94 DAS Integrate UK changes into X2X Baseline
C V04 01-NOV-94 GPR Add description for ! and $
C V03 21-OCT-94 GPR SET THE BIT EVEN IF THE RECORD IS ADDED
C V02 27-SEP-94 GPR ADJUST FOR REMOVED FIELDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program will modify a Station record.
C
C Calling sequence:
C
C     CALL X2STNMOD(REC)
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
	SUBROUTINE X2STNMOD(REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   ST,I,J                  !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT,ERR                 !Program exit/error
	INTEGER*4   FIELD                   !Field to update
	INTEGER*4   NUMCHR, TEMP, ENDFLD, BEGFLD, K, ENDOFF
	INTEGER*4   BEGOFF, PAGE, MAXPAGE
	LOGICAL     UPDATE                  !Field update flag
	LOGICAL     VALID                   !Valid data flag
	LOGICAL     PRTFLG                  !Print flag
	CHARACTER   PROMPT*50               !Input prompt
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   CHRSTR(20)*1            !Output string
	CHARACTER   X2FILNAM*20             !File name function
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
        ST = 0
	UPDATE=.FALSE.
	PRTFLG=.FALSE.
	CALL CLRSCR(5)
	CALL FASTMOV(X2XSTN_REC,DMYBUF,X2XSTN_SECT*64)
	MAXPAGE=X2XSTN_ENTRIES/30
	IF(MOD(X2XSTN_ENTRIES,30).NE.0) MAXPAGE=MAXPAGE+1
	PAGE=1
	WRITE(5,9000) PAGE,MAXPAGE
	IF(PRTFLG) WRITE(6,9000) PAGE,MAXPAGE
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
	BEGOFF=MAX0(1,(PAGE-1)*30+1)
	ENDOFF=MIN0(BEGOFF-1+30,X2XSTN_ENTRIES)
	DO 100 I=BEGOFF,ENDOFF,2
C
C NOTE: OFFSET 3 IS HARDCODED TO ALLOW FIELD 3 (ADDRESS)
C       TO BE CORRECTLY DISPLAYED.
C
	  IF(I.EQ.3) THEN
	    CALL HTOA(CHRSTR,1,X2XSTN_ADDLEN,X2XSTN_ADDRES,ERR)
	    DO 102 J=X2XSTN_ADDLEN+1,20
	      CHRSTR(J)=' '
102	    CONTINUE
	    WRITE(5,9040) I,X2XSTN_FIELD(I),(CHRSTR(K),K=1,16),
     *	                I+1,X2XSTN_FIELD(I+1),
     *	                    X2XSTN_REC(X2XSTN_INDEX(I+1))
	    IF(PRTFLG)
     *	      WRITE(6,9040) I,X2XSTN_FIELD(I),(CHRSTR(K),K=1,16),
     *	                  I+1,X2XSTN_FIELD(I+1),
     *	                      X2XSTN_REC(X2XSTN_INDEX(I+1))
C
C NOTE: OFFSET 37 IS HARDCODED TO ALLOW FIELD 37 (Extended verifcation
C       Sequence number) TO BE CORRECTLY DISPLAYED.
C
          ELSE IF(I.EQ.37) THEN					      !V02
            CALL HTOA(CHRSTR,1,X2XSTN_EVSN_LEN,X2XSTN_EVSN,ERR)
            DO 202 J=X2XSTN_EVSN_LEN+1,20
              CHRSTR(J)=' '
202         CONTINUE
            WRITE(5,9050) I,X2XSTN_FIELD(I),(CHRSTR(K),K=1,12),
     *                    I+1,X2XSTN_FIELD(I+1),
     *                    X2XSTN_REC(X2XSTN_INDEX(I+1))
            IF(PRTFLG)
     *         WRITE(6,9050) I,X2XSTN_FIELD(I),(CHRSTR(K),K=1,12),
     *                    I+1,X2XSTN_FIELD(I+1),
     *                    X2XSTN_REC(X2XSTN_INDEX(I+1))
	  ELSE
	     WRITE(5,9010) (J,X2XSTN_FIELD(J),
     *	                      X2XSTN_REC(X2XSTN_INDEX(J)),
     *	                     J=I,MIN0(I+1,X2XSTN_ENTRIES))
	     IF(PRTFLG)
     *	       WRITE(6,9010) (J,X2XSTN_FIELD(J),
     *	                        X2XSTN_REC(X2XSTN_INDEX(J)),
     *	                      J=I,MIN0(I+1,X2XSTN_ENTRIES))
	  ENDIF
100	CONTINUE
C
C IF PRINT ENABLED TURN IT OFF.
C
	IF(PRTFLG) THEN
	  PRTFLG=.FALSE.
	  CALL USRCLOS1(6)
	ENDIF
C
C PROMPT FOR FIELD TO BE MODIFIED.
C
	WRITE(5,*)
	BEGFLD=MAX0(2,BEGOFF)
	ENDFLD=ENDOFF
	IF(ENDFLD.GE.X2XSTN_ENTRIES) ENDFLD=X2XSTN_ENTRIES-1
        WRITE (5,9022)                                                     !V04
	WRITE (PROMPT,9020) BEGFLD, ENDFLD
	CALL INPNUM(PROMPT,FIELD,2,X2XSTN_ENTRIES-1,EXT)
	IF(EXT.EQ.-2) THEN
	  PRTFLG=.TRUE.
	  OPEN(6, FILE='X2STNMOD.REP', STATUS='NEW', DISP='PRINT/DELETE')
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,'PR:','OPEN',ST,0)
	    CALL GPAUSE
	  ENDIF
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  WRITE(6,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.EQ.-7) THEN           ! BACK
          PAGE=PAGE-1
          IF(PAGE.LE.0) PAGE = MAXPAGE	 
          CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.EQ.-8) THEN           ! NEXT
	  PAGE=PAGE+1
          IF(PAGE.GT.MAXPAGE) PAGE = 1
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.EQ.-9) THEN
	  CALL X2XHLP('X2STNFLD.HLP')
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C FIELD UPDATE.
C NOTE: FOR CHARACTER VARIABLES THE BOTTOM RANGE WILL CONTAIN
C       A -1 AND THE TOP RANGE CONTAINS THE NUMBER OF BYTES.
C
110	CONTINUE
	BOT=X2XSTN_RANGE(1,FIELD)
	TOP=X2XSTN_RANGE(2,FIELD)
	IF(BOT.GE.0) THEN
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	  ENDOFF=0
	  IF(X2XSTN_VALUE(1,FIELD).NE.-1) THEN
	    DO 112 I=1,15
	      IF(X2XSTN_VALUE(I,FIELD).NE.-1) THEN
	        ENDOFF=I
	      ENDIF
112	    CONTINUE
	    WRITE(5,9025) (X2XSTN_VALUE(I,FIELD),I=1,ENDOFF)
	  ENDIF
C
C PROMPT FOR INPUT DATA.
C
	  WRITE (PROMPT2,9030) X2XSTN_FIELD(FIELD),BOT,TOP
	  CALL INPNUM(PROMPT2,TEMP,BOT,TOP,EXT)
	  IF(EXT.LT.0) THEN
	    CALL CLRSCR(5)
	    WRITE(5,9000) PAGE,MAXPAGE
	    GOTO 50
	  ENDIF
C
C CHECK TO ENSURE THE INPUT INFORMATION FAILS WITHIN ANY
C SPECIFIC FIELD VALUES.
C
	  IF(X2XSTN_VALUE(1,FIELD).NE.-1) THEN
	    VALID=.FALSE.
	    DO 120 J=1,15
	      IF(TEMP.EQ.X2XSTN_VALUE(J,FIELD)) VALID=.TRUE.
120	    CONTINUE
	    IF(.NOT.VALID) THEN
	      WRITE(5,9070) CHAR(7)
	      GOTO 110
	    ENDIF
	  ENDIF
	  X2XSTN_REC(X2XSTN_INDEX(FIELD))=TEMP
C
C BCD FIELD.
C
	ELSE IF(BOT.EQ.-2) THEN
	  WRITE (PROMPT2,9032) X2XSTN_FIELD(FIELD), TOP
	  CALL WIMG(5,PROMPT2)
	  READ(5,9060) CHRSTR
	  IF(CHRSTR(1).EQ.'E' .OR. CHRSTR(1).EQ.'e') THEN
	    CALL CLRSCR(5)
	    WRITE(5,9000) PAGE,MAXPAGE
	    GOTO 50
	  ENDIF
C
	  NUMCHR=0
	  DO 122 I=1,TOP
	    IF(CHRSTR(I).GT.' ') NUMCHR=NUMCHR+1
122	  CONTINUE
	  IF(FIELD.EQ.3)
     *	    NUMCHR=MIN0(NUMCHR,X2XSTN_ADDLEN)
        CALL ATOH(CHRSTR,1,NUMCHR,X2XSTN_REC(X2XSTN_INDEX(FIELD)),ERR)
	  IF(ERR.NE.0) THEN
	    WRITE(5,9080) CHAR(7)
	    GOTO 110
	  ENDIF
C
C HEX FIELD.
C
        ELSE IF(BOT.EQ.-3) THEN
          WRITE (PROMPT2,9035) X2XSTN_FIELD(FIELD), TOP
          CALL WIMG(5,PROMPT2)
          READ(5,9060) CHRSTR
          IF(CHRSTR(1).EQ.'E' .OR. CHRSTR(1).EQ.'e') THEN
            CALL CLRSCR(5)
            WRITE(5,9000) PAGE,MAXPAGE
            GOTO 50
          ENDIF
C
          NUMCHR=0
          DO 132 I=1,TOP
            IF(CHRSTR(I).GT.' ') NUMCHR=NUMCHR+1
132       CONTINUE
        CALL ATOH(CHRSTR,1,NUMCHR,X2XSTN_REC(X2XSTN_INDEX(FIELD)),ERR)
          IF(ERR.NE.0) THEN
            WRITE(5,9080) CHAR(7)
            GOTO 110
          ENDIF
        ENDIF
C
C UPDATE THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XSTN_UPDATE=DATBUF(VCDC)
	CALL X2BSET(X2XSTN_BITMAP,FIELD,XSTN,REC)			    !V03
	UPDATE=.TRUE.
	CALL CLRSCR(5)
	WRITE(5,9000) PAGE,MAXPAGE
	GOTO 50
C
C PROGRAM EXIT.
C
8000	CONTINUE
C
C IF A FIELD HAS BEEN MODIFIED UPDATE THE FILE.
C
	IF(UPDATE) THEN
	  CALL WRITEW(X2XSTN_FDB,REC,X2XSTN_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XSTN),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  CALL X2CHKMOD(XSTN,1)
C
C RELEASE RECORD LOCK.
C
	ELSE
	  CALL WRITEW(X2XSTN_FDB,REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XSTN),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	ENDIF
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(/,T26,'GTECH Distributed Network',T63,
     *	             'Page ',I2,' of ',I2,/,
     *	         T28,'Modify Station Record',/)
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9020	FORMAT(10(' '),'Enter number of field to update [',I2,
     *	       '-',I2,'] ')
9022    FORMAT(9(' '),'! RUNSYS is required to update; ',               !V04
     *         '$ X2BLDNET is required to update',/)
9025	FORMAT(T12,'Specific values: ',13(I5,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9032	FORMAT(10(' '),'Enter ',A15,' char [',I2,' bytes] ',10(' '))
9035    FORMAT(10(' '),'Enter ',A15,' Hex Digits [',I2,'] ',10(' '))
9040	FORMAT(T10,I2.2,'.',1X,A10,1X,16A,1X,
     *	           I2.2,'.',1X,A15,1X,I10)
9050    FORMAT(T10,I2.2,'.',1X,A15,12A,1X,
     *             I2.2,'.',1X,A13,1X,I10)
9060	FORMAT(20A1)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
9080	FORMAT(1X,'Invalid Entry - try again',A)
	END
