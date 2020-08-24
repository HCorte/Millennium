C
C SUBROUTINE X2GBLMOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GBLMOD.FOV                                 $
C  $Date::   17 Apr 1996 16:18:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2gblmod.for;1 **
C
C X2GBLMOD.FOR
C
C V05 27-DEC-94 DAS Fixed problem with back and next commands
C V04 13-DEC-94 GPR Integrate UK changes into X2X Baseline
C V03 01-NOV-94 GPR Add description for ! and $
C V02 21-OCT-94 GPR SET THE BIT EVEN IF THE RECORD IS ADDED
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This program will update the X2X Global parameters file.
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
	SUBROUTINE X2GBLMOD
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XGBL.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   ST,I,J                  !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT                     !Program exit
	INTEGER*4   FIELD                   !Field to update
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   TEMP, ENDFLD, BEGFLD, ENDOFF, BEGOFF, PAGE, MAXPAGE
C
	LOGICAL     UPDATE                  !Field update flag
	LOGICAL     VALID                   !Field valid flag
	LOGICAL     PRTFLG                  !Print screen flag
	CHARACTER   PROMPT*50               !Input prompt
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   X2FILNAM*20             !File name function

	INTEGER*4   STATUS						!V04
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
        ST = 0
	UPDATE=.FALSE.
	PRTFLG=.FALSE.
	CALL CLRSCR(5)
	CALL FASTMOV(X2XGBL_REC,DMYBUF,X2XGBL_SECT*64)
	MAXPAGE=X2XGBL_ENTRIES/30
	IF(MOD(X2XGBL_ENTRIES,30).NE.0) MAXPAGE=MAXPAGE+1
	PAGE=1
	WRITE(5,9000) PAGE,MAXPAGE
	IF(PRTFLG) WRITE(6,9000) PAGE,MAXPAGE
	CALL FASTMOV(X2XGBL_REC,DMYBUF,X2XGBL_SECT*64)
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
	ENDOFF=MIN0(BEGOFF-1+30,X2XGBL_ENTRIES)
	DO 100 I=BEGOFF,ENDOFF,2
	  WRITE(5,9010) (J,X2XGBL_FIELD(J),
     *	                   X2XGBL_REC(X2XGBL_INDEX(J)),
     *	                 J=I,MIN0(I+1,X2XGBL_ENTRIES))
	  IF(PRTFLG)
     *	    WRITE(6,9010) (J,X2XGBL_FIELD(J),
     *	                     X2XGBL_REC(X2XGBL_INDEX(J)),
     *	                   J=I,MIN0(I+1,X2XGBL_ENTRIES))
100	CONTINUE
C
C IF PRINT FLAG IS SET, TURN IT OFF.
C
	IF(PRTFLG) THEN
	  PRTFLG=.FALSE.
	  CALL USRCLOS1(6)
	ENDIF
C
C PROMPT FOR FIELD TO BE MODIFIED.
C
	WRITE(5,*)
	BEGFLD=MAX0(1,BEGOFF)
	ENDFLD=ENDOFF
	IF(ENDFLD.GE.X2XGBL_ENTRIES) ENDFLD=X2XGBL_ENTRIES-1
	WRITE (5,9040)							   !V03
	WRITE (PROMPT,9020) BEGFLD, ENDFLD				   !V04
	CALL INPNUM(PROMPT,FIELD,1,X2XGBL_ENTRIES-1,EXT)
	IF(EXT.EQ.-2) THEN
	  PRTFLG=.TRUE.
	  OPEN(6, FILE='X2GBLMOD.REP', STATUS='NEW', DISP='PRINT/DELETE')
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,'PR:','OPEN',ST,0)
	    CALL GPAUSE
	  ENDIF
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  WRITE(6,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.EQ.-7) THEN
	  PAGE=PAGE-1
          IF(PAGE .LE. 0) PAGE = MAXPAGE      !BACK
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.EQ.-8) THEN
	  PAGE=PAGE+1
          IF(PAGE .GT. MAXPAGE) PAGE = 1      !NEXT
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.EQ.-9) THEN
	  CALL X2XHLP('X2GBLMOD.HLP/2')
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C FIELD UPDATE.
C
110	CONTINUE
C
C	CHECK FOR THE FIELD DESCRIPTION					!V04
C
	CALL X2CHKFLD(X2XGBL_FIELD(FIELD),STATUS)			!V04
	IF (STATUS.NE.0) GOTO 50					!V04
C
	BOT=X2XGBL_RANGE(1,FIELD)
	TOP=X2XGBL_RANGE(2,FIELD)
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	ENDOFF=0
	IF(X2XGBL_VALUE(1,FIELD).NE.-1) THEN
	  DO 112 I=1,15
	    IF(X2XGBL_VALUE(I,FIELD).NE.-1) THEN
	      ENDOFF=I
	    ENDIF
112	  CONTINUE
	  WRITE(5,9025) (X2XGBL_VALUE(I,FIELD),I=1,ENDOFF)
	ENDIF
C
C PROMPT FOR INPUT DATA.
C
	WRITE (PROMPT2,9030) X2XGBL_FIELD(FIELD),BOT,TOP
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
	IF(X2XGBL_VALUE(1,FIELD).NE.-1) THEN
	  VALID=.FALSE.
	  DO 120 J=1,15
	    IF(TEMP.EQ.X2XGBL_VALUE(J,FIELD)) VALID=.TRUE.
120	  CONTINUE
	  IF(.NOT.VALID) THEN
	    WRITE(5,9070) CHAR(7)
	    GOTO 110
	  ENDIF
	ENDIF
	X2XGBL_REC(X2XGBL_INDEX(FIELD))=TEMP
C
C SET THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XGBL_UPDATE=DATBUF(VCDC)
	UPDATE=.TRUE.
	CALL X2BSET(X2XGBL_BITMAP,FIELD,XGBL,1)				    !V02
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
	  CALL WRITEW(X2XGBL_FDB,1,X2XGBL_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XGBL),'WRITEW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  CALL X2CHKMOD(XGBL,1)
C
C RELEASE RECORD LOCK.
C
	ELSE
	  CALL WRITEW(X2XGBL_FDB,1,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XGBL),'WRITEW',ST,1)
	    CALL GPAUSE
	  ENDIF
	ENDIF
	RETURN
C
C     ================== Format Statements =====================
C
 
9000	FORMAT(/,T26,'GTECH Distributed Network',T63,			  !V04
     *	              'Page ',I2,' of ',I2,/,
     *	          T24,'Global Network Configuration',/)
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9020	FORMAT(10(' '),'Enter number of field to update [',I2,
     *	       '-',I2,'] ')
9025	FORMAT(T12,'Specific values: ',13(I3,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9040	FORMAT(9(' '),'! RUNSYS is required to update; ',		!V03
     *	       '$ X2BLDNET is required to update',/)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
	END
