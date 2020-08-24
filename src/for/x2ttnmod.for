C
C SUBROUTINE X2TTNMOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2TTNMOD.FOV                                 $
C  $Date::   17 Apr 1996 16:39:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2ttnmod.for;1 **
C
C X2TTNMOD.FOR
C
C V05 22-DEC-94 DAS Fixed back and next commands
C V04 12-DEC-94 DAS Integrate UK changes into X2X Baseline
C V03 01-NOV-94 GPR Add description for ! and $
C V02 21-OCT-94 GPR SET THE BIT EVEN IF THE RECORD IS ADDED
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program will modify TITAN parameters.
C
C Calling sequence:
C
C     CALL X2TTNMOD(REC)
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
	SUBROUTINE X2TTNMOD(REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XTTN.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   ST,I,J,CNT              !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT                     !Program exit/error
	INTEGER*4   FIELD                   !Field to update
	INTEGER*4   INDX                    !String index
	INTEGER*4   PAGE                    !Page number
	INTEGER*4   TEMP, ENDFLD, BEGFLD, ENDOFF, BEGOFF, MAXPAGE
	LOGICAL     UPDATE                  !Field update flag
	LOGICAL     VALID                   !Valid data flag
	LOGICAL     PRTFLG                  !Print flag
	CHARACTER   PROMPT*70               !Input prompt
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   CHRSTR(20)*1            !Output string
	CHARACTER   X2FILNAM*20             !File name function
	INTEGER*4   DUMMY, DUMMY1
	INTEGER*4   STATUS
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
        ST = 0
	UPDATE=.FALSE.
	PRTFLG=.FALSE.
	CALL CLRSCR(5)
	CALL FASTMOV(X2XTTN_REC,DMYBUF,X2XTTN_SECT*64)
	MAXPAGE=X2XTTN_ENTRIES/30
	IF(MOD(X2XTTN_ENTRIES,30).NE.0) MAXPAGE=MAXPAGE+1
	PAGE=1
	WRITE(5,9000) PAGE,MAXPAGE
C
C DETERMINE THE CDC DATE FROM THE SYSTEM DATE.
C
	CALL XDAT(SYSDATE)
	DATBUF(VYEAR)=SYSDATE(1)
	DATBUF(VMON)=SYSDATE(2)
	DATBUF(VDAY)=SYSDATE(3)
	CALL BDATE(DATBUF)
C
C DISPLAY ALL GLOBAL INFORMATION PRINTING TWO VARIABLES
C PER LINE.
C
50	CONTINUE
	BEGOFF=MAX0(1,(PAGE-1)*30+1)
	ENDOFF=MIN0(BEGOFF-1+30,X2XTTN_ENTRIES)
	DO 100 I=BEGOFF,ENDOFF,2
	  WRITE(5,9010) I,X2XTTN_FIELD(I),
     *	                  X2XTTN_REC(X2XTTN_INDEX(I)),
     *	              I+1,X2XTTN_FIELD(I+1),
     *	                  X2XTTN_REC(X2XTTN_INDEX(I+1))
	  IF(PRTFLG)
     *	    WRITE(6,9010) I,X2XTTN_FIELD(I),
     *	                  X2XTTN_REC(X2XTTN_INDEX(I)),
     *	              I+1,X2XTTN_FIELD(I+1),
     *	                  X2XTTN_REC(X2XTTN_INDEX(I+1))
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=0
	IF(ENDOFF.EQ.X2XTTN_ENTRIES) CNT=MOD(X2XTTN_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  J=(I-1)*2+1
	  WRITE(5,9010) J,X2XTTN_FIELD(J),
     *	                  X2XTTN_REC(X2XTTN_INDEX(J))
	  IF(PRTFLG)
     *	    WRITE(6,9010) J,X2XTTN_FIELD(J),
     *	                  X2XTTN_REC(X2XTTN_INDEX(J))
	ENDIF
C
C IF PRTFLG SET, CLOSE THE PRINTER.
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
	IF(ENDFLD.GE.X2XTTN_ENTRIES) ENDFLD=X2XTTN_ENTRIES-1
        WRITE (5,9022)                                                     !V03
	WRITE (PROMPT,9020) BEGFLD, ENDFLD
	CALL INPNUM(PROMPT,FIELD,BEGFLD,ENDFLD,EXT)
	IF(EXT.EQ.-9) THEN
	  CALL X2XHLP('X2TTNFLD.HLP')
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.EQ.-2) THEN
	  PRTFLG=.TRUE.
	  OPEN(6, FILE='X2TTNMOD.REP', STATUS='NEW', DISP='PRINT/DELETE')
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
          IF (PAGE .LE. 0) PAGE = MAXPAGE     !BACK 
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.EQ.-8) THEN
	  PAGE=PAGE+1
          IF (PAGE .GT. MAXPAGE) PAGE =1      !NEXT
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
C
C	CHECK FOR THE FIELD DESCRIPTION
C
	CALL X2CHKFLD(X2XTTN_FIELD(FIELD),STATUS)
	IF (STATUS.NE.0) GOTO 50

	BOT=X2XTTN_RANGE(1,FIELD)
	TOP=X2XTTN_RANGE(2,FIELD)
	IF(BOT.NE.-2) THEN
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	  ENDOFF=0
	  IF(X2XTTN_VALUE(1,FIELD).NE.-1) THEN
	    DO 112 I=1,15
	      IF(X2XTTN_VALUE(I,FIELD).NE.-1) THEN
	        ENDOFF=I
	      ENDIF
112	    CONTINUE
	    WRITE(5,9025) (X2XTTN_VALUE(I,FIELD),I=1,ENDOFF)
	  ENDIF
C
C PROMPT FOR INPUT DATA.
C
	  WRITE (PROMPT2,9030) X2XTTN_FIELD(FIELD),BOT,TOP
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
	  IF(X2XTTN_VALUE(1,FIELD).NE.-1) THEN
	    VALID=.FALSE.
	    DO 120 J=1,15
	      IF(TEMP.EQ.X2XTTN_VALUE(J,FIELD)) VALID=.TRUE.
120	    CONTINUE
	    IF(.NOT.VALID) THEN
	      WRITE(5,9070) CHAR(7)
	      GOTO 110
	    ENDIF
	  ENDIF
	  X2XTTN_REC(X2XTTN_INDEX(FIELD))=TEMP
C
C CHARACTER FIELD.
C
	ELSE IF(BOT.EQ.-1) THEN
	  WRITE (PROMPT2,9032) X2XTTN_FIELD(FIELD), TOP
	  CALL WIMG(5,PROMPT2)
	  READ(5,9060) CHRSTR
	  IF(CHRSTR(1).EQ.'E' .OR. CHRSTR(1).EQ.'e') THEN
	    CALL CLRSCR(5)
	    WRITE(5,9000) PAGE,MAXPAGE
	    GOTO 50
	  ENDIF
C
	  INDX=(X2XTTN_INDEX(FIELD)-1)*4+1
	  DO 130 J=1,TOP
	    X2XTTN_CREC(INDX)=CHRSTR(J)
	    INDX=INDX+1
130	  CONTINUE
	ENDIF
C
C UPDATE THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XTTN_UPDATE=DATBUF(VCDC)
	CALL X2BSET(X2XTTN_BITMAP,FIELD,XTTN,REC)			    !V02
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
	  CALL WRITEW(X2XTTN_FDB,REC,X2XTTN_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XTTN),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  CALL CMDXTTN(DUMMY,X2XTTN_REC,DUMMY1)	
	  CALL X2CHKMOD(XTTN,1)
C
C RELEASE RECORD LOCK.
C
	ELSE
	  CALL WRITEW(X2XTTN_FDB,REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XTTN),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	ENDIF
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(/,T26,'GTECH Distributed Network',T63,
     *	              'Page ',I2,' of ',I2,/,
     *	          T28,'Modify TITAN Parameters',/)
9010	FORMAT(T10,2(I3.2,'.',1X,A15,1X,I10,2X))
9020	FORMAT(10(' '),'Enter number of field to update [',I3.2,
     *	               '..',I3.2,'] ')
9022    FORMAT(9(' '),'! RUNSYS is required to update; ',               !V03
     *         '$ X2BLDNET is required to update',/)
9025	FORMAT(T12,'Specific values: ',13(I5,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9032	FORMAT(10(' '),'Enter ',A15,' char [',I2,' bytes] ',10(' '))
9060	FORMAT(20A1)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
	END
