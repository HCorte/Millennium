C
C SUBROUTINE X2BROCOPY
C
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BROCOPY.FOV                                $
C  $Date::   17 Apr 1996 16:10:48                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2brocopy.for;1 **
C
C X2BROCOPY.FOR
C 
C V01 01-DEC-95 JAC RELEASED FOR VAX 
C
C This subroutine will copy a X2X Relay Broadcast record
C
C This program will copy a relay application file from an existing one.
C If the source is empty or the destination exists, an error message is 
C displayed.  The user is prompted to overwrite the existing record.
C 
C The X2BROMEN.HLP file was updated to include help on this subject.
C
C Calling sequence:
C
C     CALL X2BROCOPY(REC,TO_REC)
C
C Input parameters:
C
C     REC     Int*4   source Record
C
C     TO_REC  Int*4   Record to be created
C
C Output parameters:
C
C     NONE
C
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2BROCOPY(REC,TO_REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XBRO.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   LINES,OFFSET            !Line printing
	INTEGER*4   ST,I,J,CNT              !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT                     !Program exit
	INTEGER*4   FIELD                   !Field to update
	INTEGER*4   TEMP, ENDOFF, REC, TO_REC
C
	LOGICAL     UPDATE                  !Field update flag
	LOGICAL     VALID                   !Field valid flag
	LOGICAL     PRTFLG                  !Print screen flag
	CHARACTER   PROMPT*50               !Input prompt
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   X2FILNAM*20             !File name function
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
        ST = 0
	UPDATE=.FALSE.
	PRTFLG=.FALSE.
	CALL CLRSCR(5)
        X2XBRO_REC(1) = TO_REC
	CALL FASTMOV(X2XBRO_REC,DMYBUF,X2XBRO_SECT*64)
	WRITE(5,9000)
	IF(PRTFLG) WRITE(6,9000)
	WRITE (PROMPT,9020) X2XBRO_ENTRIES-1
C
C DETERMINE THE CDC DATE FROM THE SYSTEM DATE.
C
	CALL XDAT(SYSDATE)
	DATBUF(VYEAR)=SYSDATE(1)
	DATBUF(VMON)=SYSDATE(2)
	DATBUF(VDAY)=SYSDATE(3)
	CALL BDATE(DATBUF)
C
C DISPLAY ALL INFORMATION PRINTING TWO VARIABLES
C PER LINE.
C
50	CONTINUE
	LINES=X2XBRO_ENTRIES/2
	DO 100 I=1,LINES
	  OFFSET=(I-1)*2+1
	  WRITE(5,9010) (J,X2XBRO_FIELD(J),
     *	                   X2XBRO_REC(X2XBRO_INDEX(J)),
     *	                 J=OFFSET,OFFSET+1)
	  IF(PRTFLG)
     *	    WRITE(6,9010) (J,X2XBRO_FIELD(J),
     *	                     X2XBRO_REC(X2XBRO_INDEX(J)),
     *	                   J=OFFSET,OFFSET+1)
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=MOD(X2XBRO_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  OFFSET=(I-1)*2+1
	  WRITE(5,9010) OFFSET,X2XBRO_FIELD(OFFSET),
     *	                X2XBRO_REC(X2XBRO_INDEX(OFFSET))
	  IF(PRTFLG)
     *	    WRITE(6,9010) OFFSET,X2XBRO_FIELD(OFFSET),
     *	                  X2XBRO_REC(X2XBRO_INDEX(OFFSET))
	ENDIF
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
        WRITE (5,9022)                                                     !V03
	CALL INPNUM(PROMPT,FIELD,2,X2XBRO_ENTRIES-1,EXT)
C				    If "P" was pressed...
	IF(EXT.EQ.-2) THEN
	  PRTFLG=.TRUE.
	  OPEN(6, FILE='X2BROMOD.REP', STATUS='NEW', DISP='PRINT/DELETE')
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,'PR:','OPEN',ST,0)
	    CALL GPAUSE
	  ENDIF
	  CALL CLRSCR(5)
	  WRITE(5,9000)
	  WRITE(6,9000)
	  GOTO 50
C				    If "H" was pressed...
	ELSE IF(EXT.EQ.-9) THEN
	  CALL X2XHLP('X2BROFLD.HLP/2')
	  CALL CLRSCR(5)
	  WRITE(5,9000)
	  GOTO 50
C				    If "E" or any letter is pressed...
	ELSE IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C				    If any number was input, FIELD = that #
C FIELD UPDATE.
C
110	CONTINUE
	BOT=X2XBRO_RANGE(1,FIELD)
	TOP=X2XBRO_RANGE(2,FIELD)
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	ENDOFF=0
	IF(X2XBRO_VALUE(1,FIELD).NE.-1) THEN
	  DO 112 I=1,15
	    IF(X2XBRO_VALUE(I,FIELD).NE.-1) THEN
	      ENDOFF=I
	    ENDIF
112	  CONTINUE
	  WRITE(5,9025) (X2XBRO_VALUE(I,FIELD),I=1,ENDOFF)
	ENDIF
C
C PROMPT FOR INPUT DATA.
C
	WRITE (PROMPT2,9030) X2XBRO_FIELD(FIELD),BOT,TOP
	CALL INPNUM(PROMPT2,TEMP,BOT,TOP,EXT)
C				    If any letter was input...
	IF(EXT.LT.0) THEN
	  CALL CLRSCR(5)
	  WRITE(5,9000)
	  GOTO 50
	ENDIF
C
C CHECK TO ENSURE THE INPUT INFORMATION FALLS WITHIN ANY
C SPECIFIC FIELD VALUES.
C				    TEMP is the input
	IF(X2XBRO_VALUE(1,FIELD).NE.-1) THEN
	  VALID=.FALSE.
	  DO 120 J=1,15
	    IF(TEMP.EQ.X2XBRO_VALUE(J,FIELD)) VALID=.TRUE.
120	  CONTINUE
C				    If invalid input
	  IF(.NOT.VALID) THEN
	    WRITE(5,9070) CHAR(7)
	    GOTO 110
	  ENDIF
	ENDIF
C
C				    Update the field
	X2XBRO_REC(X2XBRO_INDEX(FIELD))=TEMP
C
C SET THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XBRO_UPDATE=DATBUF(VCDC)
	UPDATE=.TRUE.
C				    Set the bitmap
	CALL X2BSET(X2XBRO_BITMAP,FIELD,XBRO,TO_REC)
	CALL CLRSCR(5)
	WRITE(5,9000)
	GOTO 50
C
C PROGRAM EXIT.
C
8000	CONTINUE
C
C IF A FIELD HAS BEEN MODIFIED UPDATE THE FILE.
C
	IF(UPDATE) THEN
	  CALL WRITEW(X2XBRO_FDB,TO_REC,X2XBRO_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBRO),'WRITEW',ST,TO_REC)
	    CALL GPAUSE
	  ENDIF
	  CALL X2CHKMOD(XBRO,1)
C
C RELEASE RECORD LOCK.
C
	ELSE
	  CALL WRITEW(X2XBRO_FDB,TO_REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBRO),'WRITEW',ST,TO_REC)
	    CALL GPAUSE
	  ENDIF
	ENDIF
C
C     ================== Format Statements =====================
C
9000	FORMAT(/,T26,'GTECH Distributed Network',/,
     *	          T24,'Broadcast Relay Configuration',/)
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9020	FORMAT(10(' '),'Enter number of field to update [2..',
     *	       I2.2,'] ')
9022    FORMAT(9(' '),'! RUNSYS is required to update; ',               !V03
     *         '$ X2BLDNET is required to update',/)
9025	FORMAT(T12,'Specific values: ',13(I3,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
	END
