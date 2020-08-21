C
C SUBROUTINE X2NPCMOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2NPCMOD.FOV                                 $
C  $Date::   17 Apr 1996 16:25:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2npcmod.for **
C
C X2NPCMOD.FOR
C
C V03 01-NOV-94 GPR Add description for ! and $
C V02 21-OCT-94 GPR SET THE BIT EVEN IF THE RECORD IS ADDED
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program will modify a Network Port record.
C
C Calling sequence:
C
C     CALL X2NPCMOD(REC)
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
	SUBROUTINE X2NPCMOD(REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   LINES,OFFSET            !Line printing
	INTEGER*4   ST,I,J,CNT              !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT,ERR                 !Program exit/error
	INTEGER*4   FIELD                   !Field to update
	INTEGER*4   NUMCHR, TEMP, ENDOFF
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
	CALL FASTMOV(X2XNPC_REC,DMYBUF,X2XNPC_SECT*64)
	WRITE(5,9000)
	WRITE (PROMPT,9020) X2XNPC_ENTRIES-1
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
	LINES=X2XNPC_ENTRIES/2
	DO 100 I=1,LINES
	  OFFSET=(I-1)*2+1
C
C NOTE: OFFSET 7 AND 11 IS HARDCODED TO ALLOW FIELD (ADDRESS)
C       TO BE CORRECTLY DISPLAYED.
C
	  IF(OFFSET.EQ.7) THEN
	    CALL HTOA(CHRSTR,1,X2XNPC_ADDLEN,X2XNPC_ADDRES,ERR)
	    DO 102 J=X2XNPC_ADDLEN+1,20
	      CHRSTR(J)=' '
102	    CONTINUE
	    WRITE(5,9040) 7,X2XNPC_FIELD(7),(CHRSTR(J),J=1,16),
     *	                  8,X2XNPC_FIELD(8),
     *	                    X2XNPC_REC(X2XNPC_INDEX(8))
	    IF(PRTFLG)
     *	      WRITE(6,9040) 7,X2XNPC_FIELD(7),(CHRSTR(J),J=1,16),
     *	                  8,X2XNPC_FIELD(8),
     *	                    X2XNPC_REC(X2XNPC_INDEX(8))
	  ELSE IF(OFFSET.EQ.11) THEN
	    CALL HTOA(CHRSTR,1,X2XNPC_ADDLEN,X2XNPC_HUNTADR,ERR)
	    DO 104 J=X2XNPC_ADDLEN+1,20
	      CHRSTR(J)=' '
104	    CONTINUE
	    WRITE(5,9040) 11,X2XNPC_FIELD(11),(CHRSTR(J),J=1,16),
     *	                  12,X2XNPC_FIELD(12),
     *	                     X2XNPC_REC(X2XNPC_INDEX(12))
	    IF(PRTFLG)
     *	      WRITE(6,9040) 11,X2XNPC_FIELD(11),(CHRSTR(J),J=1,16),
     *	                    12,X2XNPC_FIELD(12),
     *	                     X2XNPC_REC(X2XNPC_INDEX(12))
	  ELSE
	    J=OFFSET
	    WRITE(5,9010) J,X2XNPC_FIELD(J),
     *	                    X2XNPC_REC(X2XNPC_INDEX(J)),
     *	                J+1,X2XNPC_FIELD(J+1),
     *	                    X2XNPC_REC(X2XNPC_INDEX(J+1))
	    IF(PRTFLG)
     *	      WRITE(6,9010) J,X2XNPC_FIELD(J),
     *	                    X2XNPC_REC(X2XNPC_INDEX(J)),
     *	                J+1,X2XNPC_FIELD(J+1),
     *	                    X2XNPC_REC(X2XNPC_INDEX(J+1))
	  ENDIF
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=MOD(X2XNPC_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  J=(I-1)*2+1
	  WRITE(5,9010) J,X2XNPC_FIELD(J),
     *	                  X2XNPC_REC(X2XNPC_INDEX(J))
	  IF(PRTFLG)
     *	    WRITE(6,9010) J,X2XNPC_FIELD(J),
     *	                    X2XNPC_REC(X2XNPC_INDEX(J))
	ENDIF
C
C IF PRINT FLAG ENABLE, TURN IT OFF.
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
	CALL INPNUM(PROMPT,FIELD,2,X2XNPC_ENTRIES-1,EXT)
	IF(EXT.EQ.-9) THEN
	  CALL X2XHLP('X2NPCFLD.HLP')
	  CALL CLRSCR(5)
	  WRITE(5,9000)
	  GOTO 50
	ELSE IF(EXT.EQ.-2) THEN
	  PRTFLG=.TRUE.
	  OPEN(6, FILE='X2NPCMOD.REP', STATUS='NEW', DISP='PRINT/DELETE')
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,'PR:','OPEN',ST,0)
	    CALL GPAUSE
	  ENDIF
	  CALL CLRSCR(5)
	  WRITE(5,9000)
	  WRITE(6,9000)
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
	BOT=X2XNPC_RANGE(1,FIELD)
	TOP=X2XNPC_RANGE(2,FIELD)
	IF(BOT.NE.-2) THEN
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	  ENDOFF=0
	  IF(X2XNPC_VALUE(1,FIELD).NE.-1) THEN
	    DO 112 I=1,15
	      IF(X2XNPC_VALUE(I,FIELD).NE.-1) THEN
	        ENDOFF=I
	      ENDIF
112	    CONTINUE
	    WRITE(5,9025) (X2XNPC_VALUE(I,FIELD),I=1,ENDOFF)
	  ENDIF
C
C PROMPT FOR INPUT DATA.
C
	  WRITE (PROMPT2,9030) X2XNPC_FIELD(FIELD),BOT,TOP
	  CALL INPNUM(PROMPT2,TEMP,BOT,TOP,EXT)
	  IF(EXT.LT.0) THEN
	    CALL CLRSCR(5)
	    WRITE(5,9000)
	    GOTO 50
	  ENDIF
C
C CHECK TO ENSURE THE INPUT INFORMATION FAILS WITHIN ANY
C SPECIFIC FIELD VALUES.
C
	  IF(X2XNPC_VALUE(1,FIELD).NE.-1) THEN
	    VALID=.FALSE.
	    DO 120 J=1,15
	      IF(TEMP.EQ.X2XNPC_VALUE(J,FIELD)) VALID=.TRUE.
120	    CONTINUE
	    IF(.NOT.VALID) THEN
	      WRITE(5,9070) CHAR(7)
	      GOTO 110
	    ENDIF
	  ENDIF
	  X2XNPC_REC(X2XNPC_INDEX(FIELD))=TEMP
C
C BCD FIELD.
C
	ELSE IF(BOT.EQ.-2) THEN
	  WRITE (PROMPT2,9032) X2XNPC_FIELD(FIELD), TOP
	  CALL WIMG(5,PROMPT2)
	  READ(5,9060) CHRSTR
	  IF(CHRSTR(1).EQ.'E' .OR. CHRSTR(1).EQ.'e') THEN
	    CALL CLRSCR(5)
	    WRITE(5,9000)
	    GOTO 50
	  ENDIF
C
	  NUMCHR=0
	  DO 122 I=1,TOP
	    IF(CHRSTR(I).GT.' ') NUMCHR=NUMCHR+1
122	  CONTINUE
	  IF(FIELD.EQ.7 .OR. FIELD.EQ.11)
     *	    NUMCHR=MIN0(NUMCHR,X2XNPC_ADDLEN)
        CALL ATOH(CHRSTR,1,NUMCHR,X2XNPC_REC(X2XNPC_INDEX(FIELD)),ERR)
	  IF(ERR.NE.0) THEN
	    WRITE(5,9080) CHAR(7)
	    GOTO 110
	  ENDIF
	ENDIF
C
C UPDATE THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XNPC_UPDATE=DATBUF(VCDC)
	CALL X2BSET(X2XNPC_BITMAP,FIELD,XNPC,REC)			    !V02
	UPDATE=.TRUE.
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
	  CALL WRITEW(X2XNPC_FDB,REC,X2XNPC_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XNPC),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  CALL X2CHKMOD(XNPC,1)
C
C RELEASE RECORD LOCK.
C
	ELSE
	  CALL WRITEW(X2XNPC_FDB,REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XNPC),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	ENDIF
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T29,'Modify Network Port',//)
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9020	FORMAT(10(' '),'Enter number of field to update [2..',I2.2,
     *	               '] ')
9022    FORMAT(9(' '),'! RUNSYS is required to update; ',               !V03
     *         '$ X2BLDNET is required to update',/)
9025	FORMAT(T12,'Specific values: ',13(I3,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9032	FORMAT(10(' '),'Enter ',A15,' char [',I2,' bytes] ',10(' '))
9040	FORMAT(T10,I2.2,'.',1X,A10,1X,16A,1X,
     *	           I2.2,'.',1X,A15,1X,I10)
9060	FORMAT(20A1)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
9080	FORMAT(1X,'Invalid Entry - try again',A)
	END
