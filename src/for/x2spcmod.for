C
C SUBROUTINE X2SPCMOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SPCMOD.FOV                                 $
C  $Date::   17 Apr 1996 16:35:24                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2spcmod.for;2 **
C
C X2SPCMOD.FOR
C
C V05 31-Jul-95 DAS Added callto x2cnvdrp
C V04 12-DEC-94 DAS Integrate UK changes into X2X Baseline
C V03 01-NOV-94 GPR Add description for ! and $
C V02 21-OCT-94 GPR SET THE BIT EVEN IF THE RECORD IS ADDED
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program will modify a port configuration on a station.
C
C Calling sequence:
C
C     CALL X2SPCMOD(REC)
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
	SUBROUTINE X2SPCMOD(REC)
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
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   OFFSET                  !Line printing
	INTEGER*4   ST,STATUS,I,J           !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT                     !Program exit/error
	INTEGER*4   FIELD                   !Field to update
	INTEGER*4   INDX                    !String index
	INTEGER*4   INDX1,INDX2             !Char string display
	INTEGER*4   LEN1,LEN2               !Char string display
	INTEGER*4   TEMP, BEGOFF,ENDOFF, K
	INTEGER*4   DROP_INDEX
	INTEGER*4   PAGE,MAXPAGE	    !Max number of display pages
	INTEGER*4   BEGFLD, ENDFLD	    !Multi page display
	LOGICAL     UPDATE                  !Field update flag
	LOGICAL     VALID                   !Valid data flag
	LOGICAL     PRTFLG                  !Print flag
	CHARACTER   PROMPT*50               !Input prompt
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   CHRSTR(20)*1            !Output string
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
        STATUS = 0
	UPDATE=.FALSE.
	PRTFLG=.FALSE.
	CALL CLRSCR(5)
	CALL FASTMOV(X2XSPC_REC,DMYBUF,X2XSPC_SECT*64)
	MAXPAGE=X2XSPC_ENTRIES/30
        IF(MOD(X2XSPC_ENTRIES,30).NE.0) MAXPAGE=MAXPAGE+1
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
        ENDOFF=MIN0(BEGOFF-1+30,X2XSPC_ENTRIES)
        DO 100 OFFSET=BEGOFF,ENDOFF,2
	  J=OFFSET
C
C NOTE: OFFSETS 4-9 ARE HARDCODED TO ALLOW THE CHAR FIELDS
C       TO BE CORRECTLY DISPLAYED.
C
	  IF(OFFSET.GE.3.AND.OFFSET.LE.43) THEN
	    INDX1=(X2XSPC_INDEX(OFFSET)-1)*4+1
	    LEN1=X2XSPC_RANGE(2,OFFSET)
	    IF(OFFSET.LT.43) THEN
	       INDX2=(X2XSPC_INDEX(OFFSET+1)-1)*4+1
	       LEN2=X2XSPC_RANGE(2,OFFSET+1)
	    ENDIF
	    IF(OFFSET.EQ.3) THEN
	      WRITE(5,9042) J,X2XSPC_FIELD(J),
     *	                      X2XSPC_REC(X2XSPC_INDEX(J)),
     *	                  J+1,X2XSPC_FIELD(J+1),
     *	                     (X2XSPC_CREC(K),K=INDX2,INDX2+LEN2-1)
	      IF(PRTFLG)
     *	        WRITE(6,9042) J,X2XSPC_FIELD(J),
     *	                        X2XSPC_REC(X2XSPC_INDEX(J)),
     *	                    J+1,X2XSPC_FIELD(J+1),
     *	                       (X2XSPC_CREC(K),K=INDX2,INDX2+LEN2-1)
	    ELSEIF(OFFSET.EQ.43) THEN
	      WRITE(5,9010) J,X2XSPC_FIELD(J),
     *	                      X2XSPC_REC(X2XSPC_INDEX(J))
	      IF(PRTFLG)
     *	        WRITE(6,9010) J,X2XSPC_FIELD(J),
     *	                        X2XSPC_REC(X2XSPC_INDEX(J))
	    ELSE
	      WRITE(5,9040) J,X2XSPC_FIELD(J),
     *	                     (X2XSPC_CREC(K),K=INDX1,INDX1+LEN1-1),
     *	                  J+1,X2XSPC_FIELD(J+1),
     *	                     (X2XSPC_CREC(K),K=INDX2,INDX2+LEN2-1)
	      IF(PRTFLG)
     *	        WRITE(6,9040) J,X2XSPC_FIELD(J),
     *	                       (X2XSPC_CREC(K),K=INDX1,INDX1+LEN1-1),
     *	                    J+1,X2XSPC_FIELD(J+1),
     *	                       (X2XSPC_CREC(K),K=INDX2,INDX2+LEN2-1)
	    ENDIF
	  ELSE
	    WRITE(5,9010) J,X2XSPC_FIELD(J),
     *	                    X2XSPC_REC(X2XSPC_INDEX(J)),
     *	                J+1,X2XSPC_FIELD(J+1),
     *	                    X2XSPC_REC(X2XSPC_INDEX(J+1))
	    IF(PRTFLG)
     *	      WRITE(6,9010) J,X2XSPC_FIELD(J),
     *	                      X2XSPC_REC(X2XSPC_INDEX(J)),
     *	                  J+1,X2XSPC_FIELD(J+1),
     *	                      X2XSPC_REC(X2XSPC_INDEX(J+1))
	  ENDIF
100	CONTINUE
C
C IF PRINT FLAG ENABLE TURN IT OFF.
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
        IF(ENDFLD.GE.X2XSPC_ENTRIES) ENDFLD=X2XSPC_ENTRIES-1
        WRITE (5,9022)                                                     !V03
	WRITE (PROMPT,9020) BEGFLD, ENDFLD
	CALL INPNUM(PROMPT,FIELD,3,X2XSPC_ENTRIES-1,EXT)
	IF(EXT.EQ.-2) THEN
	  PRTFLG=.TRUE.
          OPEN(6,FILE='X2SPCMOD.REP',STATUS='NEW',DISP='PRINT/DELETE')
	  IF(STATUS.NE.0) THEN
	    CALL OS32ER(5,'X2SPCMOD.REP','OPEN',STATUS,0)
	    CALL GPAUSE
	  ENDIF
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE, MAXPAGE
	  WRITE(6,9000) PAGE, MAXPAGE
	  GOTO 50
	ELSE IF(EXT.EQ.-7) THEN
          PAGE=PAGE-1
          CALL CLRSCR(5)
          WRITE(5,9000) PAGE,MAXPAGE
          GOTO 50
        ELSE IF(EXT.EQ.-8) THEN
          IF(PAGE.LT.MAXPAGE) PAGE=PAGE+1
          CALL CLRSCR(5)
          WRITE(5,9000) PAGE,MAXPAGE
          GOTO 50
	ELSEIF(EXT.EQ.-9) THEN
	  CALL X2XHLP('X2SPCFLD.HLP')
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE, MAXPAGE
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
	BOT=X2XSPC_RANGE(1,FIELD)
	TOP=X2XSPC_RANGE(2,FIELD)
	IF(BOT.GE.0) THEN
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	  ENDOFF=0
	  IF(X2XSPC_VALUE(1,FIELD).NE.-1) THEN
	    DO 112 I=1,15
	      IF(X2XSPC_VALUE(I,FIELD).NE.-1) THEN
	        ENDOFF=I
	      ENDIF
112	    CONTINUE
	    WRITE(5,9025) (X2XSPC_VALUE(I,FIELD),I=1,ENDOFF)
	  ENDIF
C
C PROMPT FOR INPUT DATA.
C
	  WRITE (PROMPT2,9030) X2XSPC_FIELD(FIELD),BOT,TOP
	  CALL INPNUM(PROMPT2,TEMP,BOT,TOP,EXT)
	  IF(EXT.LT.0) THEN
	    CALL CLRSCR(5)
	    WRITE(5,9000) PAGE, MAXPAGE
	    GOTO 50
	  ENDIF
C
C CHECK TO ENSURE THE INPUT INFORMATION FAILS WITHIN ANY
C SPECIFIC FIELD VALUES.
C
	  IF(X2XSPC_VALUE(1,FIELD).NE.-1) THEN
	    VALID=.FALSE.
	    DO 120 J=1,15
	      IF(TEMP.EQ.X2XSPC_VALUE(J,FIELD)) VALID=.TRUE.
120	    CONTINUE
	    IF(.NOT.VALID) THEN
	      WRITE(5,9070) CHAR(7)
	      GOTO 110
	    ENDIF
	  ENDIF
	  X2XSPC_REC(X2XSPC_INDEX(FIELD))=TEMP
C
C CHARACTER FIELD.
C
	ELSE IF(BOT.EQ.-1) THEN
	  WRITE (PROMPT2,9032) X2XSPC_FIELD(FIELD), TOP
	  CALL WIMG(5,PROMPT2)
	  READ(5,9060) CHRSTR
	  INDX=(X2XSPC_INDEX(FIELD)-1)*4+1
C
C IF DROP ADDRESS CHECK FOR VALID RANGE.
C
	  IF(FIELD.GE.4 .AND. FIELD.LE.42) THEN
C
C CHECK FOR NORMAL ADDRESSING.
C
            CALL X2CNVDRP(CHRSTR,DROP_INDEX)    !...v05
            IF(DROP_INDEX.LE.0) THEN            
                WRITE(5,9070) CHAR(7)
                GOTO 110
            ENDIF
C
C VALID DROP HAS BEEN ENTERED, NOW UPDATE RECORD.
C
            IF(X2XSPC_DROPS(DROP_INDEX).EQ.CHRSTR(1)//CHRSTR(2)) THEN
              X2XSPC_DROPS(DROP_INDEX)=' '
            ELSE
              X2XSPC_DROPS(DROP_INDEX)=CHRSTR(1)//CHRSTR(2)
            ENDIF
C
C NORMAL CHARACTER FIELD.
C
	  ELSE
	    IF(CHRSTR(1).EQ.'E' .OR. CHRSTR(1).EQ.'e') THEN
	      CALL CLRSCR(5)
	      WRITE(5,9000) PAGE,MAXPAGE
	      GOTO 50
	    ENDIF
C
	    DO 130 J=1,TOP
	      X2XSPC_CREC(INDX)=CHRSTR(J)
	      INDX=INDX+1
130	    CONTINUE
	  ENDIF
	ENDIF
C
C UPDATE THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XSPC_UPDATE=DATBUF(VCDC)
	CALL X2BSET(X2XSPC_BITMAP,FIELD,XSPC,REC)			    !V02
	UPDATE=.TRUE.
	CALL CLRSCR(5)
	WRITE(5,9000) PAGE, MAXPAGE
	GOTO 50
C
C PROGRAM EXIT.
C
8000	CONTINUE
C
C IF A FIELD HAS BEEN MODIFIED UPDATE THE FILE.
C
	IF(UPDATE) THEN
	  CALL WRITX2X(4,REC,X2XSPC_REC,ST)
	  CALL X2CHKMOD(XSPC,1)
C
C RELEASE RECORD LOCK.
C
	ELSE
	  CALL WRITX2X(4,REC,DMYBUF,ST)
	ENDIF
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(/,T26,'GTECH Distributed Network',T63,
     *                'Page ',I2,' of ',I2,/,
     *	          T26,'Modify a Port on a Station',/)
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9020	FORMAT(10(' '),'Enter number of field to update [',I2,
     *	               '-',I2,'] ')
9022    FORMAT(9(' '),'! RUNSYS is required to update; ',               !V03
     *         '$ X2BLDNET is required to update',/)
9025	FORMAT(T12,'Specific values: ',13(I3,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9032	FORMAT(10(' '),'Enter ',A15,' char [',I2,' bytes] ',10(' '))
9040	FORMAT(T10,I2.2,'.',1X,A15,1X,8A,4X,
     *	           I2.2,'.',1X,A15,1X,8A)
9042	FORMAT(T10,I2.2,'.',1X,A15,1X,I10,2X,
     *	           I2.2,'.',1X,A15,1X,8A)
9043	FORMAT(T10,I2.2,'.',1X,A15,1X,8A,4X,
     *	           I2.2,'.',1X,A15,1X,I10)
9060	FORMAT(20A1)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
	END
