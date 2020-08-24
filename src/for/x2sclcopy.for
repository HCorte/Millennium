C
C SUBROUTINE X2SCLCOPY
C
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SCLCOPY.FOV                                $
C  $Date::   17 Apr 1996 16:33:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2sclcopy.for;1 **
C
C X2SCLCOPY.FOR
C
C V01 11-DEC-95 JAC RELEASED FOR VAX
C
C This subroutine will copy a Station Class record.  It is a modified
C version of X2SCLMOD.FOR
C
C This subroutine will copy a station class record from an existing one.
C It is called from the X2SCLMEN.FOR program which must check if the source
C is empty or the destination exists.  If either condition is true, an
C an error message is displayed.  The user is prompted to overwrite an
C existing record. 
C
C Calling sequence:
C
C     CALL X2SCLCOPY(REC,TO_REC)
C
C Input parameters:
C
C     REC     Int*4   Record to be modified from (source)
C
C     TO_REC  Int*4   Record to be created (destination)
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
	SUBROUTINE X2SCLCOPY(REC,TO_REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC,TO_REC              !source, destination Records
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   ST,I,J                  !Work variables
	INTEGER*4   TOP,BOT                 !Validation range
	INTEGER*4   EXT                     !Program exit/error
	INTEGER*4   FIELD                   !Field to update
	INTEGER*4   INDX                    !String index
	INTEGER*4   INDX1,LEN1              !String display
	INTEGER*4   INDX2,LEN2              !String display
	INTEGER*4   TEMP, ENDFLD, BEGFLD, K, ENDOFF, BEGOFF, PAGE
	INTEGER*4   MAXPAGE
	LOGICAL     UPDATE                  !Field update flag
	LOGICAL     VALID                   !Valid data flag
	LOGICAL     PRTFLG                  !Print flag
	CHARACTER   PROMPT*50               !Input prompt
	CHARACTER   PROMPT2*70              !Input prompt
	CHARACTER   CHRSTR(20)*1            !Output string
	CHARACTER   X2FILNAM*20             !File name function
	INTEGER*4   STATUS
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
        ST = 0
	UPDATE=.FALSE.
	PRTFLG=.FALSE.
	CALL CLRSCR(5)
        X2XSCL_REC(1) = TO_REC
	CALL FASTMOV(X2XSCL_REC,DMYBUF,X2XSCL_SECT*64)
	MAXPAGE=X2XSCL_ENTRIES/30
	IF(MOD(X2XSCL_ENTRIES,30).NE.0) MAXPAGE=MAXPAGE+1
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
	ENDOFF=MIN0(BEGOFF-1+30,X2XSCL_ENTRIES)
	DO 100 I=BEGOFF,ENDOFF,2
C
C NOTE : This code detect a character variable in any place of
C        record, displaying it correctly. It does not detect BCD
C        variables. If you add any BCD variable into the record
C        don't forget to modify this code
C
        IF (I+1.GT.X2XSCL_ENTRIES) THEN
           IF(X2XSCL_RANGE(1,I).EQ.-1) THEN
              INDX1=(X2XSCL_INDEX(I)-1)*4+1
              LEN1 =X2XSCL_RANGE(2,I)
              WRITE(5,9090) I,X2XSCL_FIELD(I),
     *                     (X2XSCL_CREC(K),K=INDX1,INDX1+LEN1-1)
           ELSE
              WRITE(5,9100) I,X2XSCL_FIELD(I),
     *                        X2XSCL_REC(X2XSCL_INDEX(I))
           ENDIF
        ELSE IF (X2XSCL_RANGE(1,I).EQ.-1.AND.X2XSCL_RANGE(1,I+1).EQ.-1)
     *   THEN
              INDX1=(X2XSCL_INDEX(I)-1)*4 + 1
              LEN1 =X2XSCL_RANGE(2,I)
              INDX2=(X2XSCL_INDEX(I+1)-1)*4 + 1
              LEN2 =X2XSCL_RANGE(2,I+1)
              WRITE (5,9075) I,X2XSCL_FIELD(I),
     *                     (X2XSCL_CREC(K),K=INDX1,INDX1+LEN1-1),
     *                     I+1,X2XSCL_FIELD(I+1),
     *                     (X2XSCL_CREC(K),K=INDX2,INDX2+LEN2-1)
              IF(PRTFLG)
     *          WRITE (6,9075) I,X2XSCL_FIELD(I),
     *                      (X2XSCL_CREC(K),K=INDX1,INDX1+LEN1-1),
     *                      I+1,X2XSCL_FIELD(I+1),
     *                      (X2XSCL_CREC(K),K=INDX2,INDX2+LEN2-1)
            ELSEIF
     *       (X2XSCL_RANGE(1,I).EQ.-1.AND.X2XSCL_RANGE(1,I+1).NE.-1)THEN
              INDX1=(X2XSCL_INDEX(I)-1)*4+1
              LEN1 =X2XSCL_RANGE(2,I)
              WRITE(5,9080) I,X2XSCL_FIELD(I),
     *                     (X2XSCL_CREC(K),K=INDX1,INDX1+LEN1-1),
     *                     I+1,X2XSCL_FIELD(I+1),
     *                     X2XSCL_REC(X2XSCL_INDEX(I+1))
              IF(PRTFLG)
     *            WRITE(6,9080) I,X2XSCL_FIELD(I),
     *                 (X2XSCL_CREC(K),K=INDX1,INDX1+LEN1-1),
     *                 I+1,X2XSCL_FIELD(I+1),
     *                 X2XSCL_REC(X2XSCL_INDEX(I+1))
            ELSEIF
     *       (X2XSCL_RANGE(1,I).NE.-1.AND.X2XSCL_RANGE(1,I+1).EQ.-1)THEN
              INDX1=(X2XSCL_INDEX(I+1)-1)*4+1
              LEN1=X2XSCL_RANGE(2,I+1)
              WRITE(5,9040) I,X2XSCL_FIELD(I),
     *                      X2XSCL_REC(X2XSCL_INDEX(I)),
     *                     I+1,X2XSCL_FIELD(I+1),
     *                     (X2XSCL_CREC(K),K=INDX1,INDX1+LEN1-1)
            IF(PRTFLG)
     *         WRITE(6,9040) I,X2XSCL_FIELD(I),
     *                        X2XSCL_REC(X2XSCL_INDEX(I)),
     *                       I+1,X2XSCL_FIELD(I+1),
     *                       (X2XSCL_CREC(K),K=INDX1,INDX1+LEN1-1) 
	  ELSE
	    WRITE(5,9010) (J,X2XSCL_FIELD(J),
     *	                     X2XSCL_REC(X2XSCL_INDEX(J)),
     *	                   J=I,MIN0(I+1,X2XSCL_ENTRIES))
	    IF(PRTFLG)
     *	      WRITE(6,9010) (J,X2XSCL_FIELD(J),
     *	                       X2XSCL_REC(X2XSCL_INDEX(J)),
     *	                     J=I,MIN0(I+1,X2XSCL_ENTRIES))
	  ENDIF
100	CONTINUE
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
	IF(ENDFLD.GE.X2XSCL_ENTRIES) ENDFLD=X2XSCL_ENTRIES-1
C Display "! RUNSYS is required to update; $ X2BLDNET is required to update "
	WRITE (5,9022)
C
C Display "Enter number of field to update [',I2.2,'-',I2.2] "
	WRITE (PROMPT,9020) BEGFLD, ENDFLD
	CALL INPNUM(PROMPT,FIELD,2,X2XSCL_ENTRIES-1,EXT)
C
	IF(EXT.EQ.-2) THEN               ! PRINT
	  PRTFLG=.TRUE.
	  OPEN(6, FILE='X2SCLMOD.REP', STATUS='NEW', DISP='PRINT/DELETE')
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
	ELSE IF(EXT.EQ.-8) THEN           !NEXT
	  PAGE=PAGE+1
          IF(PAGE.GT.MAXPAGE) PAGE = 1
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  GOTO 50
	ELSE IF(EXT.EQ.-9) THEN           !HELP
	  CALL X2XHLP('X2SCLFLD.HLP')
	  CALL CLRSCR(5)
	  WRITE(5,9000) PAGE,MAXPAGE
	  GOTO 50
C					  If "E" or any letter is pressed
	ELSE IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C FIELD UPDATE.
C NOTE: FOR CHARACTER VARIABLES THE BOTTOM RANGE WILL CONTAIN
C       A -1 AND THE TOP RANGE CONTAINS THE NUMBER OF BYTES.
C
C	CHECK FOR THE FIELD DESCRIPTION
C	user must type in the field description as shown in the menu
C       in order to update that field... sort of a double check
C
110	CONTINUE
	CALL X2CHKFLD(X2XSCL_FIELD(FIELD),STATUS)
C	If user input does not match field description, don't modify it!
	IF (STATUS.NE.0) GOTO 50
	BOT=X2XSCL_RANGE(1,FIELD)
	TOP=X2XSCL_RANGE(2,FIELD)
	IF(BOT.GE.0) THEN
C
C DISPLAY SPECIFIC VALUES (IF THEY EXIST).
C
	  ENDOFF=0
	  IF(X2XSCL_VALUE(1,FIELD).NE.-1) THEN
	    DO 112 I=1,15
	      IF(X2XSCL_VALUE(I,FIELD).NE.-1) THEN
	        ENDOFF=I
	      ENDIF
112	    CONTINUE
C  	    Display "Specific values: ",13(I5,1X)
	    WRITE(5,9025) (X2XSCL_VALUE(I,FIELD),I=1,ENDOFF)
	  ENDIF

C
C PROMPT FOR INPUT DATA.
C
C		Display "Enter ',A15,' range [',I8,'-',I8,'] '"
	  WRITE (PROMPT2,9030) X2XSCL_FIELD(FIELD),BOT,TOP
	  CALL INPNUM(PROMPT2,TEMP,BOT,TOP,EXT)
	  IF(EXT.LT.0) THEN
	    CALL CLRSCR(5)
	    WRITE(5,9000) PAGE,MAXPAGE
	    GOTO 50
	  ENDIF
C
C CHECK TO ENSURE THE INPUT INFORMATION FALLS WITHIN ANY
C SPECIFIC FIELD VALUES.
C
	  IF(X2XSCL_VALUE(1,FIELD).NE.-1) THEN
	    VALID=.FALSE.
	    DO 120 J=1,15
	      IF(TEMP.EQ.X2XSCL_VALUE(J,FIELD)) VALID=.TRUE.
120	    CONTINUE
	    IF(.NOT.VALID) THEN
	      WRITE(5,9070) CHAR(7)
	      GOTO 110
	    ENDIF
	  ENDIF
	  X2XSCL_REC(X2XSCL_INDEX(FIELD))=TEMP
C
C CHARACTER FIELD.
C
	ELSE IF(BOT.EQ.-1) THEN
	  WRITE (PROMPT2,9032) X2XSCL_FIELD(FIELD), TOP
	  CALL WIMG(5,PROMPT2)
	  READ(5,9060) CHRSTR
	  IF(CHRSTR(1).EQ.'E' .OR. CHRSTR(1).EQ.'e') THEN
	    CALL CLRSCR(5)
	    WRITE(5,9000) PAGE,MAXPAGE
	    GOTO 50
	  ENDIF
C
	  INDX=(X2XSCL_INDEX(FIELD)-1)*4+1
	  DO 130 J=1,TOP
	    X2XSCL_CREC(INDX)=CHRSTR(J)
	    INDX=INDX+1
130	  CONTINUE
	ENDIF
C
C UPDATE THE LAST UPDATED DATE, AND CLEAR THE SCREEN.
C
	X2XSCL_UPDATE=DATBUF(VCDC)
C                                   Set the bitmap
	CALL X2BSET(X2XSCL_BITMAP,FIELD,XSCL,TO_REC)
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
	  CALL WRITEW(X2XSCL_FDB,TO_REC,X2XSCL_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XSCL),'WRITEW',ST,TO_REC)
	    CALL GPAUSE
	  ENDIF
	  CALL X2CHKMOD(XSCL,1)
C
C RELEASE RECORD LOCK.
C
	ELSE
	  CALL WRITEW(X2XSCL_FDB,TO_REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XSCL),'WRITEW',ST,TO_REC)
	    CALL GPAUSE
	  ENDIF
	ENDIF
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(T26,'GTECH Distributed Network',T63,
     *	              'Page ',I2,' of ',I2,/,
     *	       T28,'Copy Station Class',/)
9010	FORMAT(T3,2(I2.2,'.',1X,A15,7X,I10,2X))
9020	FORMAT(10(' '),'Enter number of field to update [',I2.2,
     *	               '-',I2.2,'] ')
9022	FORMAT(2(' '),'! RUNSYS is required to update; ',		!V03
     *	       '$ X2BLDNET is required to update',/)
9023	FORMAT(T12,(' '),'! DEBUG #1 ',/)
9024	FORMAT(T12,(' '),'! DEBUG #2 ',/)
9025	FORMAT(T12,'Specific values: ',13(I5,1X))
9030	FORMAT(10(' '),'Enter ',A15,' range [',I8,'-',I8,'] ')
9032	FORMAT(10(' '),'Enter ',A15,' char [',I2,' bytes] ',5(' '))
9040	FORMAT(T3,I2.2,'.',1X,A15,7X,I10,2X,
     *	           I2.2,'.',1X,A15,1X,16A)
9060	FORMAT(20A1)
9070	FORMAT(1X,'Invalid - input does not match specific values',A)
9075    FORMAT(T3,2(I2.2,'.',1X,A15,1X,15A,2X))
9080    FORMAT(T3,I2.2,'.',1X,A15,1X,16A,2X,I2.2,'.',1X,A15,7X,I10)
9090    FORMAT(T3,I2.2,'.',1X,A15,1X,16A)
9100    FORMAT(T3,I2.2,'.',1X,A15,7X,I10)
	END
