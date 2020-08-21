C
C SUBROUTINE X2POSTX
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2POSTX.FOV                                  $
C  $Date::   17 Apr 1996 16:26:24                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C ** Source - x2postx.for;1 **
C
C X2POSTX.FOR
C V03 21-OCT-94 GPR TEST THE FIRST BIT OF THE BITMAP NOT THE LAST
C V01 6-AUG-1994 WS RELEASE FOR UK, MODIFIED X2POST TO USE X2POST_QUE
C
C This program will search through the X2X database, and if
C the game is online and the bitmap field is set indicating
C a modified field, queue a command to CMDPRO to update
C memory.  After the command has been queued the bitmap is
C cleared.
C
C Input parameters:
C
C     DISLOG  Logical     Display modified fields flag
C     PRTFLG  Logical     Display to printer flag
C     UPDFLG  Logical     Update or clear bitmap
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
	SUBROUTINE X2POSTX(DISLOG,PRTFLG,UPDFLG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XFIL.DEF'
	INCLUDE 'INCLIB:X2XGBL.DEF'
	INCLUDE 'INCLIB:X2XPOST.DEF'
C
	INTEGER*4   ALLREC(128)                 !Record buffer
	INTEGER*4   FILE                        !File number
	INTEGER*4   FLD                         !Field number
	INTEGER*4   FLDIDX                      !Actual field index
	INTEGER*4   ADDFLG                      !New record flag
	INTEGER*4   VALUE(4)                    !Modified field value
	INTEGER*4   DMYVAL(4)                   !Dummy value field
	INTEGER*4   CBUF(CDLEN)                 !Dummy command buffer
	INTEGER*4   FDB(7)                      !File descriptor block
	INTEGER*4   I, ST, FILIDX, ANS, REC, X2FILSEC
	CHARACTER   X2FILNAM*20                 !File name function
	CHARACTER   FLDNAM*15                   !Field name
	CHARACTER   X2FLDNAM*15                 !Function
	CHARACTER   PROMPT*60                   !Program prompt
	CHARACTER   INTERUP*1                   !Interrupt execution
	CHARACTER   NULLEQV(60)*1               !Null string
	CHARACTER   NULL*60                     !Null string
	LOGICAL     DISLOG                      !Display log flag
	LOGICAL     PRTFLG                      !Display to printer flag
	LOGICAL     UPDFLG                      !Update flag
	EXTERNAL    X2FLDNAM
C
	DATA        NULLEQV /60*Z00/
	EQUIVALENCE (NULL,NULLEQV)
C
C CLEAR THE SCREEN AND DISPLAY THE MENU.
C
	CALL CLRSCR(5)
        IF(UPDFLG) THEN
          WRITE(5,9000)
        ELSE
          WRITE(5,9010)
        ENDIF
	REC=0
C
C CHECK TO ENSURE THE GAME IS ONLINE.
C
	IF(UPDFLG.AND.DAYSTS.NE.DSOPEN) THEN
	  WRITE(5,*)
	  PROMPT=NULL
	  WRITE (PROMPT,9060)
	  CALL WIMG(5,PROMPT)
	  CALL YESNO(ANS)
	  IF(ANS.NE.1) GOTO 8000
	  UPDFLG=.FALSE.
	ENDIF
C
C LOOP THROUGH ALL X2X NETWORK FILES.
C
	DO 100 FILIDX=1,X2XFIL_MAX_FILES
	  FILE=X2XFIL_FILE_LIST(FILIDX)
	  IF(FILE.EQ.XBLD) GOTO 100
	  CALL OPENX(1,X2FILNAM(FILE),4,0,0,ST)
	  IF(ST.NE.0)THEN
	      CALL OS32ER(5,X2FILNAM(FILE),'OPENX',ST,0)
	      CALL GPAUSE
	  ENDIF
	  CALL IOINIT(FDB,1,X2FILSEC(FILE)*256)
          WRITE(5,9070) X2FILNAM(FILE)
C
C READ THROUGH ENTIRE FILE SKIPPING EMPTY SLOTS.
C
200	  CONTINUE
	  CALL RTL(REC,X2POST_QUE(1,FILIDX),ST)
	  IF (ST.EQ.GLIST_STAT_EMPTY) THEN
	      CALL CLOSEFIL(FDB)
	    GOTO 100
	  ENDIF

	  CALL READW(FDB,REC,ALLREC,ST)
	  IF(ST.NE.0) THEN
	      CALL OS32ER(5,X2FILNAM(FILE),'READW',ST,REC)
	      CALL GPAUSE
	  ENDIF
C
C IF NO MORE RECORDS, CLOSE THE CURRENT FILE.
C
C
C CHECK BITMAP FOR ANY MODIFIED FIELDS.
C
	  IF(ALLREC(X2XFIL_BITMAP(FILIDX))  .NE.0 .OR.
     *	     ALLREC(X2XFIL_BITMAP(FILIDX)+1).NE.0 .OR.
     *	     ALLREC(X2XFIL_BITMAP(FILIDX)+2).NE.0 .OR.
     *	     ALLREC(X2XFIL_BITMAP(FILIDX)+3).NE.0) THEN
C
C SET FLAG INDICATING WHETHER A MODIFICATION (1), OR AN
C ADDITION(0).
C
	    ADDFLG=1
	    IF(BTEST(ALLREC(X2XFIL_BITMAP(FILIDX)),0) .AND.		    !V03
     *	       ALLREC(X2XFIL_BITMAP(FILIDX)+1).EQ.0) ADDFLG=0
C
C LOOP THROUGH ALL FIELDS.
C
	    DO 300 FLD=1,X2XFIL_BITMAP(FILIDX)-1
	      IF(TSBIT(ALLREC(X2XFIL_BITMAP(FILIDX)),FLD).OR.
     *	         ADDFLG.EQ.0) THEN
	        FLDNAM=X2FLDNAM(FILE,FLD)
	        IF(FLDNAM.NE.' ') THEN
	          CALL X2FLDVAL(FILE,FLD,ALLREC,VALUE,FLDIDX)
	          IF(UPDFLG) THEN
	            CALL X2BLDCMD(1,FILE,REC,FLDIDX,VALUE,1,0,CBUF)
	          ENDIF
	          IF(DISLOG) THEN
	            IF(VALUE(2).EQ.-1) VALUE(2)=0
	            IF(VALUE(3).EQ.-1) VALUE(3)=0
	            IF(VALUE(4).EQ.-1) VALUE(4)=0
	            WRITE(5,9020) X2FILNAM(FILE),
     *	                          REC, FLDNAM(1:10),(VALUE(I),I=1,4)
	            IF(PRTFLG) WRITE(6,9020) X2FILNAM(FILE),
     *	                          REC, FLDNAM(1:10),(VALUE(I),I=1,4)
	          ENDIF
	        ELSE IF(UPDFLG) THEN
	          IF(ADDFLG.EQ.0) CALL X2BLDCMD(0,FILE,REC,0,
     *	                                        DMYVAL,1,0,CBUF)
	          GOTO 310
	        ENDIF
	      ENDIF
300	    CONTINUE
C
C CLEAR THE BITMAP FLAGS AND REWRITE THE RECORD.
C
310	    CONTINUE
	    ALLREC(X2XFIL_BITMAP(FILIDX))=0
	    ALLREC(X2XFIL_BITMAP(FILIDX)+1)=0
	    ALLREC(X2XFIL_BITMAP(FILIDX)+2)=0
	    ALLREC(X2XFIL_BITMAP(FILIDX)+3)=0
	    CALL WRITEW(FDB,REC,ALLREC,ST)
	    IF(ST.NE.0) THEN
	        CALL OS32ER(5,X2FILNAM(FILE),'WRITEW',ST,REC)
	        CALL GPAUSE
	    ENDIF
	  ENDIF
C
C READ THE NEXT RECORD
C
	  GOTO 200
100	CONTINUE
C
C INTERRUPT PROGRAM OUTPUT.
C
	WRITE (PROMPT,9040)
	CALL WIMG(5,PROMPT)
	READ(5,9050) INTERUP
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T31,'Post to Memory',/)
9010    FORMAT(//,T26,'GTECH Distributed Network',/,
     *            T30,'Clearing Bitmaps',/)
9020	FORMAT(1X,A15,' Rec: ',I5,1X,A10,' Val= ',Z8,'/',
     *	          Z8,'/',Z8,'/',Z8)					! V02
9040	FORMAT(15(' '),'    Press RETURN to continue         ')
9050	FORMAT(A)
9060	FORMAT(15(' '),'Game is not up - continue ? [Y/N]')
9070    FORMAT(1X,'Scanning file ',A15,'.........')
	END
