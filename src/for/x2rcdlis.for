C
C SUBROUTINE X2RCDLIS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RCDLIS.FOV                                 $
C  $Date::   17 Apr 1996 16:27:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2rcdlis.for;1 **
C
C X2RCDLIS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will display defined report codes
C stored in the X2XRCD file to the screen.  The user
C will have the option of displaying a specific code,
C or to page forward or back.
C
C Calling sequence:
C
C     CALL X2RCDLIS
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
	SUBROUTINE X2RCDLIS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XRCD.DEF'
C
	INTEGER*4   RCD,RCDCNT          !Rec #/station count
	INTEGER*4   LASTRCD             !Last station displayed
	INTEGER*4   EOFCNT              !End of file count
	INTEGER*4   I,ST,K              !Work variables
	INTEGER*4   PREV, LAST_PREV
	CHARACTER   PROMPT*65           !Output prompt
	CHARACTER   OPT*4               !Input option
	CHARACTER   X2FILNAM*20         !File name function
	CHARACTER   YESNO(0:1)*3        !Yes/no
	LOGICAL     MORE                !More data flag
C
	DATA        YESNO /'yes', ' no'/
C
C DISPLAY THE NEXT SCREEN OF CODES.
C
	RCD=0
	LASTRCD=0
	LAST_PREV=0
	PREV=0
100	CONTINUE
	IF(RCD.LT.0) RCD=0
	EOFCNT=0
	RCDCNT=0
	CALL CLRSCR(5)
	WRITE(5,9080)
	WRITE(5,9090)
C
C READ A STATION FROM THE FILE AND DISPLAY
C IT TO THE SCREEN.
C
200	CONTINUE
	IF(RCDCNT.LT.10 .AND. EOFCNT.LE.100) THEN
250	  RCD=RCD+1
	  CALL READW(X2XRCD_FDB,RCD,X2XRCD_REC,ST)
	  IF(ST.EQ.144) GOTO 300
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XRCD),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XRCD_REC(1).LE.0) THEN
	    GOTO 250
C         EOFCNT=EOFCNT+1
C         IF(EOFCNT.LE.100) GOTO 250
C         GOTO 300
	  ELSE
	    EOFCNT=0
	  ENDIF
C
C DISPLAY THE CODE TO THE SCREEN.
C
	  RCDCNT=RCDCNT+1
	  IF(RCDCNT.EQ.1) THEN
	    LAST_PREV=PREV
	    PREV=RCD
	  ENDIF
	  LASTRCD=RCD
	  WRITE(5,9100) X2XRCD_CODE,  X2XRCD_TYPE,
     *	                YESNO(X2XRCD_RESET),
     *	               (X2XRCD_DESC(I),I=1,12)
	  GOTO 200
	ENDIF
C
C IF NO CODE EXIST, DISPLAY AN APPROPRIATE MESSAGE.
C
300	CONTINUE
	IF(RCDCNT.EQ.0) THEN
	  WRITE(5,9050)
	ENDIF
C
C BUILD THE APPROPRIATE OUTPUT PROMPT.
C
	IF(EOFCNT.LE.100) THEN
	  WRITE (PROMPT,9000)
	  MORE=.TRUE.
	ELSE
	  WRITE (PROMPT,9010)
	  MORE=.FALSE.
	ENDIF
C
C ASK USER WHAT THEY WANT TO DO.
C
	WRITE(5,*)
	WRITE(5,*)
350	CONTINUE
	CALL WIMG(5,PROMPT)
	READ(5,9020) OPT
	IF(OPT(1:1).EQ.'E'.OR.OPT(1:1).EQ.'e') THEN
	  GOTO 8000
	ELSE IF(MORE.AND.(OPT(1:1).EQ.'N'.OR.OPT(1:1).EQ.'n')) THEN
	  RCD=LASTRCD
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'P'.OR.OPT(1:1).EQ.'p') THEN
	  RCD=LAST_PREV-1
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'X'.OR.OPT(1:1).EQ.'x') THEN
	  RCD=CTOI(OPT(2:4),K)-1
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'H'.OR.OPT(1:1).EQ.'h') THEN
	  CALL X2XHLP('X2RCDLIS.HLP')
	  RCD=PREV-1
	  GOTO 100
	ELSE
	  WRITE(5,9040)
	  GOTO 350
	ENDIF
C
C PROGRAM EXIT
C
8000	CONTINUE
	RETURN
C
C     ==================== Format Statements ===================
C
9000	FORMAT(15(' '),'Enter N for next, P for Previous, ',
     *	               'or X# for code')
9010	FORMAT(15(' '),'Enter P for previous or X# for code',13(' '))
9020	FORMAT(A4)
9040	FORMAT(15(' '),'Invalid option input ')
9050	FORMAT(15(' '),'Sorry, no more codes exist ')
9080	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T28,'Report Code Listing',//)
9090	FORMAT(T12,'Code',T24,'Type',T36,'Reset',
     *	       T50,'Desc',/,11X,46('='))
9100	FORMAT(T13,I3,T25,I3,T36,A3,T46,12A)
	END
