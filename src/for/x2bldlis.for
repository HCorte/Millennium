C
C SUBROUTINE X2BLDLIS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BLDLIS.FOV                                 $
C  $Date::   17 Apr 1996 16:09:18                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2bldlis.for **
C
C X2BLDLIS.FOR
C
C V03 19-AUG-94 GPR MODIFY TO HANDLE 12 CHARS FOR UK - Integrate UK changes 
C		    into X2X Baseline
C V02 18-FEB-94 GPR USE I5 FORMAT FOR STATION AND TERMINAL TYPE-OUTS
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will display defined auto add records in
C the auto build file to the screen.  The user
C will have the option of displaying a specific station,
C or to page forward or back.
C
C Calling sequence:
C
C     CALL X2BLDLIS
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
	SUBROUTINE X2BLDLIS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XBLD.DEF'
C
	INTEGER*4   BLD,BLDCNT          !Rec #/station count
	INTEGER*4   LASTBLD             !Last station displayed
	INTEGER*4   EOFCNT              !End of file count
	INTEGER*4   I,ST,K,ERR          !Work variables
	INTEGER*4   PREV, LAST_PREV
C
	CHARACTER   PROMPT*65           !Output prompt
	CHARACTER   OPT*4               !Input option
	CHARACTER   CHRSTR(12)          !ASCII for BCD address	    !V03
	CHARACTER   X2FILNAM*20         !File name function
	LOGICAL     MORE                !More data flag
C
C DISPLAY THE NEXT SCREEN OF RECORDS.
C
	BLD=0
	LASTBLD=0
	LAST_PREV=0
	PREV=0
100	CONTINUE
	IF(BLD.LT.0) BLD=0
	EOFCNT=0
	BLDCNT=0
	CALL CLRSCR(5)
	WRITE(5,9080)
	WRITE(5,9090)
C
C READ A RECORD FROM THE FILE AND DISPLAY
C IT TO THE SCREEN.
C
200	CONTINUE
	IF(BLDCNT.LT.10 .AND. EOFCNT.LE.100) THEN
250	  BLD=BLD+1
	  CALL READW(X2XBLD_FDB,BLD,X2XBLD_REC,ST)
	  IF(ST.EQ.144) GOTO 300
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBLD),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XBLD_REC(1).LE.0) THEN
	    GOTO 250
C         EOFCNT=EOFCNT+1
C         IF(EOFCNT.LE.100) GOTO 250
C         GOTO 300
	  ELSE
	    EOFCNT=0
	  ENDIF
C
C DISPLAY THE RECORD TO THE SCREEN.
C
	  BLDCNT=BLDCNT+1
	  IF(BLDCNT.EQ.1) THEN
	    LAST_PREV=PREV
	    PREV=BLD
	  ENDIF
	  LASTBLD=BLD
	  CALL HTOA(CHRSTR,1,12,X2XBLD_ADDRES,ERR)		!V03
	  WRITE(5,9100) X2XBLD_STN,(CHRSTR(I),I=1,12),		!V03
     *	                X2XBLD_STNCLS, X2XBLD_REPCLS
	  GOTO 200
	ENDIF
C
C IF NO RECORDS EXIST, DISPLAY AN APPROPRIATE MESSAGE.
C
300	CONTINUE
	IF(BLDCNT.EQ.0) THEN
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
	  BLD=LASTBLD
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'P'.OR.OPT(1:1).EQ.'p') THEN
	  BLD=LAST_PREV-1
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'X'.OR.OPT(1:1).EQ.'x') THEN
	  BLD=CTOI(OPT(2:4),K)-1
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
     *	               'or X# for stn')
9010	FORMAT(15(' '),'Enter P for previous or X# for stn',13(' '))
9020	FORMAT(A4)
9040	FORMAT(15(' '),'Invalid option input ')
9050	FORMAT(15(' '),'Sorry, no more stations exist ')
9080	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T30,'Auto Add Listing',//)
9090	FORMAT(T42,'Station',T53,'Report',/,
     *	       T12,'Station',T23,'Address',
     *	       T43,'Class',T54,'Class',/,10X,48('='))
9100	FORMAT(T13,I5,T22,12A,T44,I3,T55,I3)				! V03
	END
