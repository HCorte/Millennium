C
C SUBROUTINE X2STNLIS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STNLIS.FOV                                 $
C  $Date::   17 Apr 1996 16:36:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2stnlis.for **
C
C X2STNLIS.FOR
C
C V04 22-AUG-94 GPR REMOVE PRTCNT FROM LIST - Integrate UK changes 
C		    into X2X Baseline
C V03 10-OCT-94 SCD INCREASE OPT FROM 4 TO 6 SO WE CAN SELECT STATION
C		    NUMBERS GREATER THAN 999.
C V02 03-FEB-94 GPR USE I5 FORMAT FOR STATION AND TERMINAL TYPE-OUTS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will display defined stations in the
C station configuration file to the screen.  The user
C will have the option of displaying a specific station,
C or to page forward or back.
C
C Calling sequence:
C
C     CALL X2STNLIS
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
	SUBROUTINE X2STNLIS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
C
	INTEGER*4   STN,STNCNT          !Rec #/station count
	INTEGER*4   LASTSTN             !Last station displayed
	INTEGER*4   EOFCNT              !End of file count
	INTEGER*4   I,ST,K,ERR          !Work variables
	INTEGER*4   J, PREV, LAST_PREV
	CHARACTER   PROMPT*65           !Output prompt
	CHARACTER   OPT*6               !Input option - V03
	CHARACTER   CHRSTR(20)          !ASCII for BCD address
	CHARACTER   X2FILNAM*20         !File name function
	LOGICAL     MORE                !More data flag
C
C DISPLAY THE NEXT SCREEN OF STATIONS.
C
	STN=0
	LASTSTN=0
	LAST_PREV=0
	PREV=0
100	CONTINUE
	IF(STN.LT.0) STN=0
	EOFCNT=0
	STNCNT=0
	CALL CLRSCR(5)
	WRITE(5,9080)
	WRITE(5,9090)
C
C READ A STATION FROM THE FILE AND DISPLAY
C IT TO THE SCREEN.
C
200	CONTINUE
	IF(STNCNT.LT.10 .AND. EOFCNT.LE.100) THEN
250	  STN=STN+1
	  CALL READW(X2XSTN_FDB,STN,X2XSTN_REC,ST)
	  IF(ST.EQ.144) GOTO 300
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XSTN),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XSTN_REC(1).LE.0) THEN
	    GOTO 250
C         EOFCNT=EOFCNT+1
C         IF(EOFCNT.LE.100) GOTO 250
C         GOTO 300
	  ELSE
	    EOFCNT=0
	  ENDIF
C
C DISPLAY THE STATION TO THE SCREEN.
C
	  STNCNT=STNCNT+1
	  IF(STNCNT.EQ.1) THEN
	    LAST_PREV=PREV
	    PREV=STN
	  ENDIF
	  LASTSTN=STN
	  CALL HTOA(CHRSTR,1,X2XSTN_ADDLEN,X2XSTN_ADDRES,ERR)
	  DO 252 J=X2XSTN_ADDLEN+1,20
	    CHRSTR(J)=' '
252	  CONTINUE
	  WRITE(5,9100) X2XSTN_STN,(CHRSTR(I),I=1,16),X2XSTN_STNCLS,
CV04     *	                X2XSTN_REPCLS, X2XSTN_PRTCNT, X2XSTN_SERIAL,
     *	                X2XSTN_SERIAL,					  !V04
     *                  X2XSTN_STATE
	  GOTO 200
	ENDIF
C
C IF NO STATIONS EXIST, DISPLAY AN APPROPRIATE MESSAGE.
C
300	CONTINUE
	IF(STNCNT.EQ.0) THEN
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
	  STN=LASTSTN
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'P'.OR.OPT(1:1).EQ.'p') THEN
	  STN=LAST_PREV-1
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'X'.OR.OPT(1:1).EQ.'x') THEN
	  STN=CTOI(OPT(2:6),K)-1					!V03
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
9020	FORMAT(A6)							!V03
9040	FORMAT(15(' '),'Invalid option input ')
9050	FORMAT(15(' '),'Sorry, no more stations exist ')
9080	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T30,'Station Listing',//)
9090    FORMAT(T12,'Station',T23,'Address',T33,'Station',
     *         T63,'Serial',T72,'Station',/				!V04
     *         T34,'Class',T65,'#',T73,'State',/			!V04
     *         10X,68('='))
9100    FORMAT(T12,I5,T18,16A,T35,I2,T59,I10,T72,I7)	! V02
	END
