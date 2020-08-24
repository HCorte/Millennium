C
C SUBROUTINE X2GRPLIS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GRPLIS.FOV                                 $
C  $Date::   21 May 1996 20:00:22                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2grplis.for;1 **
C
C X2GRPLIS.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will display defined relay groups
C stored in the X2XGRP file to the screen.  The user
C will have the option of displaying a specific code,
C or to page forward or back.
C
C Calling sequence:
C
C     CALL X2GRPLIS
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
	SUBROUTINE X2GRPLIS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XGRP.DEF'
C
	INTEGER*4   GRP,GRPCNT          !Rec #/station count
	INTEGER*4   LASTGRP             !Last station displayed
	INTEGER*4   EOFCNT              !End of file count
	INTEGER*4   ST,K                !Work variables
	INTEGER*4   PREV, LAST_PREV
	CHARACTER   X2FILNAM*20
	CHARACTER   PROMPT*65           !Output prompt
	CHARACTER   OPT*4               !Input option
	LOGICAL     MORE                !OK to display more
C
C DISPLAY THE NEXT SCREEN OF GROUPS.
C
	GRP=0
	LASTGRP=0
	LAST_PREV=0
	PREV=0
100	CONTINUE
	IF(GRP.LT.0) GRP=0
	EOFCNT=0
	GRPCNT=0
	CALL CLRSCR(5)
	WRITE(5,9080)
	WRITE(5,9090)
C
C READ A RECORD FROM THE FILE AND DISPLAY
C IT TO THE SCREEN.
C
200	CONTINUE
	IF(GRPCNT.LT.10 .AND. EOFCNT.LE.100) THEN
250	  GRP=GRP+1
	  CALL READW(X2XGRP_FDB,GRP,X2XGRP_REC,ST)
	  IF(ST.EQ.144) GOTO 300
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XGRP),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XGRP_REC(1).LE.0) THEN
	    EOFCNT=EOFCNT+1
	    IF(EOFCNT.LE.100) GOTO 250
	    GOTO 300
	  ELSE
	    EOFCNT=0
	  ENDIF
C
C DISPLAY THE GROUP TO THE SCREEN.
C
	  GRPCNT=GRPCNT+1
	  IF(GRPCNT.EQ.1) THEN
	    LAST_PREV=PREV
	    PREV=GRP
	  ENDIF
	  LASTGRP=GRP
          IF (GRP.GT.X2X_NUM_GROUPS) THEN
            EOFCNT=101
            GOTO 300
          ENDIF
	  WRITE(5,9100) X2XGRP_GROUP,  X2XGRP_DESC
	  GOTO 200
	ENDIF
C
C IF NO GROUPS EXIST, DISPLAY AN APPROPRIATE MESSAGE.
C
300	CONTINUE
	IF(GRPCNT.EQ.0) THEN
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
	  GRP=LASTGRP
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'P'.OR.OPT(1:1).EQ.'p') THEN
	  GRP=LAST_PREV-1
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'X'.OR.OPT(1:1).EQ.'x') THEN
	  GRP=CTOI(OPT(2:4),K)-1
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
     *	               'or X# for group')
9010	FORMAT(15(' '),'Enter P for previous or X# for group',13(' '))
9020	FORMAT(A4)
9040	FORMAT(15(' '),'Invalid option input ')
9050	FORMAT(15(' '),'Sorry, no more groups exist ')
9080	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T29,'Relay Group Listing',//)
9090	FORMAT(T7,'Group',T17,'Desc',
     *	       /,T7,19('='))
9100	FORMAT(T6,I5,T14,A12)
	END
