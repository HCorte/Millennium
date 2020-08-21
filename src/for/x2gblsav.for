C
C SUBROUTINE X2GBLSAV
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GBLSAV.FOV                                 $
C  $Date::   17 Apr 1996 16:18:50                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2gblsav.for;1 **
C
C X2GBLSAV.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will save the current configuration stored
C into record 1, into another specified record.
C NOTE: The X2XGBL must have been previously opened.
C
C Input parameters:
C
C     OUTREC  Int*4   Output record.
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
	SUBROUTINE X2GBLSAV(OUTREC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XGBL.DEF'
C
	INTEGER*4   OUTREC                  !Output record.
	INTEGER*4   TMPREC(64)              !Temp record buffer
	INTEGER*4   ANS                     !Yes/no response
	INTEGER*4   CNT, J, OFFSET, I, LINES, ST
	CHARACTER   X2FILNAM*20             !File name function
	CHARACTER   PROMPT*60               !Output prompt
	CHARACTER   NULEQV(60)*1 /60*Z00/   !Null string
	CHARACTER   NULL*60                 !Null string
	EQUIVALENCE(NULL,NULEQV)
C
C CLEAR THE SCREEN.
C
	CALL CLRSCR(5)
	WRITE(5,9050)
C
C READ THE SPECIFIED OUTPUT RECORD.
C
	CALL READW(X2XGBL_FDB,OUTREC,TMPREC,ST)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XGBL),'READW',ST,1)
	  CALL GPAUSE
	ENDIF
C
C IF DATA EXISTS IN THE RECORD, ASK THE USER WHETHER THEY WANT
C TO REPLACE IT.
C
	IF(TMPREC(1).GT.0) THEN
	  PROMPT=NULL
	  WRITE (PROMPT,9000) OUTREC
	  CALL WIMG(5,PROMPT)
	  CALL YESNO(ANS)
	  IF(ANS.NE.1) GOTO 8000
	ENDIF
C
C DISPLAY THE CURRENT CONFIGURATION AND ENSURE THE USER WANTS
C TO SAVE THE RECORD.
C
	LINES=X2XGBL_ENTRIES/2
	DO 100 I=1,LINES
	  OFFSET=(I-1)*2+1
	  WRITE(5,9010) (J,X2XGBL_FIELD(J),
     *	                   X2XGBL_REC(X2XGBL_INDEX(J)),
     *	                 J=OFFSET,OFFSET+1)
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=MOD(X2XGBL_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  OFFSET=(I-1)*2+1
	  WRITE(5,9010) OFFSET,X2XGBL_FIELD(OFFSET),
     *	                X2XGBL_REC(X2XGBL_INDEX(OFFSET))
	ENDIF
C
C ASK WHETHER USER WANTS TO SAVE THIS RECORD.
C
	PROMPT=NULL
	WRITE (PROMPT,9020)
	WRITE(5,*)
	CALL WIMG(5,PROMPT)
	CALL YESNO(ANS)
	IF(ANS.EQ.1) THEN
	  CALL WRITEW(X2XGBL_FDB,OUTREC,X2XGBL_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XGBL),'WRITEW',ST,OUTREC)
	    CALL GPAUSE
	  ENDIF
	  PROMPT=NULL
	  WRITE (PROMPT,9030)
	ELSE
	  PROMPT=NULL
	  WRITE (PROMPT,9040)
	ENDIF
	CALL XWAIT(2,2,ST)
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
C
C =================== Format Statements =====================
C
9000	FORMAT(10(' '),'Record ',I3,' already exists - '
     *	               'replace it [Y/N] ')
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9020	FORMAT(10(' '),'Do you want to save this configuration [Y/N] ')
9030	FORMAT(10(' '),'Configuration saved to record ',I3)
9040	FORMAT(10(' '),'Configuration has not been saved ')
9050	FORMAT(T24,'Global Network Configuration',/)
	END
