C
C SUBROUTINE X2GBLRES
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GBLRES.FOV                                 $
C  $Date::   17 Apr 1996 16:18:44                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2gblres.for;1 **
C
C X2GBLRES.FOR
C
C V02 21-OCT-94 GPR SET THE FIRST BIT OF THE BITMAP NOT THE LAST - Integrate
C		    UK changes into X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will restore the configuration record stored
C at record 1, from an existing configuration stored in another
C record.
C NOTE: The X2XGBL must have been previously opened.
C
C Input parameters:
C
C     INPREC  Int*4   Output record.
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
	SUBROUTINE X2GBLRES(INPREC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XGBL.DEF'
C
	INTEGER*4   INPREC                  !Output record.
	INTEGER*4   ANS                     !Yes/no response
        INTEGER*4   DMYREC(128)             !DUMMY RECORD
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
C READ THE SPECIFIED INPUT RECORD.
C
        CALL FASTMOV(X2XGBL_REC,DMYREC,X2XGBL_SECT*64)
	CALL READW(X2XGBL_FDB,INPREC,X2XGBL_REC,ST)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XGBL),'READW',ST,1)
	  CALL GPAUSE
	ENDIF
C
C IF DATA DOES NOT EXIST IN THE RECORD, DISPLAY AN ERROR AND EXIT.
C
	IF(X2XGBL_REC(1).LE.0) THEN
	  PROMPT=NULL
	  WRITE(5,9000) INPREC
          CALL X2UNLOCK(XGBL,X2XGBL_FDB,1,DMYREC)
	  CALL XWAIT(2,2,ST)
	  GOTO 8000
	ENDIF
C
C DISPLAY THE CONFIGURATION AND ENSURE THE USER WANTS
C TO SAVE THE RECORD TO RECORD 1.
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
	CALL WIMG(5,PROMPT)
	CALL YESNO(ANS)
	IF(ANS.EQ.1) THEN
CV02	  X2XGBL_BITMAP=-1
	  X2XGBL_BITMAP=1						    !V02
	  CALL WRITEW(X2XGBL_FDB,1,X2XGBL_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XGBL),'WRITEW',ST,INPREC)
	    CALL GPAUSE
	  ENDIF
	  WRITE(5,9030) INPREC
	ELSE
          CALL X2UNLOCK(XGBL,X2XGBL_FDB,1,DMYREC)
	  WRITE(5,9040)
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
9000	FORMAT(T12,'Record ',I3,' does not exist.')
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9020	FORMAT(10(' '),'Do you want to restore this ',
     *	               'configuration [Y/N] ')
9030	FORMAT(T12,'Configuration restored from record ',I3)
9040	FORMAT(T12,'Configuration has not been restored ')
9050	FORMAT(T24,'Global Network Configuration',/)
	END
