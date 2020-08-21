C
C PROGRAM UPDTKT
C $Log:   GXAFXT:[GOLS]UPDTKT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:43:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:58:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - updtkt.for **
C
C UPDTKT.FOR
C
C V01 20-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM UPDTKT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:RECTMS.DEF'
C
	INTEGER*4    TMSFDB(7), ST, FLAG, GIND, SCFNAM(5),OPT
	INTEGER*4    SCFFDB(7), LIST(NUMAGT), I, EXT, COUNT,TER
C
	CHARACTER*27 FSTLIN,SCDLIN
	BYTE	     LINE1,LINE2
	EQUIVALENCE(LINE1,FSTLIN)
	EQUIVALENCE(LINE2,SCDLIN)
C
	DATA         SCFNAM/'SCF.','FIL ',3*'    '/
C
	CALL COPYRITE
C
C READ SCF RECORD
C
	CALL OPENW(1,SCFNAM,4,0,0,ST)
	CALL IOINIT(SCFFDB,1,SCFSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
	CALL READW(SCFFDB,1,SCFREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
	CALL CLOSEFIL(SCFFDB)
	DO 10 I=1,MAXFIL
	IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
10	CONTINUE
C
C OPEN TICKET MESSAGE FILE
C
	CALL OPENW(1,SCFSFN(1,TKTM),4,0,0,ST)
	CALL IOINIT(TMSFDB,1,TMSSEC*256)
	IF(ST.NE.0) THEN
	  CALL CLOSEFIL(TMSFDB)
	  CALL FILERR(SCFSFN(1,TKTM),1,ST,0)
	ENDIF
C
C READ TICKET MESSAGE FILE
C
	CALL READW(TMSFDB,1,TMSREC,ST)
	IF(ST.NE.0) THEN
	   CALL FILERR(SCFSFN(1,TKTM),2,ST,0)
	ENDIF
C
C GET OPTION
C
100	CONTINUE
	WRITE(5,900)
	CALL INPNUM('Enter option [E-Exit] ',OPT,1,3,EXT)
	IF(EXT.NE.0) GOTO 8000
	GOTO (1000,2000,3000) OPT
C
C ENTER A TICKET MESSAGE
C
1000	CONTINUE
C
	CALL INPNUM('Enter game number of message [E-Exit] ',
     *               GIND,1,MAXGAM,EXT)
	IF(EXT.NE.0) GOTO 100
C
C GET FIRST LINE OF MESSAGE
C
	WRITE(5,901)
	CALL WIMG(5,'Enter first line  ')
	READ(5,902) FSTLIN
C
C GET SECOND LINE OF MESSAGE
C
	WRITE(5,901)
	CALL WIMG(5,'Enter second line ')
	READ(5,902) SCDLIN
C
C ASK IF MESSAGE IS CORRECT
C
	CALL WIMG(5,'Is message correct Y/N ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 100
C
C PUT MESSAGE INTO TICKET MESSAGE RECORD
C
	TYPE *,'Updating record with new ticket message'
	CALL MOVBYT(LINE1,1,TMSMES(1,GIND),1,27)
	CALL MOVBYT(LINE2,1,TMSMES(1,GIND),28,27)
	GOTO 100
C
C CHANGE A GAMES STATUS
C
2000	CONTINUE
	CALL INPNUM('Enter Game number you wish to change [E-Exit] ',
     *               GIND,1,MAXGAM,EXT)
	IF(EXT.NE.0) GOTO 100	
C
C DISABLE OR ENABLE
C
	IF(TMSGFL(GIND).NE.0) THEN
	  CALL WIMG(5,'Game currently enabled do you wish to disable ')
	  CALL YESNO(FLAG)
	  IF(FLAG.NE.1) THEN
	    TYPE *,'Game not changed'
	  ELSE
	    TYPE *,'Game is now disabled'
	    TMSGFL(GIND) = 0
	  ENDIF
	ELSE
	  CALL WIMG(5,'Game currently disabled do you wish to enable ')
	  CALL YESNO(FLAG)
	  IF(FLAG.NE.1) THEN
	    TYPE *,'Game not changed'
	  ELSE
	    TYPE *,'Game is now enabled'
	    TMSGFL(GIND) = 1
	  ENDIF
	ENDIF
	GOTO 100
C
C CHANGE AN AGENTS STATUS
C
3000	CONTINUE
	CALL ASELECT(3,LIST,COUNT,EXT)
	IF(EXT.LT.0) GOTO 100
	IF(COUNT.EQ.0) THEN
	  TYPE*,IAM(),' No agents found with field entered, try again'
	  GOTO 3000
	ELSE
	  TYPE*,IAM(),COUNT,' agents selected'
	ENDIF
C
C
	CALL INPNUM('Enter option [1=enable, 2=disable, E=exit]',
     *              OPT,1,2,EXT)
	IF(EXT.LT.0) GOTO 3000
	DO 3010 I=1,COUNT
	TER=LIST(I)
	IF(OPT.EQ.1) TMSAFL(TER)=1
	IF(OPT.EQ.2) TMSAFL(TER)=0
3010	CONTINUE
	GOTO 100
C
C WRITE TICKET MESSAGE FILE
C
8000	CONTINUE
	CALL WRITEW(TMSFDB,1,TMSREC,ST)
	IF(ST.NE.0) THEN
	   WRITE(5,903) (SCFSFN(I,TKTM),I=1,5),ST,1
	   CALL GPAUSE
	ENDIF
C
C CLOSE TICKET MESSAGE FILE
C
	CALL CLOSEFIL(TMSFDB)
	CALL GSTOP(GEXIT_SUCCESS)
C
C FORMAT SECTION
C
900	FORMAT(1X,'1. CHANGE A GAMES MESSAGE ',/,
     *         1X,'2. ENABLE/DISABLE A TICKET MESSAGE BY GAME ',/,
     *         1X,'3. ENABLE/DISABLE A TICKET MESSAGE BY AGENT ',/)
901	FORMAT(39X,'123456789012345678901234567')
902     FORMAT(A27)
903     FORMAT(1X,5A4,' write error ',I4,' record ',I4)
	END
