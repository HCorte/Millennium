C
C SUBROUTINE SECSGNON
C $Log:   GXAFXT:[GOLS]SECSGNON.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:54:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:35:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_secsgnon.for **
C
C SECSGNON.FOR
C
C V01 26-MAY-92 WLM INITIAL RELEASE FOR NETHERLANDS
C
C
C Subroutine to check password for Security Manager
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SECSGNON(INDEX,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	CHARACTER*4  EXIT
	CHARACTER*6  CHANGE
	CHARACTER*8  PASSENT, UNSPWORD
	INTEGER*4    SIND, STATUS, SLEVEL
	INTEGER*4    INDEX, ID, ST, I
	CHARACTER*20 PASPAS
	EQUIVALENCE (PASPAS,PASSENT,EXIT,CHANGE)
C
	I = INDEX
	STATUS=1
 	CALL OPENX(2,'GXTSK:USER.FIL',4,0,0,ST)
 	CALL IOINIT(SONFDB,2,USESEC*256)
	IF (ST.NE.0) THEN
	    CALL CLRSCR(5)
	    RETURN
	ENDIF
C
C Ask for user ID
C
3	 CONTINUE
	 CALL CLRSCR(5)
	 TYPE *,'You are acting now as a Security Manager.'
C
C Read USER file for password and group level
C
	 ID = 999	 
	 CALL READW(SONFDB,ID,USEREC,ST)
	 IF (ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
C Ask for password regardless
C
	CALL PASSWORD(5,PASPAS)
	CALL CHKPAS(PASSENT,ST)
	IF(ST.NE.0) GOTO 3
C
C If CHANGE  send to change-password routine
C
	IF(CHANGE.EQ.'CHANGE') THEN
	  CALL CLRSCR(5)
	  CALL CHGPASS(ID,INDEX,SLEVEL,SIND)
	  TYPE *,IAM(),'Password changed...'
	  CALL XWAIT(3,2,ST)
	  GOTO 3
	ENDIF
C
C If user wishes to exit send status
C
	IF(EXIT.EQ.'EXIT') THEN
	  CALL CLRSCR(5)
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C Test if password entered is correct
C Unscramble password
C
	CALL ENCPAS(ID,PASWORD1,UNSPWORD)
	IF(UNSPWORD.NE.PASSENT) GOTO 3
C
C Levels are stored in nibbles  extract and put in SECLEV
C
	CALL GETNIBLE(SLEVEL,USERSECL,I)
C
	IF(SLEVEL.EQ.SECURITY)	STATUS = 0
C
 	RETURN
	END
