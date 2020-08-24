C
C PROGRAM TVERIFY
C $Log:   GXAFXT:[GOLS]TVERIFY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:40:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:55:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tverify.for **
C
C TVERIFY.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C PROGRAM TO VERIFY TMF/TCF TAPES
C
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
	PROGRAM TVERIFY
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
C
	INTEGER*4 I, TYP, EXT, DRIVE, ST
	INTEGER*4 FDB(7),TRANS(6),LOGREC(LREC*3)
	CHARACTER*20 MAGNAM
	CHARACTER*12 TYPNAM(6)
	LOGICAL EOT/.FALSE./
	DATA MAGNAM/'MAGX:'/
	DATA TYPNAM/'WAGERS      ',
     *	            'CANCELS     ',
     *	            'DELETES     ',
     *	            'VALIDATIONS ',
     *	            'SPECIALS    ',
     *	            'COMMANDS    '/
C
C
	CALL COPYRITE
C
C
	CALL INPNUM('Enter tape drive number ',DRIVE,1,9,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	MAGNAM(4:4)=CHAR(DRIVE+48)
	CALL FASTSET(0,TRANS,6)
	CALL TAPOPEN(FDB,MAGNAM,ST)
	CALL TAPINT(FDB,1,8192)
	CALL XREWIND(FDB,ST)
	CALL OPENX(6,'PR:',4,0,0,ST)
C
C
20	CONTINUE
	CALL READTAPE(LOGREC,FDB,EOT)
	IF(EOT) GOTO 30
	CALL LOGTRA(TRABUF,LOGREC)
	TYP=TRABUF(TTYP)
	IF(TYP.LT.1.OR.TYP.GT.6) THEN
	  TYPE*,'Bad transaction type ',TYP,' for serial ',TRABUF(TSER)
	  GOTO 20
	ENDIF
	TRANS(TYP)=TRANS(TYP)+1
	GOTO 20
C
C
30	CONTINUE
	CALL XREWIND(FDB,ST)
	WRITE(5,901)
	DO 40 I=1,6
	WRITE(5,902) TYPNAM(I),TRANS(I)
	WRITE(6,902) TYPNAM(I),TRANS(I)
40	CONTINUE
	TYPE*,'Tape verify complete'
	WRITE(5,901)
	CALL USRCLOS1(     6)
	CALL TAPCLOS(FDB,ST)
	CALL GSTOP(GEXIT_SUCCESS)
C
C
901	FORMAT(////)
902	FORMAT(1X,A12,1X,I8)
	END
