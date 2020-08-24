C
C SUBROUTINE NBRVER
C $Log:   GXAFXT:[GOLS]NBRVER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:09:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:05:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nbrver.for **
C
C NBRVER.FOR
C
C V03 22-JAN-92 GCAN ALSO SET BOX AND STRAIGHT BET WINNING NUMBERS.
C V02 12-NOV-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF NUMBERS RESULTS.
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
	SUBROUTINE NBRVER(GNUM,GIND,DRAW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INTEGER*4  FLAG, EXT, NUM, ST, DRAW, GIND, GNUM, MAX
	INTEGER*4 BDROFF
	CHARACTER*20 WINBUF(MAXBDR)
	DATA WINBUF/'Enter winning number',
     *	            'Enter bonus 1 number',
     *	            'Enter bonus 2 number',
     *	            'Enter bonus 3 number',
     *              'Enter bonus 4 number'/
C
C
	MAX=999
	IF(DNBTYP.EQ.NB4TYP) MAX=9999
	WRITE(5,901) IAM(),GTNAMES(TNBR),GIND,DRAW
	DO 125 BDROFF=1,DNBBDR+1
100	   CONTINUE
	   CALL INPNUM(WINBUF(BDROFF),NUM,0,MAX,EXT)
	   IF(EXT.LT.0) GOTO 100
	   DNBHLD(BDROFF)=NUM
110	   CONTINUE
C
C
	   IF (DNBTYP.EQ.NB4TYP) THEN
	       WRITE(5,904) IAM(),DNBHLD(BDROFF)
	   ELSE
	       WRITE(5,903) IAM(),DNBHLD(BDROFF)
	   ENDIF
	   CALL WIMG(5,'Is the number entered correct (Y/N) ')
	   CALL YESNO(FLAG)
	   IF(FLAG.NE.1) GOTO 100
C
C CHECK AGAINST OPERATOR ENTRY
C
	   IF(DNBWIN(TNB3ST,BDROFF).NE.DNBHLD(BDROFF)) THEN
	     TYPE*,IAM(),' Verification error, please re-enter'
	     OPDONE=0
	     DNBSTS=GAMBFD
	     ST=-1
	     RETURN
	   ENDIF
130	   CONTINUE
125	CONTINUE
C
C
	ST=0
	DNBSTS=GAMENV
	RETURN
C
C
901	FORMAT(1X,A,1X,A8,I1,' draw ',I4)
903	FORMAT(1X,A,' Number entered:  ',I3.3)
904	FORMAT(1X,A,' Number entered: ',I4.4)
	END
