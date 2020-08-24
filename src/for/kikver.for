C
C SUBROUTINE KIKVER
C $Log:   GXAFXT:[GOLS]KIKVER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:43:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   23 Jul 1993 18:34:20   SXH
C  Released for Finland
C  
C     Rev 1.0   21 Jan 1993 16:44:48   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - kikver.for **
C
C KIKVER.FOR
C
C V02 12-NOV-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF KICKER RESULTS.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE KIKVER(GNUM,GIND,DRAW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'

        ! arguments
        INTEGER*4  GNUM          !
        INTEGER*4  GIND          !
        INTEGER*4  DRAW          !
        INTEGER*4  ST            !

        ! variables
	INTEGER*4  FLAG          !
	INTEGER*4  EXT           !
	INTEGER*4  NUM           !
C
	WRITE(5,901) IAM(),GTNAMES(TKIK),GIND,DRAW
100	CONTINUE
	CALL INPNUM('Enter winning number: ',NUM,0,DKKMAX,EXT)
	IF(EXT.LT.0) GOTO 100
	DKKHLD=NUM
C
C
110	CONTINUE
	WRITE(5,903) IAM(),DKKHLD
	CALL WIMG(5,'Is the number entered correct (Y/N) ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 100
C
C CHECK AGAINST OPERATOR ENTRY
C
	IF(DKKWIN.NE.DKKHLD) THEN
	    TYPE*,IAM(),' Verification error, please re-enter'
	    OPDONE=0
	    DKKSTS=GAMBFD
	    ST=-1
	    RETURN
	ENDIF
C
C
	ST=0
	DKKSTS=GAMENV
C
C PORTUGAL HAVE 5 DIVISION FIXED AND ONE SHAREABLE
C
C	IF(DKKSPR.EQ.0) DKKSTS=GAMDON   !FIXED PAYOUT GAME
	RETURN
C
C
901	FORMAT(1X,A,1X,A8,I1,' draw ',I4)
903	FORMAT(1X,A,' Number entered:  ',I7.7)

	END
