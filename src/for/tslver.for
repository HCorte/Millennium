C
C SUBROUTINE TSLVER
C $Log:   GXAFXT:[GOLS]TSLVER.FOV  $
C  
C     Rev 1.1   09 Sep 1996 12:21:04   RXK
C  Rfss 259. Possibilty to postpone Winsel over initialized drawdate
C  
C     Rev 1.1   27 Jan 1994 17:19:38   JXP
C  Changed output to screen
C  
C     Rev 1.0   21 Jan 1993 17:54:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tslver.for **
C
C TSLVER.FOR
C
C V04 02-SEP-92 GCAN ADDED CHECK FOR EVENT DRAW DATE INSTEAD OF ENDING
C                    SALES DATE (FOR POSTPONED WINNER SELECTION).
C V02 12-NOV-91 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TSLVER(GNUM,GIND,DRAW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 EFLG, MORE, ROW, K, FLAG, ST, GNUM, I, GIND, DRAW
	INTEGER*4 CDC, EXT
	INTEGER*2 DATE(LDATE_LEN)
	INTEGER*4 RES, POOL
	DIMENSION POOL(3)
	DATA POOL/ROWWIN,ROWLOS,ROWTIE/
C
C SCAN THROUGH THE DATE TABLE FOR TOTO SELECT AND SEE WHAT
C ROWS ARE TO BE DRAW ON.
C
	CALL FASTMOV(DTSDAT,TROWDAT(1,2),MAXSRW)
	CALL FASTMOV(DTSWIN,TROWWIN(1,2),MAXSRW)
	EFLG=0
	MORE=0
	DO 1000 ROW=1,DTSRWS
	IF(DTSDAT(ROW).GT.DAYCDC) MORE=1
	IF(DTSDAT(ROW).NE.DAYCDC) GOTO 1000
	EFLG=1
	WRITE(5,900)ROW,(DTSNMS(K,1,ROW),K=1,3),
     *	                (DTSNMS(K,2,ROW),K=1,3)
C
C
	CALL INPNUM
     *	  ('Enter C=Cancel, P=Postpone, 1=Home, X=draw, 2=away, E=Exit',
     *	  RES,1,2,EXT)
	IF(EXT.EQ.-1) THEN              ! Exit
            EFLG=0
            GOTO 1200
        ENDIF
   
	IF(EXT.EQ.-2) THEN		! Postpone
	    DATE(5)=DTSDTE
	    CALL LCDATE(DATE)
10	    CONTINUE
	    WRITE(5,904) (DATE(K),K=7,13)
	    TYPE*,'Enter new date for this row '
	    CALL INPDAT(CDC,EXT)
	    IF(EXT.LT.0) RETURN
            IF(CDC.LE.DAYCDC) THEN
               TYPE*,IAM(),' Invalid date entered'
               GOTO 10
            ENDIF
            IF(CDC.GT.DTSDTE) THEN
               TYPE*,IAM(),' Date entered',CDC
               TYPE*,IAM(),' Event draw date is',DTSDTE
               CALL WIMG(5,
     *              'Do you want postpone over event draw date [Y/N]')
               CALL YESNO(FLAG)
               IF(FLAG.NE.1) GOTO 10
            ENDIF
	    TROWDAT(ROW,2)=CDC
	    GOTO 1000
	ENDIF
C
C
	IF(EXT.EQ.-5) THEN
	    TROWWIN(ROW,2)=ROWCAN
	    GOTO 1000
	ENDIF
C
C
	IF((EXT.EQ.-6).OR.(RES.EQ.1).OR.(RES.EQ.2)) THEN
           IF(EXT.EQ.-6) RES = 3                                !Draw
	   TROWWIN(ROW,2)=POOL(RES)
        ENDIF

1000	CONTINUE

1200	CONTINUE
	IF(EFLG.EQ.0) THEN
	  ST=0
	  TYPE 902,IAM(),(SCFLGN(K,GNUM),K=1,4)
	  RETURN
	ENDIF
C
C CHECK AGAINST OPERATOR ENTRY
C
	DO 1010 I=1,DTSRWS
	IF(TROWWIN(I,1).NE.TROWWIN(I,2)) THEN
	  TYPE*,'Verification error, please re-enter'
	  OPDONE=0
	  DTSSTS=GAMBFD
	  ST=-1
	  RETURN
	ENDIF
	IF(TROWDAT(I,1).NE.TROWDAT(I,2)) THEN
	  TYPE*,'Verification error, please re-enter'
	  OPDONE=0
	  DTSSTS=GAMBFD
	  ST=-1
	  RETURN
	ENDIF
1010	CONTINUE
C
	DTSSTS=GAMENV
C
	ST=0
	RETURN
C
C
900	FORMAT(1X,'Result entry for row ',I4,1X,3A4,' vs ',3A4)
902	FORMAT(1X,A,' No result entry today for ',4A4)
903	FORMAT(' Postponing row ',I4,1X,3A4,' vs ',3A4)
904	FORMAT(' Last valid date for this match is ',7A2)
	END
