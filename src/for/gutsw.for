C
C SUBROUTINE GUTSW
C $Log:   GXAFXT:[GOLS]GUTSW.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:28:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   03 Sep 1993 14:43:48   SXH
C  Fix problem with PRMTEXT
C  
C     Rev 1.2   03 Sep 1993 11:57:24   SXH
C  Fix DCL error,copy=1, added IAM()
C  
C     Rev 1.1   07 Jun 1993 15:27:24   HXN
C  ADD PARAMETER SWAP_LU=12
C  
C     Rev 1.0   21 Jan 1993 16:34:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gutsw.for **
C
C GUTSW.FOR
C
C V06 01-SEP-92 HDB  CORRECTED HARDWARE ID 6, DON'T FORGET NAME IN
C                    BLDSYS, OTHERWISE FORTRAN WILL CREATE FILE XXX.DAT
C V06 29-JUL-92 WLM  TOOK SWAP FILE NAME FROM SFNAMES TABLE
C V05 07-JUL-92 GCAN CHANGED RECORD LENGTH TO 120 (MUST BE MULTIPLE OF 4)
C                    AND CHANGED OPEN TO USE FORTRAN OPEN.
C V04 30-MAY-92 WLM  FIXED INCONSISTENCIES IN RECORD LAYOUT 
C		     (HERE AND IN RECGUT.DEF)
C V03 11-DEC-91 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V02 03-JUN-91 TKO  Reverse bytes for GLITS
C V01 28-MAR-91 JPJ  INITAL RELEASE FOR MARYLAND
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
	SUBROUTINE GUTSW
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
C	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:RECGUT.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:TMSAGT.DEF'
	INCLUDE 'INCLIB:TMFREP.DEF'
C
	INTEGER*4   LUN
C
	INTEGER*4   LOCTAB(NUMAGT)
	INTEGER*4   TERMINAL
	INTEGER*4   I,J,K,ST
	INTEGER*4   IND
	INTEGER*4   HR,MIN,SEC
	INTEGER*4   YNFLG
	INTEGER*4   AGTNBR
	INTEGER*2   DATE(12)
	INTEGER*4   DD,MM,YY
	INTEGER*4   TIME(3)
	INTEGER*4   DD2,MM2,YY2
        INTEGER*4   INLEN

	CHARACTER    CHRBUF*120
        CHARACTER*4  CYNFLG

	LOGICAL     FIRST/.TRUE./
	LOGICAL	    CHANGE
	LOGICAL	    ERRREP/.FALSE./

	INTEGER*4   I4TEMP
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I1TEMP)

	CHARACTER*20 SWAP
	EQUIVALENCE (SFNAMES(1,GTSW),SWAP)
        EQUIVALENCE(YNFLG,CYNFLG)


C BEGIN CODE -----------------------------------

C IF FIRST, OPEN SWAP FILE AND OPEN GUTS.FIL
C ------------------------------------------
	IF(FIRST)THEN
	  FIRST = .FALSE.
	  DATE(VCDC) = DAYCDC
	  CALL CDATE(DATE)
	  DD2 = DATE(VDAY)
	  MM2 = DATE(VMON)
	  YY2 = DATE(VYEAR)
	  CALL FASTSET(0,LOCTAB,NUMAGT)
C
C         OPEN SWAP FILE
C
	  CALL DFILX(SWAP,0,0,ST)
C
	  LUN = 12
          OPEN(UNIT=LUN,FILE=SWAP,IOSTAT=ST,
     *         STATUS='NEW',RECL=120/4,ACCESS='SEQUENTIAL',
     *         CARRIAGECONTROL='NONE',RECORDTYPE='FIXED',
     *         FORM='UNFORMATTED')
	  IF(ST.NE.0) CALL FILERR(SFNAMES(1,GTSW),1,ST,0)

	  CALL ICLOCK(2,TIME)
	  HR  =  TIME(2)/3600
	  MIN = (TIME(2)-HR*3600)/60
	  SEC = (TIME(2)-HR*3600)-MIN*60

	  WRITE(CHRBUF,900) DATE(VMON),DATE(VDAY),DATE(VYEAR),
     *                  HR,MIN,SEC,
     *	  	        CHAR('0D'X),CHAR('0A'X)
	  WRITE(LUN) CHRBUF
	ENDIF
C     ====================== Main Processing ========================

	IF(EOF) GOTO 1000
	IF(TRABUF(TSTAT).NE.GOOD) GOTO 9000     !EXIT

	CHANGE = .FALSE.

	DO 300 K = 1,GUTLEN
	   IF((K.GE.11.AND.K.LE.12).OR.(K.GT.17)) GOTO 300   !POSSIBBLY ZERO
	   IF(TRABUF(TSOLD+K-1).EQ.0) THEN
	     IF(ERRREP) THEN
	       TYPE*,IAM(),' NO REVISION DATA ON SIGNON FOR TER ',TRABUF(TTER)
	       TYPE*,IAM(),' AT OFFSET  --- > ',K
	       TYPE*,IAM(),' '
	       CALL PRMTEXT('Do you wish to suppress error reporting ? ',
     *                       CYNFLG,INLEN )
	       IF(YNFLG.EQ.1) ERRREP = .FALSE.
	     ENDIF
	   ENDIF


C          IF ANY HARDWARE DIFFERENCES MARK AS CHANGED
C          -------------------------------------------
	   IF(TRABUF(TSOLD+K-1).NE.GTSTAB(TRABUF(TTER),K)) CHANGE = .TRUE.
300	CONTINUE




C UPDATE SWAP FILE IF ANY CHANGE OCCURED
C --------------------------------------
	IF(CHANGE) THEN
	  CALL FASTSET(0,RECGUT,GUTLEN)

	  AGTNBR = TRABUF(TAGT)  !PUT AGENT NUMBER INTO GUTS ARRAY

	  DATE(VCDC) = TRABUF(TCDC) !PUT DATE INTO GUTS ARRAY
	  CALL CDATE(DATE)
	  DD = DATE(VDAY)
	  MM = DATE(VMON)
	  YY = DATE(VYEAR)

	  SEC = TRABUF(TTIM)        !PUT TIME INTO GUTS ARRAY
          IF(SEC.GT.'40000000'X) SEC=SEC-'40000000'X
          HR = SEC/3600
          SEC = SEC-(HR*3600)
          MIN = SEC/60
          SEC = SEC-(MIN*60)

	  IND = 0                   !PUT TERMINAL HARDWARE ID INTO GUTS ARRAY
	  RECGUT(IND/4+1) = TRABUF(TSOLD)
	  RECGUT(IND/4+2) = TRABUF(TSOLD+1)
	  IND = IND + 8

          RECGUT(IND/4+1) = TRABUF(TSOLD+2) !PUT PRINTER HARDWARE ID INTO GUTS ARRAY
          RECGUT(IND/4+2) = TRABUF(TSOLD+3)
          IND = IND + 8

C         PUT SSR HARDWARE ID INTO GUTS ARRAY
C	  -----------------------------------
	  RECGUT(IND/4+1) = TRABUF(TSOLD+4)
	  RECGUT(IND/4+2) = TRABUF(TSOLD+5)
	  IND = IND + 8

C 	  PUT SMAT CARD HARDWARE ID INTO GUTS ARRAY
C 	  -----------------------------------------
	  RECGUT(IND/4+1) = TRABUF(TSOLD+6)
	  RECGUT(IND/4+2) = TRABUF(TSOLD+7)
	  IND = IND + 8

C         PUT TV CONTROLER HARDWARE ID INTO GUTS ARRAY
C         --------------------------------------------
	  RECGUT(IND/4+1) = TRABUF(TSOLD+8)
	  RECGUT(IND/4+2) = TRABUF(TSOLD+9)
	  IND = IND + 8

C         EMPTY SLOTS
C         -----------
	  IND = IND + 8

C         PUT CPU ROM REV INTO GUTS ARRAY
C         -------------------------------
	  RECGUT(IND/4+1) = TRABUF(TSOLD+12)
	  IND = IND + 4

C         PUT PRINTER ROM REV INTO GUTS ARRAY
C         -----------------------------------
	  RECGUT(IND/4+1) = TRABUF(TSOLD+13)
	  IND = IND + 4


C         PUT SSR ROM REV INTO GUTS ARRAY
C         -------------------------------
	  RECGUT(IND/4+1) = TRABUF(TSOLD+14)
	  IND = IND + 4

C         PUT SMART CARD ROM REV INTO GUTS ARRAY
C         --------------------------------------
	  RECGUT(IND/4+1) = TRABUF(TSOLD+15)
	  IND = IND + 4

C         PUT TV CONTROLER ROM REV INTO GUTS ARRAY
C         ----------------------------------------
	  RECGUT(IND/4+1) = TRABUF(TSOLD+16)
	  IND = IND + 4

C         WRITE OUT RECORD
C         ----------------
	  WRITE(CHRBUF,10000) AGTNBR,MM,DD,YY,HR,MIN,SEC,
     *                    (RECGUT(J),J=1,GUTLEN),
     *		          CHAR('0D'X),CHAR('0A'X)
	  WRITE(LUN) CHRBUF
	  LOCTAB(TRABUF(TTER))=LOCTAB(TRABUF(TTER))+1
	ENDIF




C       UPDATE LOCAL GUTS DATABASE WITH DATA FROM SIGNON
C       ------------------------------------------------
	DO 400 K=1,GUTLEN
	  GTSTAB(TRABUF(TTER),K)=TRABUF(TSOLD+K-1)
400	CONTINUE


	GOTO 9000

C     ==================== End of Main Processing ===================




1000	CONTINUE


C UPDATE LOCAL GUTS DATABASE FILE
C -------------------------------
	CALL OPENASF(ASF)
	TYPE *,IAM(),' UPDATING ASF.FIL WITH SWAP INFO'

	DO 1100 I = 1,NUMAGT
	   IF(LOCTAB(I).NE.0) THEN
	     CALL READASF(I,ASFREC,ST)
	     DO 1110 J = 1,GUTLEN
	        ASFGUT(J) = GTSTAB(I,J)
1110	     CONTINUE
	     CALL WRITASF(I,ASFREC,ST)
	     IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ST,0)
	   ENDIF
1100	CONTINUE

	CALL CLOSASF
	TYPE *,IAM(),' DONE ASF UPDATE '



C WRITE OUT REMAINDER OF CONFIGURATIONS TO SWAP FILE
C --------------------------------------------------
	DO 2000 TERMINAL = 1,NUMAGT
	   IF(LOCTAB(TERMINAL).GT.0)    GOTO 2000
	   IF(GTSTAB(TERMINAL,13).LE.0) GOTO 2000   !NO CPU ROM REV SKIP
	   CALL FASTSET(0,RECGUT,GUTLEN)

C          PUT AGENT NUMBER INTO GUTS ARRAY
C          --------------------------------
	   AGTNBR = AGTTAB(AGTNUM,TERMINAL)

C          PUT TERMINAL HARDWARE ID INTO GUTS ARRAY
C          ----------------------------------------
	   IND = 0
	   RECGUT(IND/4+1) = GTSTAB(TERMINAL,1)
	   RECGUT(IND/4+2) = GTSTAB(TERMINAL,2)
	   IND = IND + 8

C          PUT PRINTER HARDWARE ID INTO GUTS ARRAY
C	   ---------------------------------------
	  RECGUT(IND/4+1) = GTSTAB(TERMINAL,3)
	  RECGUT(IND/4+2) = GTSTAB(TERMINAL,4)
	  IND = IND + 8


C         PUT SSR HARDWARE ID INTO GUTS ARRAY
C	  -----------------------------------
	  RECGUT(IND/4+1) = GTSTAB(TERMINAL,5)
	  RECGUT(IND/4+2) = GTSTAB(TERMINAL,6)
	  IND = IND + 8


C 	  PUT SMART CARD HARDWARE ID INTO GUTS ARRAY
C	  ------------------------------------------
          RECGUT(IND/4+1) = GTSTAB(TERMINAL,7)
          RECGUT(IND/4+2) = GTSTAB(TERMINAL,8)
          IND = IND + 8

C 	  PUT TV CONTROLER HARDWARE ID INTO GUTS ARRAY
C	  --------------------------------------------
          RECGUT(IND/4+1) = GTSTAB(TERMINAL,9)
          RECGUT(IND/4+2) = GTSTAB(TERMINAL,10)
          IND = IND + 8


C 	  EMPTY SLOT
C	  ----------
	  IND = IND + 8


C 	  PUT CPU ROM REV INTO GUTS ARRAY
C	  -------------------------------
	  RECGUT(IND/4+1) = GTSTAB(TERMINAL,13)
	  IND = IND + 4


C 	  PUT PRINTER ROM REV INTO GUTS ARRAY
C	  -----------------------------------
	  RECGUT(IND/4+1) = GTSTAB(TERMINAL,14)
	  IND = IND + 4


C 	  PUT SSR ROM REV INTO GUTS ARRAY
C	  -------------------------------
	  RECGUT(IND/4+1)=GTSTAB(TERMINAL,15)
	  IND = IND + 4


C 	  PUT SMART CARD ROM REV INTO GUTS ARRAY
C	  --------------------------------------
          RECGUT(IND/4+1) = GTSTAB(TERMINAL,16)
          IND = IND + 4

C 	  PUT TV CONTROLER ROM REV INTO GUTS ARRAY
C	  ----------------------------------------
          RECGUT(IND/4+1) = GTSTAB(TERMINAL,17)
          IND = IND + 4



	  WRITE(CHRBUF,10000) AGTNBR,MM2,DD2,YY2,0,0,0,
     *                    (RECGUT(J),J=1,GUTLEN),
     *		          CHAR('0D'X),CHAR('0A'X)
	  WRITE(LUN) CHRBUF
2000	CONTINUE
	WRITE(CHRBUF,901) CHAR('0D'X),CHAR('0A'X)
	WRITE(LUN) CHRBUF
	CALL USRCLOS1(LUN)
9000	CONTINUE
	RETURN



10000	FORMAT(I8.8,6(I2.2),<GUTLEN>A4,'  ',2(A1))
900	FORMAT(I2.2,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2,
     *         101(' '),2(A1))
901	FORMAT('EOF',115(' '),2(A1))
	END
