C
C SUBROUTINE LTOVER
C $Log:   GXAFXT:[GOLS]LTOVER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:59:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   13 Jul 1995 15:18:30   HXK
C  Changes for Viking Bonus
C  
C     Rev 1.2   23 Jul 1993 18:32:04   SXH
C  Released for Finland
C  
C     Rev 1.1   19 Jun 1993 13:52:30   HXK
C  added third values to BONBUF
C  
C     Rev 1.0   21 Jan 1993 16:57:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - ltover.for **
C
C LTOVER.FOR
C
C V04 30-NOV-2010 MAC LUCKY NUMBER
C V03 07-JAN-93 TD  CHANGED CALL TO LTOCHK ROUTINE TO VER_LTOCHK
C V02 12-NOV-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF LOTTO RESULTS.
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE LTOVER(GIND,DRAW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'


	! arguments
	INTEGER*4  GIND                          !
	INTEGER*4  DRAW                          !
	INTEGER*4  ST                            !

	! variables
	INTEGER*4  FLAG                          !
	INTEGER*4  K                             !
	INTEGER*4  EXT                           !
	INTEGER*4  NUM                           !
	INTEGER*4  I                             !
	INTEGER*4  BDROFF                        !
	INTEGER*4  HLD_ESP_FLAG, HLD_ESP_AMT     !

	CHARACTER*20  WINBUF(LTGNBR)             !
	CHARACTER*20  BONBUF(LTGBON)             !
	CHARACTER*35  BDR_WINBUF(LTGNBR)         !
        CHARACTER*20  LNBUF                      !V04
C
	DATA WINBUF/'Enter first   number',
     *              'Enter second  number',
     *              'Enter third   number',
     *              'Enter forth   number',
     *              'Enter fifth   number',
     *              'Enter sixth   number',
     *              'Enter seventh number',
     *              'Enter eigth   number'/
C
	DATA BONBUF/'Enter first   bonus ',
     *              'Enter second  bonus ',
     *              'Enter third   bonus '/

	DATA BDR_WINBUF/'Enter (bonus draw x) first   number',
     *                  'Enter (bonus draw x) second  number',
     *                  'Enter (bonus draw x) third   number',
     *                  'Enter (bonus draw x) forth   number',
     *                  'Enter (bonus draw x) fifth   number',
     *                  'Enter (bonus draw x) sixth   number',
     *                  'Enter (bonus draw x) seventh number',
     *                  'Enter (bonus draw x) eigth   number'/
C
        DATA LNBUF /'Enter Lucky Number  '/           !V04
C
C START CODE FOR LTOVER
C
	WRITE(5,901) IAM(),GTNAMES(TLTO),GIND,DRAW
	DO BDROFF=1,DLTBDR+1
100         CONTINUE
	    DO I=1,DLTNUM
		IF(BDROFF.EQ.1) THEN
		    CALL INPNUM(WINBUF(I),NUM,1,DLTMAX,EXT)
		ELSE
		    BDR_WINBUF(I)(19:19)=CHAR(BDROFF)
		    CALL INPNUM(BDR_WINBUF(I),NUM,1,DLTMAX,EXT)
		ENDIF
		IF(EXT.LT.0) GOTO 100
		DLTHLD(I,BDROFF)=NUM
	    END DO
C
C
	    DO I=1,DLTBFL
		IF(BDROFF.EQ.1) THEN
		    CALL INPNUM(BONBUF(I),NUM,1,DLTMAX,EXT)
		    IF(EXT.LT.0) GOTO 100
		    DLTBHL(I,BDROFF)=NUM
		ENDIF
	    END DO
C
C
	    CALL VER_LTOCHK(BDROFF,ST)
	    IF(ST.NE.0) THEN
		TYPE*,IAM(),' Duplicate numbers entered, please re-enter'
		GOTO 100
	    ENDIF
C
C
            IF (DLTLFL.GT.0) THEN                             !V04...
              CALL INPNUM(LNBUF,NUM,1,DLTLFL,EXT)
              IF(EXT.LT.0) GOTO 100
              DLTLHL(BDROFF)=NUM
            ENDIF                                             !...V04
C
C
	    WRITE(5,902) IAM(),(DLTHLD(K,BDROFF),K=1,DLTNUM)
	    IF(DLTBFL.NE.0 .AND. BDROFF.EQ.1) WRITE(5,903) 
     *                      IAM(),(DLTBHL(K,BDROFF),K=1,DLTBFL)
            IF (DLTLFL.GT.0) WRITE(5,904) IAM(), DLTLHL(BDROFF) !V04
	    CALL WIMG(5,'Are the numbers entered correct (Y/N) ')
	    CALL YESNO(FLAG)
	    IF(FLAG.NE.1) GOTO 100
C
C CHECK AGAINST OPERATOR ENTRY
C
	    DO I=1,DLTNUM
		IF(DLTWIN(I,BDROFF).NE.DLTHLD(I,BDROFF)) THEN
		    TYPE*,IAM(),' Verification error, please re-enter'
		    OPDONE=0
		    DLTSTS=GAMBFD
		    ST=-1
		    RETURN
		ENDIF
	    END DO
C
C
	    DO I=1,DLTBFL
		IF(DLTBNM(I,BDROFF).NE.DLTBHL(I,BDROFF)) THEN
		    TYPE*,IAM(),' Verification error, please re-enter'
		    OPDONE=0
		    DLTSTS=GAMBFD
		    ST=-1
		    RETURN
		ENDIF
	    END DO

            IF (DLTLFL.GT.0) THEN                             !V04...
		IF(DLTLNM(BDROFF).NE.DLTLHL(BDROFF)) THEN
		    TYPE*,IAM(),' Verification error, please re-enter'
		    OPDONE=0
		    DLTSTS=GAMBFD
		    ST=-1
		    RETURN
		ENDIF
            ENDIF                                             !...V04


	END DO

        CALL WIMG(5,'Do Your Have Special Totoloto Jackpot Fund [Y/N] ?')
        CALL YESNO(HLD_ESP_FLAG)
        HLD_ESP_AMT = 0
        IF(HLD_ESP_FLAG.EQ.1) THEN
            CALL INPMONY('Enter Total Jackpot Fund To Be Added:',HLD_ESP_AMT,BETUNIT,EXT)
            IF (EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
        ENDIF

	IF (HLD_ESP_AMT.NE.ESP_AMT) THEN
	    TYPE*,IAM(),' Verification error, please re-enter'
            OPDONE=0
            DLTSTS=GAMBFD
            ST=-1
            RETURN
	ENDIF
C
C
	ST=0
	DLTSTS=GAMENV
	IF(DLTSPR.EQ.0) DLTSTS=GAMDON     !FIXED PAYOUT GAME

	RETURN
C
C
901     FORMAT(1X,A,1X,A8,I1,' draw ',I4)
902     FORMAT(1X,A,' Numbers entered: ',8(I2.2,1X))
903     FORMAT(1X,A,'           Bonus: ',4(I2.2,1X))
904     FORMAT(1X,A,'    Lucky Number: ',I2.2)        !V04
	END
