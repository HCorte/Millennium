C
C SUBROUTINE LTOENT
C $Log:   GXAFXT:[GOLS]LTOENT.FOV  $
C
C  V07 26-NOV-2010 MAC LUCKY NUMBER
C  V06 17-DEC-1999 PXO Added a call to result-report subroutine 
C		       and close game file 
C  V05 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                      Instant Pass Thru Phase 1
C  V04 13-JUL-1995 HXK Changes for Viking BonuS
C  V03 23-JUL-1993 SXH Released for Finland
C  V02 19-JUN-1993 HXK added third bonus values to BONBUF
C  V01 21-JAN-1993 DAB Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - ltoent.for **
C
C LTOENT.FOR
C
C V02 01-NOV-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS OPERATOR ENTRY OF LOTTO RESULTS.
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
	SUBROUTINE LTOENT(GNUM,GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'


	! arguments
	INTEGER*4  GNUM                         !
	INTEGER*4  GIND                         !
	INTEGER*4  DRAW                         !

	! variables
	INTEGER*4  FDB(7)                       !
	INTEGER*4  FLAG                         !
	INTEGER*4  K                            !
	INTEGER*4  EXT                          !
	INTEGER*4  NUM                          !
	INTEGER*4  I                            !
	INTEGER*4  ST                           !
	INTEGER*4  BDROFF                       !
	INTEGER*4  CBUF(CDLEN)                  !
	INTEGER*4  OFF                          !
	INTEGER*4  ESP_FLAG                     !      

	CHARACTER*20  WINBUF(LTGNBR)            !
	CHARACTER*20  BONBUF(LTGBON)            !
	CHARACTER*20  LNBUF                     !V07
	CHARACTER*35  BDR_WINBUF(LTGNBR)        !

	DATA WINBUF/'Enter first   number',
     *              'Enter second  number',
     *              'Enter third   number',
     *              'Enter forth   number',
     *              'Enter fifth   number',
     *              'Enter sixth   number',
     *              'Enter seventh number',
     *              'Enter eigth   number'/

	DATA BONBUF/'Enter first   bonus ',
     *              'Enter second  bonus ',
     *              'Enter third   bonus '/

	DATA LNBUF /'Enter Lucky Number  '/           !V07

	DATA BDR_WINBUF/'Enter (bonus draw x) first   number',
     *                  'Enter (bonus draw x) second  number',
     *                  'Enter (bonus draw x) third   number',
     *                  'Enter (bonus draw x) forth   number',
     *                  'Enter (bonus draw x) fifth   number',
     *                  'Enter (bonus draw x) sixth   number',
     *                  'Enter (bonus draw x) seventh number',
     *                  'Enter (bonus draw x) eigth   number'/
C
C
	IF(ONLINE) THEN
	    CALL GAMLOG(TLTO,GIND,DLTREC,LTOBLK)
	ELSE
	    CALL OPENW(2,SCFGFN(1,GNUM),4,0,0,ST)
	    CALL IOINIT(FDB,2,DLTSEC*256)
	    IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

	    CALL READW(FDB,DRAW,DLTREC,ST)
	    IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
	ENDIF
	IF(DLTSTS.NE.GAMBFD) THEN
	    WRITE(5,900) IAM(),GTNAMES(TLTO),GIND,DRAW,DLTSTS
	    CALL GPAUSE
	ENDIF
	WRITE(5,901) IAM(),GTNAMES(TLTO),GIND,DRAW
C
C
90      CONTINUE
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
		DLTWIN(I,BDROFF)=NUM
	    END DO
C
C
	    DO I=1,DLTBFL
		IF(BDROFF.EQ.1) THEN
		    CALL INPNUM(BONBUF(I),NUM,1,DLTMAX,EXT)
		    IF(EXT.LT.0) GOTO 100
		    DLTBNM(I,BDROFF)=NUM
		ENDIF
	    END DO
C
C
	    CALL LTOCHK(BDROFF,ST)
	    IF(ST.NE.0) THEN
		TYPE*,IAM(),' Duplicate numbers entered, please re-enter'
		 GOTO 100
	    ENDIF
C
C
            IF (DLTLFL.GT.0) THEN                             !V07...
              CALL INPNUM(LNBUF,NUM,1,DLTLFL,EXT)
              IF(EXT.LT.0) GOTO 100
              DLTLNM(BDROFF)=NUM
            ENDIF                                             !...V07
C
C
	    WRITE(5,902) IAM(),(DLTWIN(K,BDROFF),K=1,DLTNUM)
	    IF(DLTBFL.NE.0 .AND. BDROFF.EQ.1) WRITE(5,903) IAM(),
     *                     (DLTBNM(K,BDROFF),K=1,DLTBFL)
            IF (DLTLFL.GT.0) WRITE(5,904) IAM(), DLTLNM(BDROFF) !V07
	    CALL WIMG(5,'Are the numbers entered correct [Y/N] ')
	    CALL YESNO(FLAG)
	    IF(FLAG.NE.1) GOTO 100

	END DO

	CALL WIMG(5,'Do You Have Special Totoloto Jackpot Fund [Y/N] ?')
	CALL YESNO(ESP_FLAG)
	ESP_AMT = 0
	IF(ESP_FLAG.EQ.1) THEN
	    CALL INPMONY('Enter Total Jackpot Fund To Be Added:',ESP_AMT,BETUNIT,EXT)
	    IF (EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
	ENDIF

	DLTSTS = GAMEN1
	OPDONE = 1
C
C WAIT FOR VERIFICATION FROM REMOTE TERMINAL.
C
130     CONTINUE

	TYPE*,IAM(),' Waiting for verification from remote terminal'
	IF(DLTSTS.GE.GAMENV) THEN
	    IF(ONLINE) THEN
		DO BDROFF=1,DLTBDR+1
		    CALL FASTSET(0,CBUF,CDLEN)
		    CBUF(1) = 2
		    CBUF(2) = BDROFF
		    CBUF(3) = TCLTO
		    CBUF(6) = 'SYS '
		    CBUF(8) = GIND
		    OFF = 0

		    DO I=1,DLTNUM
			CALL ISBYTE(DLTWIN(I,BDROFF),CBUF(9),OFF)
			OFF=OFF+1
		    END DO

		    DO I=1,DLTBFL
			CALL ISBYTE(DLTBNM(I,BDROFF),CBUF(9),OFF)
			OFF=OFF+1
		    END DO

                    CALL ISBYTE(DLTLNM(BDROFF),CBUF(9),OFF)     !V07
                    OFF=OFF+1                                   !V07

		    CALL RESCMD(CBUF)

		END DO

		IF (ESP_FLAG.EQ.1) THEN
		    CALL FASTSET(0,CBUF,CDLEN)
		    CBUF(1) = 11
		    CBUF(2) = ESP_AMT
		    CBUF(3) = TCLTO
		    CBUF(6) = 'SYS '
		    CBUF(8) = GIND
		    CALL RESCMD(CBUF)
	        ENDIF

		CALL FASTSET(0,CBUF,CDLEN)
		CBUF(1) = 1
		CBUF(2) = DLTSTS
		CBUF(3) = TCLTO
		CBUF(6) = 'SYS '
		CBUF(8) = GIND
		CALL RESCMD(CBUF)

	    ELSE
		CALL WRITEW(FDB,DRAW,DLTREC,ST)
		IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
		CALL CLOSEFIL(FDB)        
		CALL LORESULT(GIND,DRAW)
	    ENDIF

	    RETURN

	ENDIF

	CALL XWAIT(5,2,ST)
	IF(DLTSTS.EQ.GAMBFD) THEN
	    TYPE*,IAM(),' Verification error, please re-enter '
	    GOTO 90
	ENDIF
	GOTO 130
C
C
C
900     FORMAT(1X,A,1X,A8,I1,' draw ',I4,' invalid game status> ',I4)
901     FORMAT(1X,A,1X,A8,I1,' draw ',I4)
902     FORMAT(1X,A,' Numbers entered: ',8(I2.2,1X))
903     FORMAT(1X,A,'           Bonus: ',4(I2.2,1X))
904     FORMAT(1X,A,'    Lucky Number: ',I2.2)        !V07
	END
