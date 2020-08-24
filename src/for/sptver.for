C SUBROUTINE SPTVER
C  
C V07 30-MAR-2015 MTK Modified Super 14 game
C V06 30-OCT-2003 FRP  Modify for Batch2 Totobola Changes.
C V05 31-JAN-2000 OXK SPGNBR -> DSPMAX (Vakio changes)
C V04 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V03 23 Jul 1993 SXH Released for Finland
C V02 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF SPORTS RESULTS
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE SPTVER(GNUM,GIND,DRAW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C

        ! arguments
	INTEGER*4  GNUM                    !
	INTEGER*4  GIND                    !
	INTEGER*4  DRAW                    !
	INTEGER*4  ST                      !

        ! variables
	INTEGER*4  Y                       !
	INTEGER*4  EXT                     !
	INTEGER*4  I,J                     !
	INTEGER*4  SCORE(2)
	INTEGER*4  BCNT

	CHARACTER*22 STRING(SPGNBR)        !
        CHARACTER*22  RSTRING2
	CHARACTER*50 RSTRING(2)
	CHARACTER    CHVAL(0:4)            !
	CHARACTER    RCHVAL(0:4)

	DATA CHVAL/' ','1','X','C','2'/
	DATA RCHVAL/' ','0','1','C','M'/

	DATA STRING/'Enter row    1:','Enter row    2:',
     *	            'Enter row    3:','Enter row    4:',
     *	            'Enter row    5:','Enter row    6:',
     *	            'Enter row    7:','Enter row    8:',
     *	            'Enter row    9:','Enter row   10:',
     *	            'Enter row   11:','Enter row   12:',
     *	            'Enter row   13:','Enter row   14:',
     *	            'Enter row   15:','Enter row   16:',
     *	            'Enter row   17:' /

        DATA RSTRING/'Enter SUPER 14 Home result:  ',
     *               'Enter SUPER 14 Away result:  '/

        DATA RSTRING2/'Enter SUPER 14 row:   '/

C
C ENTER WINNING RESULTS
C
        BCNT = 0
        IF(DSPFRG.NE.0) BCNT = 1

20	CONTINUE

	DO I=1,DSPMAX-BCNT
	    CALL INPRES(STRING(I), DSPHLD(I), DSPECD(I), .TRUE., EXT)
	    IF(EXT.LT.0) GOTO 20
        END DO

21      CONTINUE
        IF(DSPFRG.EQ.1) THEN
          DO J=1,2
            CALL INPRRES(RSTRING(J), SCORE(J), DSPECD(DSPMAX), .TRUE., EXT)
            IF(EXT.LT.0) GOTO 21
          ENDDO
          DSPHLD(DSPMAX)=ISHFT(SCORE(1),4)+IAND(SCORE(2),'0F'X)
        ENDIF

        IF(DSPFRG.EQ.2) THEN
          CALL INPRES(RSTRING2, DSPHLD(DSPMAX), DSPECD(DSPMAX), .TRUE., EXT)
          IF(EXT.LT.0) GOTO 21
        ENDIF


        WRITE(6,901) GTNAMES(TSPT),GIND,(CHVAL(DSPHLD(I)),I=1,DSPMAX-BCNT)

        IF(DSPFRG.EQ.1) THEN
          SCORE(1)=ISHFT(DSPHLD(DSPMAX),-4)
          SCORE(2)=IAND(DSPHLD(DSPMAX),'0F'X)
          WRITE(6,903) (RCHVAL(SCORE(J)),J=1,2)
        ENDIF

        IF(DSPFRG.EQ.2) THEN
          WRITE(6,904)(CHVAL(DSPHLD(14)))
        ENDIF

	CALL WIMG(6,'Are the results entered ok [Y/N]? ')
	CALL YESNO(Y)
	IF(Y.NE.1) GOTO 20
C
C VERIFY AGAINST OPERATOR ENTRY
C
	DO I=1,DSPMAX
	    IF(DSPWIN(I).NE.DSPHLD(I)) THEN
	        TYPE*,'Verification error, please re-enter'
	        OPDONE=0
	        DSPSTS=GAMBFD
	        ST=-1
	        RETURN
	    ENDIF
        END DO
C
C
	ST=0
	DSPSTS=GAMENV

	RETURN
C
901	FORMAT(1X,A8,I1,' results entered ',<DSPMAX-BCNT>(A1,1X))
903     FORMAT(1X,'SUPER 14 results entered ',2(A1,1X))
904     FORMAT(1X,'SUPER 14 results entered ',A1)

	END
