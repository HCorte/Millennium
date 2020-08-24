C SETCCC.FOR
C
C V03 Made functional until CDC 9999
C V02 12-AUG-2011 RXK "Millennium" replaced with "ES Evolution"
C V01 08-DEC-2010 HXK RELEASED FOR PORTUGAL
C
C SUBROUTINE TO DEFINE LOTTO GAME PARAMETERS.
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
C Copyright 2010 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM SETCCC
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DESPAR.DEF'

	COMMON SCFREC
C
        INTEGER*4 FILE3(5)/'FILE',':L3F','.FIL','    ','    '/
        INTEGER*4 FILE4(5)/'FILE',':L4F','.FIL','    ','    '/

        ! Variables
                                     !
	INTEGER*4 FDB3(7)            !
	INTEGER*4 FDB4(7)            !
C
	INTEGER*4 ST                 !
	INTEGER*4 DRAW3              !
	INTEGER*4 DRAW4              !
	INTEGER*4 EXT                !
        INTEGER*4 OPT, GAM, CDC, NEGCDC 
	INTEGER*4 CURYEAR, YEAR, YEAR1, DUMMY
	INTEGER*4 FIRSTDRW, CCC, CCC1
	INTEGER*4 REV1, REV2
	LOGICAL   SKIP
	LOGICAL   SKIP3
	LOGICAL   SKIP4
C
        ! start of code
C
	SKIP=.FALSE.
	WRITE (6,800)
        CALL INPNUM('Enter option',OPT,1,2,EXT)                                
        IF(EXT.NE.0) GOTO 1000                      
        IF(OPT.EQ.2) SKIP=.TRUE.            
C
10	CONTINUE
C
	CALL INPNUM('Enter CDC for draw from which draw id counter is to be updated',
     *	             CDC,3000,20000,EXT)
	IF(EXT.LT.0) GOTO 1000
C
	NEGCDC = 0-CDC
	CALL FIGCCC(NEGCDC,DUMMY,YEAR)
	CURYEAR = YEAR
	YEAR1   = YEAR
C
	IF(SKIP) THEN
C
	  CALL INPNUM('Enter TOTOLOTO game index for which draw is skipped',
     *	               GAM,3,4,EXT)
	  IF(EXT.LT.0) GOTO 1000
C
	  CALL INPNUM('Enter ES Evolution draw number to skip for that TOTOLOTO game',
     *	            FIRSTDRW,1,5000,EXT)
	  IF(EXT.LT.0) GOTO 1000
C
	  WRITE(6,906) GAM,FIRSTDRW,CDC,CURYEAR
C
	ELSE
C
	  CALL INPNUM('Enter first CCC value',
     *	               CCC,1,105,EXT)
	  IF(EXT.LT.0) GOTO 1000
	  CCC1 = CCC
C
	  CALL INPNUM('Enter Lotto game index to which this first ccc applies',
     *	               GAM,3,4,EXT)
	  IF(EXT.LT.0) GOTO 1000
C
	  CALL INPNUM('Enter draw number from which that ccc applies',
     *	            FIRSTDRW,1,5000,EXT)
	  IF(EXT.LT.0) GOTO 1000
C
	  WRITE(6,907) CCC,GAM,FIRSTDRW,CDC,CURYEAR
	ENDIF
C
C UPDATE LOTTO3, LOTTO4 GAME FILES
C
	SKIP3=.FALSE.
	SKIP4=.FALSE.
	IF(SKIP) THEN
          IF(GAM.EQ.3) THEN
	    SKIP3=.TRUE.
	  ELSE
	    SKIP4=.TRUE.
	  ENDIF
	ENDIF
C
	CALL OPENW(2,FILE3,4,0,0,ST)
	CALL IOINIT(FDB3,2,DLTSEC*256)
	IF(ST.NE.0) CALL FILERR(FILE3,1,ST,0)
C
	CALL OPENW(3,FILE4,4,0,0,ST)
	CALL IOINIT(FDB4,3,DLTSEC*256)
	IF(ST.NE.0) CALL FILERR(FILE4,1,ST,0)
C
	DRAW4 = FIRSTDRW     ! TOTOLOTO QUARTA draw 1 will be very first draw
	DRAW3 = FIRSTDRW     ! TOTOLOTO SABADO draw 1 will be second draw

	IF(GAM.EQ.3) THEN
	  DRAW4 = DRAW4 + 1
	  GOTO 300
	ENDIF
C
C UPDATE LOTTO 4
C
400     CONTINUE
C
	CALL READW(FDB4,DRAW4,DLTREC,ST)
	IF(ST.EQ.144) THEN
	    TYPE*,' Last CCC/YEAR set:',CCC-1,' /',CURYEAR
            TYPE*,' Last TOTOLOTO QUARTA draw initialized - ',DRAW4-1
	    TYPE*,' Last TOTOLOTO SABADO draw initialized - ',DRAW3-1
	    GOTO 1000
	ENDIF
	IF(ST.NE.0) CALL FILERR(FILE4,2,ST,DRAW4)
C
	IF(DLTSTS.GT.GAMOPN) THEN
	    TYPE*,'TOTOLOTO QUARTA already closed for draw ',DRAW4
	    IF(CCC-1.GT.CCC1.OR.CURYEAR.GT.YEAR1) THEN
	      TYPE*,' Last CCC/YEAR set:',CCC-1,' /',YEAR1
	      TYPE*,' Last TOTOLOTO QUARTA draw initialized - ',DRAW4-1
              TYPE*,' Last TOTOLOTO SABADO draw initialized - ',DRAW3-1
	    ELSE
	      TYPE*,' No CCC set'
	    ENDIF
	    GOTO 1000
	ENDIF
C
	IF(SKIP4) THEN
          CCC  = DLTCCC-1
          CCC1 = CCC
   	  DLTCCC = 0
          SKIP4 =.FALSE.
	ELSE
	  DLTCCC = CCC
	ENDIF
C
C UPDATE CONTROL REVISION NUMBERS FOR LOTTO 4 IF SKIPPING DRAW
C
	IF(SKIP) THEN
          CALL ILBYTE(REV1,DLTREV,0)
	  CALL ILBYTE(REV2,DLTREV,1)
          IF(REV1.NE.0.AND.REV2.NE.0) THEN
            REV1 = REV1 + 1
          ELSE
	    REV1 = 1
            REV2 = MOD(DRAW4,255)
          ENDIF
C
          CALL ISBYTE(REV1,DLTREV,0)
          CALL ISBYTE(REV2,DLTREV,1)
	ENDIF
C
	CALL WRITEW(FDB4,DRAW4,DLTREC,ST)
	IF(ST.EQ.144) THEN
	    TYPE*,'Last CCC/YEAR set:',CCC,' /',CURYEAR
            TYPE*,'Last TOTOLOTO QUARTA draw initialized - ',DRAW4-1
	    TYPE*,'Last TOTOLOTO SABADO draw initialized - ',DRAW3-1
	    GOTO 1000
	ENDIF
	IF(ST.NE.0) CALL FILERR(FILE4,3,ST,DRAW4)
        TYPE*,'UPDATED LOTTO 4 FOR DRAW,CCC,YEAR:',DRAW4,CCC,CURYEAR
C
	CCC   = CCC + 1
        CDC   = CDC + 3  !number of days to next Sabado draw = 3
	DRAW4 = DRAW4+1
	NEGCDC = 0-CDC
        CALL FIGCCC(NEGCDC,DUMMY,YEAR)
	IF(YEAR.NE.CURYEAR.OR.CCC.GT.105) THEN   ! year changes so reset CCC 
          CURYEAR = CURYEAR + 1
          CCC = 1    
        ENDIF
        IF(CDC.GT.9999) THEN
          TYPE*,'Last CCC/YEAR set:',CCC-1,' /',CURYEAR-1
          TYPE*,'Last TOTOLOTO QUARTA draw initialized - ',DRAW4-1
          TYPE*,'Last TOTOLOTO SABADO draw initialized - ',DRAW3-1
          GOTO 1000
        ENDIF
C
C UPDATE LOTTO3
C
300	CONTINUE
	CALL READW(FDB3,DRAW3,DLTREC,ST)
	IF(ST.EQ.144) THEN
	    TYPE*,' Last CCC/YEAR set:',CCC-1,' /',CURYEAR
	    TYPE*,' Last TOTOLOTO SABADO draw initialized - ',DRAW3-1
            TYPE*,' Last TOTOLOTO QUARTA draw initialized - ',DRAW4-1
	    GOTO 1000
	ENDIF
	IF(ST.NE.0) CALL FILERR(FILE3,2,ST,DRAW3)
C
	IF(DLTSTS.GT.GAMOPN) THEN
	    TYPE*,'TOTOLOTO SABADO already closed for draw ',DRAW3
	    IF(CCC-1.GT.CCC1.OR.CURYEAR.GT.YEAR1) THEN
	      TYPE*,' Last CCC/YEAR set:',CCC-1,' /',CURYEAR
	      TYPE*,' Last TOTOLOTO SABADO draw initialized - ',DRAW3-1
              TYPE*,' Last TOTOLOTO QUARTA draw initialized - ',DRAW4-1
	    ELSE
	      TYPE*,' No CCC set'
	    ENDIF
	    GOTO 1000
	ENDIF
C
	IF(SKIP3) THEN
          CCC  = DLTCCC-1
          CCC1 = CCC
	  DLTCCC = 0
          SKIP3 =.FALSE.
	ELSE
	  DLTCCC = CCC
	ENDIF
C
C UPDATE CONTROL REVISION NUMBERS FOR LOTTO 3 IF SKIPPING DRAW
C
	IF(SKIP) THEN
          CALL ILBYTE(REV1,DLTREV,0)
 	  CALL ILBYTE(REV2,DLTREV,1)
          IF(REV1.NE.0.AND.REV2.NE.0) THEN
            REV1 = REV1 + 1
          ELSE
	    REV1 = 1
            REV2 = MOD(DRAW3,255)
          ENDIF
C
          CALL ISBYTE(REV1,DLTREV,0)
          CALL ISBYTE(REV2,DLTREV,1)
	ENDIF
C
	CALL WRITEW(FDB3,DRAW3,DLTREC,ST)
	IF(ST.EQ.144) THEN
	    TYPE*,'Last CCC/YEAR set:',CCC,' /',CURYEAR
	    TYPE*,'Last TOTOLOTO SABADO draw initialized - ',DRAW3-1
            TYPE*,'Last TOTOLOTO QUARTA draw initialized - ',DRAW4-1
	    GOTO 1000
	ENDIF
	IF(ST.NE.0) CALL FILERR(FILE3,3,ST,DRAW3)
        TYPE*,'UPDATED LOTTO 3 FOR DRAW,CCC,YEAR:',DRAW3,CCC,CURYEAR
	CCC=CCC+1
        CDC=CDC+4       ! number of days to next Quarta draw = 4
	DRAW3 = DRAW3 + 1
	NEGCDC = 0-CDC
        CALL FIGCCC(NEGCDC,DUMMY,YEAR)
	IF(YEAR.NE.CURYEAR.OR.CCC.GT.105) THEN    ! year changes so reset ccc
          CURYEAR = CURYEAR + 1
          CCC = 1    
        ENDIF
        IF(CDC.GT.9999) THEN
          TYPE*,'Last CCC/YEAR set:',CCC-1,' /',CURYEAR-1
          TYPE*,'Last TOTOLOTO QUARTA draw initialized - ',DRAW4-1
          TYPE*,'Last TOTOLOTO SABADO draw initialized - ',DRAW3-1
          GOTO 1000
        ENDIF

	GOTO 400
C
C
1000	CONTINUE

	CALL CLOSEFIL(FDB3)
	CALL CLOSEFIL(FDB4)
	CALL GSTOP(GEXIT_SUCCESS)
C
C
800   FORMAT(' Program to set TOTOLOTO draw counter ',/,
     *       /,
     *       ' 1 - Set Totoloto Draw (CCC) counter from existing week/year',/,
     *       ' 2 - Allow Totoloto Draw counter to skip a draw',/,
     *       ' E - Exit')
906	FORMAT(1X,' TOTOLOTO:',I1,' skipping ES Evolution host draw:',I4,/,
     *         1X,'      cdc:',I4,' year:',I4)
907	FORMAT(1X,' Running from starting CCC :',I4,/              
     *         1X,' beginning with TOTOLOTO   :   ',I1,/,
     *         1X,' from ES Evolution draw    :',I4,/,
     *         1X,' from cdc                  :',I4,/,
     *         1X,'      year                 :',I4)

	END
C
C=========================================================================
C

        OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE FIGCCX(CDC,CCC,YEARIN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*2 DATE(12),DATEAR(12)
        INTEGER*4 STRTDY, DAYNO, YEAR, CCC, CDC, YEARIN
        INTEGER*4 I,J,IX,JX,DAYS
        LOGICAL   LOTTO3
	LOGICAL   FROMTABLE
C
C
	FROMTABLE = .TRUE.
	IF(CDC.LT.0) THEN
	  FROMTABLE=.FALSE.      ! if cdc -ve the calculate ccc,year
	  CDC = 0 - CDC
	ENDIF
C
C CALCULATE YEAR
C

        DATEAR(5)=CDC
        CALL CDATE(DATEAR)
        YEAR=DATEAR(VYEAR)           !year that cdc is in
        DAYNO=DATEAR(VJUL)           !julian date of cdc

	IF(FROMTABLE) GOTO 111

        DATE(VJUL)=1                 !now set julian date to 1st Jan
        DATE(VYEAR)=YEAR             !set year
        CALL JDATE(DATE)
        STRTDY=DATE(VDOW)            !get 1st 'day' of the year (e.g. Tue =2)

        DAYS=0
        IF(STRTDY.EQ.MONDAY.OR.STRTDY.EQ.TUESDAY.OR.
     *     STRTDY.EQ.WEDNESDAY.OR.STRTDY.EQ.SUNDAY) THEN
          LOTTO3=.TRUE.   ! lotto 3 is 1st draw of year
          IF(STRTDY.EQ.MONDAY) DAYS=-1
          IF(STRTDY.EQ.TUESDAY) DAYS=-2
          IF(STRTDY.EQ.WEDNESDAY) DAYS=-3
        ELSE
          LOTTO3=.FALSE.   ! lotto 3 is 2nd draw of year
          IF(STRTDY.EQ.FRIDAY) DAYS=-1
          IF(STRTDY.EQ.SATURDAY) DAYS=-2
        ENDIF

        DO I=1,105
          IF(LOTTO3) THEN
            IF(MOD(I,2).EQ.0) THEN
              DAYS=DAYS+3
            ELSE
              DAYS=DAYS+4
            ENDIF
          ELSE
            IF(MOD(I,2).EQ.0) THEN
              DAYS=DAYS+4
            ELSE
              DAYS=DAYS+3
            ENDIF
          ENDIF
          IF(DAYS.GE.DAYNO) THEN
            CCC=I
            GOTO 50
          ENDIF
        ENDDO
        CCC=0   !error

50      CONTINUE

        IF(YEAR.LT.77) THEN
          YEARIN = YEAR+2000
        ELSE
          YEARIN = YEAR+1900
        ENDIF

        IF(CCC.EQ.0) RETURN

        IF(CCC.EQ.1 .AND.DATEAR(VMON).EQ.12) YEARIN=YEARIN+1
        IF(CCC.GE.105.AND.DATEAR(VMON).EQ.1) YEARIN=YEARIN-1

	RETURN

C
C GET CCC FROM CCC TABLE
C
111     CONTINUE

        YEAR=MOD(YEAR,100)
        YEAR=YEAR-DAYYER
        IF(YEAR.LE.1.AND.YEAR.GE.-4) THEN
          DO J=1,105
            DO I=1,2  !search both Lotto3 and Lotto4 games
              IF(CCCDRW(YEAR,J,I,LTGCCC_DCDC).GT.CDC) THEN
                IX=I
                JX=J
                GOTO 100
              ENDIF
            ENDDO
          ENDDO
C
          CCC = 0
          RETURN
C
100       CONTINUE
          IF(IX.EQ.1) THEN
            I=2
            J=JX-1
          ELSE
            I=1
          ENDIF
          CCC = CCCDRW(YEAR,J,I,LTGCCC_DCDC)
        ELSE
          CCC = 0
        ENDIF
C
        IF(YEAR.LT.77) THEN
          YEARIN = YEAR+2000
        ELSE
          YEARIN = YEAR+1900
        ENDIF
C
        RETURN
        END
