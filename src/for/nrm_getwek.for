C NRM_GETWEK.FOR
C
C V03 31-JAN-2011 HXK MOD(YEAR,100)
C V02 13-DEC-2010 HXK LOTTO 2 CHANGES - ADDED CCC
C V01 13-DEC-2000 ANG Initial release for Portugal.
C  
C INPUT:
C    DRAW NUMBER THAT YOU WANT THE WEEK OR CCC
C    GAME NUMBER
C
C OUTPUT:
C    WEEK
C    YEAR
C    ST - 0  = OK
C         -1 = ERROR
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C as authorized in writing by an officer of GTECH. Neither this item            
C nor the information it contains may be used, transferred,                     
C reproduced, published, or disclosed, in whole or in part, and                 
C directly or indirectly, except as expressly authorized by an                  
C officer of GTECH, pursuant to written agreement.                              
C                                                                               
C Copyright 2010,1999 GTECH Corporation. All rights reserved.            
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
    
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE GETWEK(DRAW,GNUM,WEEK,YEAR,ST)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'

	INTEGER*4 DRAW, GNUM, WEEK, YEAR, ST
	INTEGER*4 AUXYEAR, AUXWEEK
	INTEGER*4 GTYP,GIND
	LOGICAL   TOTOLOTO
C
C
	ST   = 0
	WEEK = 0
	YEAR = 0
C
C CHECK FOR VALID INPUT
C
	IF (DRAW.LE.0) THEN
	    ST = -1
	    RETURN
	ENDIF

        IF (GNUM.LE.0.OR.GNUM.GT.MAXGAM) THEN
            ST = -1
            RETURN
        ENDIF

	TOTOLOTO = .FALSE.
	GTYP = GNTTAB(GAMTYP,GNUM)
	GIND = GNTTAB(GAMIDX,GNUM)

        IF (GTYP.EQ.TLTO) THEN
          IF (GIND.LE.0.OR.GIND.GT.MAXIND) THEN
            ST = -1
            RETURN
	  ENDIF
	  IF(LTOLFL(GIND).NE.0) THEN
            TOTOLOTO=.TRUE.
            IF (GIND.NE.3.AND.GIND.NE.4) THEN
	      ST = -1
	      RETURN
	    ENDIF
	  ENDIF
        ENDIF
C
C TRY TO FIND IT IN MEMORY TABLE
C
	IF (TOTOLOTO) THEN     !check for ccc instead of week
	  DO AUXYEAR=-4,1
	    DO AUXWEEK=1,105
		IF (CCCDRW(AUXYEAR,AUXWEEK,GIND-2,LTGCCC_DRAW).EQ.DRAW) THEN
                    YEAR = (DAYYER - (AUXYEAR*-1)) + 2000
		    WEEK = AUXWEEK
		    RETURN
		ENDIF
	    ENDDO
	  ENDDO
	ELSE
	  DO AUXYEAR=-4,1
	    DO AUXWEEK=1,53
		IF (WEKDRW(AUXYEAR,AUXWEEK,GNUM).EQ.DRAW) THEN !para desde que comessou o sistema têm um draw para semana de cada ano?
                    YEAR = (DAYYER - (AUXYEAR*-1)) + 2000
		    WEEK = AUXWEEK
		    RETURN
		ENDIF
	    ENDDO
	  ENDDO
	ENDIF
C
C NOT IN MEMORY...SO, BYE !!
C
	ST = -1
	RETURN
	
	END
C
C ==========================================================================
C
C CCCWEK
C
C INPUT:
C    CCC  - CCC (DRAW ID) NUMBER FOR WHICH YOU WANT THE WEEK
C    YEAR
C    GNUM - GAME NUMBER
C
C OUTPUT:
C    CCCWEK - WEEK
C
C
        OPTIONS /CHECK=NOOVERFLOW/EXT
        INTEGER*4 FUNCTION CCCWEK(INYEAR,CCC,GNUM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 CCC, INYEAR, GNUM
        INTEGER*4 YEAR
        INTEGER*4 GTYP,GIND
C
C
        CCCWEK = 0
        YEAR = 0
C
C CHECK FOR VALID INPUT
C
        IF (CCC.LE.0.OR.CCC.GT.106) RETURN
C
        IF (GNUM.NE.6.AND.GNUM.NE.7) RETURN
C
        GTYP = GNTTAB(GAMTYP,GNUM)
        GIND = GNTTAB(GAMIDX,GNUM)
C
        IF (GTYP.NE.TLTO) RETURN
C
        IF (GIND.NE.3.AND.GIND.NE.4) RETURN
C
        YEAR = MOD(INYEAR,100) - DAYYER
C
C CHECK IF THIS YEAR IS IN TABLE, IF SO GET WEEK CORRESPONDING TO CCC
C
        IF (YEAR.LE.1.AND.YEAR.GE.-4) CCCWEK = CCCDRW(YEAR,CCC,GIND-2,
     *                                                LTGCCC_WEEK)
C
        RETURN
C
        END
C
C
C =========================================================================
C
C FIGCCC
C
C (BASED ON NRM_FIGWEK.FOR)
C
C INPUT:  CDC (if +ve then look up for cccdrw table [STANDARD USAGE], 
C              if -ve then calculate [SETCCC USAGE] )
C              
C OUTPUT: CCC, YEAR
C
C
        OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE FIGCCC(CDC,CCC,YEARIN)
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
        INTEGER*4 I,J,Y,JX,YX,DAYS
	INTEGER*4 LOYR,HIYR,L1CDC,L2CDC
        LOGICAL   LOTTO3
	LOGICAL   FROMTABLE
C
C
	FROMTABLE = .TRUE.
	IF(CDC.LT.0) THEN
          FROMTABLE=.FALSE.      ! if cdc -ve the calculate ccc,year
          CDC = 0 - CDC
        ENDIF

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
111	CONTINUE

        YEAR=MOD(YEAR,100)
        YEAR=YEAR-DAYYER

        IF(YEAR.LE.1.AND.YEAR.GE.-4) THEN
	  LOYR=MAX(-4,YEAR-1)
	  HIYR=MIN(1,YEAR+1)

	  DO Y=LOYR,HIYR
          DO J=1,105
            YX=Y
            JX=J
            L1CDC=CCCDRW(Y,J,1,LTGCCC_DCDC)
	    L2CDC=CCCDRW(Y,J,2,LTGCCC_DCDC)
            IF(L1CDC.GT.0) THEN 
              IF(CCCDRW(Y,J,1,LTGCCC_DCDC).GE.CDC) GOTO 100
            ELSEIF(L2CDC.GT.0) THEN 
              IF(CCCDRW(Y,J,2,LTGCCC_DCDC).GE.CDC) GOTO 100
	    ENDIF
	  ENDDO
	  ENDDO
C
          CCC = 0
          RETURN
C
100       CONTINUE
C
          CCC = JX
          YEAR = YX
        ELSE
          CCC = 0
        ENDIF
C
	YEAR = YEAR + DAYYER
        IF(YEAR.LT.77) THEN
          YEARIN = YEAR+2000
        ELSE
          YEARIN = YEAR+1900
        ENDIF
C
        RETURN
        END
C
C ========================================================================
C
C
C WEKCCC
C
C INPUT:
C    WEEK    - WEEK FOR WHICH YOU WANT THE CCC (DRAW ID) NUMBER
C    INYEAR  - YEAR
C    GNUM    - GAME NUMBER
C
C OUTPUT:
C    WEKCCC - WEEK
C
C
        OPTIONS /CHECK=NOOVERFLOW/EXT
        INTEGER*4 FUNCTION WEKCCC(INYEAR,WEEK,GNUM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 WEEK, INYEAR, GNUM
        INTEGER*4 YEAR, DRAW, ST
        INTEGER*4 GTYP,GIND
C
C
        WEKCCC = 0
        YEAR = 0
C
C CHECK FOR VALID INPUT
C
        IF (WEEK.LE.0.OR.WEEK.GT.53) RETURN
C
        IF (GNUM.NE.6.AND.GNUM.NE.7) RETURN
C
        GTYP = GNTTAB(GAMTYP,GNUM)
        GIND = GNTTAB(GAMIDX,GNUM)
C
        IF (GTYP.NE.TLTO) RETURN
C
        IF (GIND.NE.3.AND.GIND.NE.4) RETURN
C
        YEAR = MOD(INYEAR,100) - DAYYER
C
C CHECK IF THIS YEAR IS IN TABLE, IF SO GET WEEK CORRESPONDING TO CCC
C
        IF (YEAR.LE.1.AND.YEAR.GE.-4) THEN
           DRAW = WEKDRW(YEAR,WEEK,GNUM)            !get draw from week
           CALL GETWEK(DRAW,GNUM,WEKCCC,INYEAR,ST)  !get ccc from draw
        ENDIF
C
        RETURN
C
        END

