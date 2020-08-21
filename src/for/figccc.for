C FIGCCC.FOR 
C
C (BASED ON NRM_FIGWEK.FOR)
C
C V01 02-DEC-10 HXK INITIAL RELEASE FOR PORTUGAL
C
C INPUT:  CDC
C
C OUTPUT: CCC, YEAR
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
C Copyright 2010 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
!=======OPTIONS /CHECK=NOOVERFLOW
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
        INTEGER*4 I,J,IX,JX,DAYS
	LOGICAL   LOTTO3
C
C
C CALCULATE YEAR
C
        DATEAR(5)=CDC               
        CALL CDATE(DATEAR)
        YEAR=DATEAR(VYEAR)           !year that cdc is in
        DAYNO=DATEAR(VJUL)           !julian date of cdc 

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

50	CONTINUE

	IF(YEAR.LT.77) THEN
	  YEARIN = YEAR+2000
	ELSE
	  YEARIN = YEAR+1900
	ENDIF 

	IF(CCC.EQ.0) RETURN

        IF(CCC.EQ.1 .AND.DATEAR(VMON).EQ.12) YEARIN=YEARIN+1
        IF(CCC.GE.105.AND.DATEAR(VMON).EQ.1) YEARIN=YEARIN-1
C
C GET CCC FROM CCC TABLE
C
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
        RETURN
        END

