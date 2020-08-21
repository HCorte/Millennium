C
C SUBROUTINE FIGWEKP
C
C V01 16-FEB-2001 EPH RELEASED FOR ALPHA (PORTUGAL)
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE FIGWEKP(CDC,WEKNO,YEARIN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*2 DATE(12)
        INTEGER*4 STRTDY, DAYNO, YEAR, WEKNO, CDC, YEARIN
	INTEGER*4 MAKE_SUNDAY_FIRST_DAY

        DATE(5)=CDC
        CALL CDATE(DATE)
        YEAR=DATE(VYEAR)
        DAYNO=DATE(VJUL)
        STRTDY=MAKE_SUNDAY_FIRST_DAY(DATE(VDOW))
C
C	TEST IF THIS WEEK STARTED LAST YEAR
C
        IF (DAYNO-STRTDY.LT.0) THEN
C
C	   THIS WEEK IS THE LAST ONE OF LAST YEAR
C          SO...CALCULATE THE WEEK OF LAST DAY OF
C          LAST YEAR
C
           DATE(5)=CDC-DAYNO   
           CALL CDATE(DATE)
           YEAR=DATE(VYEAR)
           DAYNO=DATE(VJUL)
           STRTDY=MAKE_SUNDAY_FIRST_DAY(DATE(VDOW))
        ENDIF

	WEKNO = INT((DAYNO-STRTDY)/7) + 1

	IF(YEAR.LT.77) THEN
	    YEARIN = YEAR+2000
	ELSE
	    YEARIN = YEAR+1900
	ENDIF 

        RETURN
        END


C	**************************************************
	INTEGER*4 FUNCTION MAKE_SUNDAY_FIRST_DAY (WEEKDAY)
C	**************************************************
	INTEGER*4 STRTDY
        INTEGER*2 WEEKDAY
C       MAKE SUNDAY THE FIRST DAY OF THE WEEK
	STRTDY = WEEKDAY
	IF (STRTDY.EQ.7) THEN
           STRTDY = 1          
        ELSE
           STRTDY = STRTDY + 1
        ENDIF
	MAKE_SUNDAY_FIRST_DAY = STRTDY
	RETURN
	END
