C
C SUBROUTINE WKNUMCDC
C $Log:   GXAFXT:[GOLS]WKNUMCDC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:02:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   01 Oct 1993 11:58:52   GXA
C  Removed cluge, what was it for? No comments.....
C  
C     Rev 1.0   21 Jan 1993 18:07:08   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_wknumcdc.for **
C
C WKNUMCDC.FOR
C
C VO2 26-MAY-92 HDB ... 
C V01 01-AUG-90 XXX RELEASED FOR VAX
C V01 22-SEP-89    LOU R.  INITIAL RELEASE FOR FINLAND.
C
C THIS ROUTINE WILL FAIL UNDER THE FOLLOWING CONDITIONS:
C WHEN THE STARTUP DATE CDC = 1 AND THE JULIAN DATE =1 IN THE
C FIRST WEEK OF THE YEAR WHRE NOT THE WHOLE WEEK PERTAINS TO THE
C STARTUP YEAR. WHAT WIL HAPPEN IS THAT THE CDC DATE WILL BECOME
C NEGATIVE
C
C THIS PROGRAM TAKES A WEEK NUMBER AND CONVERTS TO A CDC DATE
C TAKING INTO CONSIDERATION WEEK ONE STARTS ON THE FIRST MONDAY
C IN JANUARY OF A GIVEN YEAR.
C
C CALLING SEQUENCE:   CALL WKNUMCDC(WEKNUM,YEAR,CDC)
C    INPUT:    WEKNUM - WEEK NUMBER 1-53
C              YEAR   - YEAR NUMBER (LAST 2 DIGITS, 88,89,90)
C
C    OUTPUT:    CDC   - CDC DATE
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WKNUMCDC(WEKNUM,YEAR,CDC)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 CDC           !cdc date
        INTEGER*4 YEAR          !year
        INTEGER*4 WEKNUM        !week number
C
        INTEGER*2 DATE(12)      !date record
        INTEGER*4 DAYWEEK       !day of week
        INTEGER*4 BEGDAT        !beginning date
C
        IF(WEKNUM.EQ.0.OR.YEAR.LT.0) THEN
          CDC=0
          RETURN
        ENDIF
C
        DATE(VYEAR)=YEAR
        DATE(VJUL)=1
        CALL JDATE(DATE)
        BEGDAT=DATE(VCDC)
        DAYWEEK=DATE(VDOW)
        IF(DAYWEEK.LT.1.OR.DAYWEEK.GT.7) GOTO 300
        IF(DAYWEEK.LE.4) THEN
100       CONTINUE
          IF(BEGDAT.LE.0) GOTO 300
          IF(DAYWEEK.EQ.MONDAY) GOTO 300
          BEGDAT=BEGDAT-1
          DAYWEEK=DAYWEEK-1
          GOTO 100
        ELSE
200       CONTINUE
          IF(BEGDAT.LE.0) GOTO 300
          IF(DAYWEEK.EQ.MONDAY) GOTO 300
          BEGDAT=BEGDAT+1
          IF(DAYWEEK.EQ.SUNDAY) DAYWEEK=MONDAY-1
          DAYWEEK=DAYWEEK+1
          GOTO 200
        ENDIF
C
300     CONTINUE
C***    IF(BEGDAT.LE.0.AND.YEAR.LE.92)THEN          !nasty patch
C***         BEGDAT= -1
C***    ELSEIF(BEGDAT.LE.0)THEN
C***         BEGDAT=BEGDAT+1
C***    ENDIF
C***
        IF(BEGDAT.LE.0) BEGDAT = BEGDAT + 1
        CDC=BEGDAT+(7*WEKNUM)-7
        IF(CDC.LE.0) CDC=1
C
        RETURN
        END
