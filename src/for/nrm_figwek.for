C
C SUBROUTINE FIGWEK
C $Log:   GXAFXT:[GOLS]FIGWEK.FOV  $
C  
C V03 03-DEC-2004 TRG/FRP Modified for SCML.
C
C     Rev 1.0   17 Apr 1996 13:09:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   11 Nov 1994 13:37:24   HXK
C  Allow for different lower week for cdcs 1517, 1881 and 1882
C  
C     Rev 1.0   21 Jan 1993 16:17:26   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_figwek.for **
C
C FIGWEK.FOR
C
C V02 11-DEC-92 TBD FIXED LEAP YEAR BUG
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
        SUBROUTINE FIGWEK(CDC,WEKNO,YEARIN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*2 DATE(12),DATEAR(12),DATENXT(12)
        INTEGER*4 STRTDY, DAYNO, YEAR, WEKNO, CDC, YEARIN
        INTEGER*4 NXTSUNDAYCDC, NXTSUNDAYYEAR
C
        DATEAR(5)=CDC
        CALL CDATE(DATEAR)
        YEAR=DATEAR(VYEAR)
        DAYNO=DATEAR(VJUL)

        DATE(VJUL)=1
        DATE(VYEAR)=YEAR
        CALL JDATE(DATE)
        STRTDY=DATE(VDOW)
        WEKNO=STRTDY
        WEKNO=WEKNO+DAYNO+6   !5
        WEKNO=INT(WEKNO/7)

        IF(DATEAR(VDOW).EQ.SUNDAY) THEN
         NXTSUNDAYCDC=CDC+7
        ELSE !Search next Sunday CDC
         NXTSUNDAYCDC=(CDC - DATEAR(VDOW)) + 7
        ENDIF

	DATENXT(5)=NXTSUNDAYCDC
        CALL CDATE(DATENXT)
        NXTSUNDAYYEAR=DATENXT(VYEAR)

        IF(NXTSUNDAYYEAR.GT.YEAR) THEN
	  WEKNO=1 !SCML: restart WEKNO if next Sunday is for next year
 	ENDIF

	IF(YEAR.LT.77) THEN
	    YEARIN = YEAR+2000
	ELSE
	    YEARIN = YEAR+1900
	ENDIF 
        IF(WEKNO.EQ.1 .AND.DATEAR(VMON).EQ.12) YEARIN=YEARIN+1
        IF(WEKNO.GE.52.AND.DATEAR(VMON).EQ.1)  YEARIN=YEARIN-1

        RETURN
        END

