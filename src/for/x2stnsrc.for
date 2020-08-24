C
C SUBROUTINE X2STNSRC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STNSRC.FOV                                 $
C  $Date::   17 Apr 1996 16:37:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2comsnp.for;2 **
C
C X2STNSRC.FTN
C
C     THIS ROUTINE SHOULD BE REPLACED BY ROUTINE PASSING THE ADDRESS
C     OF STATION AND SEARCHING POSITION BY LOOKING FOR ADDRESS OF
C     STATION
C     WILL RUN 20 TIMES FASTER
C
C V02 24-JUN-90 MRM CHECK ADDRESS LENGTH.
C V01 19-FEB-90 MRM INITIAL RELEASE.
C
C This routine will binary search the X2X_SORTED_ADR table
C to find the input station address, and will return the
C station number.
C
C Calling sequence:
C
C     CALL X2STNSRC(STATION,POSITION,STATUS)
C
C Input parameters:
C
C     STATION Int*4       Station number
C
C
C Output parameters:
C
C     POSITION INT*4      position in the table
C     STATUS  Int*4       Return status
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2STNSRC(STATION,POSITION,STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4   STATION                     !Return station number
	INTEGER*4   STATUS                      !Search status
	INTEGER*4   POSITION                    !POSITION IN SEARCH TAB
        INTEGER*4   NEXT_STATION
	INTEGER*4   LAST_STATION /-1/
	INTEGER*4   LAST_POSITION /-1/
	INTEGER*4   LAST_STATUS /-1/
C
C
C IF FIRST PASS, SEARCH THROUGH THE SORTED ADDRESS
C TABLE TO FIND THE FIRST VALID STATION NUMBER.
C
	IF (STATION.EQ.LAST_STATION) THEN
	   POSITION=LAST_POSITION
	   STATUS=LAST_STATUS
	   RETURN
	ENDIF
C
	LAST_STATION=STATION
	DO 100 NEXT_STATION=1,X2X_STATIONS
	    IF(X2X_SORTED_ADR(0,NEXT_STATION).EQ.STATION) GOTO 110
100	CONTINUE
	POSITION=1
	LAST_POSITION=POSITION
	STATUS=-1
	LAST_STATUS = STATUS
	RETURN
110	CONTINUE
	POSITION = NEXT_STATION
	LAST_POSITION = POSITION
	STATUS = 0
	LAST_STATUS = STATUS
	RETURN
	END
