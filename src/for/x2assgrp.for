C
C SUBROUTINE X2ASSGRP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2ASSGRP.FOV                                 $
C  $Date::   17 Apr 1996 16:08:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2assgrp.for;1 **
C
C X2ASSGRP.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will scan the sorted station address table and
C will assign relay groups to stations automatically.  The relay
C group will be updated in the station file.
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
	SUBROUTINE X2ASSGRP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XGBL.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
C
	INTEGER*4   STN                     !Station pointer
	INTEGER*4   GRP,GRPCNT              !Group number/group count
	INTEGER*4   IDX                     !Memory index
	INTEGER*4   XI4_X2XS_GROUP          !Equivalence to rid of warnings
	INTEGER*4   I
	EQUIVALENCE (XI4_X2XS_GROUP,X2XS_GROUP)
C
C FIND THE NEXT AVAILABLE RELAY GROUP
C
       DO 5 I=1,X2X_NUM_GROUPS
          IF(X2XG_CNT(I).EQ.0) THEN
            GRP=I
            GOTO 50
          ENDIF
5       CONTINUE
        TYPE *
        TYPE *,'X2ASSGRP: NO AVAILABLE GROUPS FOR DEFAULT ASSIGNMENT'
        TYPE *
        RETURN
C
C LOOP THROUGH ALL STATIONS STORED IN MEMORY.
C
50       CONTINUE
	 IDX=0
	 GRPCNT=0
100	 CONTINUE
C
         IDX=IDX+1
         IF(IDX.GT.X2X_STATIONS) GOTO 8000
	 IF(X2X_SORTED_ADR(0,IDX).EQ.0) GOTO 8000
         STN=X2X_SORTED_ADR(0,IDX)
         IF(STN.EQ.0) GOTO 100
         IF(X2XS_GROUP(STN).NE.0) GOTO 100
C
C DO NOT ASSIGN TO THE EXISTING GROUP
C
150      CONTINUE
         IF (GRP.GT.X2X_NUM_GROUPS) THEN                ! TOO MANY GROUPS
            TYPE *,'INSUFFICIENT NUMBER OF RELAY GROUPS'
            GOTO 8000
         ELSEIF (X2XG_CNT(GRP).GE.X2XGBL_GROUP_LEN)THEN ! MAX GRP SIZE - V04
            GRPCNT = 0
            GRP    = GRP+1
            GOTO 150
         ELSEIF (GRPCNT .EQ. 0 .AND.                    ! NEW GROUP AND
     *            X2XG_CNT(GRP) .NE. 0)THEN             ! ALREADY DEFINED
            GRPCNT = 0
            GRP    = GRP+1
            GOTO 150
         ELSE
            GRPCNT = GRPCNT + 1
         ENDIF
C
C INCREMENT THE GROUP COUNTER.
C STORE RELAY INFORMATION INTO COMMON.
C
          X2XG_CNT(GRP)  =  GRPCNT
          X2XS_GROUP(STN)=  GRP
          X2XG_LIST(X2XG_CNT(GRP),GRP)=STN
C
C PROCESS NEXT STATION.
C
          GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
C...	CALL CLOSLOD(1)
	RETURN
	END
