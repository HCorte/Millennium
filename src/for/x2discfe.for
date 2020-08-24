C
C SUBROUTINE X2DISCFE.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2DISCFE.FOV                                 $
C  $Date::   17 Apr 1996 16:15:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
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

      SUBROUTINE X2DISCFE(STATION_NO,CONN_ID,BUFFER,UNIT_LENGTH)
      IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
C
      INTEGER*4      STATION_NO    !INPUT: CONNECTION CLEARED FOR
C                                  !       THIS STATION
      INTEGER*4      CONN_ID       !INPUT: CONN_ID CLEARED
C
      INTEGER*4      BUFFER(*)     !OUTPUT: FE MESSAGE FILLED WITH
C
      INTEGER*4 HOST_ID,FLAGS,ADRESS_LEN,DISCONNECT
      INTEGER*4 HEADER,UNIT_LENGTH
      CALL ISBYTE(X2FEMES_PROTID_X2X,BUFFER,X2FEMES_PROTID-1)
      HOST_ID=0
      CALL ISBYTE(HOST_ID,BUFFER,X2FEMES_HOST_ID-1)
      CALL ISBYTE(X2FEMES_MESTYP_DOWN,BUFFER,X2FEMES_MESTYP-1)
C
C     SET DELIVERY FLAGS
C
      FLAGS=X2FEMES_FLAGS_ND
      CALL ISBYTE(FLAGS,BUFFER,X2FEMES_FLAGS-1)
C
      DISCONNECT=X2FEMES_UNCDDISC
      CALL ISBYTE(DISCONNECT,BUFFER,X2FEMES_CONNCTL-1)
      CALL I4TOBUF4(CONN_ID,BUFFER,X2FEMES_CONN_ID-1)
C
C     WE ASSUME THAT FRONT END ID COULD BE REUSED. THAT IMPLIES
C
C     THAT WE NEED EXPLICIT ADRESSING
C
C
C
      ADRESS_LEN=X2XS_ADRESS_LEN(STATION_NO)
      CALL I4TOBUF4(X2XS_ADRESS(1,STATION_NO),BUFFER,X2FEMES_ADR-1)
      IF (ADRESS_LEN.GT.1)
     * CALL I4TOBUF4(X2XS_ADRESS(2,STATION_NO),BUFFER,X2FEMES_ADR+3)
      CALL ISBYTE(ADRESS_LEN,BUFFER,X2FEMES_ADRLEN-1)
      HEADER=X2FEMES_ADRLEN
      IF (ADRESS_LEN.NE.0) HEADER=HEADER+(ADRESS_LEN+1)/2
      HEADER=(HEADER+1)/2*2                 !MAKE IT EVEN
      CALL ISBYTE(HEADER,BUFFER,X2FEMES_HEADLEN-1)
C
      UNIT_LENGTH=HEADER
      CALL I4TOBUF2(UNIT_LENGTH,BUFFER,X2FEMES_MESLEN-1)
      RETURN
      END
