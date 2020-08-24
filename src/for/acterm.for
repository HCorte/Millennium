C
C FUNCTION ACTERM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]ACTERM.FOV                                   $
C  $Date::   17 Apr 1996 12:08:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2acterm.for;1 **
C
C X2ACTERM.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This function will return the number of active terminals
C for each station port.
C
C Input parameters:
C
C     STATION     Int*4   Station number
C     PORT        Int*4   Port number
C
C Output parameters:
C
C     ACTERM      Int*4   Number of active terminals
C     HISER       Int*4   Highest serial number for the station
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
	INTEGER FUNCTION ACTERM(STATION,PORT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4   STATION     !Station number
	INTEGER*4   PORT        !Port number
	INTEGER*4   TER         !Terminal number
	INTEGER*4   STATE, I
C
C LOOP THROUGH ALL TERMINALS DEFINED.
C
	ACTERM=0
	DO 100 I=1,X2X_MAXTERMS
	  TER=X2XS_TERMS(I,PORT,STATION)
	  IF(TER.NE.0) THEN
	    CALL ILBYTE(STATE,IX2XT_STATE,TER-1)
	    IF(STATE.EQ.X2XTS_ACTIVE) ACTERM=ACTERM+1
	  ENDIF
100	CONTINUE
C
	RETURN
	END
