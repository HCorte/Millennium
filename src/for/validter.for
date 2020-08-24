C
C SUBROUTINE VALIDTER
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]VALIDTER.FOV                                 $
C  $Date::   17 Apr 1996 15:49:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2validter.for;1 **
C
C VALIDTER
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will return a valid terminal number defined
C for the input station.  This is necessary because LOGOUT
C requires a terminal number to determine which network it
C belongs to.
C
C Input parameters:
C
C     STN     Int*4   Station number
C
C Output parameters:
C
C     TER     Int*4   Terminal number belonging to station
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
	SUBROUTINE VALIDTER(STN,TER)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4   STN         !Station number
	INTEGER*4   TER         !Terminal number
	INTEGER*4   J, I
C
C FIND A VALID TERMINAL DEFINED FOR THIS STATION AS
C LOGOUT REQUIRES A VALID TERMINAL NUMBER TO QUEUE TO X2XMGR.
C
	DO 62 I=1,X2X_MAXPORT
	  DO 64 J=1,X2X_MAXTERMS
	    TER=X2XS_TERMS(J,I,STN)
	    IF(TER.NE.0) GOTO 8000
64	  CONTINUE
62	CONTINUE
C
8000	CONTINUE
	RETURN
	END
