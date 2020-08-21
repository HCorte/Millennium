C
C FUNCTION GIVCON
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]GIVCON.FOV                                   $
C  $Date::   17 Apr 1996 13:25:08                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - givcon.for;1 **
C
C GIVCON.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 06-JUN-89 MBK ORIGINAL RELEASE
C
C CONN=GIVCON(SSAP,DSAP)
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
	INTEGER FUNCTION GIVCON(SSAP,DSAP)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 DIND, SIND, DSAP, SSAP
C
	SIND=MIN0(SSAP,DSAP)
	DIND=MAX0(SSAP,DSAP)
C
	IF(SIND.EQ.DIND.OR.
     *	   SIND.GT.MAXSAP.OR.
     *	   DIND.GT.MAXSAP.OR.
     *	   SIND.EQ.0.OR.DIND.EQ.0) THEN
	   GIVCON=0
	   RETURN
	ENDIF
C
	GIVCON=(SIND-SIND*SIND+2*MAXSAP*SIND-2*MAXSAP)/2+(DIND-SIND)
	RETURN
	END
