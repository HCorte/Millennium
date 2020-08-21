C
C SUBROUTINE STOPLAN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]STOPLAN.FOV                                  $
C  $Date::   17 Apr 1996 15:19:34                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - stoplan.for;1 **
C
C STOPLAN.FOR
C
C V01 29-NOV-90 XXX RELEASED FOR VAX
C
C      INT* LAN    - IN
C      INT*4 REPLY - OUT
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
	SUBROUTINE STOPLAN(LAN,REPLY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 REPLY, LAN, I
C
C CHECK LAN
C
	REPLY=-1
	IF(LAN.GE.1.AND.LAN.LE.MAXLAN) THEN
C
C HALT AND CLOSE
C
	   RETLUN(LUNLAN(1,LAN))=0
	   RETLUN(LUNLAN(2,LAN))=0
C
C RESET THE ADAPTER
C
	   DO 100 I=1,MAXSAP
	     CALL SYS$DASSGN(LUNLAN(I,LAN))
	     CALL SYS$DASSGN(LUNLAN(I,LAN))
	     LANOPN(I,LAN)=LSCLO
	     LANOPN(I,LAN)=LSCLO
100	   CONTINUE
	   REPLY=0
	ELSE
	   CALL OPS('**** INVALID LAN ****',LAN,0)
	ENDIF
	CALL OPS('**** LAN STOPPED ****',LAN,LUNLAN(1,LAN))
	RETURN
	END
